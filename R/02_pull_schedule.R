# 02_pull_schedule.R
# Pull the current tournament field + tee times for the upcoming event
# Used by build_slate() to populate data/slate_today.csv

library(httr)
library(jsonlite)
library(tidyverse)

PGA_GQL_URL <- "https://orchestrator.pgatour.com/graphql"

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# Reuse key + gql_post from 01_pull_pga.R (sourced before this file)
# If called standalone, source it first
if (!exists("make_pga_headers")) source("R/01_pull_pga.R", local = FALSE)

gql_post <- function(query, variables = list(), max_tries = 3) {
  body <- toJSON(list(query = query, variables = variables), auto_unbox = TRUE)
  for (i in seq_len(max_tries)) {
    resp <- POST(PGA_GQL_URL, add_headers(.headers = make_pga_headers()),
                 body = body, encode = "raw", config = httr::timeout(30))
    code <- status_code(resp)
    if (code == 200) {
      parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
      if (!is.null(parsed$data)) return(parsed$data)
    }
    if (i == 1) {
      body_text <- tryCatch(content(resp, "text", encoding = "UTF-8"), error = function(e) "")
      message("  HTTP ", code, " — ", substr(body_text, 1, 200))
    }
    if (i < max_tries) Sys.sleep(2 * i)
  }
  warning("GraphQL request failed")
  NULL
}

# ── 1. Get upcoming tournament ID ─────────────────────────────────────────────

get_upcoming_tournament <- function(year = format(Sys.Date(), "%Y")) {
  source("R/01_pull_pga.R", local = TRUE)

  schedule <- pull_schedule(as.integer(year))

  # Next tournament that hasn't finished yet
  upcoming <- schedule %>%
    filter(end_date >= Sys.Date()) %>%
    arrange(start_date) %>%
    slice(1)

  if (nrow(upcoming) == 0) {
    stop("No upcoming tournament found for ", year)
  }

  message("Next tournament: ", upcoming$tournament_name,
          " (", upcoming$tournament_id, ")",
          " | ", format(upcoming$start_date, "%b %d"), " – ", format(upcoming$end_date, "%b %d"))

  upcoming
}

# ── 2. Pull field for a tournament ────────────────────────────────────────────

pull_field <- function(tournament_id) {
  message("Pulling field for ", tournament_id, "...")

  # PlayerField has id/displayName/country directly (not nested under 'player')
  query <- '
    query Field($id: ID!) {
      field(id: $id) {
        players {
          id
          displayName
          country
        }
      }
    }
  '

  data <- gql_post(query, list(id = tournament_id))
  if (is.null(data)) return(tibble())

  players <- data$field$players
  if (is.null(players) || length(players) == 0) return(tibble())

  if (is.data.frame(players)) {
    tibble(
      player_id   = players$id          %||% NA_character_,
      player_name = players$displayName %||% NA_character_,
      country     = players$country     %||% NA_character_,
      tee_time_r1 = NA_character_
    )
  } else {
    tibble(
      player_id   = sapply(players, function(p) p$id          %||% NA_character_),
      player_name = sapply(players, function(p) p$displayName %||% NA_character_),
      country     = sapply(players, function(p) p$country     %||% NA_character_),
      tee_time_r1 = NA_character_
    )
  } %>%
    mutate(
      tournament_id = tournament_id,
      tee_time_r1   = as.POSIXct(NA_character_, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York")
    )
}

# ── 3. Get field strength proxy ───────────────────────────────────────────────
# Uses OWGR (world ranking) from PGA Tour player profiles
# Average ranking of field → lower number = stronger field

pull_owgr <- function(player_ids) {
  message("Pulling OWGR for ", length(player_ids), " players...")

  query <- '
    query PlayerProfile($playerId: ID!) {
      playerProfileStats(playerId: $playerId) {
        owgr
      }
    }
  '

  results <- list()
  for (i in seq_along(player_ids)) {
    pid  <- player_ids[i]
    data <- tryCatch(
      gql_post(query, list(playerId = pid)),
      error = function(e) NULL
    )

    if (!is.null(data) && !is.null(data$playerProfileStats)) {
      results[[i]] <- tibble(
        player_id = pid,
        owgr      = data$playerProfileStats$owgr %||% NA_integer_
      )
    } else {
      results[[i]] <- tibble(player_id = pid, owgr = NA_integer_)
    }

    if (i %% 20 == 0) message("  ", i, " / ", length(player_ids), " done")
    Sys.sleep(0.3)
  }

  bind_rows(results)
}

# ── 4. Build and save the field with rankings ─────────────────────────────────

build_field <- function(tournament_id = NULL) {

  if (is.null(tournament_id)) {
    upcoming      <- get_upcoming_tournament()
    tournament_id <- upcoming$tournament_id
  }

  field <- pull_field(tournament_id)

  if (nrow(field) == 0) {
    message("Field not yet available for ", tournament_id)
    return(invisible(NULL))
  }

  message(nrow(field), " players in field")

  # OWGR not available via this API — skip field strength calculation
  field <- field %>%
    mutate(
      owgr           = NA_integer_,
      field_strength = NA_real_,
      pulled_at      = Sys.time()
    )

  saveRDS(field, "data/raw/pga_field_current.rds")
  message("Field saved (", nrow(field), " players)")

  field
}

# To run (from the project root):
# source("R/00_setup.R")
# source("R/02_pull_schedule.R")
# field <- build_field()    # auto-detects upcoming tournament
# field <- build_field("R2026475")  # or specify tournament ID
