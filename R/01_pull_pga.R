# 01_pull_pga.R
# Pull historical round-level strokes gained data from PGA Tour GraphQL API
# Covers: SG Total, OTT, Approach, ATG, Putting, T2G + finish positions
#
# Data flow:
#   pull_all_pga(seasons) → loops tournaments → SG per player per round → saves to data/raw/

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

PGA_GQL_URL <- "https://orchestrator.pgatour.com/graphql"

# ── Auto-fetch the live API key from pgatour.com ───────────────────────────────
# The key is an AWS AppSync public key embedded in their site JS.
# It rotates on deploys, so we scrape it fresh rather than hardcode it.

key_not_found <- function() {
  message("  Could not obtain a PGA Tour API key automatically.")
  message("  Set one manually: .PGA_API_KEY <<- 'your-key-here'  (or PGA_API_KEY in .Renviron)")
  message("  Find it at pgatour.com (DevTools > Network > any GraphQL request > x-api-key header)")
  ""
}

get_live_api_key <- function() {
  # An explicit key in the environment always wins
  env_key <- Sys.getenv("PGA_API_KEY")
  if (nzchar(env_key)) return(env_key)

  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"

  # ── Step 1: Fetch the page and extract JS bundle URLs ─────────────────────
  page_resp <- tryCatch(
    httr::GET("https://www.pgatour.com/stats/detail/02675",
              httr::add_headers("User-Agent" = ua),
              httr::timeout(20)),
    error = function(e) { message("  Could not reach pgatour.com: ", e$message); NULL }
  )
  if (is.null(page_resp)) return(key_not_found())

  html <- httr::content(page_resp, "text", encoding = "UTF-8")

  # ── Step 2: Try direct key pattern in HTML first ──────────────────────────
  direct <- regmatches(html, gregexpr("da2-[a-zA-Z0-9]{10,35}", html))[[1]]
  if (length(direct) > 0) {
    message("  API key found in HTML: ", substr(direct[1], 1, 8), "...")
    return(direct[1])
  }

  # ── Step 3: Search JS bundles — key lives in Next.js chunks ───────────────
  # Extract all /_next/static/... .js references
  script_srcs <- unique(regmatches(html,
    gregexpr('/_next/static/[^"\'\\s]+\\.js', html, perl = TRUE))[[1]])

  # Prioritise bundles most likely to hold config (app, main, framework, pages)
  priority <- c(
    grep("pages/_app|main-|framework|webpack|_buildManifest", script_srcs, value = TRUE),
    script_srcs
  )
  priority <- head(unique(priority), 15)

  for (src in priority) {
    js_resp <- tryCatch(
      httr::GET(paste0("https://www.pgatour.com", src),
                httr::add_headers("User-Agent" = ua),
                httr::timeout(15)),
      error = function(e) NULL
    )
    if (is.null(js_resp) || httr::status_code(js_resp) != 200) next

    js <- httr::content(js_resp, "text", encoding = "UTF-8")
    key_matches <- regmatches(js, gregexpr("da2-[a-zA-Z0-9]{10,35}", js))[[1]]
    if (length(key_matches) > 0) {
      message("  API key found in JS bundle (", basename(src), "): ",
              substr(key_matches[1], 1, 8), "...")
      return(key_matches[1])
    }
  }

  # ── Step 4: Try __NEXT_DATA__ embedded JSON ────────────────────────────────
  nd_match <- regmatches(html,
    regexpr('id="__NEXT_DATA__"[^>]*>([\\s\\S]*?)</script>', html, perl = TRUE))
  if (length(nd_match) > 0) {
    json_str <- gsub('^[^{]*|[^}]*$', "", nd_match)
    tryCatch({
      flat <- jsonlite::toJSON(jsonlite::fromJSON(json_str))
      key_matches <- regmatches(flat, gregexpr("da2-[a-zA-Z0-9]{10,35}", flat))[[1]]
      if (length(key_matches) > 0) {
        message("  API key found in __NEXT_DATA__: ", substr(key_matches[1], 1, 8), "...")
        return(key_matches[1])
      }
    }, error = function(e) NULL)
  }

  key_not_found()
}

# Fetch key once at source() time and cache it
# To override: set PGA_API_KEY in your environment, or .PGA_API_KEY <<- "your-key-here"
if (!exists(".PGA_API_KEY")) .PGA_API_KEY <- get_live_api_key()

make_pga_headers <- function() {
  c(
    "Content-Type" = "application/json",
    "x-api-key"    = .PGA_API_KEY,
    "Referer"      = "https://www.pgatour.com/",
    "Origin"       = "https://www.pgatour.com",
    "User-Agent"   = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
  )
}

# ── Quick connection test ──────────────────────────────────────────────────────
test_pga_api <- function() {
  message("Testing PGA Tour GraphQL connection...")
  message("  API key loaded (", nchar(.PGA_API_KEY), " characters)")

  # Use variables — TourCode is an enum so must be passed as a variable, not inline string
  query <- '
    query TestSG($tourCode: TourCode!, $statId: String!, $year: Int) {
      statDetails(tourCode: $tourCode, statId: $statId, year: $year) {
        statTitle
        rows {
          ... on StatDetailsPlayer {
            playerName
            rank
          }
        }
      }
    }
  '

  data <- gql_post(query, list(tourCode = "R", statId = "02675", year = 2026))
  if (!is.null(data) && !is.null(data$statDetails$statTitle)) {
    rows <- data$statDetails$rows
    n    <- if (is.data.frame(rows)) nrow(rows) else length(rows)
    message("  OK — stat: ", data$statDetails$statTitle, " | players: ", n)
    if (n > 0) {
      first <- if (is.data.frame(rows)) rows$playerName[1] else rows[[1]]$playerName
      message("  First player: ", first)
    }
    return(invisible(TRUE))
  }
  message("  FAILED — see error above")
  invisible(FALSE)
}

# SG stat IDs on PGA Tour
SG_STAT_IDS <- list(
  sg_ott   = "02567",
  sg_app   = "02568",
  sg_atg   = "02569",
  sg_putt  = "02564",
  sg_t2g   = "02674",
  sg_total = "02675"
)

# ── Low-level GraphQL POST ─────────────────────────────────────────────────────

gql_post <- function(query, variables = list(), max_tries = 3) {
  body <- toJSON(list(query = query, variables = variables), auto_unbox = TRUE)

  for (i in seq_len(max_tries)) {
    resp <- POST(PGA_GQL_URL, add_headers(.headers = make_pga_headers()),
                 body = body, encode = "raw",
                 config = httr::timeout(30))

    code <- status_code(resp)

    if (code == 200) {
      # flatten = FALSE keeps rows as a proper list of objects — required for
      # fragment spreads (... on StatDetailsPlayer) where sapply iterates rows
      parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = FALSE)
      if (!is.null(parsed$data)) return(parsed$data)
      if (!is.null(parsed$errors)) {
        err_msg <- tryCatch(
          {
            errs <- parsed$errors
            if (is.data.frame(errs))  errs$message[1]
            else if (is.list(errs))   errs[[1]]$message
            else                      paste(errs, collapse = "; ")
          },
          error = function(e) paste(parsed$errors, collapse = "; ")
        )
        message("  GraphQL error: ", err_msg)
      }
      return(NULL)
    }

    # Non-200: print status + body for diagnosis
    if (i == 1) {
      body_text <- tryCatch(
        content(resp, "text", encoding = "UTF-8"),
        error = function(e) "(unreadable)"
      )
      message("  HTTP ", code, " — ", substr(body_text, 1, 200))
    }

    if (code == 403) {
      message("  403 Forbidden — API key may have rotated. Re-run source('R/01_pull_pga.R') to refresh key.")
      break
    }

    if (i < max_tries) Sys.sleep(2 * i)
  }

  warning("GraphQL request failed after ", max_tries, " tries")
  NULL
}

# ── 1. Season schedule → tournament IDs ───────────────────────────────────────

pull_schedule <- function(year) {
  message("Pulling schedule for ", year, "...")

  query <- '
    query Schedule($tourCode: String!, $year: String) {
      schedule(tourCode: $tourCode, year: $year) {
        completed {
          tournaments {
            id
            startDate
            tournamentName
          }
        }
        upcoming {
          tournaments {
            id
            startDate
            tournamentName
          }
        }
      }
    }
  '

  data <- gql_post(query, list(tourCode = "R", year = as.character(year)))
  if (is.null(data)) return(tibble())

  sched <- data$schedule

  # fromJSON returns completed/upcoming as a data frame where each row is a month
  # and $tournaments is a list-column of data frames (one per month)
  extract_tournaments <- function(months_df, status_label) {
    if (is.null(months_df) || length(months_df) == 0) return(tibble())

    # months_df$tournaments is a list of data frames, one per month
    tour_dfs <- if (is.data.frame(months_df)) months_df$tournaments
                else lapply(months_df, function(m) m$tournaments)

    if (is.null(tour_dfs)) return(tibble())

    rows <- lapply(tour_dfs, function(df) {
      if (is.null(df) || nrow(df) == 0) return(NULL)

      # startDate is a Unix timestamp in milliseconds
      raw_ms <- df$startDate %||% df$date %||% NA_real_
      start  <- if (!is.null(raw_ms) && !all(is.na(raw_ms)))
                  as.Date(as.POSIXct(raw_ms / 1000, origin = "1970-01-01", tz = "UTC"))
                else
                  as.Date(NA)

      tibble(
        tournament_id   = df$id            %||% NA_character_,
        tournament_name = df$tournamentName %||% NA_character_,
        start_date      = start,
        end_date        = start + 3L,
        status          = status_label
      )
    })

    bind_rows(Filter(Negate(is.null), rows))
  }

  result <- bind_rows(
    extract_tournaments(sched$completed, "completed"),
    extract_tournaments(sched$upcoming,  "upcoming")
  )

  if (nrow(result) == 0) return(tibble())

  result %>%
    mutate(year = year) %>%
    filter(!is.na(tournament_id))
}

# ── 2. Tournament leaderboard → finish positions ──────────────────────────────

pull_leaderboard <- function(tournament_id) {
  query <- '
    query Leaderboard($id: ID!) {
      leaderboardV3(id: $id) {
        tournamentId
        players {
          ... on PlayerRowV3 {
            id
            player { id displayName }
            scoringData {
              position
              total
              totalStrokes
              rounds
            }
          }
        }
      }
    }
  '

  data <- gql_post(query, list(id = tournament_id))
  if (is.null(data)) return(tibble())

  players <- data$leaderboardV3$players
  if (is.null(players)) return(tibble())

  # fromJSON(flatten=FALSE): players is a data frame; nested objects are also data frames
  if (!is.data.frame(players) || nrow(players) == 0) return(tibble())

  # player and scoringData are nested data frames — use direct column access
  player_ids   <- if (is.data.frame(players$player))
                    players$player$id
                  else sapply(players$player, function(p) p$id %||% NA_character_)

  player_names <- if (is.data.frame(players$player))
                    players$player$displayName
                  else sapply(players$player, function(p) p$displayName %||% NA_character_)

  positions    <- if (is.data.frame(players$scoringData))
                    players$scoringData$position
                  else sapply(players$scoringData, function(s) s$position %||% NA_character_)

  total_scores <- if (is.data.frame(players$scoringData))
                    suppressWarnings(as.integer(players$scoringData$total))
                  else sapply(players$scoringData, function(s) {
                    v <- s$total
                    if (is.null(v) || length(v) == 0) NA_integer_ else as.integer(v)
                  })

  tibble(
    tournament_id = tournament_id,
    player_id     = player_ids   %||% NA_character_,
    player_name   = player_names %||% NA_character_,
    position      = positions    %||% NA_character_,
    total_score   = total_scores
  ) %>%
    mutate(
      made_cut = !position %in% c("CUT", "WD", "DQ", "MDF"),
      finish_numeric = suppressWarnings(as.integer(gsub("[^0-9]", "", position))),
      top_5  = !is.na(finish_numeric) & finish_numeric <= 5,
      top_10 = !is.na(finish_numeric) & finish_numeric <= 10,
      top_20 = !is.na(finish_numeric) & finish_numeric <= 20,
      top_40 = !is.na(finish_numeric) & finish_numeric <= 40
    )
}

# ── 3. SG stats for a single tournament (one stat at a time) ──────────────────

pull_tournament_sg_stat <- function(tournament_id, stat_id, stat_name) {
  query <- '
    query GetStats($tourCode: TourCode!, $statId: String!, $year: Int, $eventQuery: StatDetailEventQuery) {
      statDetails(tourCode: $tourCode, statId: $statId, year: $year, eventQuery: $eventQuery) {
        rows {
          ... on StatDetailsPlayer {
            playerId
            playerName
            stats { statName statValue }
          }
        }
      }
    }
  '

  year <- as.integer(substr(tournament_id, 2, 5))

  variables <- list(
    tourCode   = "R",
    statId     = stat_id,
    year       = year,
    eventQuery = list(
      queryType    = "EVENT_ONLY",
      tournamentId = tournament_id
    )
  )

  data <- gql_post(query, variables)
  if (is.null(data)) return(tibble())

  rows <- data$statDetails$rows
  if (is.null(rows)) return(tibble())
  if (is.data.frame(rows) && nrow(rows) == 0) return(tibble())
  if (!is.data.frame(rows) && length(rows) == 0) return(tibble())

  # fromJSON(flatten=FALSE): rows is a data frame with playerId, playerName, stats columns
  if (is.data.frame(rows)) {
    tibble(
      tournament_id = tournament_id,
      player_id     = rows$playerId   %||% NA_character_,
      player_name   = rows$playerName %||% NA_character_,
      !!stat_name   := sapply(rows$stats, function(s) {
        if (is.null(s) || length(s) == 0) return(NA_real_)
        val <- if (is.data.frame(s)) s$statValue[1] else s[[1]]$statValue
        suppressWarnings(as.numeric(val))
      })
    )
  } else {
    tibble(
      tournament_id = tournament_id,
      player_id     = sapply(rows, function(r) r$playerId   %||% NA_character_),
      player_name   = sapply(rows, function(r) r$playerName %||% NA_character_),
      !!stat_name   := sapply(rows, function(r) {
        s <- r$stats
        if (is.null(s) || length(s) == 0) return(NA_real_)
        suppressWarnings(as.numeric(s[[1]]$statValue %||% NA_real_))
      })
    )
  }
}

# ── 4. Pull all SG stats for a tournament → wide table ────────────────────────

pull_tournament_sg <- function(tournament_id) {
  sg_list <- imap(SG_STAT_IDS, function(stat_id, stat_name) {
    Sys.sleep(0.4)   # be polite to the API
    pull_tournament_sg_stat(tournament_id, stat_id, stat_name)
  })

  # Join all 6 SG categories on player_id
  base <- sg_list[[1]]
  if (nrow(base) == 0) return(tibble())

  for (i in seq_along(sg_list)[-1]) {
    if (nrow(sg_list[[i]]) > 0) {
      base <- full_join(
        base,
        sg_list[[i]] %>% select(tournament_id, player_id, names(sg_list[[i]])[4]),
        by = c("tournament_id", "player_id")
      )
    }
  }

  base
}

# ── 5. Pull one season: schedule + SG + results ───────────────────────────────

pull_season <- function(year, completed_only = TRUE) {
  schedule <- pull_schedule(year)

  if (nrow(schedule) == 0) {
    message("  No schedule data for ", year)
    return(list(schedule = tibble(), results = tibble(), sg = tibble()))
  }

  if (completed_only) {
    schedule <- schedule %>% filter(status == "completed")
  }

  message("  ", nrow(schedule), " tournaments to process for ", year)

  all_results <- list()
  all_sg      <- list()

  for (i in seq_len(nrow(schedule))) {
    tid  <- schedule$tournament_id[i]
    name <- schedule$tournament_name[i]
    message("  [", i, "/", nrow(schedule), "] ", name, " (", tid, ")")

    # Leaderboard
    lb <- tryCatch(pull_leaderboard(tid), error = function(e) {
      message("    leaderboard error: ", e$message); tibble()
    })
    if (nrow(lb) > 0) {
      lb$tournament_name <- name
      lb$start_date      <- schedule$start_date[i]
      all_results[[i]]   <- lb
    }

    Sys.sleep(0.4)

    # SG stats
    sg <- tryCatch(pull_tournament_sg(tid), error = function(e) {
      message("    SG error: ", e$message); tibble()
    })
    if (nrow(sg) > 0) {
      sg$tournament_name <- name
      sg$start_date      <- schedule$start_date[i]
      all_sg[[i]]        <- sg
    }

    Sys.sleep(0.4)
  }

  list(
    schedule = schedule,
    results  = bind_rows(all_results),
    sg       = bind_rows(all_sg)
  )
}

# ── 6. Pull all seasons and save ──────────────────────────────────────────────

pull_all_pga <- function(seasons = c("2022", "2023", "2024", "2025", "2026")) {

  all_schedules <- list()
  all_results   <- list()
  all_sg        <- list()

  for (yr in seasons) {
    message("\n── Season ", yr, " ──────────────────────────────────────────")
    # 2026 includes upcoming events; others completed only
    completed_only <- yr != as.character(format(Sys.Date(), "%Y"))
    season_data    <- pull_season(as.integer(yr), completed_only = completed_only)

    all_schedules[[yr]] <- season_data$schedule
    all_results[[yr]]   <- season_data$results
    all_sg[[yr]]        <- season_data$sg
  }

  schedules <- bind_rows(all_schedules)
  results   <- bind_rows(all_results)
  sg        <- bind_rows(all_sg)

  saveRDS(schedules, "data/raw/pga_schedule.rds")
  saveRDS(results,   "data/raw/pga_results.rds")
  saveRDS(sg,        "data/raw/pga_sg.rds")

  message("\nAll PGA data saved to data/raw/")
  message("  schedule: ", nrow(schedules), " tournaments")
  message("  results:  ", nrow(results),   " player-tournament rows")
  message("  sg:       ", nrow(sg),        " player-tournament SG rows")

  list(schedules = schedules, results = results, sg = sg)
}

# ── Null coalescing helper ─────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# To run (from the project root):
# source("R/00_setup.R")
# source("R/01_pull_pga.R")
# pga_data <- pull_all_pga(seasons = c("2022", "2023", "2024", "2025", "2026"))
