# 06_build_slate.R
# Build the weekly slate from the current tournament field
#
# Workflow:
#   build_slate()  → pulls field from API, creates data/slate_today.csv
#   Review the CSV — no manual edits needed (no lineup decisions in golf)
#   run_tournament_projections()

library(tidyverse)
library(lubridate)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── Build slate from upcoming field ───────────────────────────────────────────

build_slate <- function(tournament_id = NULL) {

  source("R/02_pull_schedule.R", local = TRUE)

  # Auto-detect upcoming tournament if not specified
  if (is.null(tournament_id)) {
    upcoming      <- get_upcoming_tournament()
    tournament_id <- upcoming$tournament_id
    tourney_name  <- upcoming$tournament_name
    start_date    <- upcoming$start_date
    is_major      <- str_detect(tolower(tourney_name),
                                paste(c("masters","u.s. open","us open","open championship",
                                        "pga championship","the players"), collapse = "|"))
  } else {
    schedule      <- readRDS("data/raw/pga_schedule.rds")
    tourney_row   <- schedule %>% filter(tournament_id == !!tournament_id) %>% slice(1)
    tourney_name  <- tourney_row$tournament_name %||% "Unknown"
    start_date    <- tourney_row$start_date %||% Sys.Date()
    is_major      <- FALSE
  }

  message("Building slate for: ", tourney_name, " (", tournament_id, ")")

  # Pull field
  field <- build_field(tournament_id)

  if (is.null(field) || nrow(field) == 0) {
    message("Field not available yet — check back closer to the tournament")
    return(invisible(NULL))
  }

  # Attach last known SG total for reference
  sg_latest <- tryCatch({
    sg_raw <- readRDS("data/raw/pga_sg.rds")
    sg_raw %>%
      arrange(player_id, start_date) %>%
      group_by(player_id) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      select(player_id, sg_total_last = sg_total, last_event_date = start_date)
  }, error = function(e) tibble())

  slate <- field %>%
    left_join(sg_latest, by = "player_id") %>%
    mutate(
      tournament_id   = tournament_id,
      tournament_name = tourney_name,
      start_date      = start_date,
      is_major        = is_major,
      field_size      = nrow(field),
      sg_total_last   = round(coalesce(sg_total_last, NA_real_), 2)
    ) %>%
    select(
      player_id, player_name, country,
      tournament_id, tournament_name, start_date, is_major, field_size,
      owgr, tee_time_r1, sg_total_last, last_event_date
    ) %>%
    arrange(coalesce(owgr, 999L))

  write_csv(slate, "data/slate_today.csv")
  message("Slate saved: data/slate_today.csv (", nrow(slate), " players)")
  message("Next step: source('R/05_projections.R'); run_tournament_projections()")

  invisible(slate)
}

# To run (from the project root):
# source("R/00_setup.R")
# source("R/06_build_slate.R")
# build_slate()
# build_slate("R2026475")  # or with specific tournament ID
