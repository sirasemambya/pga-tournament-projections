# 03_feature_engineering.R
# Build the modeling dataset from raw PGA pulls
#
# Key outputs: player × tournament rows with:
#   - Decay-weighted rolling SG (last 8 events) by category
#   - Course history features (player's SG avg at this venue)
#   - Field strength proxy (avg OWGR of top-50 in field)
#   - Outcome labels: made_cut, top_20, top_40, finish_position

library(tidyverse)
library(zoo)
library(lubridate)

# ── Load raw data ──────────────────────────────────────────────────────────────

load_raw <- function() {
  list(
    schedule = readRDS("data/raw/pga_schedule.rds"),
    results  = readRDS("data/raw/pga_results.rds"),
    sg       = readRDS("data/raw/pga_sg.rds")
  )
}

# ── Decay-weighted rolling average ────────────────────────────────────────────
# More recent events weighted heavier

decay_roll <- function(x, n = 8, decay = 0.85) {
  weights <- decay ^ seq(n - 1, 0)
  rollapply(
    x,
    width   = n,
    FUN     = function(vals) weighted.mean(vals, weights[seq_along(vals)], na.rm = TRUE),
    fill    = NA,
    align   = "right",
    partial = TRUE
  )
}

# ── 1. Build player-tournament rows ───────────────────────────────────────────

build_player_tournament_rows <- function(results, sg) {

  # Join SG onto results — outer join so players with results but no SG are kept
  base <- results %>%
    left_join(
      sg %>% select(tournament_id, player_id,
                    sg_ott, sg_app, sg_atg, sg_putt, sg_t2g, sg_total),
      by = c("tournament_id", "player_id")
    ) %>%
    mutate(start_date = as.Date(start_date))

  # ── SG proxy for tournaments where PGA Tour API returns no SG data ───────────
  # Augusta National (Masters) controls its own data and doesn't feed SG into
  # the PGA Tour API. Same for The Open Championship some years.
  # Proxy: score vs field average, per round, negated so higher = better SG.
  # This is ~equivalent in units to sg_total and much better than leaving 0.
  base <- base %>%
    group_by(tournament_id) %>%
    mutate(
      rounds_played    = if_else(made_cut == TRUE, 4L, 2L),
      score_per_round  = total_score / rounds_played,
      field_avg_pr     = mean(score_per_round[made_cut == TRUE], na.rm = TRUE),
      sg_total_proxy   = -(score_per_round - field_avg_pr),
      sg_total = if_else(is.na(sg_total) & !is.na(total_score),
                         sg_total_proxy, sg_total)
    ) %>%
    ungroup() %>%
    select(-rounds_played, -score_per_round, -field_avg_pr, -sg_total_proxy)

  base
}

# ── 2. Rolling SG features per player ─────────────────────────────────────────
# Uses the L8 tournaments BEFORE the current one to avoid leakage

build_player_rolling <- function(player_tourney) {

  player_tourney %>%
    arrange(player_id, start_date) %>%
    group_by(player_id) %>%
    mutate(
      # Rolling SG by category (lagged — using past events, not current)
      roll_sg_total = lag(decay_roll(coalesce(sg_total, 0), n = 8)),
      roll_sg_app   = lag(decay_roll(coalesce(sg_app,   0), n = 8)),
      roll_sg_ott   = lag(decay_roll(coalesce(sg_ott,   0), n = 8)),
      roll_sg_atg   = lag(decay_roll(coalesce(sg_atg,   0), n = 8)),
      roll_sg_putt  = lag(decay_roll(coalesce(sg_putt,  0), n = 8)),
      roll_sg_t2g   = lag(decay_roll(coalesce(sg_t2g,   0), n = 8)),

      # Recent form: L3 vs L8 divergence (hot/cold streak signal)
      roll_sg_total_L3 = lag(decay_roll(coalesce(sg_total, 0), n = 3)),
      form_delta       = roll_sg_total_L3 - roll_sg_total,

      # Consistency: sd of SG total over last 8
      sg_consistency = rollapply(
        lag(coalesce(sg_total, 0)),
        width = 8, FUN = sd, fill = NA, align = "right", partial = TRUE
      ),

      # Events played in last 8 (participation — if sparse, less reliable)
      events_L8 = rollapply(
        lag(!is.na(sg_total)),
        width = 8, FUN = sum, fill = NA, align = "right", partial = TRUE
      )
    ) %>%
    ungroup()
}

# ── 3. Course history features ────────────────────────────────────────────────
# How has this player historically performed at this specific course?
# Key signal: some players have persistent course fit / struggles

build_course_history <- function(player_tourney) {

  # For each player × tournament, compute their avg SG total
  # at this same tournament in prior years (leave current row out)
  player_tourney %>%
    arrange(player_id, start_date) %>%
    group_by(player_id, tournament_name) %>%
    mutate(
      # Cumulative average of SG total at this course BEFORE current appearance
      course_sg_cumsum = cumsum(coalesce(sg_total, 0)) - coalesce(sg_total, 0),
      course_n_prior   = row_number() - 1,
      course_history_sg = if_else(
        course_n_prior > 0,
        course_sg_cumsum / course_n_prior,
        NA_real_
      ),
      # Cut history at this course
      course_cuts_made  = cumsum(lag(coalesce(as.integer(made_cut), 0L), default = 0L)),
      course_cut_rate   = if_else(
        course_n_prior > 0,
        course_cuts_made / course_n_prior,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    select(-course_sg_cumsum, -course_cuts_made)
}

# ── 4. Field strength and tournament context ──────────────────────────────────

build_tournament_context <- function(player_tourney, schedule) {

  # Identify majors and elevated events
  major_keywords <- c("masters", "u.s. open", "us open", "open championship",
                      "pga championship", "players championship", "the players")

  tourney_ctx <- schedule %>%
    mutate(
      tournament_lower = tolower(tournament_name),
      is_major  = str_detect(tournament_lower,
                              paste(major_keywords, collapse = "|")),
      is_elevated = str_detect(tournament_lower,
                               "players|genesis|arnold palmer|memorial|travelers|
                               |rbc canadian|bmw championship|tour championship")
    ) %>%
    select(tournament_id, is_major, is_elevated)

  # Field strength per tournament: avg finish position proxy
  # (real field strength = avg OWGR but we don't have that in training data)
  # Use number of players in field as a crude proxy — larger fields = more parity
  field_size <- player_tourney %>%
    group_by(tournament_id) %>%
    summarise(field_size = n(), .groups = "drop")

  player_tourney %>%
    left_join(tourney_ctx, by = "tournament_id") %>%
    left_join(field_size,  by = "tournament_id") %>%
    mutate(
      is_major    = coalesce(is_major,    FALSE),
      is_elevated = coalesce(is_elevated, FALSE)
    )
}

# ── 5. Final dataset assembly ──────────────────────────────────────────────────

build_model_dataset <- function() {

  raw <- load_raw()

  message("Building player-tournament rows...")
  player_tourney <- build_player_tournament_rows(raw$results, raw$sg)

  message("Computing rolling SG features...")
  player_rolling <- build_player_rolling(player_tourney)

  message("Computing course history features...")
  player_course  <- build_course_history(player_rolling)

  message("Adding tournament context...")
  full_dataset   <- build_tournament_context(player_course, raw$schedule)

  # Winsorize extreme SG values (injury/WD outliers)
  full_dataset <- full_dataset %>%
    mutate(
      across(starts_with("roll_sg"), ~ pmin(pmax(.x, -4, na.rm = TRUE), 4, na.rm = TRUE)),
      across(starts_with("sg_"),     ~ pmin(pmax(.x, -8, na.rm = TRUE), 8, na.rm = TRUE))
    )

  # Drop rows with no rolling history (first appearance — no prior form)
  model_ready <- full_dataset %>%
    filter(!is.na(roll_sg_total))

  saveRDS(full_dataset, "data/processed/model_dataset.rds")
  message("Model dataset saved: ", nrow(model_ready), " rows with rolling features (",
          nrow(full_dataset) - nrow(model_ready), " dropped for no prior form)")

  full_dataset
}

# To run (from the project root):
# source("R/00_setup.R")
# model_data <- build_model_dataset()
