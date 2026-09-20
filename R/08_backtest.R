# 08_backtest.R
# Calibration testing and accuracy tracking for the PGA model
#
# Functions:
#   run_calibration_backtest()  — train on 2022-2024, test on 2025
#   log_projections(proj)       — save weekly projections to log
#   log_outcomes(tournament_id) — pull actual results and attach to log
#   backtest_summary()          — running calibration metrics on logged projections

library(tidyverse)
library(glmmTMB)
library(lubridate)

BACKTEST_LOG <- "data/processed/backtest_log.rds"

# ── 1. Calibration backtest ────────────────────────────────────────────────────
# Train on 2022-2024, evaluate on 2025
# Key metric: Brier Score — lower = better calibrated

run_calibration_backtest <- function() {
  message("Running calibration backtest (train: 2022-2024, test: 2025)...")

  source("R/04_model.R", local = TRUE)

  df <- load_model_data()

  train <- df %>% filter(year(start_date) <= 2024)
  test  <- df %>% filter(year(start_date) == 2025)

  message("  Train rows: ", nrow(train), " | Test rows: ", nrow(test))

  # Train models on 2022-2024
  models_bt <- list(
    make_cut = train_make_cut_model(train),
    top_20   = train_top20_model(train),
    top_40   = train_top40_model(train)
  )

  # Predict on 2025
  test$p_make_cut <- predict(models_bt$make_cut, newdata = test,
                              type = "response", allow.new.levels = TRUE)
  test$p_top_20   <- predict(models_bt$top_20,   newdata = test,
                              type = "response", allow.new.levels = TRUE)
  test$p_top_40   <- predict(models_bt$top_40,   newdata = test,
                              type = "response", allow.new.levels = TRUE)

  # Brier Score = mean((predicted - actual)^2), lower = better
  # Brier Skill Score vs a naive baseline (the base rate)
  brier <- function(pred, actual) mean((pred - actual)^2, na.rm = TRUE)
  bss   <- function(pred, actual) {
    bs_model   <- brier(pred, actual)
    bs_naive   <- brier(rep(mean(actual, na.rm = TRUE), length(actual)), actual)
    1 - (bs_model / bs_naive)
  }

  results <- tibble(
    outcome     = c("make_cut", "top_20", "top_40"),
    brier_score = c(
      brier(test$p_make_cut, as.numeric(test$made_cut)),
      brier(test$p_top_20,   as.numeric(test$top_20)),
      brier(test$p_top_40,   as.numeric(test$top_40))
    ),
    brier_skill = c(
      bss(test$p_make_cut, as.numeric(test$made_cut)),
      bss(test$p_top_20,   as.numeric(test$top_20)),
      bss(test$p_top_40,   as.numeric(test$top_40))
    ),
    actual_rate = c(
      mean(test$made_cut, na.rm = TRUE),
      mean(test$top_20,   na.rm = TRUE),
      mean(test$top_40,   na.rm = TRUE)
    ),
    n_test = c(
      sum(!is.na(test$made_cut)),
      sum(!is.na(test$top_20)),
      sum(!is.na(test$top_40))
    )
  ) %>%
    mutate(across(where(is.double), ~ round(.x, 4)))

  message("\nCalibration Results (2025 holdout):")
  print(results)
  message("\n  Brier Skill Score > 0 = beats naive baseline")
  message("  BSS > 0.03 is considered good for golf")

  # Calibration curve: do predicted probabilities match actual frequencies?
  cal_curve <- test %>%
    mutate(
      cut_bucket = cut(p_make_cut, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
    ) %>%
    group_by(cut_bucket) %>%
    summarise(
      pred_prob   = mean(p_make_cut, na.rm = TRUE),
      actual_rate = mean(as.numeric(made_cut), na.rm = TRUE),
      n           = n(),
      .groups     = "drop"
    )

  message("\nMake Cut Calibration Curve:")
  print(cal_curve %>% mutate(across(where(is.double), ~ round(.x, 3))))

  list(results = results, calibration_curve = cal_curve, test_data = test)
}

# ── 2. Log projections ─────────────────────────────────────────────────────────

log_projections <- function(proj) {

  entry <- proj %>%
    mutate(logged_at = Sys.time())

  if (file.exists(BACKTEST_LOG)) {
    existing <- readRDS(BACKTEST_LOG)
    # Overwrite if same tournament already logged
    existing <- existing %>%
      filter(tournament != unique(proj$tournament)[1])
    updated <- bind_rows(existing, entry)
  } else {
    updated <- entry
  }

  saveRDS(updated, BACKTEST_LOG)
  message("Projections logged: ", nrow(entry), " players for ",
          unique(proj$tournament)[1])
}

# ── 3. Log outcomes ────────────────────────────────────────────────────────────

log_outcomes <- function(tournament_id) {
  if (!file.exists(BACKTEST_LOG)) {
    message("No backtest log found — run log_projections() first")
    return(invisible(NULL))
  }

  source("R/01_pull_pga.R", local = TRUE)
  message("Pulling actual results for ", tournament_id, "...")
  results <- pull_leaderboard(tournament_id)

  if (nrow(results) == 0) {
    message("No results found — tournament may not be complete yet")
    return(invisible(NULL))
  }

  log <- readRDS(BACKTEST_LOG)

  # Match by player_id (most reliable) or player_name
  actuals <- results %>%
    select(player_id, player_name,
           actual_made_cut  = made_cut,
           actual_top_20    = top_20,
           actual_top_40    = top_40,
           actual_position  = finish_numeric,
           actual_score     = total_score)

  updated <- log %>%
    left_join(actuals, by = "player_id") %>%
    mutate(
      # Only fill actuals for this tournament
      actual_made_cut = if_else(tournament_id == !!tournament_id,
                                actual_made_cut, NA),
      actual_top_20   = if_else(tournament_id == !!tournament_id,
                                actual_top_20, NA),
      actual_top_40   = if_else(tournament_id == !!tournament_id,
                                actual_top_40, NA)
    )

  saveRDS(updated, BACKTEST_LOG)
  message("Outcomes logged for ", nrow(actuals), " players")
}

# ── 4. Backtest summary ────────────────────────────────────────────────────────

backtest_summary <- function() {
  if (!file.exists(BACKTEST_LOG)) {
    message("No backtest log yet")
    return(invisible(NULL))
  }

  log <- readRDS(BACKTEST_LOG)

  if (!"actual_made_cut" %in% names(log)) {
    message("No outcomes logged yet — run log_outcomes(tournament_id) after a tournament finishes")
    return(invisible(NULL))
  }

  log <- log %>% filter(!is.na(actual_made_cut))

  if (nrow(log) == 0) {
    message("No completed results in log yet")
    return(invisible(NULL))
  }

  brier <- function(pred, actual) mean((pred - actual)^2, na.rm = TRUE)

  summary <- tibble(
    outcome     = c("make_cut", "top_20", "top_40"),
    n           = c(sum(!is.na(log$actual_made_cut)),
                    sum(!is.na(log$actual_top_20)),
                    sum(!is.na(log$actual_top_40))),
    brier_score = c(
      brier(log$p_cut / 100, as.numeric(log$actual_made_cut)),
      brier(log$p_top20 / 100, as.numeric(log$actual_top_20)),
      brier(log$p_top40 / 100, as.numeric(log$actual_top_40))
    ),
    avg_pred_prob = c(
      mean(log$p_cut   / 100, na.rm = TRUE),
      mean(log$p_top20 / 100, na.rm = TRUE),
      mean(log$p_top40 / 100, na.rm = TRUE)
    ),
    actual_rate = c(
      mean(as.numeric(log$actual_made_cut), na.rm = TRUE),
      mean(as.numeric(log$actual_top_20),   na.rm = TRUE),
      mean(as.numeric(log$actual_top_40),   na.rm = TRUE)
    )
  ) %>% mutate(across(where(is.double), ~ round(.x, 4)))

  message("Backtest Summary (", n_distinct(log$tournament), " tournaments logged):")
  print(summary)

  summary
}

# To run (from the project root):
# source("R/00_setup.R")
# source("R/08_backtest.R")
#
# run_calibration_backtest()              # check model quality before using live
# log_projections(proj)                   # after each tournament
# log_outcomes("R2026475")                # day after tournament ends
# backtest_summary()                      # running calibration check
