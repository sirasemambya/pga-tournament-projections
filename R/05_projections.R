# 05_projections.R
# Generate tournament projections
#
# Outputs per player:
#   p_winner                       (win probability via softmax on projected score)
#   p_cut, p_top20, p_top40        (probability of making the cut / top 20 / top 40)
#   proj_score                     (expected total score, used for head-to-head comparisons)
#   plus rolling strokes-gained form and course history context

library(tidyverse)
library(glmmTMB)
library(lme4)
library(gt)
library(lubridate)
library(openxlsx)

load_models     <- function() readRDS("data/processed/models.rds")
load_model_data <- function() readRDS("data/processed/model_dataset.rds")

clean_name <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")

# ── 1. Build projection input from slate ──────────────────────────────────────
# slate_today.csv columns:
#   player_id | player_name | tournament_id | tournament_name | start_date | is_major

load_slate <- function(path = "data/slate_today.csv") {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(
      player_id  = as.character(player_id),
      start_date = as.Date(start_date)
    )
}

# ── 2. Attach rolling player features from model dataset ──────────────────────

get_player_features <- function(slate, model_data) {

  feature_cols <- c("roll_sg_total", "roll_sg_app", "roll_sg_ott",
                    "roll_sg_atg", "roll_sg_putt", "roll_sg_t2g",
                    "roll_sg_total_L3", "form_delta", "sg_consistency", "events_L8")

  # Most recent feature row per player
  latest <- model_data %>%
    arrange(player_id, start_date) %>%
    group_by(player_id) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(player_id, all_of(feature_cols))

  result <- slate %>%
    left_join(latest, by = "player_id")

  # Impute with tour median for players with no model history (debutants)
  tour_medians <- result %>%
    filter(!is.na(roll_sg_total)) %>%
    summarise(across(all_of(feature_cols), ~ median(.x, na.rm = TRUE)))

  new_players <- result %>% filter(is.na(roll_sg_total)) %>% pull(player_name)
  if (length(new_players) > 0) {
    message("  Imputing features for (no model history): ",
            paste(clean_name(new_players), collapse = ", "))
  }

  for (col in feature_cols) {
    result[[col]] <- coalesce(result[[col]], tour_medians[[col]])
  }

  result
}

# ── 3. Attach course history features ─────────────────────────────────────────

get_course_features <- function(slate, model_data) {

  course_hist <- model_data %>%
    filter(!is.na(course_history_sg)) %>%
    arrange(player_id, tournament_name, start_date) %>%
    group_by(player_id, tournament_name) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(player_id, tournament_name, course_history_sg, course_cut_rate, course_n_prior)

  slate %>%
    left_join(course_hist, by = c("player_id", "tournament_name")) %>%
    mutate(
      course_history_sg = coalesce(course_history_sg, 0),    # neutral if no history
      course_cut_rate   = coalesce(course_cut_rate,   0.5),
      course_n_prior    = coalesce(as.integer(course_n_prior), 0L)
    )
}

# ── 4. Add tournament context columns ─────────────────────────────────────────

add_tournament_context <- function(slate) {
  major_keywords <- c("masters", "u.s. open", "us open", "open championship",
                      "pga championship", "players championship", "the players")

  slate %>%
    mutate(
      is_major_int   = as.integer(str_detect(tolower(tournament_name),
                                             paste(major_keywords, collapse = "|"))),
      log_field_size = log(coalesce(field_size, 120L)),
      form_delta     = pmin(pmax(coalesce(form_delta, 0), -3), 3)
    )
}

# ── 5. Generate projections ────────────────────────────────────────────────────

generate_projections <- function(slate_features, models) {
  nd <- slate_features

  nd$p_make_cut <- predict(models$make_cut, newdata = nd,
                           type = "response", allow.new.levels = TRUE)
  nd$p_top_20   <- predict(models$top_20,   newdata = nd,
                           type = "response", allow.new.levels = TRUE)
  nd$p_top_40   <- predict(models$top_40,   newdata = nd,
                           type = "response", allow.new.levels = TRUE)

  # Scoring model: expected tournament score (used for head-to-head comparisons)
  nd$proj_score <- tryCatch(
    predict(models$scoring, newdata = nd, allow.new.levels = TRUE),
    error = function(e) NA_real_
  )

  nd
}

# ── 6. Head-to-head finish probability from the scoring model ─────────────────
# P(player A finishes ahead of B) via normal approximation

finish_ahead_prob <- function(score_a, score_b, sigma = 6.5) {
  # Lower score = better finish
  # P(A beats B) = P(score_A < score_B) = P(score_A - score_B < 0)
  diff_mean <- score_a - score_b
  diff_sd   <- sqrt(2) * sigma
  pnorm(0, mean = diff_mean, sd = diff_sd)
}

# ── 7. Format output ──────────────────────────────────────────────────────────

format_projections <- function(proj_df) {

  proj_df %>%
    mutate(
      player = clean_name(player_name),

      # Cap probabilities to sensible ranges
      p_make_cut = pmax(pmin(p_make_cut, 0.99), 0.01),
      p_top_20   = pmax(pmin(p_top_20,   0.95), 0.005),
      p_top_40   = pmax(pmin(p_top_40,   0.95), 0.005),

      # Winner probability via softmax on projected score (lower score = better)
      # k=0.4 calibrated so field-average player gets ~1/field_size probability
      p_winner   = { s <- proj_score; s[is.na(s)] <- mean(s, na.rm=TRUE)
                     w <- exp(-0.4 * s); pmax(w / sum(w), 1e-5) }
    ) %>%
    transmute(
      player,
      # Winner % up front — used for sorting
      p_winner   = round(p_winner * 100, 2),
      tournament = tournament_name,
      start_date,
      # Rolling form
      sg_total   = round(roll_sg_total, 2),
      sg_app     = round(roll_sg_app,   2),
      sg_putt    = round(roll_sg_putt,  2),
      sg_ott     = round(roll_sg_ott,   2),
      form_delta = round(form_delta,    2),
      course_sg  = round(course_history_sg, 2),
      course_n   = course_n_prior,
      # Finish probabilities (%)
      p_cut      = round(p_make_cut * 100, 1),
      p_top20    = round(p_top_20   * 100, 1),
      p_top40    = round(p_top_40   * 100, 1),
      # Expected total score
      proj_score = round(proj_score, 1)
    ) %>%
    arrange(desc(p_winner))
}

# ── 8. Excel export ───────────────────────────────────────────────────────────

export_projections_xlsx <- function(df, path) {

  wb <- createWorkbook()
  addWorksheet(wb, "Projections")

  col_idx <- function(nm) which(names(df) == nm)

  groups <- list(
    winner  = list(cols = c("p_winner"),
                   hdr = "#C00000", cell = "#FCE4D6"),
    form    = list(cols = c("sg_total","sg_app","sg_putt","sg_ott","form_delta","course_sg","course_n"),
                   hdr = "#2C5F8A", cell = "#D6E8F5"),
    cut     = list(cols = c("p_cut"),
                   hdr = "#538135", cell = "#E2EFDA"),
    top20   = list(cols = c("p_top20"),
                   hdr = "#2E75B6", cell = "#D9E1F2"),
    top40   = list(cols = c("p_top40"),
                   hdr = "#BF9000", cell = "#FFF2CC"),
    scoring = list(cols = c("proj_score"),
                   hdr = "#7030A0", cell = "#EAD1F5")
  )

  base_hdr   <- createStyle(fontName = "Calibri", fontSize = 11,
                             textDecoration = "bold", fontColour = "#FFFFFF",
                             halign = "center", fgFill = "#2C3E50")
  number_2dp <- createStyle(numFmt = "0.00", halign = "center")
  number_1dp <- createStyle(numFmt = "0.0",  halign = "center")
  integer_s  <- createStyle(numFmt = "0",    halign = "center")
  alt_row    <- createStyle(fgFill = "#F5F5F5")

  writeData(wb, "Projections", df, headerStyle = base_hdr)

  nrows     <- nrow(df)
  ncols     <- ncol(df)
  data_rows <- 2:(nrows + 1)
  even_rows <- seq(3, nrows + 1, by = 2)

  if (length(even_rows) > 0)
    addStyle(wb, "Projections", alt_row, rows = even_rows, cols = 1:ncols,
             gridExpand = TRUE, stack = TRUE)

  for (g in groups) {
    present <- intersect(g$cols, names(df))
    if (length(present) == 0) next
    ci      <- sapply(present, col_idx)
    grp_hdr  <- createStyle(fontName = "Calibri", fontSize = 11,
                             textDecoration = "bold", fontColour = "#FFFFFF",
                             halign = "center", fgFill = g$hdr)
    grp_cell <- createStyle(fgFill = g$cell, halign = "center")
    addStyle(wb, "Projections", grp_hdr,  rows = 1,         cols = ci, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Projections", grp_cell, rows = data_rows, cols = ci, gridExpand = TRUE, stack = TRUE)
  }

  for (col in intersect(names(df), c("sg_total","sg_app","sg_putt","sg_ott",
                                      "form_delta","course_sg","proj_score")))
    addStyle(wb, "Projections", number_2dp, rows = data_rows, cols = col_idx(col), stack = TRUE)

  for (col in intersect(names(df), c("p_cut","p_top20","p_top40","p_winner")))
    addStyle(wb, "Projections", number_1dp, rows = data_rows, cols = col_idx(col), stack = TRUE)

  for (col in intersect(names(df), "course_n"))
    addStyle(wb, "Projections", integer_s, rows = data_rows, cols = col_idx(col), stack = TRUE)

  freezePane(wb, "Projections", firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, "Projections", cols = 1:ncols, widths = "auto")
  saveWorkbook(wb, path, overwrite = TRUE)
  system(paste("open", shQuote(path)))
}

# ── 9. Print gt table ──────────────────────────────────────────────────────────

print_projections <- function(proj) {
  proj %>%
    select(player, p_winner, sg_total, sg_app, sg_putt, form_delta, course_sg,
           p_cut, p_top20, p_top40) %>%
    head(40) %>%
    gt() %>%
    tab_header(
      title    = paste("PGA Tournament Projections —", unique(proj$tournament)[1]),
      subtitle = paste("Generated:", format(Sys.time(), "%b %d %Y %H:%M"))
    ) %>%
    cols_label(
      player     = "Player",   p_winner = "Win%",
      sg_total   = "SG Tot",   sg_app   = "SG App",
      sg_putt    = "SG Putt",  form_delta = "Form Δ",
      course_sg  = "Course SG",
      p_cut      = "P(Cut)%",  p_top20  = "P(T20)%", p_top40 = "P(T40)%"
    ) %>%
    fmt_number(columns = c(sg_total, sg_app, sg_putt, form_delta, course_sg), decimals = 2) %>%
    fmt_number(columns = c(p_winner, p_cut, p_top20, p_top40), decimals = 1) %>%
    data_color(columns = p_top20, palette = "Blues")
}

# ── Master runner ──────────────────────────────────────────────────────────────

run_tournament_projections <- function(slate_path  = "data/slate_today.csv",
                                       export_xlsx = TRUE) {

  message("Loading models and data...")
  models     <- load_models()
  model_data <- load_model_data()

  message("Loading slate...")
  slate <- load_slate(slate_path)
  message("  ", nrow(slate), " players loaded")

  message("Attaching player features...")
  slate <- get_player_features(slate, model_data)

  message("Attaching course history...")
  slate <- get_course_features(slate, model_data)

  message("Adding tournament context...")
  slate <- add_tournament_context(slate)

  message("Generating projections...")
  projections <- generate_projections(slate, models)

  output <- format_projections(projections)

  if (export_xlsx) {
    dir.create("output", showWarnings = FALSE)
    out_path <- paste0("output/projections_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".xlsx")
    export_projections_xlsx(output, out_path)
    message("\nSaved: ", out_path)
  }

  print_projections(output)
  invisible(output)
}

# Run (from the project root):
# source("R/00_setup.R")
# source("R/05_projections.R")
# proj <- run_tournament_projections()
