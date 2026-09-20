# 04_model.R
# Train the PGA projection models
#
# Stage 1: Make Cut model    (glmmTMB binomial)
# Stage 2: Top 20 model      (glmmTMB binomial) — conditional on making cut
# Stage 3: Top 40 model      (glmmTMB binomial) — conditional on making cut
# Stage 4: Scoring model     (lme4 LMM)         — expected tournament score (head-to-head)

library(tidyverse)
library(lme4)
library(glmmTMB)

# ── Load processed data ────────────────────────────────────────────────────────

load_model_data <- function() {
  df <- readRDS("data/processed/model_dataset.rds")

  df %>%
    filter(!is.na(roll_sg_total)) %>%    # require prior form
    mutate(
      made_cut_int = as.integer(made_cut),
      top_20_int   = as.integer(top_20),
      top_40_int   = as.integer(top_40),
      is_major_int = as.integer(is_major),

      # Impute course history with 0 if no prior appearances (neutral)
      course_history_sg  = coalesce(course_history_sg,  0),
      course_cut_rate    = coalesce(course_cut_rate,    0.5),
      course_n_prior     = coalesce(as.integer(course_n_prior), 0L),

      # Winsorize form delta
      form_delta = pmin(pmax(coalesce(form_delta, 0), -3), 3),

      # Log field size (larger field = harder to finish top 20)
      log_field_size = log(coalesce(field_size, 120))
    )
}

# ── Stage 1: Make Cut Model ────────────────────────────────────────────────────
# Key predictors: SG total form, putting form (cuts often decided by putting),
# course history, major/elevated indicator

train_make_cut_model <- function(df) {
  message("Training make cut model (binomial)...")

  m <- glmmTMB(
    made_cut_int ~
      roll_sg_total +          # overall form — strongest predictor
      roll_sg_putt +           # putting form — drives cut lines
      roll_sg_app +            # approach form
      course_history_sg +      # course fit
      course_cut_rate +        # historical cut rate at this venue
      form_delta +             # hot/cold streak
      is_major_int +           # majors have harder cut lines
      (1 | player_id),
    family = binomial,
    data   = df
  )

  message("Make cut model AIC: ", round(AIC(m), 1))
  m
}

# ── Stage 2: Top 20 Model ──────────────────────────────────────────────────────
# Conditional on being in the field — not conditioned on making cut
# (a top-20 finish is defined over the full field, not just players who survive the cut)

train_top20_model <- function(df) {
  message("Training top 20 model (binomial)...")

  m <- glmmTMB(
    top_20_int ~
      roll_sg_total +
      roll_sg_app +            # approach is biggest skill separator for top finishes
      roll_sg_putt +
      roll_sg_ott +            # driving matters more on certain course types
      course_history_sg +
      form_delta +
      is_major_int +
      log_field_size +         # harder to finish top 20 in 156-man field vs 70-man
      (1 | player_id),
    family = binomial,
    data   = df
  )

  message("Top 20 model AIC: ", round(AIC(m), 1))
  m
}

# ── Stage 3: Top 40 Model ──────────────────────────────────────────────────────

train_top40_model <- function(df) {
  message("Training top 40 model (binomial)...")

  m <- glmmTMB(
    top_40_int ~
      roll_sg_total +
      roll_sg_app +
      roll_sg_putt +
      course_history_sg +
      form_delta +
      is_major_int +
      log_field_size +
      (1 | player_id),
    family = binomial,
    data   = df
  )

  message("Top 40 model AIC: ", round(AIC(m), 1))
  m
}

# ── Stage 4: Scoring Model (for head-to-head matchup projections) ─────────────────────
# Predicts expected total score (vs par) for a tournament
# Used for head-to-head comparisons: who finishes better?

train_scoring_model <- function(df) {
  message("Training scoring model (LMM)...")

  df_scores <- df %>%
    filter(!is.na(total_score), made_cut == TRUE) %>%
    mutate(total_score = as.numeric(total_score))

  m <- lmer(
    total_score ~
      roll_sg_total +
      roll_sg_app +
      roll_sg_putt +
      roll_sg_ott +
      course_history_sg +
      form_delta +
      is_major_int +
      log_field_size +
      (1 | player_id),
    data    = df_scores,
    REML    = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )

  message("Scoring model: σ_player = ",
          round(as.data.frame(VarCorr(m))$sdcor[1], 3),
          ", σ_residual = ",
          round(sigma(m), 3))

  m
}

# ── Train and save all models ──────────────────────────────────────────────────

train_all_models <- function() {

  df <- load_model_data()
  message("Training on ", nrow(df), " player-tournament rows")

  models <- list(
    make_cut = train_make_cut_model(df),
    top_20   = train_top20_model(df),
    top_40   = train_top40_model(df),
    scoring  = train_scoring_model(df)
  )

  saveRDS(models, "data/processed/models.rds")
  message("All models saved to data/processed/models.rds")

  models
}

# To run (from the project root):
# source("R/00_setup.R")
# models <- train_all_models()
