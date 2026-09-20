# ══════════════════════════════════════════════════════════════════════════════
# PGA TOURNAMENT PROJECTIONS — RUNBOOK
# Copy and paste each block as needed. Run from the project root
# (open the repo folder in RStudio / set your working directory there first).
# ══════════════════════════════════════════════════════════════════════════════

# ── FIRST TIME ONLY: Install packages ─────────────────────────────────────────

source("R/00_setup.R")

# ── STEP 1: PULL HISTORICAL DATA (once — takes 30-60 min, all seasons) ────────
# Loops through every tournament 2022-2026, pulls strokes gained + results via
# GraphQL. Run this once to build your training dataset, then just update weekly.

source("R/01_pull_pga.R")
pga_data <- pull_all_pga(seasons = c("2022", "2023", "2024", "2025", "2026"))

# ── STEP 2: REBUILD FEATURES (after every data pull) ─────────────────────────

source("R/03_feature_engineering.R")
model_data <- build_model_dataset()

# ── STEP 3: CALIBRATION CHECK (first time — verify model quality) ─────────────

source("R/08_backtest.R")
run_calibration_backtest()
# Look for: Brier Skill Score > 0 on make_cut and top_20
# If BSS < 0, the model is not better than a naive base-rate guess

# ── STEP 4: RETRAIN MODELS (after calibration check passes) ───────────────────

source("R/04_model.R")
models <- train_all_models()

# ══════════════════════════════════════════════════════════════════════════════
# PER-TOURNAMENT WORKFLOW (repeat each tournament week)
# ══════════════════════════════════════════════════════════════════════════════

# ── STEP 5: UPDATE DATA (Monday/Tuesday — pull latest completed tournament) ───

source("R/01_pull_pga.R")
pga_data <- pull_all_pga(seasons = c("2025", "2026"))   # only recent seasons needed

source("R/03_feature_engineering.R")
model_data <- build_model_dataset()

source("R/04_model.R")
models <- train_all_models()

# ── STEP 6: BUILD SLATE (Tuesday/Wednesday — when field is announced) ─────────

source("R/06_build_slate.R")
build_slate()    # auto-detects upcoming tournament, saves data/slate_today.csv

# ── STEP 7: RUN PROJECTIONS (Wednesday or closer to first tee time) ───────────

source("R/05_projections.R")
proj <- run_tournament_projections()

# Output saved to: output/projections_YYYY-MM-DD_HHMMSS.xlsx
system("open output/")

# ── STEP 8: LOG PROJECTIONS (immediately after running) ───────────────────────

source("R/08_backtest.R")
log_projections(proj)

# ── STEP 9: LOG OUTCOMES (Monday after tournament — results are final) ─────────
# Get the tournament ID from data/raw/pga_schedule.rds or the output filename

source("R/08_backtest.R")
log_outcomes("R2026475")    # ← swap in the correct tournament ID

# ── STEP 10: CHECK RUNNING PERFORMANCE ────────────────────────────────────────

source("R/08_backtest.R")
backtest_summary()

# ── FINDING TOURNAMENT IDs ────────────────────────────────────────────────────
# Format: R{year}{event_number}  e.g. R2026475
# To look up:
schedule <- readRDS("data/raw/pga_schedule.rds")
schedule %>% filter(year == 2026) %>% select(tournament_id, tournament_name, start_date)
