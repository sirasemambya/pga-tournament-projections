# PGA Tournament Projections

An R pipeline that projects each golfer's chances in an upcoming PGA Tour event: probability of making the cut, finishing in the top 20, finishing in the top 40, and winning, plus an expected total score. It is built on round-level strokes gained (SG) data and hierarchical mixed-effects models.

It was built to be run once per tournament. A first run pulls several seasons of history and trains the models, and each tournament week after that is a refresh, a slate build, and a projection run.

## What it does

1. Pulls every completed tournament from 2022 onward from the PGA Tour's GraphQL service: leaderboards, finish positions, and strokes gained by category (off the tee, approach, around the green, putting, tee to green, total).
2. Builds a player-by-tournament dataset with decay-weighted rolling SG over each player's previous 8 events, a hot/cold form signal, a consistency measure, and each player's history at the specific tournament.
3. Trains four models (see below).
4. Builds the upcoming field, tee times, and world ranking from the API, then projects every player and writes a color-coded Excel workbook.
5. Logs each week's projections and, after the event, the actual results, so calibration can be tracked over time.

## Pipeline order

| Script | Purpose |
|---|---|
| `R/00_setup.R` | Installs and loads dependencies |
| `R/01_pull_pga.R` | Pulls schedule, results, and strokes gained for past tournaments |
| `R/02_pull_schedule.R` | Pulls the upcoming event's field, tee times, and world ranking |
| `R/03_feature_engineering.R` | Builds rolling SG features, course history, and tournament context |
| `R/04_model.R` | Trains the make-cut, top-20, top-40, and scoring models |
| `R/06_build_slate.R` | Builds `data/slate_today.csv` for the upcoming tournament |
| `R/05_projections.R` | Generates projections and the Excel export |
| `R/08_backtest.R` | Holdout calibration test, weekly projection and outcome logging, running accuracy summary |

`RUNBOOK.R` is a copy-paste checklist for the whole cycle.

## Running it

From the project root:

```r
source("R/00_setup.R")

source("R/01_pull_pga.R")            # first run takes 30 to 60 minutes
pga_data <- pull_all_pga(seasons = c("2022", "2023", "2024", "2025", "2026"))

source("R/03_feature_engineering.R"); model_data <- build_model_dataset()
source("R/04_model.R");               models <- train_all_models()

source("R/06_build_slate.R");         build_slate()      # when the field is announced
source("R/05_projections.R");         proj <- run_tournament_projections()
```

The projections workbook is written to `output/`. The PGA Tour endpoint needs an API key. `01_pull_pga.R` looks for a `PGA_API_KEY` environment variable first, and otherwise tries to read the key from the public pgatour.com web app. If neither works, set one manually with `.PGA_API_KEY <<- "your-key"`.

## Key modeling choices

- **Strokes gained, not results.** The models use decay-weighted SG by category rather than finish positions, because SG is a much less noisy measure of skill than where a golfer happened to finish.
- **No leakage.** Rolling features are lagged so each row only uses events that finished before it, and course history excludes the current appearance.
- **Recent form vs baseline.** `form_delta` is the gap between a player's last-3 and last-8 event SG, capped to limit the effect of outliers.
- **Course history.** Each player's average SG at the same tournament in prior years, and their cut rate there, neutral when they have no history.
- **Player random effects.** Every model has a random intercept per player, so the model can hold a persistent view of each golfer beyond the observed features.
- **Four models.** Binomial mixed models (`glmmTMB`) for make cut, top 20, and top 40, and a linear mixed model (`lme4`) for expected total score among players who make the cut.
- **A proxy where the data is missing.** Some events, such as the Masters, do not report SG through the API. For those, total SG is approximated from score per round relative to the field, so those events still feed the rolling features.
- **Winner probability.** Computed as a softmax over projected score, so win probabilities always sum to 100% across the field.

## Backtest

Trained on 2022 to 2024 and tested on 2025 (5,802 player-tournaments), Brier skill score against a naive base-rate forecast:

| Outcome | Brier skill | Base rate |
|---|---|---|
| Make cut | 0.097 | 57.3% |
| Top 20 | 0.109 | 18.7% |
| Top 40 | 0.144 | 34.0% |

All three beat the naive baseline. As a sanity check on a 91-player field, the projected top-20 probabilities summed to about 21 expected finishers and the top-40 probabilities to about 40, close to the 20 and 40 spots available, even though each outcome is modeled independently.

## Sample output

A pre-tournament run for the 2026 Masters (top 6 by win probability):

| player | win % | SG total | course SG | P(cut) | P(top 20) | P(top 40) | proj score |
|---|---|---|---|---|---|---|---|
| Scheffler, Scottie | 6.1% | +1.91 | +2.64 | 86.8% | 71.6% | 82.8% | -4.0 |
| McIlroy, Rory | 2.8% | +1.14 | +0.83 | 80.9% | 61.4% | 76.3% | -2.0 |
| Schauffele, Xander | 2.7% | +1.63 | +0.33 | 86.5% | 60.0% | 80.2% | -1.9 |
| Cantlay, Patrick | 2.1% | +0.38 | -0.05 | 78.2% | 44.9% | 71.3% | -1.3 |
| Fitzpatrick, Matt | 2.1% | +1.61 | +0.27 | 79.9% | 49.1% | 69.7% | -1.3 |
| Rahm, Jon | 2.1% | +1.29 | +0.70 | 81.1% | 53.3% | 72.4% | -1.3 |

`sg_total` is rolling strokes gained per round over the previous 8 events and `course SG` is the player's average at that tournament in past years. These projections have not been scored against the actual tournament results.

## Limitations

- **Winner probability is a heuristic.** The softmax temperature is set by hand so an average player gets roughly one over the field size. It is not fitted and has not been backtested. Only the cut, top-20, and top-40 models have been evaluated.
- **Skill is modest.** Brier skill scores around 0.10 to 0.14 mean the models add real but limited information. Golf outcomes are very noisy, and a single tournament tells you little.
- **Unofficial data source.** The PGA Tour GraphQL service is not a supported public API. The key is read from their web app and rotates on site deploys, the service can change without notice, and you should check the PGA Tour's terms of use before relying on it. The raw data is not included in this repository for the same reason.
- **Field strength is crudely measured.** Field size is used as a stand-in for field quality, since ranking data for past fields was not available in the training set.
- **Cut rules are simplified.** Cut lines vary by event and only a major indicator is modeled.
- **No course or weather features.** Beyond a player's own history at a venue, nothing about the course type, conditions, or draw is modeled.
- **New players fall back to tour medians.** Golfers with no prior events get imputed features and much less reliable projections.
- **macOS only for auto-open, and no tests.** The export opens with the macOS `open` command, and correctness so far has been checked through the holdout backtest and manual review.

## What I'd build next

- Score the Masters projections against the actual results, and fit the winner-probability temperature against historical outcomes instead of setting it by hand.
- Add course-type features (for example, how much a course rewards driving versus approach play) so form is weighted by what a venue demands.
- Replace the field-size proxy with a real field strength measure using world ranking history.
- Model the cut line explicitly per event, and add a simulation layer so all outcomes come from one consistent joint model rather than four independent ones.
- Add tests around the feature engineering so an upstream API change fails loudly.
