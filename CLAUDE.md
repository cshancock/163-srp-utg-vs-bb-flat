# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This project analyzes Game Theory Optimal (GTO) solver output for a poker scenario: UTG open vs BB flat call in a single raised pot. The analysis clusters poker flops based on equity, expected value (EV), and strategic action frequencies using R and Quarto notebooks.

## Tech Stack

- **Language**: R
- **Notebook format**: Quarto (`.qmd` files)
- **Key libraries**: tidyverse, tidymodels, tidyclust, readxl, plotly
- **Data source**: Excel workbook from GTO+ solver in `data-raw/`

## Project Structure

```
├── R/                    # Reusable functions (R package convention)
│   ├── compositional.R   # Simplex transformations, Mahalanobis distance
│   └── metrics.R         # Entropy, perplexity, divergence measures
├── analysis/             # Project-specific Quarto notebooks
├── data-raw/             # Source data (GTO+ Excel exports)
├── data/                 # Processed data (future use)
├── tests/testthat/       # Unit tests
└── .github/              # Ideas and AI assistant instructions
```

## Common Commands

```bash
# Render a notebook
quarto render analysis/01-bb-flop-action.qmd

# Run tests
Rscript -e "testthat::test_dir('tests/testthat')"

# Load functions during development (in R console)
source("R/compositional.R")
source("R/metrics.R")
```

## Domain Concepts

### GTO Solver Data Structure
- Excel sheets contain flop-level data: `Tree` (board texture), `Equity`, `EV`, and action probability columns
- Action columns named like `Bet 4.5`, `Bet 5.9`, `Bet 3`, `Check` represent frequencies (probabilities summing to 1)
- Sheet names indicate position/scenario: `BB`, `UTG v Check`, `UTG v Bet4.5`, `Ranges`

### Compositional Data Problem
Action probabilities form a **simplex** (sum to 1), creating dependency between variables. The project uses:

1. **Square root transformation**: Reduces interdependency
2. **Reference variable transformation**: Transform d variables to d-1 space using one action as denominator
   ```r
   # Generalized in add_compositional_distance():
   `Bet 5.9_ref` = sqrt(`Bet 5.9` + eps) / sqrt(Check + eps)
   `Bet 3_ref` = sqrt(`Bet 3` + eps) / sqrt(Check + eps)
   ```
3. **Mahalanobis distance**: Measures how unusual a flop's strategy is relative to all flops

### tidymodels Clustering Pattern
```r
recipe(~., data = df) |> step_normalize(all_numeric_predictors())
workflow() |> add_recipe() |> add_model(k_means(num_clusters = K))
fit() |> extract_cluster_assignment()
```

## Analysis Progression

1. `analysis/01-bb-flop-action.qmd`: BB's action on flop (rarely bets); K-means with Equity, EV, bet frequency
2. `analysis/02-utg-vs-check.qmd`: UTG's response to BB check; 3-action compositional transform, Mahalanobis distance

## Future Development

See `.github/ideas-for-later-work.md` for planned features:
- Circular ring flop visualization
- ggplot2 hand-range grid geom
- Strategy complexity/divergence metrics (partially implemented in `R/metrics.R`)
- Polarization metrics
