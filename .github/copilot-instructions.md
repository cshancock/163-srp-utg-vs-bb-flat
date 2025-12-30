# Copilot Instructions - Poker GTO Analysis

## Project Overview
This project analyzes Game Theory Optimal (GTO) solver output for poker situations, specifically a UTG vs BB flat calling scenario. The analysis uses R, Quarto notebooks, and tidymodels for clustering poker flops based on equity, expected value (EV), and strategic action frequencies.

## Tech Stack & Environment
- **Primary language**: R
- **Notebook format**: Quarto (`.qmd` files)
- **Key libraries**: tidyverse, tidymodels, readxl
- **Data source**: Excel workbooks (`.gto.xlsx`) from **GTO+ solver** with sheets for data and ranges

## Key Concepts & Domain Knowledge

### Poker GTO Solver Data
- Solver output contains **Tree** (flop board), **Equity**, **EV**, and action probabilities
- Action columns are named like `Bet 4.5` (bet size), `Check`, etc.
- Action probabilities form a **simplex**: they sum to 1, creating dependency between variables

### Compositional Data Transformation
The project began in 01-notebook.qmd by using a critical transformation for clustering simplex data:
```r
# Square root transformation + reference variable approach
bet_4.5_sqrt = sqrt(`Bet 4.5`)
check_sqrt = sqrt(Check)
bet_4.5_sqrt_ref = bet_4.5_sqrt / check_sqrt
```
- **Why**: Simplex variables are interdependent; transformations can mitigate this dependency
- **Two approaches**: (1) Square root, (2) d-1 reference variable transformation
- For 2 actions, reference transformation is simple; 3+ actions need more general approach

We are in the process of developing more general approaches for this problem.

## Workflow & Development Patterns

### Working with Quarto Notebooks
- Execute R chunks directly in the notebook, not in terminal
- Maintain linear narrative: exploration → transformation → modeling → results
- Add explanatory text in markdown sections between code chunks

### Data Pipeline Pattern
1. **Load**: Read Excel workbook sheets (solver data + ranges)
2. **Transform**: Apply compositional transformations to action probabilities
3. **Model**: K-means clustering with normalized features via tidymodels recipes
4. **Analyze**: Compare different K values to find optimal clustering

### tidymodels Workflow Structure
```r
# Standard pattern used in this project:
recipe() |> step_normalize(all_numeric_predictors())
workflow() |> add_recipe() |> add_model()
fit() |> extract_cluster_assignment()
```

## File Naming Convention
- Notebooks: `##-notebook.qmd` (sequential numbering)
- Data files: `<project-name>.gto.xlsx` matching the workspace folder name

## Common Tasks

**Add new clustering analysis**: Create new `kmeans_spec_*` with different `num_clusters`, add to workflow, fit, and append cluster column to `flops_clustered`

**Process new solver output**: Update `workbook` path, verify sheet structure (sheet 1 = data, sheet 2 = ranges), check action column names match GTO+ solver export format

**Transform action probabilities**: Always apply sqrt + reference transformation for simplex data before clustering

**Visualize clusters**: Generate cluster plots to visualize flop groupings across different K values and examine cluster characteristics


