# UTG vs BB Flat Calling Analysis

This repository collects Game Theory Optimal (GTO) solver analysis for UTG vs. BB flat calls. The data pipeline is R/Quarto based, with Excel exports to be transformed, clustered, and visualized through tidyverse/tidymodels workflows.

## Getting started

- Open analysis/xx-notebook.qmd in Quarto to rerun or extend the narrative steps.
- Solver data lives under data/ and data-raw/; update the workbook path in R/helpers.R (or where appropriate) when new exports arrive.
- Use quarto render analysis/<notebook>.qmd or quarto preview to regenerate the HTML reports locally.

## Notes

- Compositional transformations in R/compositional.R (sqrt + reference or ALR) ensure action probabilities stay on a simplex-friendly scale before clustering.
- The workflow in R/metrics.R / analysis uses tidymodels recipes that normalize numeric predictors, fit K-means, and tag each flop with a cluster.