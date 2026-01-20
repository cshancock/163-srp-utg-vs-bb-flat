# Strategy complexity and divergence metrics
#
# Functions for measuring how mixed/concentrated a strategy is, and how
# different it is from a reference strategy.

#' Shannon entropy of a strategy
#'
#' Measures how mixed the strategy is. Max at uniform distribution, min at pure.
#' Designed to work with dplyr pipelines using tidy-select column specifications.
#'
#' @param ... Column selections (unquoted names or tidy-select helpers)
#' @param base Logarithm base (default: natural log)
#' @param eps Smoothing value for zero handling (default: 0, no smoothing)
#'
#' @return Shannon entropy value (vector, one per row)
#'
#' @examples
#' # In a pipeline:
#' df |> mutate(entropy = strategy_entropy(`Bet 4.5`, Check))
#'
strategy_entropy <- function(..., base = exp(1), eps = 0) {
  # Helper function to compute entropy for a single row
  compute_entropy <- function(...) {
    p <- c(...)
    p <- p / sum(p)

    if (eps > 0) {
      p <- p + eps
      p <- p / sum(p)
    }

    # Convention: 0 * log(0) := 0
    terms <- ifelse(p > 0, p * log(p, base = base), 0)
    -sum(terms)
  }

  # Apply across selected columns using pmap_dbl for row-wise operation
  cur_data() |>
    select(...) |>
    pmap_dbl(compute_entropy)
}

#' Perplexity (effective number of actions)
#'
#' Converts entropy to an interpretable "effective number of actions" scale.
#' For d=3 actions, perplexity ranges from 1 (pure) to 3 (uniform).
#' Designed to work with dplyr pipelines using tidy-select column specifications.
#'
#' @param ... Column selections (unquoted names or tidy-select helpers)
#' @param base Logarithm base (default: natural log)
#' @param eps Smoothing value for zero handling (default: 0)
#'
#' @return Perplexity value (vector, one per row)
#'
#' @examples
#' # In a pipeline:
#' df |> mutate(perplex = strategy_perplexity(`Bet 4.5`, Check))
#'
strategy_perplexity <- function(..., base = exp(1), eps = 0) {
  # Helper function to compute perplexity for a single row
  compute_perplexity <- function(...) {
    p <- c(...)
    p <- p / sum(p)

    if (eps > 0) {
      p <- p + eps
      p <- p / sum(p)
    }

    # Convention: 0 * log(0) := 0
    terms <- ifelse(p > 0, p * log(p, base = base), 0)
    entropy <- -sum(terms)
    exp(entropy)
  }

  # Apply across selected columns using pmap_dbl for row-wise operation
  cur_data() |>
    select(...) |>
    pmap_dbl(compute_perplexity)
}

#' Kullback-Leibler divergence
#'
#' Measures divergence from distribution p to q. Not symmetric.
#'
#' @param p Numeric vector of probabilities (from)
#' @param q Numeric vector of probabilities (to)
#' @param eps Smoothing value to avoid log(0) (default: 1e-12)
#'
#' @return KL divergence value
#'
#' @examples
#' # Example 1: divergence from a baseline (global mean strategy)
#' action_cols <- c("Bet 4.5", "Check")
#' q <- df |>
#'   dplyr::summarize(dplyr::across(dplyr::all_of(action_cols), mean)) |>
#'   unlist(use.names = FALSE)
#' p <- df[1, action_cols] |> unlist(use.names = FALSE)
#' kl_divergence(p, q)
#'
#' # Example 2: comparing two spots (or two conditions) with the same action set
#' # Here: compare two flops' action mixes as a stand-in for two spots.
#' p <- df[1, action_cols] |> unlist(use.names = FALSE)
#' q <- df[2, action_cols] |> unlist(use.names = FALSE)
#' kl_divergence(p, q)
#'
#' # Example 3: cluster diagnostics via distance to cluster centroid
#' # First, create cluster labels (e.g., from k-means or domain rules).
#' # Then compute each observation's KL divergence to its cluster's mean strategy.
#' \dontrun{
#' action_cols <- c("Bet 4.5", "Check")
#' 
#' df_clustered <- df |>
#'   dplyr::mutate(
#'     cluster = dplyr::case_when(
#'       `Bet 4.5` < 1 ~ "low_bet",
#'       `Bet 4.5` < 10 ~ "mid_bet",
#'       TRUE ~ "high_bet"
#'     )
#'   )
#'
#' # Compute KL divergence from each flop to its cluster's mean strategy (centroid)
#' df_with_divergence <- df_clustered |>
#'   dplyr::group_by(cluster) |>
#'   dplyr::mutate(
#'     mean_bet = mean(`Bet 4.5`),
#'     mean_check = mean(Check)
#'   ) |>
#'   dplyr::rowwise() |>
#'   dplyr::mutate(
#'     kl_to_centroid = kl_divergence(
#'       c(`Bet 4.5`, Check),
#'       c(mean_bet, mean_check)
#'     )
#'   ) |>
#'   dplyr::ungroup()
#' }
#'
#' # Example 4: strategy concentration vs a mixed baseline
#' # Compare a near-pure "all check" strategy to a 50/50 reference.
#' p <- c("Bet 4.5" = 0, "Check" = 100)
#' q <- c("Bet 4.5" = 50, "Check" = 50)
#' kl_divergence(p, q)
#'
kl_divergence <- function(p, q, eps = 1e-12) {
  p <- as.numeric(p)
  q <- as.numeric(q)
  p <- p / sum(p)
  q <- q / sum(q)

  # Smoothing to avoid log(0) and division by 0

  p <- p + eps
  q <- q + eps
  p <- p / sum(p)
  q <- q / sum(q)

  sum(p * log(p / q))
}

#' Jensen-Shannon divergence
#'
#' Symmetric, bounded divergence between two distributions.
#' Useful for comparing strategy to a baseline (e.g., population mean).
#'
#' @param p Numeric vector of probabilities
#' @param q Numeric vector of probabilities
#' @param eps Smoothing value (default: 1e-12)
#'
#' @return JS divergence value (bounded, symmetric)
#'
#' @examples
#' # Example 1: distance-to-baseline vs global mean strategy
#' #
#' # A common pattern is to compute each flop/node's divergence to a baseline
#' # strategy, such as the population mean action mix.
#' \dontrun{
#' action_cols <- c("Bet 4.5", "Check")
#' 
#' # Global mean strategy (baseline)
#' q <- df |>
#'   dplyr::summarize(dplyr::across(dplyr::all_of(action_cols), mean)) |>
#'   unlist(use.names = FALSE)
#' 
#' # Compute JS divergence to the global mean strategy
#' df_js_baseline <- df |>
#'   dplyr::rowwise() |>
#'   dplyr::mutate(js_to_baseline = js_divergence(c(`Bet 4.5`, Check), q)) |>
#'   dplyr::ungroup()
#' }
#' 
#' # Example 2: cluster diagnostic distance-to-centroid
#' #
#' # If you have cluster labels, you can compute each observation's divergence
#' # to its cluster's centroid strategy (mean action mix) as a diagnostic.
#' \dontrun{
#' action_cols <- c("Bet 4.5", "Check")
#' 
#' df_clustered <- df |>
#'   dplyr::mutate(
#'     cluster = dplyr::case_when(
#'       `Bet 4.5` < 1 ~ "low_bet",
#'       `Bet 4.5` < 10 ~ "mid_bet",
#'       TRUE ~ "high_bet"
#'     )
#'   )
#'
#' # Compute cluster centroids
#' df_centroids <- df_clustered |>
#'   dplyr::group_by(cluster) |>
#'   dplyr::summarize(
#'     centroid_bet = mean(`Bet 4.5`),
#'     centroid_check = mean(Check)
#'   )
#'
#' # Compute JS divergence to cluster centroids
#' df_js_centroid <- df_clustered |>
#'   dplyr::left_join(df_centroids, by = "cluster") |>
#'   dplyr::rowwise() |>
#'   dplyr::mutate(js_to_centroid = js_divergence(c(`Bet 4.5`, Check), c(centroid_bet, centroid_check))) |>
#'   dplyr::ungroup()
#' }
js_divergence <- function(p, q, eps = 1e-12) {
  p <- as.numeric(p)
  q <- as.numeric(q)
  p <- p / sum(p)
  q <- q / sum(q)

  p <- p + eps
  q <- q + eps
  p <- p / sum(p)
  q <- q / sum(q)

  m <- 0.5 * (p + q)
  0.5 * kl_divergence(p, m, eps = 0) + 0.5 * kl_divergence(q, m, eps = 0)
}
