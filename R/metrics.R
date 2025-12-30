# Strategy complexity and divergence metrics
#
# Functions for measuring how mixed/concentrated a strategy is, and how
# different it is from a reference strategy.

#' Shannon entropy of a strategy
#'
#' Measures how mixed the strategy is. Max at uniform distribution, min at pure.
#'
#' @param p Numeric vector of probabilities
#' @param base Logarithm base (default: natural log)
#' @param eps Smoothing value for zero handling (default: 0, no smoothing)
#'
#' @return Shannon entropy value
#'
strategy_entropy <- function(p, base = exp(1), eps = 0) {
  p <- as.numeric(p)
  p <- p / sum(p)

  if (eps > 0) {
    p <- p + eps
    p <- p / sum(p)
  }

  # Convention: 0 * log(0) := 0
  terms <- ifelse(p > 0, p * log(p, base = base), 0)
  -sum(terms)
}

#' Perplexity (effective number of actions)
#'
#' Converts entropy to an interpretable "effective number of actions" scale.
#' For d=3 actions, perplexity ranges from 1 (pure) to 3 (uniform).
#'
#' @param p Numeric vector of probabilities
#' @param base Logarithm base (default: natural log)
#' @param eps Smoothing value for zero handling (default: 0)
#'
#' @return Perplexity value
#'
strategy_perplexity <- function(p, base = exp(1), eps = 0) {
  exp(strategy_entropy(p, base = base, eps = eps))
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
