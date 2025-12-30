# Compositional data transformations for poker action probabilities
#
# Action probabilities form a simplex (sum to 1), creating dependency between
# variables. These functions transform the data to reduce this dependency.

#' Add compositional distance columns to a dataframe
#'
#' Transforms action probability columns using sqrt + reference variable
#' transformation, then calculates Mahalanobis distance from the centroid.
#'
#' @param df A dataframe containing action probability columns
#' @param action_cols Character vector of action column names. The last column
#'   is used as the reference (denominator) in the transformation.
#' @param eps Small value added to avoid division by zero (default 1e-6)
#'
#' @return The original dataframe with added columns:
#'   - `<action>_ref` for each non-reference action (sqrt ratio to reference)
#'   - `mahalanobis` distance from the centroid in transformed space
#'
#' @examples
#' # For 3 actions: Bet 5.9, Bet 3, Check (Check as reference)
#' df |> add_compositional_distance(c("Bet 5.9", "Bet 3", "Check"))
#'
add_compositional_distance <- function(df, action_cols, eps = 1e-6) {
  # Mahalanobis distance measures how far each observation is from the centroid
  # of the action frequencies for that row. A high Mahalanobis distance indicates
  # an unusual or outlier strategy compared to the population.

  # Transform: sqrt of probabilities, then reference (d-1) transformation
  ref_col <- action_cols[length(action_cols)]  # Use last action as reference

  # Create transformed columns
  transformed <- action_cols[-length(action_cols)] |>
    purrr::map_dfc(~ {
      col_name <- paste0(., "_ref")
      df |>
        dplyr::transmute(
          !!col_name := sqrt(!!rlang::sym(.) + eps) / sqrt(!!rlang::sym(ref_col) + eps)
        )
    })

  # Calculate Mahalanobis distance (T^2)
  X <- as.matrix(transformed)
  # Add small regularization term to diagonal to ensure invertibility
  cov_matrix <- stats::cov(X)
  diag(cov_matrix) <- diag(cov_matrix) + eps
  T2 <- stats::mahalanobis(X, center = colMeans(X), cov = cov_matrix)

  # Add to original dataframe
  df |>
    dplyr::bind_cols(transformed) |>
    dplyr::mutate(mahalanobis = T2)
}

#' Additive log-ratio (ALR) transformation
#'
#' Transforms compositional data to d-1 dimensional space using log-ratios
#' with a reference component.
#'
#' @param p Numeric vector of probabilities (will be normalized)
#' @param ref_index Index of the reference component (default: last)
#' @param eps Small value for zero handling (default 1e-12)
#'
#' @return Numeric vector of length d-1 containing log-ratios
#'
alr_transform <- function(p, ref_index = length(p), eps = 1e-12) {
  p <- as.numeric(p)
  p <- p / sum(p)
  p <- p + eps
  p <- p / sum(p)

  ref <- p[[ref_index]]
  log(p[-ref_index] / ref)
}
