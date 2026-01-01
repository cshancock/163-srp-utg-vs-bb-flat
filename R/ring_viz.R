# Circular ring visualization for poker flops and hands
#
# Renders poker cards as a circular ring where all 13 ranks appear positionally
# around a circle with suit symbols at populated rank positions. Designed to
# support flops (3 cards), hands (2 cards), and future extension to full boards
# (5 cards: flop + turn + river).

# Constants -------------------------------------------------------------------

#' Suit symbols (Unicode)
#' @keywords internal
SUIT_SYMBOLS <- c(s = "\u2660", h = "\u2665", d = "\u2666", c = "\u2663")

#' Suit colors (standard: spades/clubs black, hearts/diamonds red)
#' @keywords internal
SUIT_COLORS <- c(s = "black", h = "red", d = "red", c = "black")

#' Canonical rank order (wheel-aware: A adjacent to both K and 2)
#' @keywords internal
RANK_ORDER <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")


# Helper functions ------------------------------------------------------------

#' Convert rank to angular position
#'
#' Maps a poker rank to its angular position on the ring. Ace is at the top
#' (12 o'clock), with ranks proceeding clockwise: A, K, Q, J, T, 9, 8, 7, 6,
#' 5, 4, 3, 2. This places 2 adjacent to A (completing the wheel).
#'
#' @param rank Character vector of ranks (A, K, Q, J, T, 9-2)
#'
#' @return Numeric vector of angles in radians (0 to 2*pi)
#'
#' @examples
#' rank_to_position("A")
#' rank_to_position(c("A", "K", "2"))
#'
rank_to_position <- function(rank) {
  rank_index <- match(rank, RANK_ORDER)
  if (any(is.na(rank_index))) {
    invalid <- rank[is.na(rank_index)]
    stop("Invalid rank(s): ", paste(invalid, collapse = ", "))
  }
  # Position 1 (Ace) at angle 0, proceeding clockwise

  (rank_index - 1) / length(RANK_ORDER) * 2 * pi
}
