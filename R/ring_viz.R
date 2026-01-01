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


#' Parse a poker card string into individual cards
#'
#' Parses a string like "AsTdTc" (flop) or "AhKh" (hand) into a tibble of
#' individual cards with rank, suit, and display properties.
#'
#' @param card_string A string of concatenated 2-character card codes.
#'   Each card is rank (A,K,Q,J,T,9-2) followed by suit (s,h,d,c).
#'   Examples: "AsTdTc" (3 cards), "AhKh" (2 cards), "AsKhQdJcTs" (5 cards)
#'
#' @return A tibble with columns:
#'   - `rank`: Character rank (A, K, Q, J, T, 9-2)
#'   - `suit`: Character suit code (s, h, d, c)
#'   - `suit_symbol`: Unicode suit symbol
#'   - `suit_color`: Color for display ("black" or "red")
#'   - `card_index`: Position in original string (1-based)
#'
#' @examples
#' parse_cards("AsTdTc")
#' parse_cards("AhKh")
#'
parse_cards <- function(card_string) {
  if (!is.character(card_string) || length(card_string) != 1) {
    stop("card_string must be a single character string")
  }

  # Normalize: uppercase for ranks, lowercase for suits
  card_string <- toupper(card_string)

  # Extract 2-character card tokens
  n_chars <- nchar(card_string)
  if (n_chars %% 2 != 0) {
    stop("card_string must have even length (each card is 2 characters)")
  }

  n_cards <- n_chars / 2
  if (n_cards == 0) {
    stop("card_string must contain at least one card")
  }

  # Parse each card
  cards <- character(n_cards)
  for (i in seq_len(n_cards)) {
    start <- (i - 1) * 2 + 1
    cards[i] <- substr(card_string, start, start + 1)
  }

  # Extract rank and suit
  ranks <- substr(cards, 1, 1)
  suits <- tolower(substr(cards, 2, 2))


  # Validate ranks
  invalid_ranks <- ranks[!ranks %in% RANK_ORDER]
  if (length(invalid_ranks) > 0) {
    stop("Invalid rank(s): ", paste(unique(invalid_ranks), collapse = ", "))
  }

  # Validate suits
  valid_suits <- names(SUIT_SYMBOLS)
  invalid_suits <- suits[!suits %in% valid_suits]
  if (length(invalid_suits) > 0) {
    stop("Invalid suit(s): ", paste(unique(invalid_suits), collapse = ", "))
  }

  tibble::tibble(
    rank = ranks,
    suit = suits,
    suit_symbol = SUIT_SYMBOLS[suits],
    suit_color = SUIT_COLORS[suits],
    card_index = seq_len(n_cards)
  )
}
