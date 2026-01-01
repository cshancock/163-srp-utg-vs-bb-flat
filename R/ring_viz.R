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


#' Prepare data for ring visualization
#'
#' Creates a complete data frame for ggplot2 rendering, including all 13 ranks
#' with their angular positions and x/y coordinates. Cards present in the input
#' are marked and positioned, with radial offsets for pairs/trips at the same rank.
#'
#' @param card_string A string of concatenated 2-character card codes
#'   (e.g., "AsTdTc" for a flop, "AhKh" for a hand)
#' @param base_radius Base radius for card placement (default 1.0)
#' @param radial_offset Radial separation between cards at the same rank (default 0.15)
#' @param label_radius Radius for rank labels (defaults to `base_radius`)
#' @param suit_radius Absolute radius for suits (overrides offset when provided)
#' @param suit_radius_offset Additional radial distance to place suits outside the ring
#'
#' @return A list with two tibbles:
#'   - `ranks`: All 13 ranks with `rank`, `angle`, `x_label`, `y_label`, `has_cards`
#'   - `cards`: Cards present with `rank`, `suit`, `suit_symbol`, `suit_color`,
#'     `card_index`, `angle`, `r`, `x`, `y`
#'
#' @examples
#' prepare_ring_data("AsTdTc")
#' prepare_ring_data("7c7d")
#'
prepare_ring_data <- function(card_string, base_radius = 1.0, radial_offset = 0.15,
                              label_radius = NULL, suit_radius = NULL,
                              suit_radius_offset = 0.2) {
  # Parse the input cards
  cards <- parse_cards(card_string)

  if (is.null(label_radius)) {
    label_radius <- base_radius
  }
  if (is.null(suit_radius)) {
    suit_radius <- base_radius + suit_radius_offset
  }

  # Create rank positions for all 13 ranks
  ranks_data <- tibble::tibble(
    rank = RANK_ORDER,
    angle = rank_to_position(RANK_ORDER)
  ) |>
    dplyr::mutate(
      # Labels positioned on/near the card ring
      # sin/cos puts angle=0 (Ace) at top (12 o'clock), clockwise
      x_label = sin(.data$angle) * label_radius,
      y_label = cos(.data$angle) * label_radius,
      has_cards = .data$rank %in% cards$rank
    )

  # Add positioning to cards, handling pairs/trips with radial offset
  cards_positioned <- cards |>
    dplyr::mutate(angle = rank_to_position(.data$rank)) |>
    dplyr::group_by(.data$rank) |>
    dplyr::mutate(
      rank_card_num = dplyr::row_number(),
      n_at_rank = dplyr::n(),
      # Center cards radially: for n cards, spread from -offset*(n-1)/2 to +offset*(n-1)/2
      r = suit_radius + (.data$rank_card_num - (.data$n_at_rank + 1) / 2) * radial_offset
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Convert polar to Cartesian (sin/cos puts angle=0 at top, clockwise)
      x = sin(.data$angle) * .data$r,
      y = cos(.data$angle) * .data$r
    ) |>
    dplyr::select(-"rank_card_num", -"n_at_rank")

  list(
    ranks = ranks_data,
    cards = cards_positioned
  )
}


# Plotting functions ----------------------------------------------------------

#' Build the ring plot from prepared data
#'
#' Internal function that constructs the ggplot2 object from prepared ring data.
#'
#' @param ring_data List returned by `prepare_ring_data()`
#' @param show_labels Logical; show rank labels on ring (default TRUE)
#' @param empty_alpha Numeric 0-1; opacity for empty rank positions (default 0.2)
#' @param title Optional title for the plot
#' @param base_radius Base radius used in data preparation (default 1.0)
#' @param circle_color Color for the reference ring
#' @param circle_linewidth Line width for the reference ring
#' @param circle_linetype Line type for the reference ring
#' @param rank_font_family Font family for rank labels (default NULL inherits theme)
#' @param rank_font_size Font size for rank labels
#' @param rank_font_face Font face for rank labels
#'
#' @return A ggplot2 object
#'
#' @keywords internal
#'
build_ring_plot <- function(ring_data, show_labels = TRUE, empty_alpha = 0.2,
                            title = NULL, base_radius = 1.0,
                            circle_color = "gray80", circle_linewidth = 0.5,
                            circle_linetype = "solid",
                            rank_font_family = NULL, rank_font_size = 4,
                            rank_font_face = "plain") {
  ranks <- ring_data$ranks
  cards <- ring_data$cards

  # Generate circle coordinates for the reference ring
  circle_angles <- seq(0, 2 * pi, length.out = 100)
  circle_df <- tibble::tibble(
    x = sin(circle_angles) * base_radius,
    y = cos(circle_angles) * base_radius
  )

  # Build the plot
  p <- ggplot2::ggplot() +
    # Reference circle (subtle gray ring)
    ggplot2::geom_path(
      data = circle_df,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = circle_color,
      linewidth = circle_linewidth,
      linetype = circle_linetype
    )

  # Add rank labels if requested
  if (show_labels) {
    p <- p +
      ggplot2::geom_text(
        data = ranks,
        ggplot2::aes(
          x = .data$x_label,
          y = .data$y_label,
          label = .data$rank,
          alpha = ifelse(.data$has_cards, 1, empty_alpha)
        ),
        size = rank_font_size,
        color = "gray30",
        family = rank_font_family,
        fontface = rank_font_face
      ) +
      ggplot2::scale_alpha_identity()
  }

  # Add suit symbols at card positions
  if (nrow(cards) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = cards,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          label = .data$suit_symbol,
          color = .data$suit_color
        ),
        size = 8
      ) +
      ggplot2::scale_color_identity()
  }

  # Apply theme and coordinate system
  plot_limit <- base_radius + 0.5
  p <- p +
    ggplot2::coord_fixed(
      xlim = c(-plot_limit, plot_limit),
      ylim = c(-plot_limit, plot_limit)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  # Add title if provided
  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }

  p
}


# User-facing functions -------------------------------------------------------

#' Render cards as a circular ring
#'
#' Generic function to render any number of cards as a circular ring.
#' This is the core rendering function; use `render_flop_ring()`,
#' `render_turn_ring()`, `render_board_ring()`, or `render_hand_ring()`
#' for validated wrappers.
#'
#' @param card_string A string of concatenated 2-character card codes
#' @param show_labels Logical; show rank labels on ring (default TRUE)
#' @param empty_alpha Numeric 0-1; opacity for empty rank positions (default 0.2)
#' @param title Optional title for the plot
#' @param base_radius Base radius for the reference ring
#' @param radial_offset Radial spacing between multiple cards of same rank
#' @param label_radius Radius used for rank labels (defaults to `base_radius`)
#' @param suit_radius Absolute radius for suits (overrides offset when provided)
#' @param suit_radius_offset Distance to place suits outside the ring
#' @param circle_color Color for the reference ring
#' @param circle_linewidth Line width for the reference ring
#' @param circle_linetype Line type for the reference ring
#' @param rank_font_family Font family for rank labels
#' @param rank_font_size Font size for rank labels
#' @param rank_font_face Font face for rank labels
#'
#' @return A ggplot2 object
#'
#' @examples
#' render_cards_ring("AsTdTc")
#' render_cards_ring("AhKhQdJcTs", title = "Full board")
#'
render_cards_ring <- function(card_string, show_labels = TRUE,
                               empty_alpha = 0.2, title = NULL,
                               base_radius = 1.0, radial_offset = 0.15,
                               label_radius = NULL, suit_radius = NULL,
                               suit_radius_offset = 0.2,
                               circle_color = "gray80", circle_linewidth = 0.5,
                               circle_linetype = "solid",
                               rank_font_family = NULL, rank_font_size = 4,
                               rank_font_face = "plain") {
  ring_data <- prepare_ring_data(
    card_string,
    base_radius = base_radius,
    radial_offset = radial_offset,
    label_radius = label_radius,
    suit_radius = suit_radius,
    suit_radius_offset = suit_radius_offset
  )
  build_ring_plot(
    ring_data,
    show_labels = show_labels,
    empty_alpha = empty_alpha,
    title = title,
    base_radius = base_radius,
    circle_color = circle_color,
    circle_linewidth = circle_linewidth,
    circle_linetype = circle_linetype,
    rank_font_family = rank_font_family,
    rank_font_size = rank_font_size,
    rank_font_face = rank_font_face
  )
}


#' Render a poker flop as a circular ring
#'
#' Renders a 3-card flop as a circular ring where all 13 ranks appear
#' positionally around a circle with suit symbols at populated ranks.
#'
#' @param flop_string A 6-character string representing 3 cards
#'   (e.g., "AsTdTc", "6h5d4c")
#' @param show_labels Logical; show rank labels on ring (default TRUE)
#' @param empty_alpha Numeric 0-1; opacity for empty rank positions (default 0.2)
#' @param title Optional title for the plot
#' @param ... Additional styling parameters forwarded to `render_cards_ring()`
#'
#' @return A ggplot2 object
#'
#' @examples
#' render_flop_ring("AsTdTc")
#' render_flop_ring("6s5d4c", title = "Connected flop")
#' render_flop_ring("KsKhKd", title = "Trips board")
#'
render_flop_ring <- function(flop_string, show_labels = TRUE,
                              empty_alpha = 0.2, title = NULL, ...) {
  if (!is.character(flop_string) || length(flop_string) != 1) {
    stop("flop_string must be a single character string")
  }
  if (nchar(flop_string) != 6) {
    stop("flop_string must be exactly 6 characters (3 cards)")
  }

  render_cards_ring(
    flop_string,
    show_labels = show_labels,
    empty_alpha = empty_alpha,
    title = title,
    ...
  )
}


#' Render a poker hand as a circular ring
#'
#' Renders a 2-card hand as a circular ring where all 13 ranks appear
#' positionally around a circle with suit symbols at populated ranks.
#'
#' @param hand_string A 4-character string representing 2 cards
#'   (e.g., "AhKh", "7c7d")
#' @param show_labels Logical; show rank labels on ring (default TRUE)
#' @param empty_alpha Numeric 0-1; opacity for empty rank positions (default 0.2)
#' @param title Optional title for the plot
#' @param ... Additional styling parameters forwarded to `render_cards_ring()`
#'
#' @return A ggplot2 object
#'
#' @examples
#' render_hand_ring("AhKh")
#' render_hand_ring("7c7d", title = "Pocket sevens")
#'
render_hand_ring <- function(hand_string, show_labels = TRUE,
                              empty_alpha = 0.2, title = NULL, ...) {
  if (!is.character(hand_string) || length(hand_string) != 1) {
    stop("hand_string must be a single character string")
  }
  if (nchar(hand_string) != 4) {
    stop("hand_string must be exactly 4 characters (2 cards)")
  }

  render_cards_ring(
    hand_string,
    show_labels = show_labels,
    empty_alpha = empty_alpha,
    title = title,
    ...
  )
}


#' Render a poker turn board as a circular ring
#'
#' Renders a 4-card board (flop + turn) as a circular ring where all 13
#' ranks appear positionally around a circle with suit symbols.
#'
#' @param turn_string An 8-character string representing 4 cards
#'   (e.g., "AsTdTc7h")
#' @param show_labels Logical; show rank labels on ring (default TRUE)
#' @param empty_alpha Numeric 0-1; opacity for empty rank positions (default 0.2)
#' @param title Optional title for the plot
#' @param ... Additional styling parameters forwarded to `render_cards_ring()`
#'
#' @return A ggplot2 object
#'
#' @examples
#' render_turn_ring("AsTdTc7h")
#' render_turn_ring("6s5d4c3h", title = "Four to a straight")
#'
render_turn_ring <- function(turn_string, show_labels = TRUE,
                              empty_alpha = 0.2, title = NULL, ...) {
  if (!is.character(turn_string) || length(turn_string) != 1) {
    stop("turn_string must be a single character string")
  }
  if (nchar(turn_string) != 8) {
    stop("turn_string must be exactly 8 characters (4 cards)")
  }

  render_cards_ring(
    turn_string,
    show_labels = show_labels,
    empty_alpha = empty_alpha,
    title = title,
    ...
  )
}


#' Render a poker river board as a circular ring
#'
#' Renders a 5-card board (flop + turn + river) as a circular ring where
#' all 13 ranks appear positionally around a circle with suit symbols.
#'
#' @param board_string A 10-character string representing 5 cards
#'   (e.g., "AsTd7c2hKs")
#' @param show_labels Logical; show rank labels on ring (default TRUE)
#' @param empty_alpha Numeric 0-1; opacity for empty rank positions (default 0.2)
#' @param title Optional title for the plot
#' @param ... Additional styling parameters forwarded to `render_cards_ring()`
#'
#' @return A ggplot2 object
#'
#' @examples
#' render_board_ring("AsTd7c2hKs")
#' render_board_ring("6s5d4c3h2s", title = "Straight board")
#'
render_board_ring <- function(board_string, show_labels = TRUE,
                               empty_alpha = 0.2, title = NULL, ...) {
  if (!is.character(board_string) || length(board_string) != 1) {
    stop("board_string must be a single character string")
  }
  if (nchar(board_string) != 10) {
    stop("board_string must be exactly 10 characters (5 cards)")
  }

  render_cards_ring(
    board_string,
    show_labels = show_labels,
    empty_alpha = empty_alpha,
    title = title,
    ...
  )
}
