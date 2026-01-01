#' Render poker cards with unicode suits and colors
#'
#' Converts a string of cards (e.g. "AhKs9c") into a formatted HTML string
#' with unicode suit symbols and optional coloring.
#'
#' @param x A character vector of card strings.
#' @param colorize Logical, whether to apply CSS colors (red for hearts/diamonds, black for spades/clubs).
#' @return A character vector of rendered HTML strings.
#' @export
render_cards <- function(x, colorize = TRUE) {
  # Define suit mappings
  suits <- c("c" = "\u2663", "d" = "\u2666", "h" = "\u2665", "s" = "\u2660")
  colors <- c("c" = "black", "d" = "red", "h" = "red", "s" = "black")
  
  # Helper to process a single string
  process_string <- function(s) {
    if (is.na(s) || s == "") return(s)
    
    # Extract all card pairs (Rank + Suit)
    # Assuming standard notation: Rank (2-9, T, J, Q, K, A) + Suit (c, d, h, s)
    # We use a regex that captures the rank and the suit
    matches <- gregexpr("([2-9TJQKA])([cdhs])", s)
    card_data <- regmatches(s, matches)[[1]]
    
    if (length(card_data) == 0) return(s)
    
    # Process each card found in the string
    rendered <- vapply(card_data, function(card) {
      rank <- substr(card, 1, 1)
      suit <- substr(card, 2, 2)
      
      symbol <- suits[suit]
      
      if (colorize) {
        color <- colors[suit]
        sprintf("<span style='color:%s'>%s%s</span>", color, rank, symbol)
      } else {
        paste0(rank, symbol)
      }
    }, character(1))
    
    paste(rendered, collapse = " ")
  }
  
  # Apply to all elements in the input vector
  vapply(x, process_string, character(1), USE.NAMES = FALSE)
}
