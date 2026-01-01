# Tests for ring visualization functions

# parse_cards tests -----------------------------------------------------------

test_that("parse_cards handles standard flop", {
  source("../../R/ring_viz.R")
  result <- parse_cards("AsTdTc")
  expect_equal(nrow(result), 3)
  expect_equal(result$rank, c("A", "T", "T"))
  expect_equal(result$suit, c("s", "d", "c"))
})

test_that("parse_cards handles standard hand", {
  source("../../R/ring_viz.R")
  result <- parse_cards("AhKh")
  expect_equal(nrow(result), 2)
  expect_equal(result$rank, c("A", "K"))
  expect_equal(as.character(result$suit_color), c("red", "red"))
})

test_that("parse_cards assigns correct suit symbols", {
  source("../../R/ring_viz.R")
  result <- parse_cards("AsKhQdJc")
  expect_equal(as.character(result$suit_symbol), c("\u2660", "\u2665", "\u2666", "\u2663"))
})

test_that("parse_cards assigns correct colors", {
  source("../../R/ring_viz.R")
  result <- parse_cards("AsKhQdJc")
  expect_equal(as.character(result$suit_color), c("black", "red", "red", "black"))
})

test_that("parse_cards handles lowercase input", {
  source("../../R/ring_viz.R")
  result <- parse_cards("askdqh")
  expect_equal(result$rank, c("A", "K", "Q"))
  expect_equal(result$suit, c("s", "d", "h"))
})

test_that("parse_cards validates input length", {
  source("../../R/ring_viz.R")
  expect_error(parse_cards("As"), NA)  # 1 card is valid
  expect_error(parse_cards("A"), "even length")  # Odd length is invalid
})

test_that("parse_cards validates empty input", {
  source("../../R/ring_viz.R")
  expect_error(parse_cards(""), "at least one card")
})

test_that("parse_cards validates ranks", {
  source("../../R/ring_viz.R")
  expect_error(parse_cards("Xs"), "Invalid rank")
  expect_error(parse_cards("1s"), "Invalid rank")
})

test_that("parse_cards validates suits", {
  source("../../R/ring_viz.R")
  expect_error(parse_cards("Ax"), "Invalid suit")
  expect_error(parse_cards("A1"), "Invalid suit")
})

test_that("parse_cards handles 5-card board", {
  source("../../R/ring_viz.R")
  result <- parse_cards("AsTdTc7h2s")
  expect_equal(nrow(result), 5)
  expect_equal(result$card_index, 1:5)
})


# rank_to_position tests ------------------------------------------------------

test_that("rank_to_position places Ace first", {
  source("../../R/ring_viz.R")
  expect_equal(rank_to_position("A"), 0)
})

test_that("rank_to_position places 2 last (adjacent to A)", {
  source("../../R/ring_viz.R")
  angle_2 <- rank_to_position("2")
  # 2 should be at position 12/13 of the circle
  expect_equal(angle_2, 12 / 13 * 2 * pi)
})

test_that("rank positions are evenly spaced", {
  source("../../R/ring_viz.R")
  angles <- rank_to_position(c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"))
  diffs <- diff(angles)
  expected_diff <- 2 * pi / 13
  expect_true(all(abs(diffs - expected_diff) < 1e-10))
})

test_that("rank_to_position validates input", {
  source("../../R/ring_viz.R")
  expect_error(rank_to_position("X"), "Invalid rank")
  expect_error(rank_to_position("1"), "Invalid rank")
})

test_that("rank_to_position handles vector input", {
  source("../../R/ring_viz.R")
  result <- rank_to_position(c("A", "K", "2"))
  expect_length(result, 3)
  expect_equal(result[1], 0)  # A at 0
  expect_equal(result[2], 1 / 13 * 2 * pi)  # K at 1/13
})


# prepare_ring_data tests -----------------------------------------------------

test_that("prepare_ring_data includes all 13 ranks", {
  source("../../R/ring_viz.R")
  result <- prepare_ring_data("AsKd")
  expect_equal(nrow(result$ranks), 13)
  expect_setequal(result$ranks$rank, c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"))
})

test_that("prepare_ring_data marks present cards correctly", {
  source("../../R/ring_viz.R")
  result <- prepare_ring_data("AsTdTc")
  expect_equal(nrow(result$cards), 3)
  expect_equal(result$cards$rank, c("A", "T", "T"))
  # Check has_cards flag
  ranks_with_cards <- result$ranks$rank[result$ranks$has_cards]
  expect_setequal(ranks_with_cards, c("A", "T"))
})

test_that("prepare_ring_data handles paired ranks with radial offset", {
  source("../../R/ring_viz.R")
  result <- prepare_ring_data("TdTc")
  ten_cards <- result$cards
  expect_equal(nrow(ten_cards), 2)
  # Check radial offset - cards should have different r values
  expect_true(ten_cards$r[1] != ten_cards$r[2])
  # Both should be centered around base_radius + suit_radius_offset
  expect_equal(mean(ten_cards$r), 1.2)
})

test_that("prepare_ring_data handles trips", {
  source("../../R/ring_viz.R")
  result <- prepare_ring_data("TdTcTh")
  expect_equal(nrow(result$cards), 3)
  # Middle card should be centered on the suit radius
  expect_equal(result$cards$r[2], 1.2)
})

test_that("prepare_ring_data computes x/y coordinates", {
  source("../../R/ring_viz.R")
  result <- prepare_ring_data("As")
  # Ace should be at top (y > 0, x ~ 0)
  expect_true(result$cards$y[1] > 0.9)
  expect_true(abs(result$cards$x[1]) < 0.1)
})

test_that("ranks sit on ring and suits are above it", {
  source("../../R/ring_viz.R")
  result <- prepare_ring_data("AsTdTc")
  rank_radius <- sqrt(result$ranks$x_label[1]^2 + result$ranks$y_label[1]^2)
  suit_radii <- sqrt(result$cards$x^2 + result$cards$y^2)
  expect_true(abs(rank_radius - 1.0) < 1e-8)
  expect_true(all(suit_radii > rank_radius))
})


# render function tests -------------------------------------------------------

test_that("render_flop_ring returns ggplot object", {
  source("../../R/ring_viz.R")
  p <- render_flop_ring("AsTdTc")
  expect_s3_class(p, "ggplot")
})

test_that("render_hand_ring returns ggplot object", {
  source("../../R/ring_viz.R")
  p <- render_hand_ring("AhKh")
  expect_s3_class(p, "ggplot")
})

test_that("render_turn_ring returns ggplot object", {
  source("../../R/ring_viz.R")
  p <- render_turn_ring("AsTdTc7h")
  expect_s3_class(p, "ggplot")
})

test_that("render_board_ring returns ggplot object", {
  source("../../R/ring_viz.R")
  p <- render_board_ring("AsTdTc7h2s")
  expect_s3_class(p, "ggplot")
})

test_that("render_cards_ring returns ggplot object", {
  source("../../R/ring_viz.R")
  p <- render_cards_ring("AsTdTc")
  expect_s3_class(p, "ggplot")
})

test_that("render_flop_ring validates length", {
  source("../../R/ring_viz.R")
  expect_error(render_flop_ring("As"), "6 characters")
  expect_error(render_flop_ring("AsTdTcKh"), "6 characters")
})

test_that("render_hand_ring validates length", {
  source("../../R/ring_viz.R")
  expect_error(render_hand_ring("As"), "4 characters")
  expect_error(render_hand_ring("AsTd"), NA)  # 4 chars is valid
})

test_that("render_turn_ring validates length", {
  source("../../R/ring_viz.R")
  expect_error(render_turn_ring("AsTdTc"), "8 characters")
})

test_that("render_board_ring validates length", {
  source("../../R/ring_viz.R")
  expect_error(render_board_ring("AsTdTc7h"), "10 characters")
})

test_that("render_flop_ring accepts title parameter", {
  source("../../R/ring_viz.R")
  p <- render_flop_ring("6s5d4c", title = "Test Title")
  expect_equal(p$labels$title, "Test Title")
})

test_that("render functions work with empty_alpha parameter", {
  source("../../R/ring_viz.R")
  # Should not error with different alpha values
  expect_s3_class(render_flop_ring("AsTdTc", empty_alpha = 0.1), "ggplot")
  expect_s3_class(render_flop_ring("AsTdTc", empty_alpha = 0.5), "ggplot")
})

test_that("render functions work with show_labels parameter", {
  source("../../R/ring_viz.R")
  # Should not error with labels disabled
  expect_s3_class(render_flop_ring("AsTdTc", show_labels = FALSE), "ggplot")
})

test_that("render_flop_ring supports rank font customization", {
  source("../../R/ring_viz.R")
  p <- render_flop_ring("AsTdTc", rank_font_family = "Courier")
  families <- vapply(p$layers, function(layer) {
    if (is.null(layer$aes_params$family)) NA_character_ else layer$aes_params$family
  }, character(1))
  expect_true("Courier" %in% families)
})

test_that("ring styling parameters are applied", {
  source("../../R/ring_viz.R")
  p <- render_flop_ring("AsTdTc", circle_color = "blue", circle_linewidth = 2)
  circle_layer <- p$layers[[1]]
  expect_equal(circle_layer$aes_params$colour, "blue")
  expect_equal(circle_layer$aes_params$linewidth, 2)
})
