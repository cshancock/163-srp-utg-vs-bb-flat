test_that("render_cards works for single strings", {
  # We need to source the file if not running in a package context that auto-loads it
  # But for now assuming standard testthat setup. 
  # If this fails, I will add source("../../R/card_rendering.R")
  
  expect_equal(render_cards("Ah", colorize = FALSE), "A\u2665")
  expect_equal(render_cards("Ks", colorize = FALSE), "K\u2660")
})

test_that("render_cards works for multiple cards in one string", {
  expect_equal(render_cards("AhKs", colorize = FALSE), "A\u2665 K\u2660")
  expect_equal(render_cards("Td9c", colorize = FALSE), "T\u2666 9\u2663")
})

test_that("render_cards works for vectors", {
  input <- c("Ah", "Ks")
  expected <- c("A\u2665", "K\u2660")
  expect_equal(render_cards(input, colorize = FALSE), expected)
})

test_that("render_cards applies coloring", {
  output <- render_cards("Ah", colorize = TRUE)
  expect_match(output, "<span style='color:red'>A\u2665</span>", fixed = TRUE)
  
  output_black <- render_cards("Ks", colorize = TRUE)
  expect_match(output_black, "<span style='color:black'>K\u2660</span>", fixed = TRUE)
})

test_that("render_cards handles empty or NA input", {
  expect_equal(render_cards(NA_character_), NA_character_)
  expect_equal(render_cards(""), "")
})
