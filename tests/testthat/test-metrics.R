test_that("entropy is 0 for pure strategy", {
  source("../../R/metrics.R")
  expect_equal(strategy_entropy(c(1, 0, 0)), 0)
  expect_equal(strategy_entropy(c(0, 1, 0)), 0)
})

test_that("entropy is maximized for uniform strategy", {
  source("../../R/metrics.R")
  uniform_3 <- c(1/3, 1/3, 1/3)
  expect_equal(strategy_entropy(uniform_3), log(3))
})

test_that("perplexity equals number of actions for uniform", {
  source("../../R/metrics.R")
  expect_equal(strategy_perplexity(c(0.5, 0.5)), 2)
  expect_equal(strategy_perplexity(c(1/3, 1/3, 1/3)), 3)
})

test_that("JS divergence is symmetric", {
  source("../../R/metrics.R")
  p <- c(0.7, 0.2, 0.1)
  q <- c(0.3, 0.4, 0.3)
  expect_equal(js_divergence(p, q), js_divergence(q, p))
})

test_that("JS divergence is 0 for identical distributions", {
  source("../../R/metrics.R")
  p <- c(0.5, 0.3, 0.2)
  expect_equal(js_divergence(p, p), 0, tolerance = 1e-10)
})
