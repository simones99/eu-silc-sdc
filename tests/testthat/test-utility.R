test_that("weighted_quantile matches the unweighted median with equal weights", {
  expect_equal(weighted_quantile(c(5, 1, 3), c(1, 1, 1), 0.5), 3)
  expect_equal(weighted_quantile(c(1, 2, 100), c(1, 1, 10), 0.5), 100)
})

test_that("arop_rate counts weighted persons below 60% of the median", {
  # Median 10, threshold 6: only the person with income 5 is at risk.
  expect_equal(arop_rate(c(5, 10, 20), c(1, 1, 1)), 1 / 3)
  # Doubling that person's weight doubles their share.
  expect_equal(arop_rate(c(5, 10, 20), c(2, 1, 1), threshold = 6), 0.5)
})

test_that("gini is 0 for equal incomes and close to 1 for full concentration", {
  expect_equal(gini(rep(10, 5), rep(1, 5)), 0)
  expect_gt(gini(c(rep(0, 999), 1), rep(1, 1000)), 0.99)
})

test_that("age_band builds closed bands, starts at 16 and top-codes", {
  b <- age_band(c(16, 19, 20, 24, 79, 80), width = 5)
  expect_equal(as.character(b), c("16-19", "16-19", "20-24", "20-24", "75-79", "80+"))
  b10 <- age_band(c(16, 25, 80), width = 10)
  expect_equal(as.character(b10), c("16-19", "20-29", "80+"))
})

test_that("ci_overlap is 1 for identical fits and 0 for disjoint intervals", {
  set.seed(1)
  d <- data.frame(x = rnorm(200))
  d$y <- 2 * d$x + rnorm(200)
  f <- lm(y ~ x, d)
  expect_equal(ci_overlap(f, f)$overlap, c(1, 1), ignore_attr = TRUE)
  d2 <- d
  d2$y <- d2$y + 100
  expect_equal(ci_overlap(f, lm(y ~ x, d2))$overlap[1], 0, ignore_attr = TRUE)
})
