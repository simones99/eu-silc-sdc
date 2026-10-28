test_that("release_weights gives one weight per released region and keeps its total", {
  r <- c("ITC", "ITC", "ITC", "ITF", "ITF")
  expect_equal(release_weights(r, r, c(100, 100, 400, 50, 150)), c(200, 200, 200, 100, 100))
})

test_that("suppressed records get the national weight and regional totals hold", {
  orig <- c("ITC", "ITC", "ITC", "ITC", "ITF", "ITF")
  rel  <- c("ITC", NA,    "ITC", "ITC", "ITF", "ITF")
  out <- release_weights(orig, rel, c(300, 300, 300, 300, 100, 100))
  expect_equal(out[2], 1400 / 6)
  expect_equal(out[c(1, 3, 4)], rep((1200 - 1400 / 6) / 3, 3))
  expect_equal(sum(out[orig == "ITC"]), 1200)
  expect_equal(sum(out[orig == "ITF"]), 200)
})

test_that("release_weights stops on a fully suppressed region", {
  expect_error(release_weights(c("A", "A", "B"), c(NA, NA, "B"), c(5, 5, 5)), "region A")
})

test_that("a low-weight region with blanked records keeps a positive weight", {
  # With the national weight 404 / 6 for the blanked record, region A
  # (total 2) would be left with a negative weight. The blanked records get
  # region A's mean instead, and totals hold.
  orig <- c("A", "A", rep("B", 4))
  out <- release_weights(orig, c("A", NA, rep("B", 4)), c(1, 1, rep(100, 4)))
  expect_equal(out, c(1, 1, rep(100, 4)))
})

test_that("visible records never get less than their region's mean weight", {
  orig <- c(rep("A", 4), rep("B", 4), rep("C", 2))
  rel  <- c("A", NA, "A", "A", "B", NA, NA, "B", "C", "C")
  w    <- c(rep(50, 4), rep(500, 4), rep(2000, 2))
  out <- release_weights(orig, rel, w)
  expect_equal(unique(out[is.na(rel)]), 50)
  for (r in unique(orig)) {
    expect_gte(min(out[orig == r & !is.na(rel)]), mean(w[orig == r]))
    expect_equal(sum(out[orig == r]), sum(w[orig == r]))
  }
})

test_that("release_weights stops on missing input", {
  expect_error(release_weights(c("A", NA), c("A", "A"), c(1, 1)), "missing")
  expect_error(release_weights(c("A", "A"), c("A", "A"), c(1, NA)), "missing")
})
