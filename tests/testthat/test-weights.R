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

test_that("release_weights stops on a fully suppressed region or a non-positive weight", {
  expect_error(release_weights(c("A", "A", "B"), c(NA, NA, "B"), c(5, 5, 5)), "region A")
  expect_error(release_weights(c("A", "A", rep("B", 4)), c("A", NA, rep("B", 4)),
                               c(1, 1, rep(100, 4))), "region A")
})

test_that("release_weights stops on missing input", {
  expect_error(release_weights(c("A", NA), c("A", "A"), c(1, 1)), "missing")
  expect_error(release_weights(c("A", "A"), c("A", "A"), c(1, NA)), "missing")
})
