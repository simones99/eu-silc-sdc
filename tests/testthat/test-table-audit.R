# Cells: a b c d | r1 = a+b, r2 = c+d, c1 = a+c, c2 = b+d, T.
v <- c(a = 10, b = 20, c = 30, d = 40, r1 = 30, r2 = 70, c1 = 40, c2 = 60, T = 100)
A <- rbind(c(-1, -1,  0,  0, 1, 0, 0, 0, 0),
           c( 0,  0, -1, -1, 0, 1, 0, 0, 0),
           c(-1,  0, -1,  0, 0, 0, 1, 0, 0),
           c( 0, -1,  0, -1, 0, 0, 0, 1, 0),
           c( 0,  0,  0,  0, -1, -1, 0, 0, 1),
           c( 0,  0,  0,  0, 0, 0, -1, -1, 1))

test_that("a single suppressed cell is recomputed exactly", {
  st <- c("u", rep("s", 8))
  iv <- protection_intervals(v, st, A)
  expect_equal(c(iv$L, iv$U), c(10, 10))
})

test_that("a suppressed rectangle gives the analytic interval", {
  st <- c("u", "x", "x", "x", rep("s", 5))
  iv <- protection_intervals(v, st, A)
  expect_equal(c(iv$L, iv$U), c(0, 30))
})

test_that("an infeasible model is an error, not a protected cell", {
  bad <- v; bad["T"] <- 101
  expect_error(protection_intervals(bad, c("u", rep("s", 8)), A), "cell 1")
})

test_that("protection_level follows the p% rule", {
  expect_equal(protection_level(100, 100, 0), 10)     # one contributor
  expect_equal(protection_level(100, 70, 30), 7)      # two contributors
  expect_equal(protection_level(100, 80, 15), 3)
  expect_equal(protection_level(100, 50, 30), 0)
})
