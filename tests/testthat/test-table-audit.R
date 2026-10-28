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

persons <- make_persons(400, seed = 7)

test_that("the additivity constraints hold on the true cell values", {
  tab <- protect_income_table(persons)
  A <- table_constraints(tab$final[, .(region, occupation, sex)])
  expect_lt(max(abs(A %*% tab$final$income)), 1e-6 * max(tab$final$income))
})

test_that("a NUTS 1 with a single NUTS 2 is a parent equal to its child", {
  one <- persons[nuts2 != "ITH3"]                      # ITH keeps only ITH5
  tab <- protect_income_table(one)
  A <- table_constraints(tab$final[, .(region, occupation, sex)])
  expect_lt(max(abs(A %*% tab$final$income)), 1e-6 * max(tab$final$income))
})

test_that("cell_contributions matches the table", {
  tab <- protect_income_table(persons)
  cc <- cell_contributions(tab$micro)
  m <- merge(tab$final, cc, by = c("region", "occupation", "sex"))
  expect_equal(nrow(m), nrow(tab$final))
  expect_equal(m$X, m$income)
  expect_equal(m$n, m$Freq)
  expect_true(all(m$x1 >= m$x2))
})
