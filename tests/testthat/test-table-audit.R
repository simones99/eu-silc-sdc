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

test_that("there is one constraint per parent cell and dimension", {
  tab <- protect_income_table(persons)
  regions <- unique(tab$final$region)
  n_regions <- length(regions)
  n_nuts1 <- sum(nchar(regions) == 3)
  A <- table_constraints(tab$final[, .(region, occupation, sex)])
  expect_equal(nrow(A), (1 + n_nuts1) * 30 + n_regions * 3 + n_regions * 10)
})

test_that("a NUTS 1 with a single NUTS 2 is a parent equal to its child", {
  one <- persons[nuts2 != "ITH3"]                      # ITH keeps only ITH5
  tab <- protect_income_table(one)
  cells <- tab$final[, .(region, occupation, sex)]
  A <- table_constraints(cells)
  expect_lt(max(abs(A %*% tab$final$income)), 1e-6 * max(tab$final$income))
  # Rows where an ITH cell is the parent of the region breakdown.
  ith_rows <- Filter(function(r) {
    cells$region[A[r, ] == 1] == "ITH" && all(cells$region[A[r, ] == -1] != "ITH")
  }, seq_len(nrow(A)))
  expect_length(ith_rows, 30)
  for (r in ith_rows) {
    parent <- which(A[r, ] == 1)
    child <- which(A[r, ] == -1)
    expect_length(child, 1)
    expect_equal(cells$region[child], "ITH5")
    expect_equal(cells[child, .(occupation, sex)], cells[parent, .(occupation, sex)])
  }
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

test_that("dimension_moves keeps every sum of the dimension", {
  children <- list(IT = c("ITC", "ITH"), ITC = c("ITC1", "ITC4"), ITH = "ITH5")
  expect_equal(dimension_moves("ITC1", children), list(c("ITC1", "ITC4")))
  expect_equal(dimension_moves("ITC", children),
               list(c("ITC", "ITC1", "ITH", "ITH5"), c("ITC", "ITC4", "ITH", "ITH5")))
  expect_equal(dimension_moves("IT", children),
               list(c("IT", "ITC", "ITC1"), c("IT", "ITC", "ITC4"), c("IT", "ITH", "ITH5")))
  expect_length(dimension_moves("ITH5", children), 0)       # an only child
  expect_equal(dimension_moves("F", list(Total = c("F", "M"))), list(c("F", "M")))
})

test_that("augment_suppression protects a pattern with too few suppressions", {
  tab <- protect_income_table(persons)
  micro <- tab$micro
  weak <- data.table::copy(tab$final)[sdcStatus == "x", sdcStatus := "s"]
  aug <- augment_suppression(weak, micro)
  expect_true(all(audit_protection_intervals(aug$final, micro)$protected))
  changed <- which(aug$final$sdcStatus != weak$sdcStatus)
  expect_gt(length(changed), 0)
  expect_true(all(weak$sdcStatus[changed] == "s"))
  expect_true(all(aug$final$sdcStatus[changed] == "x"))
  expect_equal(aug$final[sdcStatus == "u", .(region, occupation, sex)],
               weak[sdcStatus == "u", .(region, occupation, sex)])
  expect_equal(nrow(aug$added), length(changed))
  expect_setequal(paste(aug$added$region, aug$added$occupation, aug$added$sex),
                  paste(weak$region, weak$occupation, weak$sex)[changed])
  expect_equal(aug$added$income, weak[aug$added, income, on = c("region", "occupation", "sex")])
})

test_that("augment_suppression changes nothing on a protected pattern", {
  tab <- protect_income_table(persons)
  aug <- augment_suppression(tab$final, tab$micro)
  expect_equal(nrow(aug$added), 0)
  expect_identical(aug$final, tab$final)
})

test_that("augment_suppression stops after max_rounds, naming the cell", {
  tab <- protect_income_table(persons)
  weak <- data.table::copy(tab$final)[sdcStatus == "x", sdcStatus := "s"]
  expect_error(augment_suppression(weak, tab$micro, max_rounds = 0),
               "IT[A-Z0-9]*/(OC0[1-9]|Total)/(M|F|Total).*decide with the user")
})
