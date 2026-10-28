persons <- make_persons(400, seed = 7)
res <- protect_income_table_audited(persons)

test_that("no published cell has fewer than 3 contributors", {
  published <- res$final[sdcStatus %in% c("s", "z")]
  expect_false(any(published$Freq %in% c(1, 2)))
})

test_that("every primary cell passes the income audit", {
  expect_gt(nrow(res$audit), 0)
  expect_true(all(res$audit$protected))
})

test_that("primary suppression alone does not protect", {
  expect_false(any(res$audit_without_secondary$protected))
})

test_that("the method is reported, with the added cells", {
  expect_true(res$method %in% c("SIMPLEHEURISTIC", "OPT", "OPT + audit-driven additions"))
  if (res$method != "OPT + audit-driven additions") expect_equal(nrow(res$added), 0)
})

test_that("the table margins add up", {
  f <- res$final
  total <- f[region == "IT" & occupation == "Total" & sex == "Total", income]
  by_sex <- f[region == "IT" & occupation == "Total" & sex != "Total", sum(income)]
  expect_equal(by_sex, total)
  expect_equal(total, sum(persons[emp_income > 0 & !is.na(isco1), emp_income * weight]))
})

test_that("suppression_counts adds up to the number of cells", {
  s <- suppression_counts(res$final)
  expect_equal(s$published + s$primary + s$secondary, s$cells)
  expect_gt(s$income_hidden_pct, 0)
})

test_that("a pattern failing the audit falls back to OPT, then to added suppressions", {
  methods <- character()
  augmented <- NULL
  local_mocked_bindings(
    protect_income_table = function(persons, max_n = 2, p = 10, method = "SIMPLEHEURISTIC") {
      methods <<- c(methods, method)
      final <- data.table::copy(res$final)[, method := method]
      list(final = final, micro = res$micro)
    },
    audit_protection_intervals = function(final, micro, p = 10, status = final$sdcStatus, ...) {
      data.table::data.table(region = c("ITC1", "ITH3"), occupation = "OC01", sex = "F",
                             protected = c(TRUE, FALSE))
    },
    augment_suppression = function(final, micro, p = 10, max_rounds = 100) {
      augmented <<- final
      list(final = data.table::copy(final)[, augmented := TRUE],
           added = data.table::data.table(region = "ITC1", occupation = "OC02", sex = "M",
                                          income = 1))
    }
  )
  out <- protect_income_table_audited(persons)
  expect_equal(methods, c("SIMPLEHEURISTIC", "OPT"))
  expect_equal(unique(augmented$method), "OPT")
  expect_true(all(out$final$augmented))
  expect_equal(out$method, "OPT + audit-driven additions")
  expect_equal(out$added$occupation, "OC02")
})
