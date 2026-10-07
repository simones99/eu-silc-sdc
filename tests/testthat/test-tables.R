persons <- make_persons(400, seed = 7)
tab <- protect_income_table(persons)

test_that("no published cell has fewer than 3 contributors", {
  published <- tab$final[sdcStatus %in% c("s", "z")]
  expect_false(any(published$Freq %in% c(1, 2)))
})

test_that("secondary suppression protects every primary-suppressed cell", {
  expect_gt(nrow(tab$audit), 0)
  expect_true(all(tab$audit$protected))
})

test_that("the table margins add up", {
  f <- tab$final
  total <- f[region == "IT" & occupation == "Total" & sex == "Total", income]
  by_sex <- f[region == "IT" & occupation == "Total" & sex != "Total", sum(income)]
  expect_equal(by_sex, total)
  expect_equal(total, sum(persons[emp_income > 0 & !is.na(isco1), emp_income * weight]))
})

test_that("suppression_counts adds up to the number of cells", {
  s <- suppression_counts(tab$final)
  expect_equal(s$published + s$primary + s$secondary, s$cells)
  expect_gt(s$income_hidden_pct, 0)
})
