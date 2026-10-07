persons <- make_persons(300)
hh_income <- protect_household_income(build_household_file(persons))

test_that("household income microaggregation preserves the mean and the ranking", {
  hh <- build_household_file(persons)
  merged <- merge(hh, hh_income, by = "hh_id", suffixes = c("", "_prot"))
  expect_equal(nrow(merged), nrow(hh))
  expect_equal(mean(merged$hh_disp_income_prot), mean(merged$hh_disp_income))
  expect_gt(cor(merged$hh_disp_income, merged$hh_disp_income_prot), 0.95)
  # Every released value is shared by at least 3 households.
  expect_gte(min(table(merged$hh_disp_income_prot)), 3)
})

test_that("scenario S3 reaches 5-anonymity on the key variables", {
  res <- run_scenario(persons, SCENARIOS$S3, hh_income)
  final <- res$risk[step == "after local suppression"]
  expect_equal(final$violating_5_anonymity, 0)
  expect_equal(final$sample_uniques, 0)
  expect_lte(final$expected_reidentifications,
             res$risk[step == "after global recoding", expected_reidentifications])
})

test_that("release file has no survey identifiers and coherent households", {
  rel <- run_scenario(persons, SCENARIOS$S2, hh_income)$release
  expect_false(any(c("person_id", "hh_id", "age") %in% names(rel)))
  expect_equal(nrow(rel), nrow(persons))
  expect_true(all(stats::na.omit(as.character(rel$age_band)) %in% AGE_BREAKS$`10`$labels))
  # One household income per household.
  expect_equal(rel[, data.table::uniqueN(hh_disp_income), by = hh_release_id][, max(V1)], 1)
  expect_setequal(stats::na.omit(unique(as.character(rel$isced))),
                  c("Low (0-2)", "Medium (3-4)", "High (5-6)"))
})

test_that("employee income is microaggregated, not released exactly", {
  rel <- run_scenario(persons, SCENARIOS$S2, hh_income)$release
  positive <- rel$emp_income[rel$emp_income > 0]
  original <- persons$emp_income[persons$emp_income > 0]
  expect_lt(mean(positive %in% original), 0.5)
  expect_equal(sum(rel$emp_income * rel$weight), sum(persons$emp_income * persons$weight),
               tolerance = 0.02)
})
