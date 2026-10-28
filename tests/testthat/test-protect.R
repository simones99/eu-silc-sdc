persons <- make_persons(300)
hh_income <- protect_household_income(build_household_file(persons))

test_that("equivalised income microaggregation preserves the mean and the ranking", {
  hh <- build_household_file(persons)
  merged <- merge(hh, hh_income, by = "hh_id", suffixes = c("", "_prot"))
  expect_equal(nrow(merged), nrow(hh))
  expect_equal(mean(merged$eq_income_prot), mean(merged$eq_income))
  expect_gt(cor(merged$eq_income, merged$eq_income_prot, method = "spearman"), 0.95)
  # Every released value is shared by at least 3 households.
  expect_gte(min(table(merged$eq_income_prot)), 3)
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
  expect_false(any(c("eq_size", "hh_disp_income") %in% names(rel)))
  expect_equal(rel[, data.table::uniqueN(eq_income), by = hh_release_id][, max(V1)], 1)
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

test_that("a household key suppressed for one member is suppressed for all", {
  small <- persons[hh_id %in% persons[, .N, by = hh_id][N >= 2, hh_id][1:3]]
  obj <- new_sdc(as_sdc_input(small, "nuts2"))
  keys <- sdcMicro::get.sdcMicroObj(obj, "manipKeyVars")
  keys$region[1] <- NA
  obj <- sdcMicro::set.sdcMicroObj(obj, "manipKeyVars", list(keys))
  out <- sdcMicro::get.sdcMicroObj(harmonise_household_keys(obj), "manipKeyVars")
  hh <- small$hh_id
  expect_true(all(is.na(out$region[hh == hh[1]])))
  expect_false(any(is.na(out$region[hh != hh[1]])))
})

test_that("suppression_summary counts only values blanked by protection", {
  recoded <- as.data.frame(setNames(rep(list(c("a", "b", "c")), length(KEY_VARS)), KEY_VARS))
  recoded$isco1[1] <- NA                       # missing in the source
  final <- recoded
  final$region[2] <- NA
  final$hh_size[3] <- NA
  s <- suppression_summary(recoded, final, "S")
  expect_equal(s[variable == "region", suppressed], 1L)
  expect_equal(s[variable == "hh_size", suppressed], 1L)
  expect_equal(s[variable == "isco1", suppressed], 0L)
  expect_equal(sum(s$suppressed), 2L)
})
