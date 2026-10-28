persons <- make_persons(300)
hh_income <- protect_household_income(build_household_file(persons))
runs <- lapply(SCENARIOS, function(sc) run_scenario(persons, sc, hh_income))

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
  res <- runs$S3
  final <- res$risk[step == "after local suppression"]
  expect_equal(final$violating_5_anonymity, 0)
  expect_equal(final$sample_uniques, 0)
  expect_lte(final$expected_reidentifications,
             res$risk[step == "after global recoding", expected_reidentifications])
})

test_that("release file has exactly the release columns and coherent households", {
  rel <- runs$S2$release
  expect_equal(names(rel), c("person_release_id", "hh_release_id", "region", "nuts1", "sex",
                             "age_band", "marital", "citizenship", "isced", "isco1", "hh_size",
                             "activity", "emp_income", "eq_income", "weight"))
  expect_equal(nrow(rel), nrow(persons))
  expect_true(all(stats::na.omit(as.character(rel$age_band)) %in% AGE_BREAKS$`10`$labels))
  # One household income per household.
  expect_equal(rel[, data.table::uniqueN(eq_income), by = hh_release_id][, max(V1)], 1)
  expect_setequal(stats::na.omit(unique(as.character(rel$isced))),
                  c("Low (0-2)", "Medium (3-4)", "High (5-6)"))
})

test_that("the protected file keeps person_id for every person", {
  prot <- runs$S2$protected
  expect_true("person_id" %in% names(prot))
  expect_equal(nrow(prot), nrow(persons))
  expect_setequal(prot$person_id, persons$person_id)
})

test_that("released weights preserve the national total in every scenario", {
  for (id in names(runs)) {
    expect_equal(sum(runs[[id]]$release$weight), sum(persons$weight), tolerance = 1e-8, info = id)
  }
})

test_that("released weights preserve totals by original NUTS 1 in S2", {
  prot <- runs$S2$protected
  joined <- merge(prot[, .(person_id, weight)], persons[, .(person_id, nuts1, weight_original = weight)],
                  by = "person_id")
  tot <- joined[, .(released = sum(weight), original = sum(weight_original)), by = nuts1]
  expect_equal(tot$released, tot$original, tolerance = 1e-8)
})

test_that("employee income is microaggregated, not released exactly", {
  res <- runs$S2
  positive <- res$release$emp_income[res$release$emp_income > 0]
  original <- persons$emp_income[persons$emp_income > 0]
  expect_lt(mean(positive %in% original), 0.5)
  joined <- merge(res$protected[, .(person_id, emp_income)],
                  persons[, .(person_id, weight_original = weight)], by = "person_id")
  expect_equal(sum(joined$emp_income * joined$weight_original),
               sum(persons$emp_income * persons$weight), tolerance = 0.02)
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

test_that("protection never leaves household size blank", {
  # The source has no missing household size, so any blank is a suppression.
  expect_false(anyNA(persons$hh_size))
  for (id in names(runs)) {
    expect_equal(sum(is.na(runs[[id]]$release$hh_size)), 0, info = id)
    expect_equal(runs[[id]]$suppressions[variable == "hh_size", suppressed], 0L, info = id)
    expect_gte(runs[[id]]$suppression_passes, 1)
  }
})

test_that("suppression stops when household size is still blank after the last pass", {
  sc <- SCENARIOS$S3
  obj <- recode_keys(new_sdc(as_sdc_input(persons, sc$region)), sc)
  recoded <- sdcMicro::get.sdcMicroObj(obj, "manipKeyVars")
  expect_error(suppress_keeping_hh_size(obj, sc$k, recoded, max_passes = 1),
               "household size")
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

test_that("release ids differ between scenarios", {
  # S2 and S3 release the same household incomes; with the same random ids
  # the two files would link household by household.
  ids <- function(rel) unique(rel[, .(hh_release_id, eq_income)])
  joined <- merge(ids(runs$S2$release), ids(runs$S3$release), by = "hh_release_id")
  expect_lt(mean(joined$eq_income.x == joined$eq_income.y, na.rm = TRUE), 0.1)
})
