persons <- make_persons(300)
hh_income <- protect_household_income(build_household_file(persons))
runs <- lapply(SCENARIOS, function(sc) run_scenario(persons, sc, hh_income))

test_that("every scenario passes the release checks", {
  for (id in names(runs)) {
    ch <- release_checks(runs[[id]]$protected, runs[[id]]$release, persons, SCENARIOS[[id]])
    expect_equal(ch$check, c("min_fk_with_weight", "weights_spanning_one_nuts2",
                             "households_inconsistent_keys", "forbidden_columns"))
    expect_false(any(ch$pass %in% FALSE), info = id)
  }
})

test_that("the weight check is not applicable where NUTS 2 is released", {
  ch <- release_checks(runs$S1$protected, runs$S1$release, persons, SCENARIOS$S1)
  expect_true(is.na(ch[check == "weights_spanning_one_nuts2", pass]))
  expect_true(is.na(ch[check == "weights_spanning_one_nuts2", value]))
})

test_that("the checks catch the original weight and a forbidden column", {
  prot <- data.table::copy(runs$S3$protected)
  prot[, weight := persons$weight[match(person_id, persons$person_id)]]
  rel <- build_release(prot)
  rel[, eq_size := 1]
  ch <- release_checks(prot, rel, persons, SCENARIOS$S3)
  expect_true(any(ch[check %in% c("min_fk_with_weight", "weights_spanning_one_nuts2"), pass] %in% FALSE))
  expect_equal(ch[check == "forbidden_columns", value], 1)
  expect_false(ch[check == "forbidden_columns", pass])
})

test_that("a shared weight on suppressed-region records adds nothing to the key", {
  # With one weight for all suppressed-region records, fk with the weight
  # equals fk on the released keys alone.
  for (id in names(runs)) {
    rel <- runs[[id]]$release
    expect_gt(sum(is.na(rel$region)), 0)
    expect_equal(data.table::uniqueN(rel[is.na(region), weight]), 1)
    ch <- release_checks(runs[[id]]$protected, rel, persons, SCENARIOS[[id]])
    keys <- as.data.frame(rel[, ..RELEASE_KEYS])
    expect_equal(ch[check == "min_fk_with_weight", value],
                 min(sdcMicro::freqCalc(keys, keyVars = RELEASE_KEYS)$fk), info = id)
  }
})

test_that("the weight of suppressed-region records is a wildcard only if shared", {
  region <- c("A", "A", NA, NA, "B")
  expect_equal(weight_key(region, c(10, 10, 7, 7, 20)), c(10, 10, NA, NA, 20))
  expect_equal(weight_key(region, c(10, 10, 7, 8, 20)), c(10, 10, 7, 8, 20))
})

test_that("households with a key missing for some members only are counted", {
  rel <- data.table::copy(runs$S2$release)
  hh <- rel[, .N, by = hh_release_id][N >= 2, hh_release_id][1]
  rel[which(hh_release_id == hh)[1], hh_size := NA]
  ch <- release_checks(runs$S2$protected, rel, persons, SCENARIOS$S2)
  expect_equal(ch[check == "households_inconsistent_keys", value], 1)
  expect_false(ch[check == "households_inconsistent_keys", pass])
})
