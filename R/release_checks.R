# Checks on the release file, run for every scenario before release.

RELEASE_KEYS <- c("region", "sex", "age_band", "marital", "citizenship", "isced", "isco1", "hh_size")

FORBIDDEN_COLUMNS <- c("eq_size", "hh_disp_income", "person_id", "hh_id", "age")

# One weight shared by all suppressed-region records says only "region
# suppressed", which the NA already says: treat it as NA (matches any value).
weight_key <- function(region, weight) {
  na <- is.na(region)
  if (data.table::uniqueN(weight[na]) == 1) weight[na] <- NA
  weight
}

# One row per check: value and pass (NA when the check does not apply).
# The release is assumed to be build_release(protected): the release has no
# survey ids, so the link to NUTS 2 goes through the protected file.
release_checks <- function(protected, release, persons, scenario) {
  keys <- as.data.frame(release[, ..RELEASE_KEYS])
  keys$weight <- factor(weight_key(release$region, release$weight))
  min_fk <- min(sdcMicro::freqCalc(keys, keyVars = c(RELEASE_KEYS, "weight"))$fk)

  if (scenario$region == "nuts2") {
    spanning <- NA_real_
  } else {
    w <- data.table(weight = weight_key(protected$region, protected$weight),
                    nuts2 = persons$nuts2[match(protected$person_id, persons$person_id)])
    spanning <- w[!is.na(weight), .(n = data.table::uniqueN(nuts2)), by = weight][, sum(n == 1)]
  }

  inconsistent <- release[, .(bad = (any(is.na(region)) && !all(is.na(region))) ||
                                    (any(is.na(hh_size)) && !all(is.na(hh_size)))),
                          by = hh_release_id][, sum(bad)]

  forbidden <- sum(FORBIDDEN_COLUMNS %in% names(release))

  data.table(check = c("min_fk_with_weight", "weights_spanning_one_nuts2",
                       "households_inconsistent_keys", "forbidden_columns"),
             value = as.numeric(c(min_fk, spanning, inconsistent, forbidden)),
             pass = c(min_fk >= scenario$k, spanning == 0, inconsistent == 0, forbidden == 0))
}
