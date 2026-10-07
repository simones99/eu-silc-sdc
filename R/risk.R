# Disclosure-risk measures for a person-level file, computed by sdcMicro.
#
# Intruder scenario: someone who knows a few facts about a person (where they
# live, sex, age, household size, ...) looks for records that match them.
# Those facts are the categorical key variables (quasi-identifiers). A record
# whose combination of keys is rare in the sample is at risk.

KEY_VARS <- c("region", "sex", "age", "marital", "citizenship", "isced", "isco1", "hh_size")

# sdcMicro treats categorical keys as factors; age stays numeric so that
# globalRecode() can band it, and is a key all the same.
as_sdc_input <- function(dt, region = c("nuts2", "nuts1")) {
  region <- match.arg(region)
  out <- data.table::copy(dt)
  out[, region := get(region)]
  # sdcMicro's household risk needs a numeric household identifier.
  out[, hh_id := as.integer(hh_id)]
  for (v in setdiff(KEY_VARS, "age")) set(out, j = v, value = factor(out[[v]]))
  as.data.frame(out)
}

new_sdc <- function(df, num_vars = "emp_income", seed = 2026) {
  sdcMicro::createSdcObj(
    dat = df,
    keyVars = KEY_VARS,
    numVars = num_vars,
    weightVar = "weight",
    hhId = "hh_id",
    seed = seed
  )
}

# One row of risk measures for an sdcMicro object.
risk_summary <- function(obj, label) {
  ind <- obj@risk$individual
  fk <- ind[, "fk"]
  data.table(
    scenario = label,
    records = length(fk),
    sample_uniques = sum(fk == 1),
    violating_3_anonymity = sum(fk < 3),
    violating_5_anonymity = sum(fk < 5),
    expected_reidentifications = obj@risk$global$risk_ER,
    global_risk_pct = 100 * obj@risk$global$risk,
    household_risk_pct = 100 * obj@risk$global$hier_risk,
    max_individual_risk = max(ind[, "risk"]),
    records_risk_above_10pct = sum(ind[, "risk"] > 0.1)
  )
}

# Values suppressed (set to NA) by local suppression, per key variable.
suppression_summary <- function(obj, label) {
  ls <- obj@localSuppression
  if (is.null(ls)) {
    counts <- setNames(rep(0L, length(KEY_VARS)), KEY_VARS)
  } else {
    counts <- unlist(ls$supps)
  }
  data.table(scenario = label, variable = names(counts), suppressed = as.integer(counts))
}
