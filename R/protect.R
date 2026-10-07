# Protection scenarios. Each one is a sequence of standard SDC methods
# (Hundepool et al., Handbook on Statistical Disclosure Control, 2012):
#
# 1. global recoding: coarser categories for the key variables;
# 2. local suppression: blank single key values until every combination of
#    keys is shared by at least k records (k-anonymity);
# 3. microaggregation of incomes: replace each value by the mean of a group
#    of at least 3 similar values, so that no exact income is released.
#
# Direct identifiers (survey person and household numbers) are replaced by
# random numbers before release.

AGE_BREAKS <- list(
  `5` = list(breaks = c(15, seq(19, 79, 5), 80),
             labels = c("16-19", paste0(seq(20, 75, 5), "-", seq(24, 79, 5)), "80+")),
  `10` = list(breaks = c(15, 19, 29, 39, 49, 59, 69, 79, 80),
              labels = c("16-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))
)

# Groupings applied in the "coarse" scenarios. Codes on the left, release
# category on the right.
GROUPINGS <- list(
  # ISCO-08 major groups to skill-based groups (ISCO-08, table 2).
  isco1 = list(before = c("0 - 1", "2", "3", "4", "5", "6", "7", "8", "9"),
               after  = c("Managers, professionals, technicians", "Managers, professionals, technicians",
                          "Managers, professionals, technicians", "Clerical, services, sales",
                          "Clerical, services, sales", "Skilled manual", "Skilled manual",
                          "Skilled manual", "Elementary")),
  # ISCED 1997 levels to low / medium / high attainment.
  isced = list(before = c("0", "1", "2", "3", "4", "5", "6"),
               after  = c("Low (0-2)", "Low (0-2)", "Low (0-2)", "Medium (3-4)", "Medium (3-4)",
                          "High (5-6)", "High (5-6)")),
  # Separated, widowed and divorced become "previously married".
  marital = list(before = c("1", "2", "3", "4", "5"),
                 after  = c("Never married", "Married", "Previously married",
                            "Previously married", "Previously married")),
  hh_size = list(before = as.character(1:10),
                 after  = c("1", "2", "3", "4", rep("5+", 6)))
)

# Importance for local suppression: lower = suppressed last. Region, sex and
# age carry most of the analytical value and are protected from suppression.
SUPPRESSION_IMPORTANCE <- c(region = 1, sex = 1, age = 2, marital = 5,
                            citizenship = 6, isced = 4, isco1 = 7, hh_size = 3)

SCENARIOS <- list(
  S1 = list(label = "S1: NUTS 2, 5-year age bands, detailed codes, k = 3",
            region = "nuts2", age_width = 5, group = FALSE, k = 3),
  S2 = list(label = "S2: NUTS 1, 10-year age bands, grouped codes, k = 3",
            region = "nuts1", age_width = 10, group = TRUE, k = 3),
  S3 = list(label = "S3: NUTS 1, 10-year age bands, grouped codes, k = 5",
            region = "nuts1", age_width = 10, group = TRUE, k = 5)
)

recode_keys <- function(obj, scenario) {
  ab <- AGE_BREAKS[[as.character(scenario$age_width)]]
  obj <- sdcMicro::globalRecode(obj, column = "age", breaks = ab$breaks, labels = ab$labels)
  if (isTRUE(scenario$group)) {
    for (v in names(GROUPINGS)) {
      g <- GROUPINGS[[v]]
      present <- g$before %in% levels(sdcMicro::get.sdcMicroObj(obj, "manipKeyVars")[[v]])
      obj <- sdcMicro::groupAndRename(obj, var = v, before = g$before[present],
                                      after = g$after[present])
    }
  }
  obj
}

# Microaggregate household income once per household (MDAV, groups of 3).
protect_household_income <- function(households, aggr = 3) {
  hh <- households[!is.na(hh_disp_income)]
  m <- sdcMicro::microaggregation(as.data.frame(hh[, .(hh_disp_income)]),
                                  variables = "hh_disp_income", aggr = aggr, method = "mdav")
  data.table(hh_id = hh$hh_id, hh_disp_income = m$mx$hh_disp_income)
}

# Run one scenario. Returns the risk after each step, the final sdcMicro
# object and the release file.
run_scenario <- function(persons, scenario, hh_income_protected, seed = 2026) {
  df <- as_sdc_input(persons, scenario$region)
  obj <- new_sdc(df, num_vars = "emp_income", seed = seed)
  obj <- recode_keys(obj, scenario)
  after_recoding <- risk_summary(obj, scenario$label)[, step := "after global recoding"]
  obj <- sdcMicro::localSuppression(obj, k = scenario$k,
                                    importance = unname(SUPPRESSION_IMPORTANCE[KEY_VARS]))
  obj <- sdcMicro::microaggregation(obj, variables = "emp_income", aggr = 3, method = "mdav")
  final <- risk_summary(obj, scenario$label)[, step := "after local suppression"]
  list(risk = rbind(after_recoding, final), obj = obj,
       release = build_release(obj, hh_income_protected, seed))
}

build_release <- function(obj, hh_income_protected, seed = 2026) {
  rel <- data.table::as.data.table(sdcMicro::extractManipData(obj))
  # extractManipData() turns a recoded numeric key back into integer codes;
  # take the labelled keys straight from the object instead.
  keys <- sdcMicro::get.sdcMicroObj(obj, "manipKeyVars")
  for (v in KEY_VARS) set(rel, j = v, value = keys[[v]])
  rel[, hh_id := as.character(hh_id)]
  rel[, hh_disp_income := NULL]
  rel <- merge(rel, hh_income_protected, by = "hh_id", all.x = TRUE)
  rel[, eq_income := hh_disp_income / eq_size]
  rel[, nuts1 := substr(as.character(region), 1, 3)]
  rel[, age_band := age]

  # Replace survey identifiers with random numbers.
  set.seed(seed)
  hh_map <- data.table(hh_id = unique(rel$hh_id))
  hh_map[, hh_release_id := sample.int(.N)]
  rel <- merge(rel, hh_map, by = "hh_id")
  rel[, person_release_id := sample.int(.N)]
  keep <- c("person_release_id", "hh_release_id", "region", "nuts1", "sex", "age_band",
            "marital", "citizenship", "isced", "isco1", "hh_size", "activity",
            "eq_size", "emp_income", "hh_disp_income", "eq_income", "weight")
  rel <- rel[, ..keep]
  data.table::setorder(rel, hh_release_id, person_release_id)
  rel[]
}
