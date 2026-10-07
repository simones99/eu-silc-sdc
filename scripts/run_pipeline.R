# Full pipeline: download, prepare, measure risk, protect (three scenarios),
# measure utility, protect the income table, write outputs.
#
#   Rscript scripts/run_pipeline.R            all scenarios (about 15 minutes)
#   Rscript scripts/run_pipeline.R S2 S3      only the listed scenarios
#
# Outputs go to output/; the report (reports/report.Rmd) reads them.

suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
scenario_ids <- if (length(args)) args else names(SCENARIOS)
dir.create("output", showWarnings = FALSE)
write_out <- function(x, name) data.table::fwrite(x, file.path("output", name))

manifest <- read_manifest()
zip_path <- download_puf(manifest)
persons <- build_person_file(read_silc(zip_path, manifest$year_used, "d"),
                             read_silc(zip_path, manifest$year_used, "h"),
                             read_silc(zip_path, manifest$year_used, "p"))
message(sprintf("Person file: %d persons aged 16+, %d households",
                nrow(persons), data.table::uniqueN(persons$hh_id)))

# Risk of the unprotected file, at full detail.
original_obj <- new_sdc(as_sdc_input(persons, "nuts2"))
risk <- list(risk_summary(original_obj, "Original file")[, step := "no protection"])

hh_income <- protect_household_income(build_household_file(persons))

suppressions <- list()
indicators <- list()
overlap <- list()
for (id in scenario_ids) {
  sc <- SCENARIOS[[id]]
  message("Running ", sc$label)
  t0 <- Sys.time()
  res <- run_scenario(persons, sc, hh_income)
  message(sprintf("  done in %.1f minutes", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  risk[[id]] <- res$risk
  suppressions[[id]] <- suppression_summary(res$obj, sc$label)

  original <- data.table::copy(persons)[, age_band := age_band(age, sc$age_width)]
  indicators[[id]] <- compare_indicators(indicator_set(original),
                                         indicator_set(res$release))[, scenario := sc$label]
  overlap[[id]] <- ci_overlap(fit_wage_model(original),
                              fit_wage_model(res$release))[, scenario := sc$label]
  write_out(res$release, sprintf("release_%s.csv", id))
}

write_out(data.table::rbindlist(risk, use.names = TRUE), "risk.csv")
write_out(data.table::rbindlist(suppressions), "suppressions.csv")
write_out(data.table::rbindlist(indicators), "indicators.csv")
write_out(data.table::rbindlist(overlap), "ci_overlap.csv")

message("Protecting the income table")
tab <- protect_income_table(persons)
write_out(tab$final, "table_protected.csv")
write_out(tab$audit, "table_audit.csv")
write_out(tab$audit_without_secondary, "table_audit_without_secondary.csv")
write_out(suppression_counts(tab$final), "table_summary.csv")

jsonlite::write_json(list(
  source = manifest$source, url = manifest$url, sha256 = manifest$sha256,
  year = manifest$year_used, persons = nrow(persons),
  households = data.table::uniqueN(persons$hh_id),
  scenarios = scenario_ids, sdcMicro = as.character(utils::packageVersion("sdcMicro")),
  sdcTable = as.character(utils::packageVersion("sdcTable")),
  run_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
), "output/run_info.json", auto_unbox = TRUE, pretty = TRUE)
message("Outputs written to output/")
