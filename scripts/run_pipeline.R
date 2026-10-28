# Full pipeline: download, prepare, measure risk, protect (three scenarios),
# measure utility, protect the income table, write outputs.
#
#   Rscript scripts/run_pipeline.R            all scenarios (about 15 minutes)
#   Rscript scripts/run_pipeline.R S2 S3      only the listed scenarios
#
# Outputs go to output/ (public: the publish workflow uploads output/*.csv and
# run_info.json); full table audit detail goes to output/internal/ (not published).
# The report (reports/report.Rmd) reads them.

suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
scenario_ids <- if (length(args)) args else names(SCENARIOS)
dir.create("output", showWarnings = FALSE)
dir.create("output/internal", showWarnings = FALSE)
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
checks <- list()
for (id in scenario_ids) {
  sc <- SCENARIOS[[id]]
  message("Running ", sc$label)
  t0 <- Sys.time()
  res <- run_scenario(persons, sc, hh_income)
  message(sprintf("  done in %.1f minutes", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  risk[[id]] <- res$risk
  suppressions[[id]] <- res$suppressions

  chk <- release_checks(res$protected, res$release, persons, sc)
  if (any(chk$pass %in% FALSE)) {
    print(chk[pass %in% FALSE])
    stop("Release checks failed for ", sc$label, "; release not written.")
  }
  checks[[id]] <- data.table::copy(chk)[, scenario := sc$label]

  # The original-file age_band copy is used only for the wage regression.
  original <- data.table::copy(persons)[, age_band := age_band(age, sc$age_width)]
  indicators[[id]] <- loss_by_source(persons, res$protected, res$release, sc$label)
  overlap[[id]] <- ci_overlap(fit_wage_model(original),
                              fit_wage_model(res$release))[, scenario := sc$label]
  write_out(res$release, sprintf("release_%s.csv", id))
}

write_out(data.table::rbindlist(risk, use.names = TRUE), "risk.csv")
write_out(data.table::rbindlist(checks, use.names = TRUE)[, .(scenario, check, value, pass)],
          "release_checks.csv")
write_out(data.table::rbindlist(suppressions, use.names = TRUE), "suppressions.csv")
write_out(data.table::rbindlist(indicators, use.names = TRUE), "indicators.csv")
write_out(data.table::rbindlist(overlap), "ci_overlap.csv")

message("Protecting the income table")
tab <- protect_income_table_audited(persons)
if (!all(tab$audit$protected)) {
  print(tab$audit[protected %in% FALSE])
  stop("Income table has unprotected cells; nothing written.")
}

# Public table: income and frequency are hidden for suppressed (u, x) cells.
pub <- data.table::copy(tab$final)
hide <- pub$sdcStatus %in% c("u", "x")
pub[hide, income := NA_real_]
if ("Freq" %in% names(pub)) pub[hide, Freq := NA]
write_out(pub, "table_protected.csv")

# Public audit: only the protected flag. Margins (L, U, PL, ratios) would let
# an intruder solve for a cell's value, so they stay in output/internal/.
public_audit <- function(a) a[, .(region, occupation, sex, protected)]
write_out(public_audit(tab$audit), "table_audit_income.csv")
write_out(public_audit(tab$audit_without_secondary), "table_audit_income_without_secondary.csv")

# Full detail stays internal (output/internal/ is not matched by output/*.csv).
internal_cols <- function(a) a[, .(region, occupation, sex, X, x1, x2, PL, L, U, protected)]
write_out(internal_cols(tab$audit), "internal/table_audit_income.csv")
write_out(internal_cols(tab$audit_without_secondary), "internal/table_audit_income_without_secondary.csv")
write_out(tab$added, "internal/table_added_cells.csv")

summary_tab <- suppression_counts(tab$final)
summary_tab[, income_hidden_pct := round(income_hidden_pct, 1)]
summary_tab[, cells_added := nrow(tab$added)]
write_out(summary_tab, "table_summary.csv")

jsonlite::write_json(list(
  source = manifest$source, url = manifest$url, sha256 = manifest$sha256,
  year = manifest$year_used, persons = nrow(persons),
  households = data.table::uniqueN(persons$hh_id),
  table_method = tab$method, table_cells_added = nrow(tab$added),
  scenarios = scenario_ids, sdcMicro = as.character(utils::packageVersion("sdcMicro")),
  sdcTable = as.character(utils::packageVersion("sdcTable")),
  run_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
), "output/run_info.json", auto_unbox = TRUE, pretty = TRUE)
message("Outputs written to output/")
