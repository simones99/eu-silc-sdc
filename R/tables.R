# Protection of a magnitude table with cell suppression (sdcTable).
#
# Table: estimated total employee income by region (Italy > NUTS 1 > NUTS 2),
# occupation (ISCO-08 major group) and sex. A cell is sensitive when
#   - it has fewer than 3 contributors (threshold rule), or
#   - the second-largest contributor could estimate the largest one to within
#     10% by subtracting its own value from the cell total (p% rule, p = 10).
# Sensitive cells are suppressed (primary suppression). Because the table has
# margins, a suppressed cell could be recomputed from its row or column
# totals, so more cells must be hidden (secondary suppression).

ISCO_CODES <- c("0 - 1" = "OC01", "2" = "OC02", "3" = "OC03", "4" = "OC04", "5" = "OC05",
                "6" = "OC06", "7" = "OC07", "8" = "OC08", "9" = "OC09")

table_microdata <- function(persons) {
  emp <- persons[!is.na(emp_income) & emp_income > 0 & !is.na(isco1)]
  emp[, .(region = nuts2, occupation = unname(ISCO_CODES[isco1]),
          sex = ifelse(sex == "1", "M", "F"), income = emp_income * weight)]
}

region_hierarchy <- function(nuts2_codes) {
  h <- sdcHierarchies::hier_create(root = "IT", nodes = sort(unique(substr(nuts2_codes, 1, 3))))
  for (n1 in sort(unique(substr(nuts2_codes, 1, 3)))) {
    h <- sdcHierarchies::hier_add(h, root = n1,
                                  nodes = sort(unique(nuts2_codes[startsWith(nuts2_codes, n1)])))
  }
  h
}

flat_hierarchy <- function(codes) {
  sdcHierarchies::hier_create(root = "Total", nodes = sort(unique(codes)))
}

# Primary suppression (threshold rule, maxN = 2, and p% rule on the weighted
# incomes) and secondary suppression with sdcTable. Returns the final table
# and the microdata it was built from.
protect_income_table <- function(persons, max_n = 2, p = 10,
                                 method = "SIMPLEHEURISTIC") {
  micro <- table_microdata(persons)
  prob <- sdcTable::makeProblem(
    data = as.data.frame(micro),
    dimList = list(region = region_hierarchy(micro$region),
                   occupation = flat_hierarchy(micro$occupation),
                   sex = flat_hierarchy(micro$sex)),
    dimVarInd = 1:3,
    numVarInd = 4
  )
  prob <- sdcTable::primarySuppression(prob, type = "freq", maxN = max_n)
  prob <- sdcTable::primarySuppression(prob, type = "p", p = p, numVarName = "income")
  res <- sdcTable::protectTable(prob, method = method)
  final <- data.table::as.data.table(sdcTable::getInfo(res, type = "finalData"))
  list(final = final, micro = micro)
}

# Protected table whose pattern passes the income audit. The quick heuristic
# is tried first, then the optimal method; if neither pattern protects every
# primary cell the run stops. `audit_without_secondary` audits the primary
# suppressions alone, to show that the secondary ones are needed.
protect_income_table_audited <- function(persons, p = 10) {
  for (method in c("SIMPLEHEURISTIC", "OPT")) {
    tab <- protect_income_table(persons, p = p, method = method)
    audit <- audit_protection_intervals(tab$final, tab$micro, p = p)
    if (all(audit$protected)) {
      primary_only <- ifelse(tab$final$sdcStatus == "x", "s", tab$final$sdcStatus)
      return(list(final = tab$final, micro = tab$micro, audit = audit,
                  audit_without_secondary = audit_protection_intervals(
                    tab$final, tab$micro, p = p, status = primary_only),
                  method = method))
    }
  }
  failing <- audit[!(protected)]
  stop(sprintf(paste("income table: %d of %d primary cells not protected with",
                     "SIMPLEHEURISTIC or OPT (%s); decide with the user how to proceed"),
               nrow(failing), nrow(audit),
               paste(failing$region, failing$occupation, failing$sex, sep = "/", collapse = ", ")),
       call. = FALSE)
}

# Cell counts by status: published (s), primary (u) and secondary (x)
# suppressions. The hidden share refers to the most detailed cells.
suppression_counts <- function(final) {
  leaf <- nchar(final$region) == 4 & final$occupation != "Total" & final$sex != "Total"
  total <- final[region == "IT" & occupation == "Total" & sex == "Total", income]
  final[, .(cells = .N,
            published = sum(sdcStatus %in% c("s", "z")),
            primary = sum(sdcStatus == "u"),
            secondary = sum(sdcStatus == "x"),
            income_hidden_pct = 100 * sum(income[leaf & sdcStatus %in% c("u", "x")]) / total)]
}
