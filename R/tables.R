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

protect_income_table <- function(persons, max_n = 2, p = 10,
                                 method = "SIMPLEHEURISTIC") {
  micro <- as.data.frame(table_microdata(persons))
  prob <- sdcTable::makeProblem(
    data = micro,
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
  list(final = final,
       audit = audit_suppression(prob, final),
       audit_without_secondary = data.table::as.data.table(sdcTable::attack(prob, verbose = FALSE)))
}

# Independent check of the suppression pattern: apply it to the problem and
# solve the intruder's linear programs for every primary-suppressed cell. A
# cell is protected when its value cannot be pinned down from the published
# cells (upper bound > lower bound). sdcTable attacks the contributor counts;
# whether a cell can be recomputed exactly depends only on which cells are
# suppressed, so the check covers the income values as well.
audit_suppression <- function(prob, final) {
  # A NUTS 1 area with a single NUTS 2 region is one cell for sdcTable; the
  # NUTS 2 row is a duplicate and cannot be addressed separately.
  nuts2 <- unique(final[nchar(region) == 4, region])
  single <- names(which(table(substr(nuts2, 1, 3)) == 1))
  duplicate <- nchar(final$region) == 4 & substr(final$region, 1, 3) %in% single
  secondary <- as.data.frame(final[sdcStatus == "x" & !duplicate, .(region, occupation, sex)])
  if (nrow(secondary) > 0) {
    prob <- sdcTable::change_cellstatus(prob, specs = secondary, rule = "x", verbose = FALSE)
  }
  data.table::as.data.table(sdcTable::attack(prob, verbose = FALSE))
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
