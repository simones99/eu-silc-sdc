# p% rule protection level: X cell value, x1 and x2 the two largest
# contributions (x2 = 0 for a single contributor).
protection_level <- function(X, x1, x2, p = 10) {
  pmax(0, p / 100 * x1 - (X - x1 - x2))
}

# Intruder interval [L, U] for each primary-suppressed cell ("u"), given the
# published cells (status "s" or "z", fixed at their value) and the additivity
# constraints A %*% v = 0. Suppressed cells ("u", "x") lie in [0, Inf).
protection_intervals <- function(values, status, A) {
  A <- as.matrix(A)
  fixed <- status %in% c("s", "z")
  lower <- ifelse(fixed, values, 0)
  upper <- ifelse(fixed, values, Inf)
  rhs <- rep(0, nrow(A))
  solve_bound <- function(cell, maximum) {
    obj <- numeric(length(values))
    obj[cell] <- 1
    res <- highs::highs_solve(L = obj, lower = lower, upper = upper, A = A,
                              lhs = rhs, rhs = rhs, maximum = maximum,
                              control = highs::highs_control(log_to_console = FALSE))
    if (!identical(res$status_message, "Optimal")) {
      stop(sprintf("intruder LP for cell %d (%s) not solved to optimality: %s",
                   cell, if (maximum) "max" else "min", res$status_message),
           call. = FALSE)
    }
    res$objective_value
  }
  cells <- which(status == "u")
  data.table::data.table(
    cell = cells,
    L = vapply(cells, solve_bound, numeric(1), maximum = FALSE),
    U = vapply(cells, solve_bound, numeric(1), maximum = TRUE)
  )
}

# Number of contributors n, cell value X and the two largest contributions
# x1 >= x2 (x2 = 0 with a single contributor) for every cell of the table,
# margins included. A contribution is one record of table_microdata(), the
# weighted income that sdcTable sums into the cell and uses for the p% rule.
# Cell codes as sdcTable writes them: region IT > NUTS 1 (3 characters) >
# NUTS 2 (4 characters), occupation Total > OC01..OC09, sex Total > M, F.
cell_contributions <- function(micro) {
  micro <- data.table::as.data.table(micro)
  long <- list()
  for (region in list("IT", substr(micro$region, 1, 3), micro$region)) {
    for (occupation in list("Total", micro$occupation)) {
      for (sex in list("Total", micro$sex)) {
        long[[length(long) + 1]] <- data.table::data.table(
          region = region, occupation = occupation, sex = sex, income = micro$income)
      }
    }
  }
  long <- data.table::rbindlist(long)
  data.table::setorder(long, -income)
  cells <- long[, .(n = .N, X = sum(income), x1 = income[1],
                    x2 = if (.N > 1) income[2] else 0),
                by = .(region, occupation, sex)]
  # Cells without contributors are in the table too, with value 0.
  grid <- data.table::CJ(region = unique(long$region), occupation = unique(long$occupation),
                         sex = unique(long$sex))
  cells <- cells[grid, on = c("region", "occupation", "sex")]
  cells[is.na(n), `:=`(n = 0L, X = 0, x1 = 0, x2 = 0)]
  cells[]
}

# Additivity constraints of the table, one row per constraint, columns in the
# order of `cells` (columns region, occupation, sex): for every combination of
# the other two dimensions, IT = sum of NUTS 1, each NUTS 1 = sum of its
# NUTS 2 (parent = child when there is only one), Total = OC01 + ... + OC09,
# Total = M + F. Parent coefficient 1, children -1, so A %*% v = 0.
table_constraints <- function(cells) {
  cells <- data.table::as.data.table(cells)[, .(region, occupation, sex)]
  cell_key <- function(d) paste(d$region, d$occupation, d$sex, sep = "|")
  key <- cell_key(cells)
  regions <- unique(cells$region)
  nuts1 <- sort(regions[nchar(regions) == 3])
  nuts2 <- sort(regions[nchar(regions) == 4])
  children <- list(
    region = c(list(IT = nuts1),
               stats::setNames(lapply(nuts1, function(n1) nuts2[startsWith(nuts2, n1)]), nuts1)),
    occupation = list(Total = sort(setdiff(unique(cells$occupation), "Total"))),
    sex = list(Total = sort(setdiff(unique(cells$sex), "Total")))
  )
  entries <- list()
  n_rows <- 0
  for (dim in names(children)) {
    for (parent in names(children[[dim]])) {
      parents <- which(cells[[dim]] == parent)
      rows <- n_rows + seq_along(parents)
      entries[[length(entries) + 1]] <- cbind(rows, parents, 1)
      for (child in children[[dim]][[parent]]) {
        target <- cells[parents]
        data.table::set(target, j = dim, value = child)
        cols <- match(cell_key(target), key)
        if (anyNA(cols)) {
          stop(sprintf("table cell %s missing", cell_key(target)[is.na(cols)][1]), call. = FALSE)
        }
        entries[[length(entries) + 1]] <- cbind(rows, cols, -1)
      }
      n_rows <- n_rows + length(parents)
    }
  }
  entries <- do.call(rbind, entries)
  A <- matrix(0, n_rows, nrow(cells))
  A[entries[, 1:2, drop = FALSE]] <- entries[, 3]
  A
}

# Audit of a suppression pattern on income (p% rule). For every primary cell
# ("u"), the intruder's interval [L, U] from the published cells and the
# additivity constraints must keep the cell at least PL away on both sides:
# U - X >= PL and X - L >= PL, with tolerance 1e-9 * X. Threshold-rule cells
# (fewer than 3 contributors) use the same PL. `status` replaces the pattern,
# e.g. to audit the primary suppressions alone.
audit_protection_intervals <- function(final, micro, p = 10, status = final$sdcStatus) {
  keys <- c("region", "occupation", "sex")
  final <- data.table::as.data.table(final)
  cells <- final[, .(region, occupation, sex)]
  contrib <- cell_contributions(micro)[cells, on = keys]
  A <- table_constraints(cells)
  # The LP works on values divided by the grand total, so that the solver's
  # absolute tolerances apply to numbers of order 1: on euro values the
  # rounding in sums of published cells exceeds them and the LP is infeasible.
  scale <- max(final$income)
  iv <- protection_intervals(final$income / scale, status, A)
  out <- cells[iv$cell]
  out[, `:=`(X = final$income[iv$cell], x1 = contrib$x1[iv$cell], x2 = contrib$x2[iv$cell])]
  out[, PL := protection_level(X, x1, x2, p)]
  out[, `:=`(L = iv$L * scale, U = iv$U * scale)]
  out[, protected := U - X >= PL - 1e-9 * X & X - L >= PL - 1e-9 * X]
  out[]
}
