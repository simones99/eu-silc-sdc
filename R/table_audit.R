# p% rule protection level: X cell value, x1 and x2 the two largest
# contributions (x2 = 0 for a single contributor).
protection_level <- function(X, x1, x2, p = 10) {
  pmax(0, p / 100 * x1 - (X - x1 - x2))
}

# Intruder interval [L, U] for each primary-suppressed cell ("u"), or for the
# cells listed in `cells`, given the published cells (status "s" or "z", fixed
# at their value) and the additivity constraints A %*% v = 0. Suppressed cells
# ("u", "x") lie in [0, Inf).
protection_intervals <- function(values, status, A, cells = which(status == "u")) {
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

# Children of every parent code, by dimension: IT > NUTS 1 > NUTS 2,
# occupation Total > OC01..OC09, sex Total > M, F.
table_hierarchy <- function(cells) {
  regions <- unique(cells$region)
  nuts1 <- sort(regions[nchar(regions) == 3])
  nuts2 <- sort(regions[nchar(regions) == 4])
  list(
    region = c(list(IT = nuts1),
               stats::setNames(lapply(nuts1, function(n1) nuts2[startsWith(nuts2, n1)]), nuts1)),
    occupation = list(Total = sort(setdiff(unique(cells$occupation), "Total"))),
    sex = list(Total = sort(setdiff(unique(cells$sex), "Total")))
  )
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
  children <- table_hierarchy(cells)
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
# e.g. to audit the primary suppressions alone; `cells` (row numbers of
# `final`) restricts the audit to some primary cells.
audit_protection_intervals <- function(final, micro, p = 10, status = final$sdcStatus,
                                       cells = which(status == "u")) {
  keys <- c("region", "occupation", "sex")
  final <- data.table::as.data.table(final)
  index <- final[, .(region, occupation, sex)]
  contrib <- cell_contributions(micro)[index, on = keys]
  A <- table_constraints(index)
  # The LP works on values divided by the grand total, so that the solver's
  # absolute tolerances apply to numbers of order 1: on euro values the
  # rounding in sums of published cells exceeds them and the LP is infeasible.
  scale <- max(final$income)
  iv <- protection_intervals(final$income / scale, status, A, cells)
  out <- index[iv$cell]
  out[, `:=`(X = final$income[iv$cell], x1 = contrib$x1[iv$cell], x2 = contrib$x2[iv$cell])]
  out[, PL := protection_level(X, x1, x2, p)]
  out[, `:=`(L = iv$L * scale, U = iv$U * scale)]
  out[, protected := U - X >= PL - 1e-9 * X & X - L >= PL - 1e-9 * X]
  out[]
}

# Sets of codes of one dimension that can change together with `code`
# without breaking that dimension's sums: `code` and a sibling (same parent,
# so the parent is unchanged, one going up and the other down), each taken
# with a chain of children down to the most detailed level when it is itself
# a parent (a parent changes with one of its children). A code without
# siblings (IT, Total) changes with a chain of children alone. `children` is
# one dimension of table_hierarchy().
dimension_moves <- function(code, children) {
  down <- function(x) {
    kids <- children[[x]]
    if (is.null(kids)) return(list(x))
    unlist(lapply(kids, function(k) lapply(down(k), function(path) c(x, path))),
           recursive = FALSE)
  }
  parent <- names(children)[vapply(children, function(k) code %in% k, logical(1))]
  if (length(parent) == 0) return(Filter(function(path) length(path) > 1, down(code)))
  moves <- list()
  for (sibling in setdiff(children[[parent]], code)) {
    for (a in down(code)) for (b in down(sibling)) moves[[length(moves) + 1]] <- c(a, b)
  }
  moves
}

# Suppression structures around cell `cell` of `final`: the corners of the
# rectangles (two dimensions changing, the third fixed) and of the cubes
# (all three changing) built from dimension_moves(). For a cell of the most
# detailed level these are {r, r'} x {o, o'}, {r, r'} x {s, s'},
# {o, o'} x {s, s'} and {r, r'} x {o, o'} x {s, s'}, with r' a region under
# the same parent, o' another occupation and s' the other sex. One vector of
# row numbers of `final` per structure.
suppression_structures <- function(final, cell) {
  keys <- c("region", "occupation", "sex")
  hierarchy <- table_hierarchy(final)
  options <- lapply(keys, function(dim) {
    code <- final[[dim]][cell]
    c(list(code), dimension_moves(code, hierarchy[[dim]]))
  })
  choices <- expand.grid(lapply(options, seq_along))
  choices <- choices[rowSums(choices > 1) >= 2, , drop = FALSE]
  lapply(seq_len(nrow(choices)), function(i) {
    corners <- do.call(data.table::CJ, lapply(1:3, function(d) options[[d]][[choices[i, d]]]))
    data.table::setnames(corners, keys)
    final[corners, on = keys, which = TRUE]
  })
}

# Secondary suppressions added until every primary cell passes the income
# audit. Each round takes the failing cell with the largest shortfall
# max(PL - (U - X), PL - (X - L)) and, among the suppression structures around
# it (suppression_structures()), suppresses the cheapest one (least income of
# published cells newly suppressed) that makes it protected; if none does, the
# one that leaves it the widest margin min(U - X, X - L). A single added cell
# never helps: every cell sits in three additivity constraints and stays
# recomputable from the other two. Structures with an empty cell ("z") or
# nothing new to suppress are skipped. Extra suppressions only widen the
# intervals, so cells already protected stay protected; cells "u", "x" and "z"
# never change. Stops when no structure widens the failing cell's interval or
# after `max_rounds` rounds. Returns the new table and the added cells, in the
# order added.
augment_suppression <- function(final, micro, p = 10, max_rounds = 100) {
  final <- data.table::copy(data.table::as.data.table(final))
  keys <- c("region", "occupation", "sex")
  added <- final[0, .(region, occupation, sex, income)]
  margin <- function(a) pmin(a$U - a$X, a$X - a$L)
  rounds <- 0
  repeat {
    audit <- audit_protection_intervals(final, micro, p = p)
    if (all(audit$protected)) return(list(final = final, added = added))
    audit[, shortfall := PL - margin(audit)]
    worst <- audit[!(protected)][which.max(shortfall)]
    cell <- final[worst, on = keys, which = TRUE]
    stop_here <- function(reason) {
      stop(sprintf(paste("income table: cell %s/%s/%s not protected (X = %.0f, PL = %.0f,",
                         "L = %.0f, U = %.0f): %s; decide with the user how to proceed"),
                   worst$region, worst$occupation, worst$sex, worst$X, worst$PL,
                   worst$L, worst$U, reason),
           call. = FALSE)
    }
    if (rounds >= max_rounds) stop_here(sprintf("%d rounds of additions", rounds))
    structures <- Filter(function(idx) {
      !anyNA(idx) && !any(final$sdcStatus[idx] == "z") && any(final$sdcStatus[idx] == "s")
    }, suppression_structures(final, cell))
    new_cells <- lapply(structures, function(idx) idx[final$sdcStatus[idx] == "s"])
    cost <- vapply(new_cells, function(idx) sum(final$income[idx]), numeric(1))
    best <- NULL
    best_margin <- margin(worst) + 1e-9 * worst$X
    for (new in new_cells[order(cost)]) {
      status <- final$sdcStatus
      status[new] <- "x"
      a <- audit_protection_intervals(final, micro, p = p, status = status, cells = cell)
      if (a$protected) {
        best <- new
        break
      }
      if (margin(a) > best_margin) {
        best <- new
        best_margin <- margin(a)
      }
    }
    if (is.null(best)) stop_here("no suppression structure widens its interval")
    data.table::set(final, i = best, j = "sdcStatus", value = "x")
    added <- rbind(added, final[best, .(region, occupation, sex, income)])
    rounds <- rounds + 1
  }
}
