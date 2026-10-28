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
