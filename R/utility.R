# Analytical utility: the indicators a user of the released file would compute.
# Each is computed on the original and on the protected file, and the
# difference is the information loss that the user actually experiences.

weighted_quantile <- function(x, w, p = 0.5) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]
  w <- w[ok]
  o <- order(x)
  x <- x[o]
  cw <- cumsum(w[o]) / sum(w)
  x[which(cw >= p)[1]]
}

# At-risk-of-poverty rate: share of persons with equivalised disposable income
# below 60% of the national weighted median (Eurostat indicator ilc_li02).
arop_rate <- function(eq_income, w, threshold = NULL) {
  if (is.null(threshold)) threshold <- 0.6 * weighted_quantile(eq_income, w, 0.5)
  ok <- !is.na(eq_income) & !is.na(w)
  sum(w[ok & eq_income < threshold]) / sum(w[ok])
}

# Gini coefficient of a weighted distribution (trapezoid on the Lorenz curve).
gini <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]
  w <- w[ok]
  o <- order(x)
  x <- x[o]
  w <- w[o] / sum(w)
  cum_w <- cumsum(w)
  cum_xw <- cumsum(x * w) / sum(x * w)
  1 - sum(w * (cum_xw + c(0, head(cum_xw, -1))))
}

age_band <- function(age, width = 5, top = 80) {
  lower <- pmin((age %/% width) * width, top)
  lower[age < 16] <- NA
  lower[lower < 16] <- 16
  upper <- ifelse(lower >= top, NA, (lower %/% width + 1) * width - 1)
  label <- ifelse(is.na(upper), paste0(lower, "+"), paste0(lower, "-", upper))
  factor(label, levels = unique(label[order(lower)]))
}

# Official-style indicators on a person file. The poverty threshold is computed
# nationally and applied to every domain, as Eurostat does.
indicator_set <- function(dt) {
  thr <- 0.6 * weighted_quantile(dt$eq_income, dt$weight, 0.5)
  emp <- dt[!is.na(emp_income) & emp_income > 0]
  national <- data.table(
    indicator = c("AROP rate", "Median equivalised income", "Gini (equivalised income)",
                  "Mean employee income", "Median employee income"),
    domain = "Italy",
    value = c(arop_rate(dt$eq_income, dt$weight, thr),
              weighted_quantile(dt$eq_income, dt$weight, 0.5),
              gini(pmax(dt$eq_income, 0), dt$weight),
              stats::weighted.mean(emp$emp_income, emp$weight),
              weighted_quantile(emp$emp_income, emp$weight, 0.5))
  )
  by_area <- dt[, .(indicator = "AROP rate", value = arop_rate(eq_income, weight, thr)),
                by = .(domain = nuts1)]
  by_sex <- emp[, .(indicator = "Mean employee income",
                    value = stats::weighted.mean(emp_income, weight)),
                by = .(domain = ifelse(sex == "1", "Men", "Women"))]
  rbind(national, by_area, by_sex, use.names = TRUE)
}

compare_indicators <- function(original, protected) {
  out <- merge(original, protected, by = c("indicator", "domain"),
               suffixes = c("_original", "_protected"))
  out[, rel_diff_pct := 100 * (value_protected - value_original) / abs(value_original)]
  out[order(indicator, domain)]
}

# Confidence-interval overlap (Karr et al., 2006) between the coefficients of
# the same regression fitted on the original and on the protected file.
# 1 = identical intervals, 0 = no overlap.
ci_overlap <- function(fit_original, fit_protected, level = 0.95) {
  a <- stats::confint(fit_original, level = level)
  b <- stats::confint(fit_protected, level = level)
  terms <- intersect(rownames(a), rownames(b))
  a <- a[terms, , drop = FALSE]
  b <- b[terms, , drop = FALSE]
  inter <- pmax(0, pmin(a[, 2], b[, 2]) - pmax(a[, 1], b[, 1]))
  data.table(
    term = terms,
    coef_original = stats::coef(fit_original)[terms],
    coef_protected = stats::coef(fit_protected)[terms],
    overlap = 0.5 * (inter / (a[, 2] - a[, 1]) + inter / (b[, 2] - b[, 1]))
  )
}

# Wage model used for the CI-overlap check: employees with positive income.
fit_wage_model <- function(dt) {
  emp <- dt[!is.na(emp_income) & emp_income > 0]
  stats::lm(log(emp_income) ~ sex + age_band + nuts1, data = emp, weights = weight)
}
