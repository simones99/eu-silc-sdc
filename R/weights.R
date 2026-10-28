# Release weights. The original weight varies between NUTS 2 regions, so it
# would reveal a region that the release hides: the released weight must
# depend only on released variables. It is one common value for records
# with a suppressed region and one value per released region for the others;
# regional and national totals are preserved whatever the common value. The
# common value is the national mean weight, lowered to the mean weight of any
# region with suppressed records whose mean is lower, so that the other
# records of every region get at least their region's mean.
release_weights <- function(region_original, region_released, weight) {
  if (anyNA(region_original) || anyNA(weight))
    stop("region_original and weight must not have missing values")
  n <- length(weight)
  supp <- is.na(region_released)
  region_mean <- tapply(weight, region_original, mean)
  w_s <- min(sum(weight) / n, region_mean[unique(region_original[supp])])
  out <- numeric(n)
  out[supp] <- w_s
  for (r in unique(region_original)) {
    idx <- region_original == r
    n_r <- sum(idx)
    s_r <- sum(idx & supp)
    t_r <- sum(weight[idx])
    w_r <- if (n_r > s_r) (t_r - s_r * w_s) / (n_r - s_r) else NA_real_
    if (is.na(w_r) || w_r <= 0)
      stop(sprintf("release_weights: region %s has n_r = %d, s_r = %d, T_r = %s; weight not positive",
                   r, n_r, s_r, format(t_r)))
    out[idx & !supp] <- w_r
  }
  out
}
