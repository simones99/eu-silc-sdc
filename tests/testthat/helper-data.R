# Small synthetic person file with the same columns as build_person_file().
make_persons <- function(n_hh = 300, seed = 1) {
  set.seed(seed)
  nuts2 <- c("ITC1", "ITC4", "ITH5", "ITI4", "ITF3", "ITG1")
  hh <- data.table(hh_id = as.character(1000 + seq_len(n_hh)),
                   nuts2 = sample(nuts2, n_hh, replace = TRUE),
                   hh_size = sample(1:6, n_hh, replace = TRUE, prob = c(3, 3, 2, 2, 1, 0.5)),
                   hh_weight = runif(n_hh, 200, 2500),
                   hh_disp_income = round(rlnorm(n_hh, 10, 0.6)))
  hh[, eq_size := 1 + 0.5 * pmax(hh_size - 1, 0)]
  p <- hh[rep(seq_len(.N), pmin(hh_size, 3))]
  p[, `:=`(
    person_id = paste0(hh_id, sprintf("%02d", seq_len(.N))),
    weight = hh_weight,
    sex = sample(c("1", "2"), .N, replace = TRUE),
    age = sample(16:80, .N, replace = TRUE),
    marital = sample(as.character(1:5), .N, replace = TRUE, prob = c(4, 5, 1, 1, 1)),
    citizenship = sample(c("IT", "EU", "Other"), .N, replace = TRUE, prob = c(8, 1, 1)),
    isced = sample(as.character(0:6), .N, replace = TRUE),
    activity = sample(as.character(1:11), .N, replace = TRUE),
    isco1 = sample(c("0 - 1", as.character(2:9), NA), .N, replace = TRUE)
  ), by = hh_id]
  p[, emp_income := ifelse(is.na(isco1), 0, round(rlnorm(.N, 9.8, 0.5)))]
  p[, nuts1 := substr(nuts2, 1, 3)]
  p[, eq_income := hh_disp_income / eq_size]
  p[]
}
