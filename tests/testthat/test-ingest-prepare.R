test_that("download_puf stops when the file does not match the manifest hash", {
  dir <- withr::local_tempdir()
  writeLines("not the real file", file.path(dir, "IT_PUF_EUSILC.zip"))
  manifest <- list(file = "IT_PUF_EUSILC.zip", url = "https://example.invalid/x.zip",
                   sha256 = strrep("0", 64))
  expect_error(download_puf(manifest, dest_dir = dir), "SHA-256 mismatch")
})

test_that("build_person_file attaches household variables to each person", {
  d <- data.table(DB030 = c("1", "2"), DB040 = c("ITC4", "ITF3"), DB090 = c("100", "200"))
  h <- data.table(HB030 = c("1", "2"), HX040 = c("2", "1"), HX050 = c("1.5", "1"),
                  HY020 = c("30000", "12000"))
  p <- data.table(PB030 = c("101", "102", "201"), PX030 = c("1", "1", "2"),
                  PB040 = c("100", "100", "200"), PB150 = c("1", "2", "2"),
                  PX020 = c("45", "43", "80"), PB190 = c("2", "2", "4"),
                  PB220A = c("IT", "IT", "EU"), PE040 = c("3", "5", "1"),
                  PL031 = c("1", "1", "7"), PL051 = c("7", "2", NA),
                  PY010G = c("25000", "18000", "0"))
  out <- build_person_file(d, h, p)
  expect_equal(nrow(out), 3)
  expect_equal(out[person_id == "101", nuts1], "ITC")
  expect_equal(out[person_id == "101", eq_income], 20000)
  expect_equal(out[person_id == "201", c(nuts2, as.character(age))], c("ITF3", "80"))
})

test_that("build_household_file keeps one row per household", {
  persons <- make_persons(50)
  hh <- build_household_file(persons)
  expect_equal(nrow(hh), 50)
  expect_false(anyDuplicated(hh$hh_id) > 0)
})
