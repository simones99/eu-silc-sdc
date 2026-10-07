# Build the person-level "internal" file that the scenario treats as
# confidential: one row per person aged 16 or over, with household variables
# attached. Codes follow the EU-SILC 2013 guidelines (document SILC065).

NUTS2_LABELS <- c(
  ITC1 = "Piemonte", ITC2 = "Valle d'Aosta", ITC3 = "Liguria", ITC4 = "Lombardia",
  ITH1 = "Bolzano", ITH2 = "Trento", ITH3 = "Veneto", ITH4 = "Friuli-Venezia Giulia",
  ITH5 = "Emilia-Romagna", ITI1 = "Toscana", ITI2 = "Umbria", ITI3 = "Marche",
  ITI4 = "Lazio", ITF1 = "Abruzzo", ITF2 = "Molise", ITF3 = "Campania",
  ITF4 = "Puglia", ITF5 = "Basilicata", ITF6 = "Calabria", ITG1 = "Sicilia",
  ITG2 = "Sardegna"
)

as_num <- function(x) suppressWarnings(as.numeric(x))

build_person_file <- function(d, h, p) {
  d <- d[, .(hh_id = DB030, nuts2 = DB040, hh_weight = as_num(DB090))]
  h <- h[, .(hh_id = HB030, hh_size = as.integer(HX040),
             eq_size = as_num(HX050), hh_disp_income = as_num(HY020))]
  p <- p[, .(
    person_id = PB030,
    hh_id = PX030,
    weight = as_num(PB040),
    sex = PB150,
    age = as.integer(PX020),
    marital = PB190,
    citizenship = PB220A,
    isced = PE040,
    activity = PL031,
    isco1 = PL051,
    emp_income = as_num(PY010G)
  )]

  out <- merge(p, d, by = "hh_id", all.x = TRUE)
  out <- merge(out, h, by = "hh_id", all.x = TRUE)
  out[, nuts1 := substr(nuts2, 1, 3)]
  out[, eq_income := hh_disp_income / eq_size]
  data.table::setcolorder(out, c("person_id", "hh_id", "nuts1", "nuts2"))
  out[]
}

# Household-level file used to protect household income once per household,
# so that members of the same household keep the same value.
build_household_file <- function(persons) {
  unique(persons[, .(hh_id, hh_weight, hh_disp_income)], by = "hh_id")
}
