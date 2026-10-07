# Methodology

This note explains each step of the pipeline, the choices made and their
limits. The report (`reports/report.Rmd`) shows the results.

## 1. Data

**Source.** Eurostat, EU-SILC public microdata (public use files), Italy,
`IT_PUF_EUSILC.zip`. The zip holds the four EU-SILC files for 2004-2013:
household register (D), personal register (R), household data (H) and
personal data (P). The project uses 2013.

**Nature of the data.** The public microdata are fully synthetic: Eurostat
simulated them from statistical models fitted on the real survey. They have
the structure and variable names of the scientific-use files but no real
respondent. Eurostat's disclaimer states that they cannot be used for
inference about the population. In this project they stand in for a
confidential file. Everything below would apply unchanged to a real file;
only the numbers would differ.

**Reproducibility.** `data/manifest.json` records the URL, size and SHA-256
of the zip. `download_puf()` refuses to run on a file with a different hash.

**Person file.** One row per person aged 16 or over (P file, 37,209
records), joined to the household register (region, household weight) and
household data (household size, equivalised size, disposable income) on the
household identifier. 18,487 households.

Data quirks found while profiling:

- Age (PX020) is already top-coded at 80 in the public file.
- Occupation (PL051) is at ISCO-08 major group level, with groups 0 and 1
  already merged ("0 - 1").
- Equivalised disposable income (HX090) is empty, so it is recomputed as
  HY020 / HX050.
- 1,971 households (10.7%) have zero or negative disposable income (HY020),
  a side effect of the synthetic generation. They are kept: they affect
  levels, not the comparison between original and protected data.

## 2. Intruder scenario and variable roles

SDC starts from an assumption about what an intruder knows. Here the intruder
knows, for a target person, facts that are usually easy to find out:

| Role | Variables |
|---|---|
| Direct identifiers (removed) | PB030 person ID, PX030 household ID |
| Key variables (quasi-identifiers) | region (DB040), sex (PB150), age (PX020), marital status (PB190), citizenship (PB220A), education (PE040), occupation (PL051), household size (HX040) |
| Confidential numerical variables | employee cash income (PY010G), household disposable income (HY020) |
| Weight | personal cross-sectional weight (PB040) |
| Household structure | household ID, used for household risk |

Self-defined economic status (PL031) is released but not treated as a key:
it changes often and is less likely to be known to an intruder. A different
scenario would change the key set; the code takes it from one constant,
`KEY_VARS` in `R/risk.R`.

## 3. Risk measures

All measures come from sdcMicro (`createSdcObj`, `measure_risk`).

- **Frequency counts.** For each record, *f_k* is the number of sample
  records with the same combination of keys. A missing value matches any
  category, so a suppressed value enlarges the group.
- **k-anonymity.** A file is k-anonymous when every record has *f_k ≥ k*.
  Sample uniques are records with *f_k = 1*. This measure needs no model.
- **Individual risk.** The probability that a match between the intruder's
  target and a record is correct. It depends on *F_k*, the number of people
  in the population with that combination, which is estimated from the
  sampling weights (Benedetti and Franconi, 1998; the negative-binomial
  model implemented by Istat and in sdcMicro).
- **Expected re-identifications.** The sum of individual risks.
- **Household risk.** The probability that at least one member of a
  household is re-identified, because identifying one member reveals the
  others' records.

On the original file the two families disagree sharply: 65% of records are
sample uniques, but the expected number of re-identifications is about 178
(0.5% of records), because each record represents about 1,400 people. Both
views are reported. Agencies usually require a k-anonymity threshold for a
public file and check the model-based risk as a second line.

## 4. Protection methods

Each scenario applies, in this order:

1. **Global recoding** (`globalRecode`, `groupAndRename`). Age to 5- or
   10-year bands (16-19, then regular bands, 80+). In the grouped scenarios:
   NUTS 2 to NUTS 1; ISCO major groups to four skill-based groups (managers,
   professionals and technicians; clerical, service and sales; skilled
   manual; elementary); ISCED 1997 to low (0-2), medium (3-4) and high (5-6);
   marital status to never married, married, previously married; household
   size top-coded at 5+.
2. **Local suppression** (`localSuppression`). sdcMicro blanks individual
   key values, choosing those that break the most unsafe combinations, until
   the file is k-anonymous. The importance vector makes region and sex the
   last candidates, then age and household size; occupation goes first.
3. **Microaggregation** (`microaggregation`, MDAV, groups of 3). Each income
   is replaced by the mean of a group of at least three similar incomes.
   Employee income is treated at person level. Household income is treated
   once per household on a separate household file and merged back, so all
   members of a household keep the same value.
4. **Identifiers.** Person and household identifiers are replaced by random
   numbers; household membership is kept so that household-level analysis
   remains possible.

| Scenario | Region | Age | Other keys | k |
|---|---|---|---|---|
| S1 | NUTS 2 | 5-year bands | source detail | 3 |
| S2 | NUTS 1 | 10-year bands | grouped | 3 |
| S3 | NUTS 1 | 10-year bands | grouped | 5 |

S1 shows what happens when the release keeps detail and relies on
suppression: occupation loses 13,596 values (37% of records) and the run
takes about 8 minutes. S2 and S3 show that recoding first is cheaper: after
recoding only 2,669 sample uniques remain, and suppression blanks a few
thousand values.

## 5. Information loss

Utility is measured on what a user would compute, using the same code on the
original and on the protected file.

- **Suppressed values** per key variable and as a share of all key values.
- **Indicators.** At-risk-of-poverty (AROP) rate, with the threshold at 60%
  of the national weighted median equivalised disposable income (as in
  Eurostat's `ilc_li02`), nationally and by NUTS 1; median equivalised
  income; Gini; mean and median employee income, nationally and by sex.
  The AROP rate here covers persons aged 16 and over, because only the P
  file is released.
- **Regression.** Weighted least squares of log employee income on sex, age
  band and NUTS 1, for employees with positive income. For each coefficient
  the confidence-interval overlap (Karr et al., 2006) is
  `0.5 × (I / (U_o − L_o) + I / (U_p − L_p))`, where `I` is the length of the
  intersection of the original and protected 95% intervals.

Finding: the indicator differences (at most 0.27%) come from
microaggregation, which is the same in every scenario; key suppression barely
moves them. The scenarios differ in the detail published and in the number
of blanked values, which is what the risk-utility map plots.

## 6. Table protection

**Table.** Estimated total employee income (income × weight) by region
(Italy > NUTS 1 > NUTS 2), occupation (ISCO major group) and sex, for
employees with positive income and a known occupation. With all margins the
table has 810 cells.

**Primary suppression** (`primarySuppression`):

- *threshold rule*: a cell with 1 or 2 contributors is sensitive;
- *p% rule* with p = 10: a cell is sensitive if the second-largest
  contributor, by subtracting its own value from the cell total, could
  estimate the largest contributor's value to within 10%.

**Secondary suppression** (`protectTable`, `SIMPLEHEURISTIC`). A single
suppressed cell in a row can be recomputed as the row total minus the
published cells. The algorithm hides further cells, across the region
hierarchy and both margins, until every sensitive cell is protected.

**Audit** (`attack`). For each sensitive cell, sdcTable solves the
intruder's problem: the minimum and maximum value the cell can take given
every published cell and the additivity of the table. A cell is protected if
the maximum is larger than the minimum. sdcTable runs the attack on the
contributor counts; whether a cell can be recomputed *exactly* depends only
on which cells are suppressed, so the result carries over to the income
values. With only primary suppressions, all 27 sensitive cells can be
recomputed exactly; with the secondary suppressions, none can.

## 7. Limitations and next steps

- **Synthetic source.** The risk levels describe the synthetic file only.
- **One intruder scenario.** Risk depends on the key set. A sensitivity
  analysis over key sets would strengthen the release decision.
- **Outliers.** Microaggregation in groups of 3 still releases values close
  to the top incomes. A production release would add top-coding checked
  against the income distribution, or noise on the top tail.
- **Width of the protection intervals.** The audit checks that no cell can
  be recomputed exactly. A stricter check would require each interval to be
  wide enough relative to the cell value (the protection level used by
  τ-ARGUS).
- **Secondary suppression method.** `SIMPLEHEURISTIC` is fast but does not
  minimise the information hidden; `OPT` or τ-ARGUS's modular method usually
  hide less.
- **Not covered.** PRAM (randomised response on categorical variables) and
  synthetic data generation, which are the other main families of methods.

## References

- Benedetti R., Franconi L. (1998), "Statistical and technological solutions
  for controlled data dissemination", *Pre-proceedings of New Techniques and
  Technologies for Statistics*.
- Hundepool A., Domingo-Ferrer J., Franconi L., Giessing S., Schulte
  Nordholt E., Spicer K., de Wolf P.-P. (2012), *Statistical Disclosure
  Control*, Wiley.
- Karr A. F., Kohnen C. N., Oganian A., Reiter J. P., Sanil A. P. (2006), "A
  framework for evaluating the utility of data altered to protect
  confidentiality", *The American Statistician* 60(3), 224-232.
- Templ M., Kowarik A., Meindl B. (2015), "Statistical disclosure control for
  micro-data using the R package sdcMicro", *Journal of Statistical
  Software* 67(4).
- Eurostat (2013), *Description of target variables: cross-sectional and
  longitudinal, 2013 operation* (SILC065).
