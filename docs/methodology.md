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
| Confidential numerical variables | employee cash income (PY010G), equivalised disposable income (HY020 / HX050) |
| Weight | personal cross-sectional weight (PB040), replaced by a released weight (section 4) |
| Household structure | household ID, used for household risk |

Household disposable income (HY020) and the equivalised household size
(HX050) are not released. The equivalised size maps one to one to household
size, also inside the top-coded "5+" class, and the ratio of the two incomes
gives it back; either would undo the recoding of household size.

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
   last candidates, then age; occupation goes first. Household size is never
   left blank: the number of members a household has in the release gives
   its size exactly in the top class and a lower bound otherwise, so a blank
   would protect little. After each pass, household sizes blanked by
   suppression are restored and local suppression runs again with household
   size as the last candidate. sdcMicro still blanks household size when it
   is the only key that tells a record apart from its nearest records; from
   the second pass on, such a record also loses its least important key not
   yet blank (occupation, then citizenship, marital status, education, age,
   region, sex). Each pass removes detail, so the loop ends; it stops with an
   error after 10 passes. On the 2013 file it takes 4 passes in S1 and S2
   and 2 in S3 (`suppression_passes` in `run_info.json`).
3. **Household-consistent suppression.** Region is a household
   characteristic, and the release keeps a household identifier, so a value
   blanked for one member could be read from another member of the same
   household. After local suppression, if region is blank for any member, it
   is blanked for every member, and the risk is recomputed
   (`calcRisks`). A blank matches any value, so the extra blanks can only
   enlarge groups and k-anonymity is preserved. The suppression counts
   compare the final keys with the recoded keys, so they include these extra
   blanks and exclude values that are missing in the source (occupation for
   people without a job, unknown citizenship).
4. **Microaggregation** (`microaggregation`, MDAV, groups of 3). Each income
   is replaced by the mean of a group of at least three similar incomes.
   Employee income is treated at person level. Equivalised disposable income
   is treated once per household on a separate household file and merged
   back, so all members of a household keep the same value. It is the only
   household income released; the poverty rate, median and Gini are computed
   on it.
5. **Released weight** (below).
6. **Identifiers.** Person and household identifiers are replaced by random
   numbers; household membership is kept so that household-level analysis
   remains possible.

| Scenario | Region | Age | Other keys | k |
|---|---|---|---|---|
| S1 | NUTS 2 | 5-year bands | source detail | 3 |
| S2 | NUTS 1 | 10-year bands | grouped | 3 |
| S3 | NUTS 1 | 10-year bands | grouped | 5 |

S1 shows what happens when the release keeps detail and relies on
suppression: occupation loses 13,596 values (37% of records) and S1 takes
about 9 of the 15 minutes of a full run. S2 and S3 show that recoding first
is cheaper: after recoding only 2,669 sample uniques remain, and suppression
blanks a few thousand values.

**Released weight.** The original personal weight takes one value per
NUTS 2 region in the public file (21 values). Released unchanged, it would
reveal the NUTS 2 region in S2 and S3, which publish only NUTS 1. The
released weight is the original weight post-stratified to the released
region (NUTS 2 in S1, NUTS 1 in S2 and S3). Let `T_r` be the sum of original
weights of the records whose region at the released level is `r`, `n_r` their
number, `T` and `n` the national totals, and `s_r` the number of records of
`r` whose region is blanked in the release. Records with a blanked region all
get one common weight `w_s`; the other records of `r` get
`w_r = (T_r − s_r · w_s) / (n_r − s_r)`. Regional and national totals of the
weights are preserved exactly for any `w_s`; the weight is constant within
the released region, so it carries no information below it, and the weight
of a record with a blanked region does not depend on its region. The common
weight is the national mean `T / n`, lowered to the mean `T_r / n_r` of any
region with blanked records whose mean is lower:
`w_s = min(T / n, min over r with s_r > 0 of T_r / n_r)`. Then
`w_r − T_r / n_r = s_r · (T_r / n_r − w_s) / (n_r − s_r) ≥ 0`, so the other
records of every region get at least their region's mean weight, and a
low-weight region with many blanked records keeps a positive weight. If a
region has every record blanked, the run stops with an error naming the
region rather than release such a weight.

The rule has a cost. In S1, where the weight is already constant within each
NUTS 2 region, it changes the weights only through the records with a
blanked region: they get the common weight and the other records of their
region absorb the difference. On the 2013 file 458 records have their region
blanked in S1, 51 of them among the 643 records of Valle d'Aosta (ITC2),
the region with the lowest mean weight (166). The common weight is therefore
166 rather than the national mean of 1,388, and the other records of ITC2
keep their weight of 166; with the national mean they would fall to 61. The
regions with heavier weights absorb the difference: the largest increase is
in Bolzano (ITH1), where 39 of 347 records are blanked and the weight of the
others rises from 1,224 to 1,357 (+10.9%). In S2 and S3 the rule removes the correction for the different
sampling rates of the NUTS 2 regions inside each NUTS 1 area: Umbria, with about 490 persons per record,
and Lazio, with about 1,800, get the same weight. This cannot be avoided by
another choice of weight: a weight that corrects the NUTS 2 composition must
differ between NUTS 2 regions, and so reveals them. Section 6 measures the
cost. Real EU-SILC weights also vary within regions by design (non-response
adjustment and calibration); a production release would calibrate the
released weight to several published margins, for example sex and age band
within NUTS 1, and run the release checks of section 5 on the result.

## 5. Release checks

Five checks run on the release file of every scenario before it is written;
the run stops if one fails. The results are in `output/release_checks.csv`
and in the report.

- `min_fk_with_weight`: the smallest *f_k* when the released weight is added
  to the eight keys as a categorical variable. It must be at least *k*. This
  is the attack that the original weight allowed: as a key it told apart the
  NUTS 2 regions inside a NUTS 1 area.
- `weights_spanning_one_nuts2`: the number of released weight values whose
  records all come from a single NUTS 2 region. It must be 0 in S2 and S3.
  It does not apply to S1, which publishes NUTS 2, and is reported as not
  applicable there rather than as passed.
- `households_inconsistent_keys`: households in which region or household
  size is blank for some members and not for others. It must be 0.
- `hh_size_blanked`: the number of records whose household size is blank in
  the release. It must be 0.
- `forbidden_columns`: how many of equivalised household size, household
  disposable income, the survey person and household identifiers and exact
  age appear in the release. It must be 0.

Records with a blanked region need a rule in the first two checks. They all
share the common weight `w_s`, and that value says only that the region is
blank, which the blank itself already says. In `min_fk_with_weight` their
weight is therefore treated as blank too, matching any value like their
region, and in `weights_spanning_one_nuts2` that value is not counted. If
these records did not share one weight, their weights would be used as they
are. Without the rule every record with a blanked region would form a group
of its own under the weight key, and the check would report a group below *k*
although the weight adds nothing; with the original weights the check still
fails, as it should.

Results on the 2013 file (all checks pass in every scenario):

| Check | S1 | S2 | S3 |
|---|---:|---:|---:|
| `min_fk_with_weight` (k = 3, 3, 5) | 3 | 3 | 5 |
| `weights_spanning_one_nuts2` | n/a | 0 | 0 |
| `households_inconsistent_keys` | 0 | 0 | 0 |
| `forbidden_columns` | 0 | 0 | 0 |

## 6. Information loss

Utility is measured on what a user would compute, using the same code on the
original and on the protected file.

- **Suppressed values** per key variable and as a share of all key values.
- **Indicators.** At-risk-of-poverty (AROP) rate, computed as in Eurostat's
  `ilc_li02` (threshold at 60% of the national weighted median equivalised
  disposable income) but on persons aged 16 and over with a personal weight,
  because only the personal file is released; nationally and by NUTS 1.
  Median equivalised income; Gini; mean and median employee income,
  nationally and by sex.
- **Loss by source.** Each indicator is computed on four files: the
  original; the original with each record's weight replaced by its released
  weight (weight effect); the original with employee and equivalised income
  replaced by their microaggregated values (income effect); and the release
  file (total effect, which also includes blanked domains). The relative
  differences from the original are reported side by side. The two partial
  effects do not add up exactly to the total, because they interact and
  because records with a blanked region drop out of the NUTS 1 domains.
- **Regression.** Weighted least squares of log employee income on sex, age
  band and NUTS 1, for employees with positive income. For each coefficient
  the confidence-interval overlap (Karr et al., 2006) is
  `0.5 × (I / (U_o − L_o) + I / (U_p − L_p))`, where `I` is the length of the
  intersection of the original and protected 95% intervals. The model is
  fitted with `lm(weights = weight)`, which treats the sampling weights as
  precision weights, so the intervals are not design-based; they are kept
  as a device for comparing the two files, not as valid inference.

Finding: microaggregation moves no indicator by more than 0.27%, in any
scenario. In S1 the weight effect is at most 0.46%; the largest total change,
0.52% on the AROP rate of Nord-Ovest, comes from the records with a blanked
region, which drop out of the regional figures and carry the low common
weight. In S2 and S3 the weight is
the dominant source: it moves the AROP rate of Centro by −2.98%, the national
AROP rate by −0.97% and mean employee income by −2.42%, and the total changes
are about the same. The confidence-interval overlap of the wage regression
falls from 0.94 in S1 to 0.79 in S2 and S3 for the same reason. Moving from
k = 3 to k = 5 leaves the indicators unchanged; the scenarios differ there in
the number of blanked values, which is what the risk-utility map plots.

## 7. Table protection

**Table.** Estimated total employee income (income × weight) by region
(Italy > NUTS 1 > NUTS 2), occupation (ISCO major group) and sex, for
employees with positive income and a known occupation. With all margins the
table has 810 cells.

**Primary suppression** (`primarySuppression`):

- *threshold rule*: a cell with 1 or 2 contributors is sensitive;
- *p% rule* with p = 10: a cell is sensitive if the second-largest
  contributor, by subtracting its own value from the cell total, could
  estimate the largest contributor's value to within 10%.

**Secondary suppression** (`protectTable`). A single suppressed cell in a
row can be recomputed as the row total minus the published cells, so further
cells are hidden across the region hierarchy and the other two dimensions.
sdcTable's quick heuristic (`SIMPLEHEURISTIC`) is tried first, then its
optimisation method (`OPT`); the first pattern that passes the audit below is
published.

**Audit.** The audit is independent of sdcTable and works on income, the
published variable. The unknowns are the 810 cells. The constraints are the
sums of the table, for every combination of the other two dimensions: Italy
equals the sum of the NUTS 1 areas, each NUTS 1 area the sum of its NUTS 2
regions (parent equal to child when an area has a single region), the
occupation total the sum of the nine groups, the sex total men plus women.
Published cells are fixed at their value; suppressed cells are only required
to be non-negative. For each sensitive cell two linear programs (solved with
HiGHS) give the lowest and highest value the cell can take, `L` and `U`: the
interval an intruder can derive from the published table.

The interval must be wide enough for the p% rule (Hundepool et al., 2012).
With `X` the cell value and `x1 ≥ x2` its two largest contributions, the
protection level is

`PL = max(0, p/100 · x1 − (X − x1 − x2))`,

the smallest error the second-largest contributor must be left with when
estimating the largest contribution from the cell value; with less, it could
estimate `x1` to within p%. The cell is protected if `U − X ≥ PL` and
`X − L ≥ PL` (relative tolerance 10⁻⁹ · X). The same formula is applied to
cells sensitive under the threshold rule: with one contributor `PL` is p% of
the cell value, with two it is p% of the larger contribution.

**Additions driven by the audit.** sdcTable's protection levels act on the
number of contributors, not on income, so a pattern it accepts can leave the
income interval of a sensitive cell narrower than `PL`. When the `OPT` pattern
fails the audit, cells are added to it. A single extra cell never helps:
every cell sits in three sums and stays recomputable from the other two.
The candidates are therefore suppression structures around the failing cell
`c = (r, o, s)`: the corners of the rectangles {r, r'} × {o, o'}, {r, r'} ×
{s, s'} and {o, o'} × {s, s'} and of the cubes {r, r'} × {o, o'} × {s, s'},
where r' is a region with the same parent, o' another occupation and s' the
other sex; for a margin cell, the sibling is paired with a chain of children
so that the margin can move. Structures with an empty cell are skipped. Each
round takes the failing cell with the largest shortfall, hides the structure
that protects it at the least income newly hidden (if none does, the one
that widens its interval most) and runs the audit again. Extra suppressions
only widen intervals, so cells already protected stay protected; if no
structure widens the interval, the run stops.

On the 2013 file both `SIMPLEHEURISTIC` and `OPT` leave sensitive cells
with intervals narrower than `PL`, so the published pattern is the `OPT`
pattern plus 13 cells added by the audit: 27 primary and 64 secondary
suppressions out of 810 cells, hiding 4.9% of the income of the detailed
cells. With the primary suppressions alone none of the 27 sensitive cells is
protected; with the secondary suppressions all 27 are.

The public outputs give the suppression pattern and, for each sensitive
cell, only whether it is protected; the cell values, contributions and
intervals used by the audit are kept out of the published outputs, because
they would let an intruder narrow down the suppressed values.

## 8. Limitations and next steps

- **Synthetic source.** The risk levels describe the synthetic file only.
- **One intruder scenario.** Risk depends on the key set. A sensitivity
  analysis over key sets would strengthen the release decision.
- **Outliers.** Microaggregation in groups of 3 still releases values close
  to the top incomes. A production release would add top-coding checked
  against the income distribution, or noise on the top tail.
- **Released weight.** The weight is constant within each released region.
  Real EU-SILC weights vary within regions by design; a production release
  would calibrate to several published margins and check the result with the
  same release checks. In S2 and S3 no released weight can restore the
  NUTS 2 composition without revealing NUTS 2 (section 4).
- **Blanked regions and regional totals.** The released weights keep each
  region's total, and records with a blanked region share one common
  weight. Anyone who knows the official NUTS 1 population totals can
  therefore work out how many records with a blanked region belong to each
  NUTS 1 area, though not which ones. On the 2013 file 7 records in S3 have
  their region blanked: 1 in ITC, 1 in ITF and 5 in ITG.
- **Secondary suppression method.** The cells added after the audit are
  chosen one failing cell at a time, so the final pattern is protected but
  not minimal. sdcTable's protection levels work on counts; τ-ARGUS's
  modular method with protection levels on income would usually hide less.
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
