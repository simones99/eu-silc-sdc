# eu-silc-sdc

Statistical disclosure control (SDC) on EU-SILC microdata, in R with
[sdcMicro](https://cran.r-project.org/package=sdcMicro) and
[sdcTable](https://cran.r-project.org/package=sdcTable).

> **The input data are synthetic.** The project uses Eurostat's EU-SILC
> *public microdata* for Italy (2013). Eurostat generates these files from
> models of the real survey: they contain no real person and cannot be used
> to draw conclusions about the population. The project treats the file
> **as if it were confidential** and applies the procedure a statistical
> office would apply to a real file before release. The risk figures and
> indicator values illustrate the method; they say nothing about real people
> or regions.

**Report:** https://simones99.github.io/eu-silc-sdc/

## What it does

A statistical office wants to publish two things from a household survey:
a microdata file that anyone can download, and a table of income by region,
occupation and sex. Removing names is not enough, because a person can be
recognised from a combination of ordinary facts (region, age, household
size, occupation...). The project:

1. **Measures re-identification risk** in a file of 37,209 persons:
   k-anonymity and the model-based individual and household risk
   (Benedetti-Franconi, as used by Istat).
2. **Protects the file** under three scenarios, each combining global
   recoding, local suppression (consistent within households) and
   microaggregation of incomes; replaces survey identifiers with random
   numbers and the weight with one that reveals nothing below the released
   region; and checks the release file before writing it.
3. **Measures what users lose**: blanked values, the at-risk-of-poverty
   rate, Gini, mean and median incomes, and the confidence-interval overlap
   of a wage regression.
4. **Protects a magnitude table** with primary suppression (threshold rule
   and p% rule) and secondary suppression across the region hierarchy, then
   **audits it** on income: for every sensitive cell, linear programs give
   the interval an intruder can derive, which must respect the p% protection
   level.

## Results (synthetic data)

| | Original | S1 | S2 | **S3** |
|---|---:|---:|---:|---:|
| Region / age detail | NUTS 2 / 1 year | NUTS 2 / 5 years | NUTS 1 / 10 years | NUTS 1 / 10 years |
| Anonymity target | none | k = 3 | k = 3 | k = 5 |
| Sample uniques | 24,337 | 0 | 0 | 0 |
| Records shared by fewer than 5 | 34,387 | 4,083 | 2,483 | **0** |
| Smallest group, weight added to the keys | – | 3 | 3 | **5** |
| Expected re-identifications | 178.0 | 5.8 | 2.9 | **1.8** |
| Key values blanked | – | 27,883 | 5,223 | 9,634 |
| Largest change in an indicator: weight only | – | 0.18% | 2.98% | 2.98% |
| Largest change in an indicator: incomes only | – | 0.27% | 0.27% | 0.27% |
| Largest change in an indicator: released file | – | 0.46% | 2.98% | 2.98% |
| Mean CI overlap, wage regression | – | 0.98 | 0.79 | 0.79 |

**S3 is the release candidate.** Recoding first removes most rare
combinations, so reaching 5-anonymity costs far fewer blanked values than
keeping regional detail (S1). The cost of S2 and S3 is in the weight: it
cannot differ between the NUTS 2 regions of a NUTS 1 area without revealing
them, so it no longer corrects for their different sampling rates, and
indicators move by up to 2.98% (AROP rate of Centro). Microaggregation alone
moves no indicator by more than 0.27%.

Release checks pass in every scenario: with the released weight added to
the keys the smallest group is still at least k, no released weight points
to a single NUTS 2 region (S2, S3), no household has a household key blank
for some members only, and the release has no equivalised size, household
disposable income, survey identifiers or exact age.

Income table (810 cells with margins): 27 sensitive cells, 64 secondary
suppressions (sdcTable's `OPT` pattern plus 13 cells added by the income
audit), 4.9% of income hidden in the detailed cells. The audit checks every
sensitive cell's interval against its p% protection level: 0 of 27 cells
are protected with the primary suppressions alone, 27 of 27 with the
secondary suppressions.

## Run it

Requires R 4.2 or later. Package versions are pinned in `renv.lock`.

```sh
Rscript -e 'renv::restore()'            # installs the pinned packages
Rscript scripts/run_pipeline.R          # about 15 minutes; S1 takes most of it
Rscript scripts/render_report.R         # writes site/index.html
Rscript -e 'testthat::test_local()'     # unit tests on small synthetic data
```

`run_pipeline.R S2 S3` runs only the listed scenarios. The source zip is
downloaded once into `data/raw/` and checked against the SHA-256 in
[`data/manifest.json`](data/manifest.json); if Eurostat changes the file the
run stops.

## Layout

| Path | Content |
|---|---|
| `R/ingest.R` | download and hash check, reading the four EU-SILC files |
| `R/prepare.R` | person-level file with household variables |
| `R/risk.R` | key variables, sdcMicro object, risk measures |
| `R/protect.R` | recoding rules, scenarios, release file |
| `R/weights.R` | released weights |
| `R/release_checks.R` | checks on the release file |
| `R/utility.R` | poverty rate, Gini, weighted quantiles, loss by source, CI overlap |
| `R/tables.R` | magnitude table, cell suppression |
| `R/table_audit.R` | income audit of the suppression pattern, audit-driven additions |
| `reports/report.Rmd` | the published report |
| `docs/methodology.md` | methods, choices and limitations in detail |
| `.github/workflows/` | tests on every push; `publish` runs the pipeline and deploys the report |

## Sources

- Eurostat, [EU-SILC public microdata](https://ec.europa.eu/eurostat/web/microdata/public-microdata/statistics-on-income-and-living-conditions).
- Hundepool A. et al. (2012), *Statistical Disclosure Control*, Wiley.
- Templ M. (2017), *Statistical Disclosure Control for Microdata: Methods and Applications in R*, Springer.
- Karr A. F. et al. (2006), "A framework for evaluating the utility of data altered to protect confidentiality", *The American Statistician* 60(3).
- Regulation (EC) No 223/2009 on European statistics, Article 20.

## License

Code: MIT. Data: Eurostat, reuse authorised under the
[Eurostat copyright notice](https://ec.europa.eu/eurostat/about-us/policies/copyright);
the data are not redistributed in this repository.
