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
   recoding, local suppression and microaggregation of incomes, and replaces
   survey identifiers with random numbers.
3. **Measures what users lose**: blanked values, the at-risk-of-poverty
   rate, Gini, mean and median incomes, and the confidence-interval overlap
   of a wage regression.
4. **Protects a magnitude table** with primary suppression (threshold rule
   and p% rule) and secondary suppression across the region hierarchy, then
   **audits it** by solving the intruder's linear programs for every
   sensitive cell.

## Results (synthetic data)

| | Original | S1 | S2 | **S3** |
|---|---:|---:|---:|---:|
| Region / age detail | NUTS 2 / 1 year | NUTS 2 / 5 years | NUTS 1 / 10 years | NUTS 1 / 10 years |
| Anonymity target | none | k = 3 | k = 3 | k = 5 |
| Sample uniques | 24,337 | 0 | 0 | 0 |
| Records shared by fewer than 5 | 34,387 | 8,275 | 2,591 | **0** |
| Expected re-identifications | 178.0 | 8.1 | 2.9 | **1.8** |
| Key values blanked | – | 25,889 | 5,140 | 9,538 |
| Largest change in an income indicator | – | 0.27% | 0.27% | 0.27% |
| Mean CI overlap, wage regression | – | 0.98 | 0.99 | 0.99 |

**S3 is the release candidate.** Recoding first removes most rare
combinations, so reaching 5-anonymity costs far fewer blanked values than
keeping regional detail (S1). Income indicators are practically unchanged
in every scenario.

Income table (810 cells with margins): 27 sensitive cells, 64 secondary
suppressions, 5.1% of income hidden in the detailed cells. Without secondary
suppression all 27 sensitive cells could be recomputed exactly from the
margins; with it, none can.

## Run it

Requires R 4.2 or later. On macOS, sdcTable needs GLPK (`brew install glpk`).

```sh
Rscript -e 'install.packages(c("sdcMicro", "sdcTable", "sdcHierarchies", "data.table", "digest", "jsonlite", "ggplot2", "pkgload", "rmarkdown", "knitr", "scales", "testthat", "withr"))'
Rscript scripts/run_pipeline.R          # about 12 minutes; S1 takes most of it
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
| `R/utility.R` | poverty rate, Gini, weighted quantiles, CI overlap |
| `R/tables.R` | magnitude table, cell suppression, audit |
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
