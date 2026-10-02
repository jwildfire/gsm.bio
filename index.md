# gsm.bio

`gsm.bio` holds the statistics behind the
[bio.viz](https://github.com/jwildfire/bio.viz) biomarker charts. A
chart never computes a test: it hands over a table with one row per
participant, and R returns the estimates, intervals, p-value, method
name and counts the chart prints.

Each statistic is a thin wrapper around a function from the stats or
survival package, so the number on the chart is R’s own. The package
imports nothing else, which lets the same functions run in a desktop
session, in R in the browser and on a server.

## Installation

Version 0.1.0 is in development on the `dev` branch:

``` r

# install.packages("remotes")
remotes::install_github("jwildfire/gsm.bio@dev")
```

## Statistics

Each function takes a data frame with one row per participant and the
names of its columns, and returns the same plain list: a status, the
method’s name as R reports it, the estimates with their intervals, the
statistic, the p-value, the participants used, what was dropped and why,
and a reason in place of the numbers when a group is too small.

| Function | Wraps |
|----|----|
| [`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md) | [`t.test()`](https://rdrr.io/r/stats/t.test.html) (Welch), [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html), [`aov()`](https://rdrr.io/r/stats/aov.html), [`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html); pairwise comparisons adjusted by [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) |
| [`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md) | [`cor.test()`](https://rdrr.io/r/stats/cor.test.html), Pearson or Spearman, overall and per group |
| [`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md) | [`cor.test()`](https://rdrr.io/r/stats/cor.test.html) on every pair of columns, with the pair count per cell |
| [`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md) | [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html), with small expected counts flagged, and [`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) |

``` r

library(gsm.bio)
lResult <- Analyze_Contingency(Synthetic_Participants, "ARM", "RESPONSE")
lResult$method
lResult$p_value
```

The functions are defined once, in one file that needs nothing but the
stats package:
`system.file("statistics", "statistics.R", package = "gsm.bio")`. The
package is built from that file, and a chart can hand the same file to R
in the browser.

## Synthetic study

The package ships a made-up biomarker study, so that a test can assert
an answer known in advance: `Synthetic_Results` (one row per
participant, biomarker and visit), `Synthetic_Participants` and
`Synthetic_Outcomes`. Three effects are planted in it and every other
biomarker is null. `Synthetic_Truth` holds the true size of each: a
difference between arms in change from baseline, a correlation between
two biomarkers, and a hazard ratio between high and low baseline levels.
The same tables are CSV files under `inst/extdata/`. No real study data
is used.

## Status

Version 0.1.0 is in development. The group, correlation and contingency
statistics and the synthetic study are in; the survival statistics and
the biomarker screen arrive next, in the same version.
[NEWS.md](https://github.com/jwildfire/gsm.bio/blob/dev/NEWS.md) lists
what has landed, and the reference site is at
<https://jwildfire.github.io/gsm.bio/>.

The design is on the obot roadmap: [bio.viz and
gsm.bio](https://jwildfire.github.io/obot.roadmap/requirements/design/353_design.html).

## Licence

Apache License 2.0. See
[LICENSE](https://github.com/jwildfire/gsm.bio/blob/dev/LICENSE).
