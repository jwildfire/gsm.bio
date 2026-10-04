# gsm.bio

`gsm.bio` holds the statistics behind the [bio.viz](https://github.com/jwildfire/bio.viz) biomarker charts. A chart never computes a test: it hands over a table with one row per participant, and R returns the estimates, intervals, p-value, method name and counts the chart prints.

Each statistic is a thin wrapper around a function from the stats or survival package, so the number on the chart is R's own. The statistics use nothing else, which lets the same functions run in a desktop session, in R in the browser and on a server. The package also draws the charts from R, as widgets that carry R's answers in the page.

## Installation

```r
# install.packages("remotes")
remotes::install_github("jwildfire/gsm.bio@v0.1.0") # the v0.1.0 release, from its tag
remotes::install_github("jwildfire/gsm.bio@dev")    # what is on dev, the integration branch
```

[NEWS](https://github.com/jwildfire/gsm.bio/blob/dev/NEWS.md) lists what each release holds, and what is on `dev` for the next.

## Statistics

Each function takes a data frame with one row per participant and the names of its columns, and returns the same plain list: a status, the method's name as R reports it, the estimates with their intervals, the statistic, the p-value, the participants used, what was dropped and why, and a reason in place of the numbers when a group is too small.

| Function | Wraps |
|---|---|
| `Analyze_GroupDifference()` | `t.test()` (Welch), `wilcox.test()`, `aov()`, `kruskal.test()`; pairwise comparisons adjusted by `p.adjust()` |
| `Analyze_Correlation()` | `cor.test()`, Pearson or Spearman, overall and per group |
| `Analyze_CorrelationMatrix()` | `cor.test()` on every pair of columns, with the pair count per cell |
| `Analyze_Fit()` | `lm()` for a line, with the slope and intercept, their intervals from `confint()` and the confidence band from `predict()`; `loess()` for a smooth, with its band from `predict(se = TRUE)`; overall and per group, as points a chart draws as they are |
| `Analyze_Contingency()` | `chisq.test()`, with small expected counts flagged, and `fisher.test()` |
| `Analyze_Survival()` | `survdiff()` for the log-rank test, `survfit()` for median survival with its log-log interval, `coxph()` for the hazard ratio between two groups |
| `Analyze_Screen()` | One row per biomarker for one comparison: a standardised difference between two groups, a correlation with one variable, or a hazard ratio for high against low; p-values adjusted across the rows by `p.adjust()` |

```r
library(gsm.bio)
lResult <- Analyze_Contingency(Synthetic_Participants, "ARM", "RESPONSE")
lResult$method
lResult$p_value
```

Nothing is reimplemented, with one exception: the standardised difference in the screen is a few lines of the package's own, checked against `effectsize::hedges_g()`.

The functions are defined once, in one file that needs nothing but the stats and survival packages: `system.file("statistics", "statistics.R", package = "gsm.bio")`. The package is built from that file, and a chart can hand the same file to R in the browser.

A result is the answer of the R that computed it. R's own defaults can differ between versions, so a chart and a report agree when they run the same version of R.

## Widgets

`Widget_GroupComparison()` draws bio.viz's group comparison chart from R: one biomarker value across the levels of a category, as boxes, violins or points, with a test of the groups under each panel.

```r
library(gsm.bio)
Widget_GroupComparison(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(value_type = "change", baseline_visits = "Baseline", group_by = "ARM")
)
```

It takes the results table, optionally the participant table, and the chart's settings as a list under bio.viz's own names. The chart opens on an overview of every biomarker at every visit, which prints no test; a click on a biomarker opens it alone, with a panel for each visit and a test under each. `start_value = "IL-6"` opens that biomarker straight away.

The tests are computed in R when the widget is made, by `Analyze_GroupDifference()`, for each biomarker at each visit panel the chart draws at the widget's settings, and are stored in the page. Saved with `htmlwidgets::saveWidget()`, the page is one file that shows them with no R and no network, and says under the chart which R version and gsm.bio version computed them. A view that was not computed, such as another test or a filter, says that statistics are unavailable for it; it never shows another view's numbers.

`Widget_AssociationScatter()` draws bio.viz's association scatter the same way: one point per participant with a variable on each axis, a correlation coefficient under each panel and, when asked for, a fitted line.

```r
Widget_AssociationScatter(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(
    x = list(measure = "TNF-alpha", visit = "Baseline"),
    y = list(measure = "IL-10", visit = "Baseline"),
    color_by = "ARM",
    fit = "linear"
  )
)
```

For the pair the settings open on it stores `Analyze_Correlation()`'s answer for Pearson's and for Spearman's coefficient and `Analyze_Fit()`'s for the linear fit and for the smooth, four results for each panel, so the Method and Fitted line controls of a saved page are answered. The fitted line is drawn from R's stored points. Another variable, colour, filter or scale is a view that was not computed, and says so.

`Widget_CorrelationMatrix()` draws bio.viz's correlation matrix: a grid over several biomarkers at one visit, or one biomarker at several visits, each pair a cell with its coefficient and its own pair count, and no p-values. A click on a cell opens that pair's association scatter in place, with a way back.

```r
Widget_CorrelationMatrix(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(visit = "Baseline")
)
```

It stores `Analyze_CorrelationMatrix()`'s answer for the grid the settings open on and, for every cell, exactly what the scatter that cell opens asks of R, so a saved page opens any cell's scatter with its coefficient, interval and p-value. On the synthetic study that is one grid and 132 scatters, one for each cell on either side of the diagonal.

`Widget_BiomarkerScreen()` draws bio.viz's biomarker screen: one row per biomarker at one visit, each with its estimate and interval on one shared axis and its raw and adjusted p-values, as a standardised difference between two groups or a correlation with one variable. A click on a row opens that biomarker's group comparison or association scatter in place, with a way back.

```r
Widget_BiomarkerScreen(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(visit = "Week 4", value_type = "change", group_by = "ARM")
)
```

It stores `Analyze_Screen()`'s answer for the screen the settings open on and, for every row, exactly what the chart that row opens asks of R: for a difference the group comparison at the screen's one visit, of the two groups, with Welch's test; for a correlation the scatter with the biomarker along the bottom and the variable up the side. On the synthetic study, Placebo against Treatment, that is one screen and twelve group comparisons.

`Widget_CrossTab()` draws bio.viz's cross-tabulation: a two-way table of counts with its totals and percentages, beside stacked bars, and R's chi-square or Fisher's exact test under it. A click on a count lists that cell's participants.

```r
Widget_CrossTab(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(row_by = "RESPONSE", col_by = list(measure = "CRP", visit = "Baseline", cut = "median"))
)
```

It stores `Analyze_Contingency()`'s answer for the table the settings open on, by both tests. Either variable of the table, and the groups or the panels of `Widget_GroupComparison()`, can be a biomarker or a number cut into groups at its median, tertiles, quartiles or typed points, by the cut rule bio.viz uses in every chart: `quantile()` with its default for the points, and `cut()` with a value on a point in the lower group.

Every widget carries two JavaScript bundles, both copied from bio.viz with the commit and a checksum per file recorded in `inst/htmlwidgets/lib/SOURCE.json`: bio.viz v0.1.0, and safety.viz v1.9.0, the first safety.viz with the kit bio.viz's charts are built from. The safety.viz copy is the one bio.viz takes from safety.viz's `dev` branch at its v1.9.0 release preparation, and its commit is recorded too. gsm.safety carries an earlier safety.viz without the kit; once it carries v1.9.0, the widgets can take the bundle from there instead of carrying their own copy.

## Synthetic study

The package ships a made-up biomarker study, so that a test can assert an answer known in advance: `Synthetic_Results` (one row per participant, biomarker and visit), `Synthetic_Participants` and `Synthetic_Outcomes`. Three effects are planted in it and every other biomarker is null. `Synthetic_Truth` holds the true size of each: a difference between arms in change from baseline, a correlation between two biomarkers, and a hazard ratio between high and low baseline levels. The same tables are CSV files under `inst/extdata/`. No real study data is used.

## Status

[Version 0.1.0](https://github.com/jwildfire/gsm.bio/releases/tag/v0.1.0) is released: seven statistics functions, the synthetic study and four widgets, for the group comparison chart, the association scatter, the correlation matrix and the biomarker screen. Version 0.2.0 is in development on `dev`; widgets for the other charts and static figures come in later versions. [NEWS.md](https://github.com/jwildfire/gsm.bio/blob/dev/NEWS.md) lists what has landed, and the reference site is at <https://jwildfire.github.io/gsm.bio/>.

The design is on the obot roadmap: [bio.viz and gsm.bio](https://jwildfire.github.io/obot.roadmap/requirements/design/353_design.html).

## Licence

Apache License 2.0. See [LICENSE](https://github.com/jwildfire/gsm.bio/blob/dev/LICENSE).
