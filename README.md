# gsm.bio

`gsm.bio` holds the statistics behind the [bio.viz](https://github.com/jwildfire/bio.viz) biomarker charts. A chart never computes a test: it hands over a table with one row per participant, and R returns the estimates, intervals, p-value, method name and counts the chart prints.

Each statistic is a thin wrapper around a function from the stats or survival package, so the number on the chart is R's own. The statistics use nothing else, which lets the same functions run in a desktop session, in R in the browser and on a server. The package also draws the charts from R, as widgets that carry R's answers in the page.

## Installation

```r
# install.packages("remotes")
remotes::install_github("jwildfire/gsm.bio@v0.3.0") # the v0.3.0 release, from its tag
remotes::install_github("jwildfire/gsm.bio@dev")    # what is on dev, the integration branch
```

[NEWS](https://github.com/jwildfire/gsm.bio/blob/dev/NEWS.md) lists what each release holds, and what is on `dev` for the next.

## Statistics

Each function takes a data frame with one row per participant and the names of its columns (the two that answer by level take one row per participant and level), and returns the same plain list: a status, the method's name as R reports it, the estimates with their intervals, the statistic, the p-value, the participants used, what was dropped and why, and a reason in place of the numbers when a group is too small.

| Function | Wraps |
|---|---|
| `Analyze_GroupDifference()` | `t.test()` (Welch), `wilcox.test()`, `aov()`, `kruskal.test()`; pairwise comparisons adjusted by `p.adjust()` |
| `Analyze_GroupDifferenceBy()` | The same test within each level of a column, such as each visit of one biomarker, in one call: one row per level, with the p-values adjusted across the levels by `p.adjust()` when a method is named |
| `Analyze_Correlation()` | `cor.test()`, Pearson or Spearman, overall and per group |
| `Analyze_CorrelationMatrix()` | `cor.test()` on every pair of columns, with the pair count per cell |
| `Analyze_Fit()` | `lm()` for a line, with the slope and intercept, their intervals from `confint()` and the confidence band from `predict()`; `loess()` for a smooth, with its band from `predict(se = TRUE)`; overall and per group, as points a chart draws as they are |
| `Analyze_Contingency()` | `chisq.test()`, with small expected counts flagged, and `fisher.test()` |
| `Analyze_Survival()` | `survdiff()` for the log-rank test, `survfit()` for median survival with its log-log interval, `coxph()` for the hazard ratio between two groups |
| `Analyze_Screen()` | One row per biomarker for one comparison: a standardised difference between two groups, a correlation with one variable, or a hazard ratio for high against low; p-values adjusted across the rows by `p.adjust()` |
| `Analyze_DifferenceGrid()` | The screen's standardised difference between two groups for every biomarker at every level of a column, such as every visit, in one call: one row per biomarker and level, from long data, with no p-value |

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

`Widget_GroupComparison()` draws bio.viz's group comparison chart from R: how a biomarker value differs between the levels of a category, at three levels. It opens on a trend tile for every biomarker, a tile opens one biomarker across its visits with a test of the groups under each visit, and a visit opens that visit alone, as boxes, violins or points, with a test under each panel.

```r
library(gsm.bio)
Widget_GroupComparison(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(value_type = "change", baseline_visits = "Baseline", group_by = "ARM")
)
```

It takes the results table, optionally the participant table, and the chart's settings as a list under bio.viz's own names. The tiles draw each group's median across the visits and print no test. `start_value = "IL-6"` opens that biomarker over time straight away, and `visits` names the visits to open as panels. Unscheduled visits are left out at every level unless `unscheduled_visits = TRUE`.

The tests are computed in R when the widget is made and are stored in the page: for each biomarker, `Analyze_GroupDifferenceBy()`'s test under every visit of its picture over time, with the p-values as R gives them and under the adjustment `visit_adjustment` names when it names one, and `Analyze_GroupDifference()`'s test for each visit alone. Saved with `htmlwidgets::saveWidget()`, the page is one file that shows them with no R and no network, and says under the chart which R version and gsm.bio version computed them. A view that was not computed, such as another test or a filter, says that statistics are unavailable for it; it never shows another view's numbers.

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

`Widget_BiomarkerScreen()` draws bio.viz's biomarker screen: one row per biomarker at one visit, each with its estimate and interval on one shared axis and its raw and adjusted p-values, as a standardised difference between two groups, a correlation with one variable or, given an outcomes table, a hazard ratio of high against low. A click on a row opens that biomarker's group comparison, association scatter or survival curves in place, with a way back.

```r
Widget_BiomarkerScreen(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(visit = "Week 4", value_type = "change", group_by = "ARM")
)
```

It stores `Analyze_Screen()`'s answer for the screen the settings open on and, for every row, exactly what the chart that row opens asks of R: for a difference the group comparison at the screen's one visit, of the two groups, with Welch's test; for a correlation the scatter with the biomarker along the bottom and the variable up the side; for a hazard ratio the survival curves of the biomarker cut where the screen cut it. On the synthetic study, Placebo against Treatment, that is one screen and twelve group comparisons. The outcomes table is `dfOutcomes`, the screen's last argument, given by name, so every v0.1.0 call works as it did:

```r
Widget_BiomarkerScreen(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(comparison = "hazard", visit = "Baseline", endpoint = "EFS"),
  dfOutcomes = Synthetic_Outcomes
)
```

`Widget_CrossTab()` draws bio.viz's cross-tabulation: a two-way table of counts with its totals and percentages, beside stacked bars, and R's chi-square or Fisher's exact test under it. A click on a count lists that cell's participants.

```r
Widget_CrossTab(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(row_by = "RESPONSE", col_by = list(measure = "CRP", visit = "Baseline", cut = "median"))
)
```

It stores `Analyze_Contingency()`'s answer for the table the settings open on, by both tests. Either variable of the table, and the groups or the panels of `Widget_GroupComparison()`, can be a biomarker or a number cut into groups at its median, tertiles, quartiles or typed points, by the cut rule bio.viz uses in every chart: `quantile()` with its default for the points, and `cut()` with a value on a point in the lower group.

`Widget_StratifiedSurvival()` draws bio.viz's stratified survival chart: a Kaplan-Meier curve for each group on one endpoint of an outcomes table, the number at risk beneath, and R's log-rank test, each group's median and the hazard ratio under them. The outcomes table is read as ADaM holds time to event (`PARAMCD`, `PARAM`, `AVAL`, and `CNSR` with 1 for censored), or with an event flag the other way round. A click on a curve or a count at risk lists those participants.

```r
Widget_StratifiedSurvival(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(endpoint = "EFS", group_by = list(measure = "CRP", visit = "Baseline", cut = "median")),
  dfOutcomes = Synthetic_Outcomes
)
```

It stores `Analyze_Survival()`'s answer for the curves the settings open on. A cut's groups are handed to R high to low, so the hazard ratio is the higher group's hazard over the lower's.

Every widget carries two JavaScript bundles, both copied from bio.viz with the commit and a checksum per file recorded in `inst/htmlwidgets/lib/SOURCE.json`: bio.viz, as its `dev` branch stands at the recorded commit, and safety.viz v1.9.0, the first safety.viz with the kit bio.viz's charts are built from. When the copy of bio.viz is byte for byte one of its releases the record names the release. The safety.viz copy is the one bio.viz takes from safety.viz's `dev` branch at its v1.9.0 release preparation, and its commit is recorded too. gsm.safety carries an earlier safety.viz without the kit; once it carries v1.9.0, the widgets can take the bundle from there instead of carrying their own copy.

## The app

`RunApp()` is the six charts as one Shiny app: a list of the charts beside one chart drawn at a time, with every statistic computed on request by the R session behind the page.

```r
library(gsm.bio)

RunApp()                                   # the synthetic study
RunApp(dfResults, dfParticipants)          # a study's own tables
```

It reads its tables under gsm.bio's column names: `USUBJID`, `TEST`, `STRESN`, `VISIT` and `VISITNUM` in the results. Participants and outcomes are optional. It returns the app and starts nothing, so the same call is the last line of an `app.R` on a server such as Posit Connect.

A reader can load a study of their own in the app's Data view: a results file, and optionally participants and outcomes, as `.csv`, `.xpt` or `.sas7bdat`. R reads the file on the server and the view asks which column is which, filled in where a column has gsm.bio's own name. The file is held in the session's memory and nowhere else. `.xpt` and `.sas7bdat` files are read with haven, which is suggested, not imported.

## Widgets in a Shiny page

A saved page answers only the statistics stored when it was made. In a Shiny page the R session behind it answers every one: each widget has an output and a render function, and `Serve_Statistics()`, called once in the server function, answers whatever a chart asks. A reader who changes the test, the group or a filter gets R's result for that view, and the line under the chart says it was computed on this server and by which R.

```r
library(shiny)
library(gsm.bio)

shinyApp(
  ui = fluidPage(Widget_GroupComparisonOutput("chart")),
  server = function(input, output, session) {
    Serve_Statistics()
    output$chart <- renderWidget_GroupComparison(
      Widget_GroupComparison(Synthetic_Results, Synthetic_Participants)
    )
  }
)
```

The session runs the nine `Analyze_*` functions and no other; a page that asks for any other name is told so and nothing is called. The rows a chart draws are sent with each request, and nothing is kept between requests. shiny is suggested, not imported.

## Figures

Each chart also has a static figure, for a report or a slide: `Visualize_GroupComparison()`, `Visualize_AssociationScatter()`, `Visualize_CorrelationMatrix()`, `Visualize_BiomarkerScreen()`, `Visualize_CrossTab()` and `Visualize_StratifiedSurvival()`. Each takes the same tables and settings as its widget and returns a `ggplot`. The statistics printed under it come from the same `Analyze_*()` call on the same rows. ggplot2 is suggested, not imported: install it to draw figures.

```r
Visualize_StratifiedSurvival(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(
    endpoint = "EFS",
    group_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
    title = "{endpoint} by {group}",
    subtitle = "{n} participants"
  ),
  dfOutcomes = Synthetic_Outcomes
)
```

The settings `title`, `subtitle` and `footnotes` are written with the chart's placeholders, by bio.viz's rules: a name in braces is replaced by text, and a name the figure does not have is left as written. The last line under a figure is always its own: the date it was drawn, by gsm.bio, and R's method and counts behind each statistic it printed, with the R and gsm.bio versions. The [gallery](https://jwildfire.github.io/gsm.bio/articles/gallery.html) shows each figure beside its widget.

## Tables

Each chart's statistics also come as a table, for a report: `Table_GroupComparison()`, `Table_AssociationScatter()`, `Table_CorrelationMatrix()`, `Table_BiomarkerScreen()`, `Table_CrossTab()` and `Table_StratifiedSurvival()`. Each takes the same tables and settings as its widget and returns a data frame, one row per statistic R computed for the view the chart opens on. A row gives the method, each estimate with its interval, the counts, and the p-value written by the display rules: three decimals, `p < 0.001` below that, labelled exploratory with its adjustment named, and no stars. A statistic R did not compute carries R's reason instead.

```r
dfTable <- Table_StratifiedSurvival(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(
    endpoint = "EFS",
    group_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
    title = "{endpoint} by {group}"
  ),
  dfOutcomes = Synthetic_Outcomes
)
Write_RTF(dfTable, "survival.rtf")
```

`Write_RTF()` writes a table to RTF with the r2rtf package: the title and subtitle above, the footnotes beneath, the table's own last. r2rtf is suggested, not imported, and `Write_RTF()` says so when it is not installed.

## Batch runs

A bio.viz chart writes what it draws as a specification, JSON data (bio.viz's docs/output.md, "Specifications"). `Run_Specifications()` reads a list of them and draws each against a dataset, as a figure by its `Visualize_*()` function and an RTF table by its `Table_*()` function and `Write_RTF()`, into a folder, with a manifest. With `bAcrossBiomarkers = TRUE` one specification is drawn once per biomarker.

```r
dfManifest <- Run_Specifications(
  "specifications.json",
  Synthetic_Results,
  Synthetic_Participants,
  dfOutcomes = Synthetic_Outcomes,
  strFolder = "output",
  bAcrossBiomarkers = TRUE,
  chrFormats = c("png", "pdf")
)
```

A specification is data: nothing in it is evaluated, and a title that looks like code is drawn as text. One that bio.viz would refuse is refused with a sentence and listed in the manifest, and the rest still run. Give the file, the text, or the list `jsonlite::read_json()` reads (or `fromJSON(simplifyVector = FALSE)`): `fromJSON()`'s default simplifies the list into a data frame, which is refused with a sentence that says so.

The reader is held to bio.viz's own over every setting of every chart. It differs on purpose in two places: a chart's `statistic` (and the scatter's `fit_statistic`) must be the chart's own `Analyze_*()` function or null, since gsm.bio computes nothing else; and a specification given as text is checked for depth as one given as an object is ([bio.viz#74](https://github.com/jwildfire/bio.viz/issues/74)).

The manifest counts the participants each view's filters keep, and its `reason` says what was not drawn as asked, in the words of bio.viz's notices: a setting that names something the tables lack, a filter value its column lacks, a filter on a column that is not a filter. A view that keeps no participant is `failed` with "No participant passes the filters." Explicit cut points are applied unchanged to every biomarker of a run across biomarkers, and each row says so.

A PDF is drawn by cairo where this R can load it. Where it cannot (a Mac without XQuartz, for one), the pdf device draws it and a character beyond Latin-1, such as the sign of a cut, is a dot or a stand-in such as `<=`; the row says so. A file already in the folder with an output's name is replaced, and any other file there is left alone. A view that fails part way leaves none of its files.

## Synthetic study

The package ships a made-up biomarker study, so that a test can assert an answer known in advance: `Synthetic_Results` (one row per participant, biomarker and visit), `Synthetic_Participants` and `Synthetic_Outcomes`. Three effects are planted in it and every other biomarker is null. `Synthetic_Truth` holds the true size of each: a difference between arms in change from baseline, a correlation between two biomarkers, and a hazard ratio between high and low baseline levels. The same tables are CSV files under `inst/extdata/`. No real study data is used.

## Status

[Version 0.3.0](https://github.com/jwildfire/gsm.bio/releases/tag/v0.3.0) is released, the third release. It draws the group comparison at three levels, a trend tile for every biomarker, one biomarker across its visits and one visit, with R's test under each visit stored in the page. It also adds two statistics functions that answer by level in one call, and one rule for unscheduled visits in the widget, the figure and the table. [Version 0.2.0](https://github.com/jwildfire/gsm.bio/releases/tag/v0.2.0) was the second. It added widgets for the cross-tabulation and the stratified survival chart, hazard ratios in the biomarker screen, and output from R: a static figure and a statistics table for every chart, the table written to RTF, and batch runs of the specifications bio.viz's charts write. [Version 0.1.0](https://github.com/jwildfire/gsm.bio/releases/tag/v0.1.0) was the first: seven statistics functions, the synthetic study, and widgets for the group comparison, the association scatter, the correlation matrix and the biomarker screen. [NEWS.md](https://github.com/jwildfire/gsm.bio/blob/dev/NEWS.md) has the notes for each, and the reference site is at <https://jwildfire.github.io/gsm.bio/>.

The design is on the obot roadmap: [bio.viz and gsm.bio](https://jwildfire.github.io/obot.roadmap/requirements/design/353_design.html).

## Licence

Apache License 2.0. See [LICENSE](https://github.com/jwildfire/gsm.bio/blob/dev/LICENSE).
