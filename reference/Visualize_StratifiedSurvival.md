# Stratified Survival Figure

A static ggplot2 figure of the curves the stratified survival chart
opens on: each group's Kaplan-Meier curve on one endpoint, with its
confidence band, a mark at each censored time, and R's log-rank test,
medians and hazard ratio printed under the figure. The groups, who is
drawn and the test are those of
[`Widget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Widget_StratifiedSurvival.md)
at the same settings, and the test is
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)'s
on the same rows. The curves and their bands are R's own:
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
on those rows, with the log-log interval Analyze_Survival() reports its
medians by.

## Usage

``` r
Visualize_StratifiedSurvival(
  dfResults,
  dfParticipants = NULL,
  lSettings = list(),
  dfOutcomes = NULL
)
```

## Arguments

- dfResults:

  `data.frame` Long-format results, one row per participant, biomarker
  and visit. Column names are supplied by `lSettings`; the defaults
  expect `USUBJID`/`TEST`/`STRESN`/`VISIT`/`VISITNUM`/`STRESU`, the
  columns of
  [Synthetic_Results](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Results.md).

- dfParticipants:

  `data.frame` One row per participant, or `NULL`. With it the chart has
  filters, offers its category columns to the Groups control, and its
  numbers can be cut. Default: `NULL`.

- lSettings:

  `list` bio.viz stratified survival settings, as
  [`Widget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Widget_StratifiedSurvival.md)
  takes them, and `title`, `subtitle` and `footnotes`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

- dfOutcomes:

  `data.frame` The outcomes table, as
  [`Widget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Widget_StratifiedSurvival.md)
  takes it. A figure needs one.

## Value

A `ggplot` object.

## Titles and footnotes

The settings `title`, `subtitle` and `footnotes` are text with named
placeholders, as for
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md).
This figure fills `{endpoint}` (by its label), `{group}` (what the
groups are), `{n}` (the participants drawn), `{filters}`, `{date}` and
`{version}`.

## See also

[`Widget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Widget_StratifiedSurvival.md),
its interactive twin.

Other figures:
[`Run_Specifications()`](https://jwildfire.github.io/gsm.bio/reference/Run_Specifications.md),
[`Visualize_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_AssociationScatter.md),
[`Visualize_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_BiomarkerScreen.md),
[`Visualize_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CorrelationMatrix.md),
[`Visualize_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CrossTab.md),
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  Visualize_StratifiedSurvival(
    Synthetic_Results,
    Synthetic_Participants,
    lSettings = list(
      endpoint = "EFS",
      group_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
      title = "{endpoint} by {group}"
    ),
    dfOutcomes = Synthetic_Outcomes
  )
}

```
