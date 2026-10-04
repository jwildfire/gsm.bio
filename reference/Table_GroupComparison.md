# Group Comparison Table

The statistics of the view the group comparison chart opens on, as a
table: one row per panel the chart tests, with R's method, each estimate
and its interval, the counts, the p-value and its note, written as the
chart prints them. The numbers are
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md)'s
on the rows the chart draws in each panel, as
[`Widget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Widget_GroupComparison.md)
stores them; with `pairwise = TRUE` each pair of groups has a row of its
own beneath its panel.

## Usage

``` r
Table_GroupComparison(dfResults, dfParticipants = NULL, lSettings = list())
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
  filters, and offers its category columns to group, colour and panel
  by; it also says who the participants are. Without it a group comes
  from a column carried on the results rows. Default: `NULL`.

- lSettings:

  `list` bio.viz group comparison settings, as
  [`Widget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Widget_GroupComparison.md)
  takes them, and `title`, `subtitle` and `footnotes`. A table is of one
  biomarker, so `start_value` must name it. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A `data.frame` of text: `Visit` (and `Panel` with `panel_by`, and `Pair`
with `pairwise`), then `Statistic`, `Method`, `Estimate`, `Counts`,
`p-value` and `Note`. Its attributes are `title`, `subtitle`,
`footnotes`, `results` (R's answers) and `result_of` (which answer each
row is of).
[`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)
writes it to RTF.

## Display rules

A p-value is written to three decimals, `p < 0.001` below that and
`p > 0.999` above, never with stars. Every row is labelled exploratory,
with its adjustment named when it has one. An estimate is written to
four significant digits with its confidence interval. A statistic R did
not compute has no p-value, and its note is R's reason.

## Titles and footnotes

The settings `title`, `subtitle` and `footnotes` are text with named
placeholders, as for
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md):
`{measure}`, `{visits}`, `{value}`, `{group}`, `{n}`, `{filters}`,
`{date}` and `{version}`. They are the table's attributes `title`,
`subtitle` and `footnotes`, the last footnote always the table's own.

## See also

[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md)
and
[`Widget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Widget_GroupComparison.md),
the same view as a figure and as a widget.

Other tables:
[`Table_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Table_AssociationScatter.md),
[`Table_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Table_BiomarkerScreen.md),
[`Table_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Table_CorrelationMatrix.md),
[`Table_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Table_CrossTab.md),
[`Table_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Table_StratifiedSurvival.md),
[`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)

## Examples

``` r
Table_GroupComparison(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(
    start_value = "IL-6",
    visits = c("Week 4", "Week 8"),
    value_type = "change",
    baseline_visits = "Baseline",
    group_by = "ARM"
  )
)
#>    Visit          Statistic                  Method
#> 1 Week 4 Test of the groups Welch Two Sample t-test
#> 2 Week 8 Test of the groups Welch Two Sample t-test
#>                                                                                                                                       Estimate
#> 1   Mean (Placebo): 0.02473; Mean (Treatment): -1.21; Difference in means (Placebo - Treatment): 1.235, 95% confidence interval 0.844 to 1.626
#> 2 Mean (Placebo): 0.08592; Mean (Treatment): -1.273; Difference in means (Placebo - Treatment): 1.358, 95% confidence interval 0.9882 to 1.729
#>                             Counts   p-value                     Note
#> 1 Placebo n = 95, Treatment n = 91 p < 0.001 Exploratory, unadjusted.
#> 2 Placebo n = 93, Treatment n = 95 p < 0.001 Exploratory, unadjusted.
```
