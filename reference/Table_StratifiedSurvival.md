# Stratified Survival Table

R's test of the curves the stratified survival chart opens on, as a
table: a row for the log-rank test, with its counts and p-value, then a
row for each group's median survival, in the legend's order, and one for
the hazard ratio, each with its interval. A cut's hazard ratio is the
higher group's over the lower's, and is named so. The numbers are
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)'s
on the rows the chart draws.

## Usage

``` r
Table_StratifiedSurvival(
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
  takes it. A table needs one.

## Value

A `data.frame` of text: `Statistic`, `Method`, `Estimate`, `Counts`,
`p-value` and `Note`, with the attributes of
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md).

## Titles and footnotes

As for
[`Visualize_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_StratifiedSurvival.md):
`{endpoint}`, `{group}`, `{n}`, `{filters}`, `{date}` and `{version}`.

## Display rules

A p-value is written to three decimals, `p < 0.001` below that and
`p > 0.999` above, never with stars. Every row is labelled exploratory,
with its adjustment named when it has one. An estimate is written to
four significant digits with its confidence interval. A statistic R did
not compute has no p-value, and its note is R's reason.

## See also

[`Visualize_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_StratifiedSurvival.md)
and
[`Widget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Widget_StratifiedSurvival.md).

Other tables:
[`Table_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Table_AssociationScatter.md),
[`Table_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Table_BiomarkerScreen.md),
[`Table_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Table_CorrelationMatrix.md),
[`Table_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Table_CrossTab.md),
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md),
[`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)

## Examples

``` r
Table_StratifiedSurvival(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(
    endpoint = "EFS",
    group_by = list(measure = "CRP", visit = "Baseline", cut = "median")
  ),
  dfOutcomes = Synthetic_Outcomes
)
#>                                         Statistic
#> 1                                   Log-rank test
#> 2                                Median (≤ 2.783)
#> 3                                Median (> 2.783)
#> 4 Hazard ratio, high over low (> 2.783 / ≤ 2.783)
#>                                              Method
#> 1                                     Log-rank test
#> 2 Kaplan-Meier, survfit() with the log-log interval
#> 3 Kaplan-Meier, survfit() with the log-log interval
#> 4                 Cox proportional hazards, coxph()
#>                                              Estimate
#> 1                                                    
#> 2 23.32, 95% confidence interval 17.32 to not reached
#> 3          8.28, 95% confidence interval 5.24 to 9.71
#> 4        3.523, 95% confidence interval 2.43 to 5.107
#>                             Counts   p-value                     Note
#> 1 > 2.783 n = 100, ≤ 2.783 n = 100 p < 0.001 Exploratory, unadjusted.
#> 2                                                                    
#> 3                                                                    
#> 4                                                                    
```
