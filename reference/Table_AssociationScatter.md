# Association Scatter Table

The statistics of the view the association scatter opens on, as a table:
for each panel, R's correlation coefficient and, when `fit` asks for
one, its fitted line, each a row with its method, estimates and
intervals, counts, p-value and note, written as the chart prints them.
The numbers are
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md)'s
and
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md)'s
on the rows the chart draws.

## Usage

``` r
Table_AssociationScatter(dfResults, dfParticipants = NULL, lSettings = list())
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
  filters, offers its category columns to colour and panel by and its
  numeric columns on either axis; it also says who the participants are.
  Without it a colour or a number comes from a column carried on the
  results rows. Default: `NULL`.

- lSettings:

  `list` bio.viz association scatter settings, as
  [`Widget_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Widget_AssociationScatter.md)
  takes them, and `title`, `subtitle` and `footnotes`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A `data.frame` of text: `Panel` with `panel_by`, then `Statistic`,
`Method`, `Estimate`, `Counts`, `p-value` and `Note`, with the
attributes of
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md).

## Titles and footnotes

As for
[`Visualize_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_AssociationScatter.md):
`{x}`, `{y}`, `{n}`, `{filters}`, `{date}` and `{version}`.

## Display rules

A p-value is written to three decimals, `p < 0.001` below that and
`p > 0.999` above, never with stars. Every row is labelled exploratory,
with its adjustment named when it has one. An estimate is written to
four significant digits with its confidence interval. A statistic R did
not compute has no p-value, and its note is R's reason.

## See also

[`Visualize_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_AssociationScatter.md)
and
[`Widget_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Widget_AssociationScatter.md).

Other tables:
[`Table_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Table_BiomarkerScreen.md),
[`Table_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Table_CorrelationMatrix.md),
[`Table_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Table_CrossTab.md),
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md),
[`Table_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Table_StratifiedSurvival.md),
[`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)

## Examples

``` r
Table_AssociationScatter(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(
    x = list(measure = "TNF-alpha", visit = "Baseline"),
    y = list(measure = "IL-10", visit = "Baseline"),
    fit = "linear"
  )
)
#>     Statistic                               Method
#> 1 Correlation Pearson's product-moment correlation
#> 2 Fitted line                    Linear regression
#>                                                                                                                             Estimate
#> 1                                                                      Pearson’s r: 0.6384, 95% confidence interval 0.5482 to 0.7139
#> 2 Slope: 0.3275, 95% confidence interval 0.2721 to 0.3828; Intercept: 2.028, 95% confidence interval 1.356 to 2.7; R-squared: 0.4075
#>    Counts   p-value                     Note
#> 1 n = 200 p < 0.001 Exploratory, unadjusted.
#> 2 n = 200 p < 0.001 Exploratory, unadjusted.
```
