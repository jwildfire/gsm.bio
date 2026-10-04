# Cross-Tabulation Table

R's test of the table the cross-tabulation opens on, as a table of one
row: its method, the counts, the p-value and its note, written as the
chart prints them. The numbers are
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md)'s
on the rows the chart tabulates, by the test the settings open on.

## Usage

``` r
Table_CrossTab(dfResults, dfParticipants = NULL, lSettings = list())
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
  filters, offers its category columns to the Rows and Columns controls,
  and its numbers can be cut. Default: `NULL`.

- lSettings:

  `list` bio.viz cross-tabulation settings, as
  [`Widget_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CrossTab.md)
  takes them, and `title`, `subtitle` and `footnotes`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A `data.frame` of text: `Statistic`, `Method`, `Estimate`, `Counts`,
`p-value` and `Note`, with the attributes of
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md).

## Titles and footnotes

As for `Visualize_CrossTab()`: `{rows}`, `{columns}`, `{n}`,
`{filters}`, `{date}` and `{version}`.

## Display rules

A p-value is written to three decimals, `p < 0.001` below that and
`p > 0.999` above, never with stars. Every row is labelled exploratory,
with its adjustment named when it has one. An estimate is written to
four significant digits with its confidence interval. A statistic R did
not compute has no p-value, and its note is R's reason.

## See also

`Visualize_CrossTab()` and
[`Widget_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CrossTab.md).

Other tables:
[`Table_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Table_AssociationScatter.md),
[`Table_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Table_BiomarkerScreen.md),
[`Table_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Table_CorrelationMatrix.md),
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md),
[`Table_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Table_StratifiedSurvival.md),
[`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)

## Examples

``` r
Table_CrossTab(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(row_by = "ARM", col_by = "RESPONSE")
)
#>           Statistic
#> 1 Test of the table
#>                                                         Method Estimate  Counts
#> 1 Pearson's Chi-squared test with Yates' continuity correction          n = 200
#>     p-value                     Note
#> 1 p = 0.464 Exploratory, unadjusted.
```
