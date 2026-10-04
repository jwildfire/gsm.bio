# Biomarker Screen Table

The rows of the screen the biomarker screen opens on, as a table: one
row per biomarker, with R's estimate and its interval, the counts, the
unadjusted p-value and the p-value adjusted across the rows, written as
the chart prints them. The numbers are
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md)'s
on the frame the chart hands R.

## Usage

``` r
Table_BiomarkerScreen(
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
  filters, and the columns of groups and the numbers it offers are read
  from it. Default: `NULL`.

- lSettings:

  `list` bio.viz biomarker screen settings, as
  [`Widget_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Widget_BiomarkerScreen.md)
  takes them, and `title`, `subtitle` and `footnotes`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

- dfOutcomes:

  `data.frame` An outcomes table, one row per participant and endpoint
  with a time and a flag, as
  [`Widget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Widget_StratifiedSurvival.md)
  takes it, or `NULL`. With it the screen offers a hazard ratio. It
  comes after `lSettings`, so a call written for v0.1.0 works as it did.
  Default: `NULL`.

## Value

A `data.frame` of text: `Biomarker`, then `Statistic`, `Method`,
`Estimate`, `Counts`, `p-value`, `Adjusted p-value` and `Note`, with the
attributes of
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md).

## Titles and footnotes

As for
[`Visualize_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_BiomarkerScreen.md):
`{heading}`, `{comparison}`, `{visit}`, `{endpoint}`, `{biomarkers}`,
`{n}`, `{filters}`, `{date}` and `{version}`.

## Display rules

A p-value is written to three decimals, `p < 0.001` below that and
`p > 0.999` above, never with stars. Every row is labelled exploratory,
with its adjustment named when it has one. An estimate is written to
four significant digits with its confidence interval. A statistic R did
not compute has no p-value, and its note is R's reason.

## See also

[`Visualize_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_BiomarkerScreen.md)
and
[`Widget_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Widget_BiomarkerScreen.md).

Other tables:
[`Table_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Table_AssociationScatter.md),
[`Table_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Table_CorrelationMatrix.md),
[`Table_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Table_CrossTab.md),
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md),
[`Table_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Table_StratifiedSurvival.md),
[`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)

## Examples

``` r
Table_BiomarkerScreen(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(visit = "Week 4", value_type = "change", group_by = "ARM")
)
#>    Biomarker                           Statistic                  Method
#> 1        CRP Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 2    D-dimer Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 3   Ferritin Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 4  IFN-gamma Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 5   IL-1beta Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 6       IL-2 Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 7       IL-6 Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 8       IL-8 Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 9      IL-10 Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 10       LDH Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 11 TNF-alpha Standardised difference (Hedges’ g) Welch Two Sample t-test
#> 12      VEGF Standardised difference (Hedges’ g) Welch Two Sample t-test
#>                                               Estimate
#> 1   -0.09193, 95% confidence interval -0.379 to 0.1954
#> 2     0.1746, 95% confidence interval -0.118 to 0.4667
#> 3   0.06825, 95% confidence interval -0.2197 to 0.3561
#> 4     0.1288, 95% confidence interval -0.1587 to 0.416
#> 5   0.2589, 95% confidence interval -0.03126 to 0.5484
#> 6  0.005833, 95% confidence interval -0.2805 to 0.2921
#> 7      0.9133, 95% confidence interval 0.6111 to 1.213
#> 8     0.1298, 95% confidence interval -0.1577 to 0.417
#> 9  -0.09502, 95% confidence interval -0.3836 to 0.1939
#> 10   0.1342, 95% confidence interval -0.1542 to 0.4222
#> 11   -0.1621, 95% confidence interval -0.451 to 0.1272
#> 12   0.1544, 95% confidence interval -0.1356 to 0.4441
#>                              Counts   p-value Adjusted p-value
#> 1  Placebo n = 94, Treatment n = 91 p = 0.530        p = 0.636
#> 2  Placebo n = 90, Treatment n = 89 p = 0.243        p = 0.575
#> 3  Placebo n = 93, Treatment n = 91 p = 0.642        p = 0.700
#> 4  Placebo n = 94, Treatment n = 91 p = 0.383        p = 0.575
#> 5  Placebo n = 93, Treatment n = 90 p = 0.080        p = 0.481
#> 6  Placebo n = 95, Treatment n = 91 p = 0.968        p = 0.968
#> 7  Placebo n = 95, Treatment n = 91 p < 0.001        p < 0.001
#> 8  Placebo n = 94, Treatment n = 91 p = 0.378        p = 0.575
#> 9  Placebo n = 91, Treatment n = 92 p = 0.520        p = 0.636
#> 10 Placebo n = 94, Treatment n = 90 p = 0.362        p = 0.575
#> 11 Placebo n = 93, Treatment n = 90 p = 0.272        p = 0.575
#> 12 Placebo n = 93, Treatment n = 89 p = 0.297        p = 0.575
#>                                           Note
#> 1  Exploratory, adjusted (Benjamini-Hochberg).
#> 2  Exploratory, adjusted (Benjamini-Hochberg).
#> 3  Exploratory, adjusted (Benjamini-Hochberg).
#> 4  Exploratory, adjusted (Benjamini-Hochberg).
#> 5  Exploratory, adjusted (Benjamini-Hochberg).
#> 6  Exploratory, adjusted (Benjamini-Hochberg).
#> 7  Exploratory, adjusted (Benjamini-Hochberg).
#> 8  Exploratory, adjusted (Benjamini-Hochberg).
#> 9  Exploratory, adjusted (Benjamini-Hochberg).
#> 10 Exploratory, adjusted (Benjamini-Hochberg).
#> 11 Exploratory, adjusted (Benjamini-Hochberg).
#> 12 Exploratory, adjusted (Benjamini-Hochberg).
```
