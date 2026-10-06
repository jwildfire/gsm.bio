# Correlate two numeric variables

Estimates the correlation between two numeric variables on the
participants who have both, overall and, when a group column is named,
within each group.

## Usage

``` r
Analyze_Correlation(
  dfData,
  strXCol,
  strYCol,
  strMethod = "pearson",
  strGroupCol = NULL,
  chrGroups = NULL,
  nConfLevel = 0.95,
  nMinGroup = nMinGroupDefault
)
```

## Arguments

- dfData:

  `data.frame` One row per participant.

- strXCol, strYCol:

  `character` Names of the two numeric columns.

- strMethod:

  `character` The coefficient: `"pearson"` or `"spearman"`. Default:
  `"pearson"`.

- strGroupCol:

  `character` Name of a column holding each participant's group, to add
  a row per group. Default: `NULL`, no per-group rows.

- chrGroups:

  `character` The groups to give a row to, in order. Default: `NULL`,
  every group present, in sorted order.

- nConfLevel:

  `numeric` Confidence level of the intervals. Default: `0.95`.

- nMinGroup:

  `numeric` The smallest number of complete pairs the correlation is
  computed for, overall and in each group. Default: `nMinGroupDefault`,
  which is 5. See
  [StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).

## Value

The fixed result described in
[StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).
Here `counts` is the number of complete pairs; `estimates` has one row,
named as R names the coefficient (`"cor"` or `"rho"`); and `rows` has
one row per group when `strGroupCol` is given, with the columns `group`,
`counts`, `estimate`, `lower`, `upper`, `level`, `method`, `statistic`,
`p_value`, `adjustment`, `status`, `reason` and `warning`. The overall
answer uses every complete pair, whether or not the participant has a
group.

## Details

Both methods are
[`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html) with R's
defaults. For `"pearson"` the interval is the one
[`cor.test()`](https://rdrr.io/r/stats/cor.test.html) gives, by Fisher's
z. For `"spearman"`
[`cor.test()`](https://rdrr.io/r/stats/cor.test.html) gives no interval,
so none is reported and `notes` says so.

## See also

Other statistics:
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_DifferenceGrid()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_DifferenceGrid.md),
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md),
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_GroupDifferenceBy()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifferenceBy.md),
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)

## Examples

``` r
# TNF-alpha against IL-10 at Baseline
dfBaseline <- Synthetic_Results[Synthetic_Results$VISIT == "Baseline", ]
dfTNF <- dfBaseline[dfBaseline$TEST == "TNF-alpha", ]
dfIL10 <- dfBaseline[dfBaseline$TEST == "IL-10", ]
dfFrame <- Synthetic_Participants
dfFrame$TNF <- dfTNF$STRESN[match(dfFrame$USUBJID, dfTNF$USUBJID)]
dfFrame$IL10 <- dfIL10$STRESN[match(dfFrame$USUBJID, dfIL10$USUBJID)]

lResult <- Analyze_Correlation(dfFrame, "TNF", "IL10", strGroupCol = "ARM")
lResult$estimates
#>   name group  estimate     lower     upper level
#> 1  cor  <NA> 0.6383823 0.5481947 0.7138938  0.95
lResult$rows[c("group", "counts", "estimate", "lower", "upper")]
#>       group counts  estimate     lower     upper
#> 1   Placebo    100 0.5918125 0.4474016 0.7061463
#> 2 Treatment    100 0.6736870 0.5500544 0.7684239
```
