# Correlate every pair of several numeric variables

Runs [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html) on
every pair of the named columns, each pair on the participants who have
both values, and returns one row per pair with the coefficient, its
interval and the number of pairs it rests on.

## Usage

``` r
Analyze_CorrelationMatrix(
  dfData,
  chrCols,
  strMethod = "pearson",
  nConfLevel = 0.95,
  nMinPairs = nMinGroupDefault
)
```

## Arguments

- dfData:

  `data.frame` One row per participant.

- chrCols:

  `character` Names of two or more numeric columns.

- strMethod:

  `character` The coefficient: `"pearson"` or `"spearman"`. Default:
  `"pearson"`.

- nConfLevel:

  `numeric` Confidence level of the intervals. Default: `0.95`.

- nMinPairs:

  `numeric` The smallest number of complete pairs a cell is computed
  for. A pair with fewer has `status` `"too_small"` and no numbers in
  its row. Default: `nMinGroupDefault`, which is 5. See
  [StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).

## Value

The fixed result described in
[StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).
Here `p_value` is `NA` and `estimates` and `statistic` have no rows;
`counts` is a named list of column to participants with a value; and
`rows` is the matrix in long form, one row per pair of columns, each
pair once, with the columns `x`, `y`, `counts` (the complete pairs),
`estimate`, `lower`, `upper`, `level`, `status`, `reason` and `warning`.
The result's own `status` is `"too_small"` only when every pair is.

## Details

No p-values are returned. A matrix of many coefficients is for seeing
which variables move together, and a p-value on every cell would be many
unadjusted tests. To test one pair, use
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md).

## See also

Other statistics:
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md),
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)

## Examples

``` r
# Four biomarkers at Baseline, one column each
dfBaseline <- Synthetic_Results[Synthetic_Results$VISIT == "Baseline", ]
dfFrame <- Synthetic_Participants["USUBJID"]
for (strBiomarker in c("TNF-alpha", "IL-10", "IL-6", "CRP")) {
  dfOne <- dfBaseline[dfBaseline$TEST == strBiomarker, ]
  dfFrame[[strBiomarker]] <- dfOne$STRESN[match(dfFrame$USUBJID, dfOne$USUBJID)]
}

lResult <- Analyze_CorrelationMatrix(dfFrame, c("TNF-alpha", "IL-10", "IL-6", "CRP"))
lResult$rows[c("x", "y", "counts", "estimate", "lower", "upper")]
#>           x     y counts    estimate       lower      upper
#> 1 TNF-alpha IL-10    200  0.63838226  0.54819474 0.71389384
#> 2 TNF-alpha  IL-6    200  0.15152706  0.01306062 0.28429141
#> 3 TNF-alpha   CRP    200 -0.05072134 -0.18813840 0.08864347
#> 4     IL-10  IL-6    200  0.07962745 -0.05977393 0.21598238
#> 5     IL-10   CRP    200 -0.06857137 -0.20535868 0.07084362
#> 6      IL-6   CRP    200  0.01975789 -0.11931019 0.15806561
```
