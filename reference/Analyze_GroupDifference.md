# Compare a numeric variable between groups

Tests whether a value differs between two or more groups of
participants, and reports the group means and, for two groups, the
difference in means with its interval.

## Usage

``` r
Analyze_GroupDifference(
  dfData,
  strValueCol,
  strGroupCol,
  strMethod = "t",
  chrGroups = NULL,
  bPairwise = TRUE,
  strPAdjust = "holm",
  nConfLevel = 0.95,
  nMinGroup = nMinGroupDefault
)
```

## Arguments

- dfData:

  `data.frame` One row per participant.

- strValueCol:

  `character` Name of the numeric column to compare.

- strGroupCol:

  `character` Name of the column holding each participant's group.

- strMethod:

  `character` The test: `"t"`, `"wilcoxon"`, `"anova"` or `"kruskal"`.
  Default: `"t"`.

- chrGroups:

  `character` The groups to compare, in order; the difference in means
  is the first minus the second. Participants in any other group are
  dropped and counted. Default: `NULL`, every group present, in sorted
  order.

- bPairwise:

  `logical` Compare every pair of groups when there are more than two.
  Default: `TRUE`.

- strPAdjust:

  `character` The adjustment across the pairs, one of
  [stats::p.adjust.methods](https://rdrr.io/r/stats/p.adjust.html).
  Default: `"holm"`.

- nConfLevel:

  `numeric` Confidence level of the intervals. Default: `0.95`.

- nMinGroup:

  `numeric` The smallest group the test is computed for. If any group
  has fewer participants with a value, the result has `status`
  `"too_small"` and no numbers. Default: `nMinGroupDefault`, which is 5.
  See
  [StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).

## Value

The fixed result described in
[StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).
Here `counts` is a named list of group to participants used; `estimates`
has a row named `"Mean"` per group and, for two groups, a row named
`"Difference in means"`; and `rows` has one row per pair of groups when
pairwise comparisons were made, with the columns `group_1`, `group_2`,
`n_1`, `n_2`, `counts` (the two together), `estimate` (the difference in
means, `group_1` minus `group_2`), `lower`, `upper`, `level`, `method`,
`statistic`, `p_unadjusted`, `p_value` (adjusted), `adjustment`,
`status`, `reason` and `warning`. The intervals in `rows` are not
adjusted.

## Details

Each method is the base R function, called with R's defaults:

|  |  |  |
|----|----|----|
| `strMethod` | Groups | R function |
| `"t"` | two | [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html), Welch: unequal variances. |
| `"wilcoxon"` | two | [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html), with R's own switch between the exact and the approximate p-value. |
| `"anova"` | two or more | [`stats::aov()`](https://rdrr.io/r/stats/aov.html), one-way. |
| `"kruskal"` | two or more | [`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html), with its tie correction. |

With exactly two groups, `estimates` includes the difference in means,
the first group's mean minus the second's, with its interval. Both
always come from [`t.test()`](https://rdrr.io/r/stats/t.test.html)
(Welch), whichever test was asked for.

With more than two groups and `bPairwise`, every pair of groups is
compared with the two-group test of the same family,
[`t.test()`](https://rdrr.io/r/stats/t.test.html) for `"anova"` and
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) for
`"kruskal"`, and the p-values are adjusted across the pairs by
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). The pairs
are in `rows`.

## See also

Other statistics:
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)

## Examples

``` r
# Change in IL-6 from Baseline to Week 4, by arm
dfIL6 <- Synthetic_Results[Synthetic_Results$TEST == "IL-6", ]
dfBaseline <- dfIL6[dfIL6$VISIT == "Baseline", ]
dfWeek4 <- dfIL6[dfIL6$VISIT == "Week 4", ]
dfFrame <- Synthetic_Participants
dfFrame$Change <- dfWeek4$STRESN[match(dfFrame$USUBJID, dfWeek4$USUBJID)] -
  dfBaseline$STRESN[match(dfFrame$USUBJID, dfBaseline$USUBJID)]

lResult <- Analyze_GroupDifference(
  dfFrame, "Change", "ARM",
  chrGroups = c("Treatment", "Placebo")
)
lResult$method
#> [1] "Welch Two Sample t-test"
lResult$estimates
#>                  name               group    estimate     lower      upper
#> 1                Mean           Treatment -1.21031868        NA         NA
#> 2                Mean             Placebo  0.02472632        NA         NA
#> 3 Difference in means Treatment - Placebo -1.23504500 -1.626076 -0.8440141
#>   level
#> 1    NA
#> 2    NA
#> 3  0.95
lResult$p_value
#> [1] 3.229638e-09
lResult$counts
#> $Treatment
#> [1] 91
#> 
#> $Placebo
#> [1] 95
#> 
```
