# Compare a numeric variable between groups at each level of a column

Runs the group test of
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md)
within each level of one more column, a visit say, in one call, and
returns one row per level: the test's p-value, that p-value adjusted
across the levels, the counts, and for two groups the difference in
means with its interval.

## Usage

``` r
Analyze_GroupDifferenceBy(
  dfData,
  strValueCol,
  strGroupCol,
  strByCol,
  strMethod = "t",
  chrGroups = NULL,
  chrBy = NULL,
  strPAdjust = "none",
  nConfLevel = 0.95,
  nMinGroup = nMinGroupDefault
)
```

## Arguments

- dfData:

  `data.frame` One row per participant and level.

- strValueCol:

  `character` Name of the numeric column to compare.

- strGroupCol:

  `character` Name of the column holding each participant's group.

- strByCol:

  `character` Name of the column holding the level of each row, such as
  the visit. The test is run once within each level.

- strMethod:

  `character` The test: `"t"`, `"wilcoxon"`, `"anova"` or `"kruskal"`.
  Default: `"t"`.

- chrGroups:

  `character` The groups to compare at every level, in order; the
  difference in means is the first minus the second. Rows in any other
  group are dropped and counted. Default: `NULL`, every group present
  among the levels answered, in sorted order.

- chrBy:

  `character` The levels to answer, in order. Rows at any other level
  are dropped and counted. A level named here that the data do not hold
  is a row with nobody in any group. Default: `NULL`, every level
  present, in sorted order.

- strPAdjust:

  `character` The adjustment across the levels, one of
  [stats::p.adjust.methods](https://rdrr.io/r/stats/p.adjust.html).
  Default: `"none"`.

- nConfLevel:

  `numeric` Confidence level of the intervals. Default: `0.95`.

- nMinGroup:

  `numeric` The smallest group the test is computed for. A level where
  any group has fewer participants with a value has `status`
  `"too_small"` in its row and no numbers. Default: `nMinGroupDefault`,
  which is 5. See
  [StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).

## Value

The fixed result described in
[StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).
Here `p_value` is `NA` and `estimates` and `statistic` have no rows;
`method` is the first computed level's; `counts` is a named list of
level to participants used; `dropped` counts the rows with no level or a
level not asked for, and then, within the levels answered, the rows the
test left out; and `rows` has one row per level, with the columns:

- `by`, the level.

- `group_1`, `group_2` and so on, one per group, the groups compared,
  the same on every row; then `n_1`, `n_2` and so on, the participants
  used in each at that level. With two groups these are the columns a
  pairwise comparison of
  [`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md)
  has.

- `counts` (the groups together) and `dropped` (the rows of that level
  the test left out).

- `estimate`, `lower`, `upper` and `level`: with two groups, the
  difference in means, `group_1` minus `group_2`, and its interval, from
  [`t.test()`](https://rdrr.io/r/stats/t.test.html) (Welch) whatever the
  test; `NA` with more than two.

- `method` and `statistic`, as R reports them for that level.

- `p_unadjusted`, `p_value` (adjusted across the levels), `adjustment`
  and `adjusted_over`.

- `status`, `reason` and `warning` for that level.

The result's own `status` is `"ok"` when any level is, `"too_small"`
when every level is too small, and `"error"` when no level could be
computed for any other reason. Each row gives its own reason.

## Details

The data are long: one row per participant and level, as a results table
holds one biomarker across its visits. Each row of the answer is
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md)'s
own answer on the rows of that level, with `bPairwise` off and the same
groups named, so its unadjusted p-value is the one printed when the
level is looked at alone with those groups.

The groups are the same at every level: the ones named in `chrGroups`,
or every group present anywhere among the levels answered. A group with
nobody at a level is too small there; the level is not quietly compared
without it. So with three or more groups, a level where one of them has
nobody has no p-value here, while
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md)
on that level's rows alone, with `chrGroups` left out, compares the
groups that are there.

The adjustment is
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) across the
levels that have a p-value. A level that could not be computed, because
a group is below the minimum size or its values do not vary, has its own
`status` and `reason`, has no p-value, and is left out of the
adjustment; `notes` says how many levels the adjustment covered, and
each adjusted row carries that number as `adjusted_over`. The default is
no adjustment: `p_value` is then each level's own p-value. Holm guards
against any false lead among the levels; Benjamini-Hochberg controls the
share of false leads among the levels picked out.

The rows come back in the order of `chrBy`. R does not know the order of
visits, so name them in `chrBy` to have them in theirs.

## See also

Other statistics:
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_DifferenceGrid()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_DifferenceGrid.md),
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md),
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)

## Examples

``` r
# IL-6 by arm at every visit, with the p-values adjusted across the visits
dfIL6 <- merge(
  Synthetic_Results[Synthetic_Results$TEST == "IL-6", ],
  Synthetic_Participants[c("USUBJID", "ARM")]
)

lResult <- Analyze_GroupDifferenceBy(
  dfIL6, "STRESN", "ARM", "VISIT",
  chrGroups = c("Treatment", "Placebo"),
  chrBy = c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12"),
  strPAdjust = "holm"
)
lResult$rows[c("by", "n_1", "n_2", "estimate", "p_unadjusted", "p_value", "adjustment")]
#>         by n_1 n_2  estimate p_unadjusted      p_value adjustment
#> 1 Baseline 100 100 -0.252230 2.214152e-01 2.214152e-01       holm
#> 2   Week 2  93  92 -1.526236 4.840136e-10 9.680271e-10       holm
#> 3   Week 4  91  95 -1.544451 2.215792e-10 6.647376e-10       holm
#> 4   Week 8  95  93 -1.577415 5.542038e-12 2.216815e-11       holm
#> 5  Week 12  92  92 -1.596728 3.459919e-12 1.729960e-11       holm

# The same p-value as the single function on one visit's rows
Analyze_GroupDifference(
  dfIL6[dfIL6$VISIT == "Week 4", ], "STRESN", "ARM",
  chrGroups = c("Treatment", "Placebo")
)$p_value
#> [1] 2.215792e-10
```
