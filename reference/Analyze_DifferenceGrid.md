# The standardised difference for every biomarker at every level of a column

Computes the standardised difference between two groups, the estimate of
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md),
for every biomarker at every level of one more column, a visit say, in
one call, and returns one row per biomarker and level: a grid of
biomarkers by visits, in long form.

## Usage

``` r
Analyze_DifferenceGrid(
  dfData,
  strValueCol,
  strGroupCol,
  strBiomarkerCol,
  strByCol,
  chrGroups = NULL,
  chrBiomarkers = NULL,
  chrBy = NULL,
  nConfLevel = 0.95,
  nMinGroup = nMinGroupDefault
)
```

## Arguments

- dfData:

  `data.frame` One row per participant, biomarker and level.

- strValueCol:

  `character` Name of the numeric column holding the biomarker's value.

- strGroupCol:

  `character` Name of the column holding each participant's group.

- strBiomarkerCol:

  `character` Name of the column holding the biomarker of each row.

- strByCol:

  `character` Name of the column holding the level of each row, such as
  the visit.

- chrGroups:

  `character` The two groups, in order; the difference is the first
  minus the second. Rows in any other group are dropped and counted.
  Default: `NULL`, the two groups present, in sorted order.

- chrBiomarkers:

  `character` The biomarkers to answer, in order. Rows of any other
  biomarker are dropped and counted. Default: `NULL`, every biomarker
  present, in sorted order.

- chrBy:

  `character` The levels to answer, in order. Rows at any other level
  are dropped and counted. Default: `NULL`, every level present among
  the biomarkers answered, in sorted order.

- nConfLevel:

  `numeric` Confidence level of the intervals. Default: `0.95`.

- nMinGroup:

  `numeric` The smallest group a cell is computed for. A cell where
  either group has fewer participants with a value has `status`
  `"too_small"` in its row and no numbers. Default: `nMinGroupDefault`,
  which is 5. See
  [StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).

## Value

The fixed result described in
[StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).
Here `test` is `"difference"` and `method` names the estimate; `p_value`
is `NA` and `estimates` and `statistic` have no rows; `counts` is one
whole number, the rows used across the cells; `dropped` counts the rows
with no biomarker or level, or one not asked for, and then, within the
cells, the rows with no group, another group or no value; and `rows` is
the grid in long form, one row per biomarker and level, with the columns
`biomarker`, `by` (the level), `counts`, `n_1` and `n_2` (the two
groups, first and second), `dropped`, `estimate`, `lower`, `upper`,
`level`, `status`, `reason` and `warning`. The result's own `status` is
`"ok"` when any cell is, `"too_small"` when every cell is too small, and
`"error"` when no cell could be computed for any other reason, or when
the groups are not exactly two.

## Details

The data are long: one row per participant, biomarker and level, as a
results table is, with the participant's group on each row. Nothing has
to be reshaped to one column per biomarker first, as
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md)
needs. Each row of the answer is
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md)'s
own difference row for that biomarker on the rows of that level, so a
cell and the screen run at that level agree exactly: the same estimate
and interval, the same counts, and the same reason where it is not
computed.

The estimate is Hedges' g with its noncentral t interval, the first
group minus the second: see the section on the standardised difference
in
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md).
The two groups are the same in every cell, so every difference in the
grid is the same way round.

No p-values are returned. A grid of many estimates is for seeing where
and when two groups part, and a p-value in every cell would be many
unadjusted tests. To test every biomarker at one level, use
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md);
to test one biomarker at every level, use
[`Analyze_GroupDifferenceBy()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifferenceBy.md).

A cell that could not be computed has its own `status` and `reason` and
no numbers: `"too_small"` when a group has fewer participants with a
value than `nMinGroup`, which a cell with no rows at all is too, and
`"error"` with R's reason when the values do not vary. Every biomarker
has a row at every level, computed or not.

The rows come back with the biomarkers in the order of `chrBiomarkers`
and, within each, the levels in the order of `chrBy`. R does not know
the order of visits, so name them in `chrBy` to have them in theirs.

## See also

Other statistics:
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md),
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_GroupDifferenceBy()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifferenceBy.md),
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)

## Examples

``` r
# Every biomarker at every visit, Treatment against Placebo
dfLong <- merge(Synthetic_Results, Synthetic_Participants[c("USUBJID", "ARM")])

lResult <- Analyze_DifferenceGrid(
  dfLong, "STRESN", "ARM", "TEST", "VISIT",
  chrGroups = c("Treatment", "Placebo"),
  chrBy = c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
)
nrow(lResult$rows)
#> [1] 60
dfIL6 <- lResult$rows[lResult$rows$biomarker == "IL-6", ]
dfIL6[c("biomarker", "by", "n_1", "n_2", "estimate", "lower", "upper")]
#>    biomarker       by n_1 n_2   estimate      lower      upper
#> 36      IL-6 Baseline 100 100 -0.1728191 -0.4492545  0.1040510
#> 37      IL-6   Week 2  93  92 -0.9648376 -1.2671950 -0.6601111
#> 38      IL-6   Week 4  91  95 -0.9795051 -1.2815919 -0.6750333
#> 39      IL-6   Week 8  95  93 -1.0703998 -1.3739294 -0.7643423
#> 40      IL-6  Week 12  92  92 -1.0961963 -1.4038893 -0.7858732
```
