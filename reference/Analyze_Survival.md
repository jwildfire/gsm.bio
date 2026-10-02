# Compare survival between groups

Tests whether the time to an event differs between two or more groups of
participants, and reports each group's median survival with its interval
and, for two groups, the hazard ratio with its interval.

## Usage

``` r
Analyze_Survival(
  dfData,
  strTimeCol,
  strGroupCol,
  strCensorCol = NULL,
  strEventCol = NULL,
  chrGroups = NULL,
  nConfLevel = 0.95,
  nMinGroup = nMinGroupDefault
)
```

## Arguments

- dfData:

  `data.frame` One row per participant.

- strTimeCol:

  `character` Name of the numeric column holding the time to the event
  or to censoring.

- strGroupCol:

  `character` Name of the column holding each participant's group.

- strCensorCol:

  `character` Name of a censor flag column: 1 for a censored time, 0 for
  an event. Default: `NULL`.

- strEventCol:

  `character` Name of an event flag column: 1 for an event, 0 for a
  censored time. Default: `NULL`.

- chrGroups:

  `character` The groups to compare, in order; the hazard ratio is the
  first over the second. Participants in any other group are dropped and
  counted. Default: `NULL`, every group present, in sorted order.

- nConfLevel:

  `numeric` Confidence level of the intervals. Default: `0.95`.

- nMinGroup:

  `numeric` The smallest group the comparison is computed for, counted
  in participants, not events. If any group has fewer, the result has
  `status` `"too_small"` and no numbers. Default: `nMinGroupDefault`,
  which is 5. See
  [StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).

## Value

The fixed result described in
[StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).
Here `test` is `"logrank"`; `counts` is a named list of group to
participants used; `estimates` has a row named `"Median"` per group and,
for two groups, a row named `"Hazard ratio"`; and `rows` has one row per
group, with the columns `group`, `n`, `events`, `median`, `lower`,
`upper`, `level`, and, filled on the first group's row when there are
two groups, `hazard_ratio`, `hr_lower`, `hr_upper`, `hr_p_value` and
`hr_test`. A median or a bound that the curve or its band never reaches
is `NA`.

## Details

Each part is the survival package's own function:

|  |  |
|----|----|
| Part | R function |
| The test | [`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html), the log-rank test, for two or more groups. |
| Median survival and its interval | [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html) with `conf.type = "log-log"`, the interval safety.viz draws as the band of its Kaplan-Meier curve. |
| The hazard ratio and its interval | [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html), when there are exactly two groups. |

The hazard ratio is the hazard in the first group over the hazard in the
second, so the second group is the reference: with
`chrGroups = c("High", "Low")` a ratio above 1 means events come sooner
in the high group.

## Two p-values, kept apart

`p_value` is the log-rank test's. The Cox model has p-values of its own,
and the one that goes with the hazard ratio's interval is the Wald
test's. It is in `rows` as `hr_p_value`, labelled by `hr_test`, and is a
different test from the log-rank test: the two are usually close and
need not agree.

## Censor flag or event flag

The outcome is a time and a flag, and the flag can be written either way
round. Say which by the argument used: `strCensorCol` names a column
that is 1 for a censored time and 0 for an event, as ADaM's `CNSR` is;
`strEventCol` names a column that is 1 (or `TRUE`) for an event and 0
(or `FALSE`) for a censored time. Exactly one must be given, and a
column holding anything but 0 and 1 is refused. The first of `notes`
states which value was read as an event, and `rows` gives the events in
each group, so a flag read the wrong way round shows.

## See also

Other statistics:
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md),
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md)

## Examples

``` r
# Event-free survival by Baseline CRP, above against below its median
dfCRP <- Synthetic_Results[
  Synthetic_Results$TEST == "CRP" & Synthetic_Results$VISIT == "Baseline",
]
dfFrame <- merge(Synthetic_Outcomes, dfCRP[c("USUBJID", "STRESN")])
dfFrame$Level <- ifelse(dfFrame$STRESN > stats::median(dfFrame$STRESN), "High", "Low")

lResult <- Analyze_Survival(
  dfFrame, "AVAL", "Level",
  strCensorCol = "CNSR", chrGroups = c("High", "Low")
)
lResult$p_value
#> [1] 2.013876e-12
lResult$estimates
#>           name      group  estimate     lower    upper level
#> 1       Median       High  8.280000  5.240000 9.710000  0.95
#> 2       Median        Low 23.320000 17.320000       NA  0.95
#> 3 Hazard ratio High / Low  3.522974  2.430294 5.106931  0.95
lResult$rows[c("group", "n", "events", "median")]
#>   group   n events median
#> 1  High 100     89   8.28
#> 2   Low 100     44  23.32
```
