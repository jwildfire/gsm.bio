# Screen many biomarkers with one comparison

Runs one comparison, chosen once, on every biomarker and returns one row
per biomarker: a unit-free estimate with its interval, a p-value, and
that p-value adjusted across the rows.

## Usage

``` r
Analyze_Screen(
  dfData,
  chrCols,
  strComparison = "difference",
  strGroupCol = NULL,
  chrGroups = NULL,
  strWithCol = NULL,
  strCorMethod = "pearson",
  strTimeCol = NULL,
  strCensorCol = NULL,
  strEventCol = NULL,
  strPAdjust = "BH",
  nConfLevel = 0.95,
  nMinGroup = nMinGroupDefault
)
```

## Arguments

- dfData:

  `data.frame` One row per participant.

- chrCols:

  `character` Names of the numeric biomarker columns, one row of the
  screen each.

- strComparison:

  `character` The comparison: `"difference"`, `"correlation"` or
  `"hazard"`. Default: `"difference"`.

- strGroupCol:

  `character` For `"difference"`: name of the column holding each
  participant's group. Default: `NULL`.

- chrGroups:

  `character` For `"difference"`: the two groups, in order; the
  difference is the first minus the second. Default: `NULL`, the two
  groups present, in sorted order.

- strWithCol:

  `character` For `"correlation"`: name of the numeric column every
  biomarker is correlated with. Default: `NULL`.

- strCorMethod:

  `character` For `"correlation"`: `"pearson"` or `"spearman"`. Default:
  `"pearson"`.

- strTimeCol:

  `character` For `"hazard"`: name of the numeric column holding the
  time to the event or to censoring. Default: `NULL`.

- strCensorCol:

  `character` Name of a censor flag column: 1 for a censored time, 0 for
  an event. Default: `NULL`.

- strEventCol:

  `character` Name of an event flag column: 1 for an event, 0 for a
  censored time. Default: `NULL`.

- strPAdjust:

  `character` The adjustment across the rows, one of
  [stats::p.adjust.methods](https://rdrr.io/r/stats/p.adjust.html).
  Default: `"BH"`.

- nConfLevel:

  `numeric` Confidence level of the intervals. Default: `0.95`.

- nMinGroup:

  `numeric` The smallest group, or number of complete pairs, a row is
  computed for. A biomarker below it has `status` `"too_small"` in its
  row and no numbers. Default: `nMinGroupDefault`, which is 5. See
  [StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).

## Value

The fixed result described in
[StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).
Here `test` is the comparison; `p_value` is `NA` and `estimates` and
`statistic` have no rows; `counts` is a named list of biomarker to
participants used; and `rows` has one row per biomarker, with the
columns `biomarker`, `counts`, `n_1` and `n_2` (the two groups, or high
and low), `events`, `dropped`, `estimate`, `lower`, `upper`, `level`,
`method`, `statistic`, `p_unadjusted`, `p_value` (adjusted),
`adjustment`, `adjusted_over`, `status`, `reason` and `warning`. The
result's own `status` is `"ok"` when any row is.

## Details

The data stay one row per participant, with one column per biomarker.
Each row of the screen is the matching single function's answer for that
biomarker, so its unadjusted p-value is the one the single chart prints:

|  |  |  |  |
|----|----|----|----|
| `strComparison` | Estimate | p-value from | Needs |
| `"difference"` | The standardised difference between two groups. | [`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md), the Welch t-test. | `strGroupCol` |
| `"correlation"` | The correlation with one fixed variable. | [`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md). | `strWithCol` |
| `"hazard"` | The hazard ratio, high against low. | [`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md), the log-rank test. | `strTimeCol` and a flag column |

The adjustment is
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) across the
rows that have a p-value. A biomarker that could not be computed has its
own `status` and `reason`, has no p-value, and is left out of the
adjustment; `notes` says how many rows the adjustment covered, and each
adjusted row carries that number as `adjusted_over`. The default is
Benjamini-Hochberg, which controls the share of false leads among the
rows picked out, the usual aim of a screen; Holm, which guards against
any false lead at all, is stricter.

The rows come back in the order the columns were named. Sorting is the
caller's.

## The standardised difference

This is the one statistic the package computes itself rather than
handing to an existing function, to avoid a heavy dependency. It is
Hedges' g: the difference in means, first group minus second, divided by
the pooled standard deviation, times the exact small-sample correction.
Its interval is the noncentral t interval for the two-sample t statistic
with pooled variance, put on the same scale. It agrees with
[`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html),
and the package's tests check that it does.

Its p-value is not computed from it: it is
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html)'s, Welch, which
does not assume the equal variances that the pooled standard deviation
does. So when the two groups' spreads differ, a row's interval can
include zero while its p-value is below 0.05, or exclude zero while it
is above; the screen's notes say so. Both are kept on purpose, each
labelled (#25): Hedges' g is defined with the pooled standard deviation,
and the Welch p-value is the one the group comparison prints when the
row is opened.

A row by hazard ratio whose hazard ratio cannot be estimated (see
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md))
has `status` `"error"` and a reason, and no method, statistic or
p-value, as a difference row has when its standardised difference cannot
be computed.

## High against low

For `"hazard"`, each biomarker is split at its median among the
participants who can be used, those with a value, a time and a flag. A
value above the median is high and a value on the median or below it is
low. The hazard ratio is high over low, its interval is the Cox model's
and the p-value is the log-rank test's. Other cuts are not offered here.

## See also

Other statistics:
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md),
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)

## Examples

``` r
# Every biomarker's change from Baseline to Week 4, Treatment against Placebo
dfFrame <- Synthetic_Participants
chrBiomarkers <- unique(Synthetic_Results$TEST)
for (strBiomarker in chrBiomarkers) {
  dfOne <- Synthetic_Results[Synthetic_Results$TEST == strBiomarker, ]
  dfBaseline <- dfOne[dfOne$VISIT == "Baseline", ]
  dfWeek4 <- dfOne[dfOne$VISIT == "Week 4", ]
  dfFrame[[strBiomarker]] <- dfWeek4$STRESN[match(dfFrame$USUBJID, dfWeek4$USUBJID)] -
    dfBaseline$STRESN[match(dfFrame$USUBJID, dfBaseline$USUBJID)]
}

lResult <- Analyze_Screen(
  dfFrame, chrBiomarkers, "difference",
  strGroupCol = "ARM", chrGroups = c("Treatment", "Placebo")
)
dfRows <- lResult$rows[order(lResult$rows$p_value), ]
head(dfRows[c("biomarker", "estimate", "lower", "upper", "p_unadjusted", "p_value")], 3)
#>   biomarker   estimate      lower       upper p_unadjusted      p_value
#> 1      IL-6 -0.9133209 -1.2133125 -0.61107530 3.229638e-09 3.875566e-08
#> 7  IL-1beta -0.2589287 -0.5484080  0.03125959 8.010536e-02 4.806322e-01
#> 3 TNF-alpha  0.1620913 -0.1272192  0.45095583 2.723494e-01 5.748065e-01
```
