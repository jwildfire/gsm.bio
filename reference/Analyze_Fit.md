# Fit a line or a smooth to two numeric variables

Fits one numeric variable to another on the participants who have both,
overall and, when a group column is named, within each group, and
returns the fitted line with its band as points a chart can draw as they
are.

## Usage

``` r
Analyze_Fit(
  dfData,
  strXCol,
  strYCol,
  strMethod = "linear",
  strGroupCol = NULL,
  chrGroups = NULL,
  nConfLevel = 0.95,
  nMinGroup = nMinGroupDefault,
  nPoints = 50L
)
```

## Arguments

- dfData:

  `data.frame` One row per participant.

- strXCol, strYCol:

  `character` Names of the two numeric columns: y is fitted to x.

- strMethod:

  `character` The fit: `"linear"` or `"smooth"`. Default: `"linear"`.

- strGroupCol:

  `character` Name of a column holding each participant's group, to add
  a fit per group. Default: `NULL`, the overall fit only.

- chrGroups:

  `character` The groups to fit, in order. Default: `NULL`, every group
  present, in sorted order.

- nConfLevel:

  `numeric` Confidence level of the intervals. Default: `0.95`.

- nMinGroup:

  `numeric` The smallest number of complete pairs a fit is computed for,
  overall and in each group. Default: `nMinGroupDefault`, which is 5.
  See
  [StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).

- nPoints:

  `numeric` How many x values the line is given at, a whole number from
  2 to 1000. Default: `50`, which draws a smooth band at the width of a
  chart.

## Value

The fixed result described in
[StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).
Here:

- `counts` is the number of complete pairs.

- `estimates`, for the linear fit, has a row named `"Intercept"` and a
  row named `"Slope"` for the overall fit, with `group` `NA`, and then
  the same two rows for each group that was fitted, with `group` its
  name. For the smooth it has no rows.

- `statistic`, for the linear fit, holds `t` (the slope's t value), `df`
  (the residual degrees of freedom) and `r.squared`; for the smooth,
  `enp` (the equivalent number of parameters), `df` (the degrees of
  freedom of the band) and `residual.scale` (the residual standard
  error), as [`loess()`](https://rdrr.io/r/stats/loess.html) and
  [`predict()`](https://rdrr.io/r/stats/predict.html) report them.

- `p_value`, for the linear fit, is the p-value of the t-test that the
  slope is zero, from `summary(lm())`. For the smooth it is `NA`.

- `rows` holds the lines: one row per point, the overall line first,
  with `group` `NA`, and then each group's. Its columns are `group`,
  `x`, `fit`, `lower`, `upper` and `level` (the point and its band), and
  then the answer of the fit the point belongs to, repeated on each of
  its rows: `counts`, `method`, `statistic` (the slope's t value), `df`,
  `r_squared`, `p_value`, `adjustment`, `status`, `reason` and
  `warning`. A fit that could not be made, because it has too few pairs
  or R stopped, is one row with its `status` and `reason` and no point:
  `x` is `NA`.

When every x value is the same, or every y value, there is no line to
fit: `status` is `"error"` and `reason` says so. So it is when a line
leaves no residual degrees of freedom, as two pairs do with the minimum
lowered: its interval, its test and its band are not finite.

A smooth needs at least seven complete pairs, or the minimum group size
when that is larger: with its defaults
[`loess()`](https://rdrr.io/r/stats/loess.html) gives a band that is not
finite on most samples of five or six points. Below that a smooth has
`status` `"too_small"`. A smooth whose curve or band is not finite at
some x value, as [`loess()`](https://rdrr.io/r/stats/loess.html) can
give on few points or few distinct x values, has `status` `"error"`, and
no part of it is drawn.

## Details

Each method is the base R function, called with R's defaults:

|  |  |  |
|----|----|----|
| `strMethod` | R function | What is returned |
| `"linear"` | [`stats::lm()`](https://rdrr.io/r/stats/lm.html), `y ~ x` | The intercept and the slope, each with its interval from [`stats::confint()`](https://rdrr.io/r/stats/confint.html); the slope's t value and the p-value of the test that the slope is zero; R-squared and the residual degrees of freedom; the line and its confidence band from [`stats::predict()`](https://rdrr.io/r/stats/predict.html) with `interval = "confidence"`. |
| `"smooth"` | [`stats::loess()`](https://rdrr.io/r/stats/loess.html), `y ~ x` | The curve and its band from [`stats::predict()`](https://rdrr.io/r/stats/predict.html) with `se = TRUE`. No slope, no intercept and no test. |

The line is given at `nPoints` x values, equally spaced from the least
to the greatest x among the pairs the fit used, so each group's line
spans that group's own values. Nothing is left for a chart to work out.

The band of the linear fit is the confidence band of the fitted mean at
`nConfLevel`, as [`predict()`](https://rdrr.io/r/stats/predict.html)
gives it; it is not a prediction band. The band of the smooth is the
conventional pointwise band, formed here from what
[`predict()`](https://rdrr.io/r/stats/predict.html) returns: the fit,
give or take `qt((1 + nConfLevel) / 2, df) * se.fit`, with the degrees
of freedom [`predict()`](https://rdrr.io/r/stats/predict.html) reports.

A logarithmic axis is the chart's business: the values are fitted as
they are given.

## See also

Other statistics:
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_DifferenceGrid()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_DifferenceGrid.md),
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_GroupDifferenceBy()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifferenceBy.md),
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)

## Examples

``` r
# IL-10 against TNF-alpha at Baseline, the pair the synthetic study plants
# a correlation in
dfBaseline <- Synthetic_Results[Synthetic_Results$VISIT == "Baseline", ]
dfTNF <- dfBaseline[dfBaseline$TEST == "TNF-alpha", ]
dfIL10 <- dfBaseline[dfBaseline$TEST == "IL-10", ]
dfFrame <- Synthetic_Participants
dfFrame$TNF <- dfTNF$STRESN[match(dfFrame$USUBJID, dfTNF$USUBJID)]
dfFrame$IL10 <- dfIL10$STRESN[match(dfFrame$USUBJID, dfIL10$USUBJID)]

lLine <- Analyze_Fit(dfFrame, "TNF", "IL10", strGroupCol = "ARM", nPoints = 5)
lLine$estimates
#>        name     group  estimate     lower     upper level
#> 1 Intercept      <NA> 2.0281621 1.3562914 2.7000327  0.95
#> 2     Slope      <NA> 0.3274717 0.2721362 0.3828072  0.95
#> 3 Intercept   Placebo 2.3331606 1.3054931 3.3608282  0.95
#> 4     Slope   Placebo 0.3033605 0.2205317 0.3861893  0.95
#> 5 Intercept Treatment 1.7852438 0.8782961 2.6921915  0.95
#> 6     Slope Treatment 0.3474453 0.2710418 0.4238489  0.95
lLine$statistic
#>        name       value
#> 1         t  11.6702704
#> 2        df 198.0000000
#> 3 r.squared   0.4075319
lLine$rows[c("group", "x", "fit", "lower", "upper")]
#>        group        x      fit    lower    upper
#> 1       <NA>  2.93800 2.990274 2.474719 3.505829
#> 2       <NA>  6.69425 4.220340 3.895556 4.545123
#> 3       <NA> 10.45050 5.450405 5.272703 5.628108
#> 4       <NA> 14.20675 6.680471 6.470522 6.890420
#> 5       <NA> 17.96300 7.910536 7.532402 8.288671
#> 6    Placebo  4.49000 3.695249 3.024628 4.365870
#> 7    Placebo  7.68075 4.663197 4.230805 5.095588
#> 8    Placebo 10.87150 5.631144 5.378387 5.883901
#> 9    Placebo 14.06225 6.599092 6.315359 6.882825
#> 10   Placebo 17.25300 7.567039 7.080401 8.053678
#> 11 Treatment  2.93800 2.806038 2.113898 3.498178
#> 12 Treatment  6.69425 4.111130 3.679062 4.543197
#> 13 Treatment 10.45050 5.416221 5.173253 5.659189
#> 14 Treatment 14.20675 6.721312 6.411292 7.031333
#> 15 Treatment 17.96300 8.026404 7.480584 8.572224

lSmooth <- Analyze_Fit(dfFrame, "TNF", "IL10", strMethod = "smooth", nPoints = 5)
lSmooth$rows[c("x", "fit", "lower", "upper")]
#>          x      fit    lower    upper
#> 1  2.93800 2.962219 1.435579 4.488860
#> 2  6.69425 4.230937 3.841751 4.620124
#> 3 10.45050 5.359480 5.060124 5.658836
#> 4 14.20675 6.821759 6.533813 7.109704
#> 5 17.96300 7.321964 6.256843 8.387085
```
