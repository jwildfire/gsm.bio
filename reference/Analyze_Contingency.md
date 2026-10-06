# Test the association between two categories

Cross-tabulates two categorical variables and tests whether they are
associated.

## Usage

``` r
Analyze_Contingency(
  dfData,
  strRowCol,
  strColCol,
  strMethod = "chisq",
  chrRowGroups = NULL,
  chrColGroups = NULL,
  nConfLevel = 0.95,
  nMinGroup = nMinGroupDefault
)
```

## Arguments

- dfData:

  `data.frame` One row per participant.

- strRowCol, strColCol:

  `character` Names of the two categorical columns: the rows and the
  columns of the table.

- strMethod:

  `character` The test: `"chisq"` or `"fisher"`. Default: `"chisq"`.

- chrRowGroups, chrColGroups:

  `character` The categories to keep, in order, for the rows and for the
  columns. Participants in any other category are dropped and counted.
  On a two-by-two table the order sets which way round the odds ratio
  is. Default: `NULL`, every category present, in sorted order.

- nConfLevel:

  `numeric` Confidence level of the odds ratio's interval. Default:
  `0.95`.

- nMinGroup:

  `numeric` The smallest category the chi-squared test is computed for.
  If any row or column of the table totals fewer participants, the
  result for `"chisq"` has `status` `"too_small"` and no numbers. It is
  not applied to `"fisher"`. Default: `nMinGroupDefault`, which is 5.
  See
  [StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).

## Value

The fixed result described in
[StatisticsResult](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md).
Here `counts` is the number of participants in the table; `estimates`
has the odds ratio for `"fisher"` on a two-by-two table and no rows
otherwise; and `rows` is the table in long form, one row per cell, with
the columns `row`, `col`, `n`, `expected` and `small_expected` (the last
two `NA` for `"fisher"`).

## Details

Each method is the base R function, called with R's defaults:

|  |  |
|----|----|
| `strMethod` | R function |
| `"chisq"` | [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html), with the continuity correction on a two-by-two table. |
| `"fisher"` | [`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html), on a table of any size R will accept. On a two-by-two table it also estimates the odds ratio, with its interval. |

For `"chisq"`, each cell of `rows` carries the expected count that
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) returned and
is flagged where it is below 5, the count at which
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) itself warns.
When any cell is flagged, `notes` says how many, and R's warning is in
`warnings`.

The minimum group size, `nMinGroup`, applies to `"chisq"` only. A row or
a column of the table that totals fewer participants stops the
chi-squared test, with `status` `"too_small"`. Fisher's exact test is
exact at any count, which is what it is for, so it is exempt: it runs on
any table in which two or more rows and two or more columns have at
least one participant, whatever `nMinGroup` is. When a row or a column
is below the minimum, `notes` says so and names it. A category nobody is
in is no row and no column: where that leaves fewer than two either way,
the result has `status` `"too_small"` and R's reason, where
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) itself would
return a p-value of 1.

## See also

Other statistics:
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_DifferenceGrid()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_DifferenceGrid.md),
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md),
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_GroupDifferenceBy()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifferenceBy.md),
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)

## Examples

``` r
lResult <- Analyze_Contingency(Synthetic_Participants, "ARM", "RESPONSE")
lResult$method
#> [1] "Pearson's Chi-squared test with Yates' continuity correction"
lResult$p_value
#> [1] 0.4639908
lResult$rows
#>         row           col  n expected small_expected
#> 1   Placebo Non-responder 66       63          FALSE
#> 2 Treatment Non-responder 60       63          FALSE
#> 3   Placebo     Responder 34       37          FALSE
#> 4 Treatment     Responder 40       37          FALSE

Analyze_Contingency(Synthetic_Participants, "ARM", "RESPONSE", strMethod = "fisher")$estimates
#>         name group estimate     lower   upper level
#> 1 odds ratio  <NA> 1.292434 0.6994128 2.39815  0.95

# A table with a small margin: twenty participants, two of them at grade 3.
# Fisher's exact test runs; the chi-squared test is not computed.
dfGrades <- data.frame(
  arm = rep(c("A", "B"), each = 10),
  grade = c(rep(1:3, times = c(6, 3, 1)), rep(1:3, times = c(4, 5, 1)))
)
Analyze_Contingency(dfGrades, "arm", "grade", strMethod = "fisher")$p_value
#> [1] 0.8090454
Analyze_Contingency(dfGrades, "arm", "grade")$reason
#> [1] "Not computed: grade = 3 has 2. The minimum group size is 5."
```
