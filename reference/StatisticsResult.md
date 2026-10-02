# The result every statistics function returns

Every `Analyze_*` function returns the same plain named list, whatever
it computed and whether or not it could compute it. A chart hands over a
table with one row per participant and receives this back.

## Members

Always all of these, always in this order, all in lower snake case.

|  |  |
|----|----|
| Member | Holds |
| `status` | `"ok"`; `"too_small"` when a group is below the minimum size; `"error"` when R stopped or the request could not be met. |
| `reason` | Why there are no numbers, as text. `NA` when `status` is `"ok"`. For `"error"` it is R's own message where R raised one. |
| `test` | The method asked for, as the caller named it, for example `"wilcoxon"`. |
| `method` | The method's name as R reports it, for example `"Welch Two Sample t-test"`. |
| `estimates` | A data frame, one row per estimate: `name`, `group`, `estimate`, and its interval as `lower`, `upper` and `level`, which are `NA` where R gives no interval. |
| `statistic` | A data frame of `name` and `value`: the test statistic first, then its parameters, named as R names them. |
| `p_value` | One number between 0 and 1, or `NA`. |
| `adjustment` | How `p_value` was adjusted for multiple tests; `"none"` when it was not. |
| `counts` | The participants actually used, after dropping what was missing: one whole number, or a named list of group to whole number. |
| `dropped` | A data frame of `reason` and `n`: the rows left out and why. No rows when nothing was dropped. |
| `warnings` | An unnamed list of the warnings R raised inside the wrapped call, as text. They are captured here and never printed. |
| `notes` | An unnamed list of remarks of the package's own, as text. |
| `rows` | A data frame for a function's many-row results: pairwise comparisons, per-group correlations, the pairs of a matrix, the cells of a table, the groups of a survival comparison, the biomarkers of a screen. Its columns are given on each function's page. No rows when there are none. |

When `status` is not `"ok"`, `reason` says why and the numbers are
withheld: `p_value` is `NA` and `estimates` and `statistic` have no
rows. `counts` and `dropped` are still filled in where they are known,
because they are the explanation.

Wherever `p_value` and `adjustment` appear together, at the top level or
in a row of `rows`, `adjustment` describes that `p_value`. A row whose
p-value was adjusted also carries the unadjusted one as `p_unadjusted`.

## R's answer

A result is the answer of the R that computed it. Every statistic is the
R function called with R's own defaults, and those defaults are R's to
change. A few have changed between versions, so the same call on the
same data can give a different number in an older R and a newer one. One
case is known: with tied values,
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) in R 4.3.3
warns that it cannot compute an exact p-value and uses the normal
approximation, where R 4.6.1 computes the exact p-value and does not
warn. A chart computing in the browser and a report computed at a desk
agree when they run the same version of R.

## Crossing into JavaScript

The result needs only base R to become JSON, and these rules hold for
every member, at every depth:

- No factor, no matrix, no date, and no classed object other than a
  plain data frame. Text is character.

- A single value is an unnamed vector of length one. A missing value is
  `NA`.

- Anything that is a collection is a data frame or an unnamed list,
  never a bare vector, so that a collection of one is still a
  collection.

- A named list is used only where the names are the keys: the result
  itself, and `counts` by group.

## Inputs

The first argument is the data frame. Every other argument is named and
is a string, a number, a boolean or several of those: no formula,
function or expression, and nothing is ever evaluated. An argument that
takes several values accepts a vector or an unnamed list of single
values.

In the data, text may be character or factor, a number may be whole or
decimal, and a missing value is `NA`. A number that is not finite counts
as missing, and so does an empty string in a category. Rows that cannot
be used are dropped and counted in `dropped`.

No function raises an error or a warning for anything about the data or
the request. Both become part of the result.

## Minimum group size

A statistic is not computed when a group has fewer participants than
`nMinGroup`. The default is 5. It is a default, not an agreed or
validated threshold: it is the smallest size at which every wrapped test
runs and returns its interval with a little to spare, and it is the
conventional floor for an expected count in a chi-squared test.

## One definition

The functions are defined once, in the file
`system.file("statistics", "statistics.R", package = "gsm.bio")`. The
package's exported functions are built from that file, and the same file
runs as it is in a bare R session with only the stats and survival
packages attached.

## See also

[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md),
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md)
