# The statistics functions have one definition, the file
# inst/statistics/statistics.R, which the installed package also ships as it is
# for R in the browser. Nothing is defined here: the file is evaluated into the
# package's namespace when the package is built, and the lines below attach the
# documentation and the export to the functions it defined.
#
# The namespace knows where it is being built from: the installed package,
# where inst/ has been copied to the top level, or the source tree under
# devtools::load_all(), where it has not.
local({
  strRoot <- getNamespaceInfo(topenv(), "path")
  chrFile <- file.path(strRoot, c("statistics", file.path("inst", "statistics")), "statistics.R")
  chrFile <- chrFile[file.exists(chrFile)]
  if (length(chrFile) == 0L) {
    stop("gsm.bio: statistics/statistics.R was not found under ", strRoot)
  }
  sys.source(chrFile[1], envir = topenv())
})

#' The result every statistics function returns
#'
#' Every `Analyze_*` function returns the same plain named list, whatever it
#' computed and whether or not it could compute it. A chart hands over a table
#' with one row per participant and receives this back.
#'
#' @section Members:
#' Always all of these, always in this order, all in lower snake case.
#'
#' | Member | Holds |
#' |---|---|
#' | `status` | `"ok"`; `"too_small"` when a group is below the minimum size; `"error"` when R stopped or the request could not be met. |
#' | `reason` | Why there are no numbers, as text. `NA` when `status` is `"ok"`. For `"error"` it is R's own message where R raised one. |
#' | `test` | The method asked for, as the caller named it, for example `"wilcoxon"`. |
#' | `method` | The method's name as R reports it, for example `"Welch Two Sample t-test"`. |
#' | `estimates` | A data frame, one row per estimate: `name`, `group`, `estimate`, and its interval as `lower`, `upper` and `level`, which are `NA` where R gives no interval. |
#' | `statistic` | A data frame of `name` and `value`: the test statistic first, then its parameters, named as R names them. |
#' | `p_value` | One number between 0 and 1, or `NA`. |
#' | `adjustment` | How `p_value` was adjusted for multiple tests; `"none"` when it was not. |
#' | `counts` | The participants actually used, after dropping what was missing: one whole number, or a named list of group to whole number. |
#' | `dropped` | A data frame of `reason` and `n`: the rows left out and why. No rows when nothing was dropped. |
#' | `warnings` | An unnamed list of the warnings R raised inside the wrapped call, as text. They are captured here and never printed. |
#' | `notes` | An unnamed list of remarks of the package's own, as text. |
#' | `rows` | A data frame for a function's many-row results: pairwise comparisons, per-group correlations, the pairs of a matrix, the points of a fitted line, the cells of a table, the groups of a survival comparison, the biomarkers of a screen. Its columns are given on each function's page. No rows when there are none. |
#'
#' When `status` is not `"ok"`, `reason` says why and the numbers are withheld:
#' `p_value` is `NA` and `estimates` and `statistic` have no rows. `counts` and
#' `dropped` are still filled in where they are known, because they are the
#' explanation.
#'
#' Wherever `p_value` and `adjustment` appear together, at the top level or in
#' a row of `rows`, `adjustment` describes that `p_value`. A row whose p-value
#' was adjusted also carries the unadjusted one as `p_unadjusted`.
#'
#' @section R's answer:
#' A result is the answer of the R that computed it. Every statistic is the R
#' function called with R's own defaults, and those defaults are R's to change.
#' A few have changed between versions, so the same call on the same data can
#' give a different number in an older R and a newer one. One case is known:
#' with tied values, `wilcox.test()` in R 4.3.3 warns that it cannot compute an
#' exact p-value and uses the normal approximation, where R 4.6.1 computes the
#' exact p-value and does not warn. A chart computing in the browser and a
#' report computed at a desk agree when they run the same version of R.
#'
#' @section Crossing into JavaScript:
#' The result needs only base R to become JSON, and these rules hold for every
#' member, at every depth:
#'
#' * No factor, no matrix, no date, and no classed object other than a plain
#'   data frame. Text is character.
#' * A single value is an unnamed vector of length one. A missing value is
#'   `NA`.
#' * Anything that is a collection is a data frame or an unnamed list, never a
#'   bare vector, so that a collection of one is still a collection.
#' * A named list is used only where the names are the keys: the result
#'   itself, and `counts` by group.
#'
#' @section Inputs:
#' The first argument is the data frame. Every other argument is named and is
#' a string, a number, a boolean or several of those: no formula, function or
#' expression, and nothing is ever evaluated. An argument that takes several
#' values accepts a vector or an unnamed list of single values.
#'
#' In the data, text may be character or factor, a number may be whole or
#' decimal, and a missing value is `NA`. A number that is not finite counts as
#' missing, and so does an empty string in a category. Rows that cannot be used
#' are dropped and counted in `dropped`.
#'
#' No function raises an error or a warning for anything about the data or the
#' request. Both become part of the result.
#'
#' @section Minimum group size:
#' A statistic is not computed when a group has fewer participants than
#' `nMinGroup`. The default is 5. It is a default, not an agreed or validated
#' threshold: it is the smallest size at which every wrapped test runs and
#' returns its interval with a little to spare, and it is the conventional
#' floor for an expected count in a chi-squared test.
#'
#' @section One definition:
#' The functions are defined once, in the file
#' `system.file("statistics", "statistics.R", package = "gsm.bio")`. The
#' package's exported functions are built from that file, and the same file
#' runs as it is in a bare R session with only the stats and survival packages
#' attached.
#'
#' @name StatisticsResult
#' @aliases statistics-result
#' @seealso [Analyze_GroupDifference()], [Analyze_Correlation()],
#'   [Analyze_CorrelationMatrix()], [Analyze_Fit()], [Analyze_Contingency()],
#'   [Analyze_Survival()], [Analyze_Screen()]
NULL

#' Compare a numeric variable between groups
#'
#' Tests whether a value differs between two or more groups of participants,
#' and reports the group means and, for two groups, the difference in means
#' with its interval.
#'
#' Each method is the base R function, called with R's defaults:
#'
#' | `strMethod` | Groups | R function |
#' |---|---|---|
#' | `"t"` | two | [stats::t.test()], Welch: unequal variances. |
#' | `"wilcoxon"` | two | [stats::wilcox.test()], with R's own switch between the exact and the approximate p-value. |
#' | `"anova"` | two or more | [stats::aov()], one-way. |
#' | `"kruskal"` | two or more | [stats::kruskal.test()], with its tie correction. |
#'
#' With exactly two groups, `estimates` includes the difference in means, the
#' first group's mean minus the second's, with its interval. Both always come
#' from `t.test()` (Welch), whichever test was asked for.
#'
#' With more than two groups and `bPairwise`, every pair of groups is compared
#' with the two-group test of the same family, `t.test()` for `"anova"` and
#' `wilcox.test()` for `"kruskal"`, and the p-values are adjusted across the
#' pairs by [stats::p.adjust()]. The pairs are in `rows`.
#'
#' @param dfData `data.frame` One row per participant.
#' @param strValueCol `character` Name of the numeric column to compare.
#' @param strGroupCol `character` Name of the column holding each participant's
#'   group.
#' @param strMethod `character` The test: `"t"`, `"wilcoxon"`, `"anova"` or
#'   `"kruskal"`. Default: `"t"`.
#' @param chrGroups `character` The groups to compare, in order; the
#'   difference in means is the first minus the second. Participants in any
#'   other group are dropped and counted. Default: `NULL`, every group present,
#'   in sorted order.
#' @param bPairwise `logical` Compare every pair of groups when there are more
#'   than two. Default: `TRUE`.
#' @param strPAdjust `character` The adjustment across the pairs, one of
#'   [stats::p.adjust.methods]. Default: `"holm"`.
#' @param nConfLevel `numeric` Confidence level of the intervals. Default:
#'   `0.95`.
#' @param nMinGroup `numeric` The smallest group the test is computed for. If
#'   any group has fewer participants with a value, the result has `status`
#'   `"too_small"` and no numbers. Default: `nMinGroupDefault`, which is 5. See
#'   [StatisticsResult].
#'
#' @return The fixed result described in [StatisticsResult]. Here `counts` is a
#'   named list of group to participants used; `estimates` has a row named
#'   `"Mean"` per group and, for two groups, a row named
#'   `"Difference in means"`; and `rows` has one row per pair of groups when
#'   pairwise comparisons were made, with the columns `group_1`, `group_2`,
#'   `n_1`, `n_2`, `counts` (the two together), `estimate` (the difference in
#'   means, `group_1` minus `group_2`), `lower`, `upper`, `level`, `method`,
#'   `statistic`, `p_unadjusted`, `p_value` (adjusted), `adjustment`, `status`,
#'   `reason` and `warning`. The intervals in `rows` are not adjusted.
#'
#' @examples
#' # Change in IL-6 from Baseline to Week 4, by arm
#' dfIL6 <- Synthetic_Results[Synthetic_Results$TEST == "IL-6", ]
#' dfBaseline <- dfIL6[dfIL6$VISIT == "Baseline", ]
#' dfWeek4 <- dfIL6[dfIL6$VISIT == "Week 4", ]
#' dfFrame <- Synthetic_Participants
#' dfFrame$Change <- dfWeek4$STRESN[match(dfFrame$USUBJID, dfWeek4$USUBJID)] -
#'   dfBaseline$STRESN[match(dfFrame$USUBJID, dfBaseline$USUBJID)]
#'
#' lResult <- Analyze_GroupDifference(
#'   dfFrame, "Change", "ARM",
#'   chrGroups = c("Treatment", "Placebo")
#' )
#' lResult$method
#' lResult$estimates
#' lResult$p_value
#' lResult$counts
#'
#' @family statistics
#' @export
Analyze_GroupDifference <- Analyze_GroupDifference

#' Correlate two numeric variables
#'
#' Estimates the correlation between two numeric variables on the participants
#' who have both, overall and, when a group column is named, within each
#' group.
#'
#' Both methods are [stats::cor.test()] with R's defaults. For `"pearson"` the
#' interval is the one `cor.test()` gives, by Fisher's z. For `"spearman"`
#' `cor.test()` gives no interval, so none is reported and `notes` says so.
#'
#' @inheritParams Analyze_GroupDifference
#' @param strXCol,strYCol `character` Names of the two numeric columns.
#' @param strMethod `character` The coefficient: `"pearson"` or `"spearman"`.
#'   Default: `"pearson"`.
#' @param strGroupCol `character` Name of a column holding each participant's
#'   group, to add a row per group. Default: `NULL`, no per-group rows.
#' @param chrGroups `character` The groups to give a row to, in order. Default:
#'   `NULL`, every group present, in sorted order.
#' @param nMinGroup `numeric` The smallest number of complete pairs the
#'   correlation is computed for, overall and in each group. Default:
#'   `nMinGroupDefault`, which is 5. See [StatisticsResult].
#'
#' @return The fixed result described in [StatisticsResult]. Here `counts` is
#'   the number of complete pairs; `estimates` has one row, named as R names
#'   the coefficient (`"cor"` or `"rho"`); and `rows` has one row per group
#'   when `strGroupCol` is given, with the columns `group`, `counts`,
#'   `estimate`, `lower`, `upper`, `level`, `method`, `statistic`, `p_value`,
#'   `adjustment`, `status`, `reason` and `warning`. The overall answer uses
#'   every complete pair, whether or not the participant has a group.
#'
#' @examples
#' # TNF-alpha against IL-10 at Baseline
#' dfBaseline <- Synthetic_Results[Synthetic_Results$VISIT == "Baseline", ]
#' dfTNF <- dfBaseline[dfBaseline$TEST == "TNF-alpha", ]
#' dfIL10 <- dfBaseline[dfBaseline$TEST == "IL-10", ]
#' dfFrame <- Synthetic_Participants
#' dfFrame$TNF <- dfTNF$STRESN[match(dfFrame$USUBJID, dfTNF$USUBJID)]
#' dfFrame$IL10 <- dfIL10$STRESN[match(dfFrame$USUBJID, dfIL10$USUBJID)]
#'
#' lResult <- Analyze_Correlation(dfFrame, "TNF", "IL10", strGroupCol = "ARM")
#' lResult$estimates
#' lResult$rows[c("group", "counts", "estimate", "lower", "upper")]
#'
#' @family statistics
#' @export
Analyze_Correlation <- Analyze_Correlation

#' Correlate every pair of several numeric variables
#'
#' Runs [stats::cor.test()] on every pair of the named columns, each pair on
#' the participants who have both values, and returns one row per pair with
#' the coefficient, its interval and the number of pairs it rests on.
#'
#' No p-values are returned. A matrix of many coefficients is for seeing which
#' variables move together, and a p-value on every cell would be many
#' unadjusted tests. To test one pair, use [Analyze_Correlation()].
#'
#' @inheritParams Analyze_Correlation
#' @param chrCols `character` Names of two or more numeric columns.
#' @param nMinPairs `numeric` The smallest number of complete pairs a cell is
#'   computed for. A pair with fewer has `status` `"too_small"` and no numbers
#'   in its row. Default: `nMinGroupDefault`, which is 5. See
#'   [StatisticsResult].
#'
#' @return The fixed result described in [StatisticsResult]. Here `p_value` is
#'   `NA` and `estimates` and `statistic` have no rows; `counts` is a named
#'   list of column to participants with a value; and `rows` is the matrix in
#'   long form, one row per pair of columns, each pair once, with the columns
#'   `x`, `y`, `counts` (the complete pairs), `estimate`, `lower`, `upper`,
#'   `level`, `status`, `reason` and `warning`. The result's own `status` is
#'   `"too_small"` only when every pair is.
#'
#' @examples
#' # Four biomarkers at Baseline, one column each
#' dfBaseline <- Synthetic_Results[Synthetic_Results$VISIT == "Baseline", ]
#' dfFrame <- Synthetic_Participants["USUBJID"]
#' for (strBiomarker in c("TNF-alpha", "IL-10", "IL-6", "CRP")) {
#'   dfOne <- dfBaseline[dfBaseline$TEST == strBiomarker, ]
#'   dfFrame[[strBiomarker]] <- dfOne$STRESN[match(dfFrame$USUBJID, dfOne$USUBJID)]
#' }
#'
#' lResult <- Analyze_CorrelationMatrix(dfFrame, c("TNF-alpha", "IL-10", "IL-6", "CRP"))
#' lResult$rows[c("x", "y", "counts", "estimate", "lower", "upper")]
#'
#' @family statistics
#' @export
Analyze_CorrelationMatrix <- Analyze_CorrelationMatrix

#' Fit a line or a smooth to two numeric variables
#'
#' Fits one numeric variable to another on the participants who have both,
#' overall and, when a group column is named, within each group, and returns
#' the fitted line with its band as points a chart can draw as they are.
#'
#' Each method is the base R function, called with R's defaults:
#'
#' | `strMethod` | R function | What is returned |
#' |---|---|---|
#' | `"linear"` | [stats::lm()], `y ~ x` | The intercept and the slope, each with its interval from [stats::confint()]; the slope's t value and the p-value of the test that the slope is zero; R-squared and the residual degrees of freedom; the line and its confidence band from [stats::predict()] with `interval = "confidence"`. |
#' | `"smooth"` | [stats::loess()], `y ~ x` | The curve and its band from [stats::predict()] with `se = TRUE`. No slope, no intercept and no test. |
#'
#' The line is given at `nPoints` x values, equally spaced from the least to the
#' greatest x among the pairs the fit used, so each group's line spans that
#' group's own values. Nothing is left for a chart to work out.
#'
#' The band of the linear fit is the confidence band of the fitted mean at
#' `nConfLevel`, as `predict()` gives it; it is not a prediction band. The band
#' of the smooth is the conventional pointwise band, formed here from what
#' `predict()` returns: the fit, give or take
#' `qt((1 + nConfLevel) / 2, df) * se.fit`, with the degrees of freedom
#' `predict()` reports.
#'
#' A logarithmic axis is the chart's business: the values are fitted as they
#' are given.
#'
#' @inheritParams Analyze_Correlation
#' @param strXCol,strYCol `character` Names of the two numeric columns: y is
#'   fitted to x.
#' @param strMethod `character` The fit: `"linear"` or `"smooth"`. Default:
#'   `"linear"`.
#' @param strGroupCol `character` Name of a column holding each participant's
#'   group, to add a fit per group. Default: `NULL`, the overall fit only.
#' @param chrGroups `character` The groups to fit, in order. Default: `NULL`,
#'   every group present, in sorted order.
#' @param nMinGroup `numeric` The smallest number of complete pairs a fit is
#'   computed for, overall and in each group. Default: `nMinGroupDefault`,
#'   which is 5. See [StatisticsResult].
#' @param nPoints `numeric` How many x values the line is given at, a whole
#'   number from 2 to 1000. Default: `50`, which draws a smooth band at the
#'   width of a chart.
#'
#' @return The fixed result described in [StatisticsResult]. Here:
#'
#' * `counts` is the number of complete pairs.
#' * `estimates`, for the linear fit, has a row named `"Intercept"` and a row
#'   named `"Slope"` for the overall fit, with `group` `NA`, and then the same
#'   two rows for each group that was fitted, with `group` its name. For the
#'   smooth it has no rows.
#' * `statistic`, for the linear fit, holds `t` (the slope's t value), `df`
#'   (the residual degrees of freedom) and `r.squared`; for the smooth, `enp`
#'   (the equivalent number of parameters), `df` (the degrees of freedom of the
#'   band) and `residual.scale` (the residual standard error), as `loess()` and
#'   `predict()` report them.
#' * `p_value`, for the linear fit, is the p-value of the t-test that the slope
#'   is zero, from `summary(lm())`. For the smooth it is `NA`.
#' * `rows` holds the lines: one row per point, the overall line first, with
#'   `group` `NA`, and then each group's. Its columns are `group`, `x`, `fit`,
#'   `lower`, `upper` and `level` (the point and its band), and then the answer
#'   of the fit the point belongs to, repeated on each of its rows: `counts`,
#'   `method`, `statistic` (the slope's t value), `df`, `r_squared`, `p_value`,
#'   `adjustment`, `status`, `reason` and `warning`. A fit that could not be
#'   made, because it has too few pairs or R stopped, is one row with its
#'   `status` and `reason` and no point: `x` is `NA`.
#'
#' When every x value is the same there is no line to fit: `status` is
#' `"error"` and `reason` says so.
#'
#' @examples
#' # IL-10 against TNF-alpha at Baseline, the pair the synthetic study plants
#' # a correlation in
#' dfBaseline <- Synthetic_Results[Synthetic_Results$VISIT == "Baseline", ]
#' dfTNF <- dfBaseline[dfBaseline$TEST == "TNF-alpha", ]
#' dfIL10 <- dfBaseline[dfBaseline$TEST == "IL-10", ]
#' dfFrame <- Synthetic_Participants
#' dfFrame$TNF <- dfTNF$STRESN[match(dfFrame$USUBJID, dfTNF$USUBJID)]
#' dfFrame$IL10 <- dfIL10$STRESN[match(dfFrame$USUBJID, dfIL10$USUBJID)]
#'
#' lLine <- Analyze_Fit(dfFrame, "TNF", "IL10", strGroupCol = "ARM", nPoints = 5)
#' lLine$estimates
#' lLine$statistic
#' lLine$rows[c("group", "x", "fit", "lower", "upper")]
#'
#' lSmooth <- Analyze_Fit(dfFrame, "TNF", "IL10", strMethod = "smooth", nPoints = 5)
#' lSmooth$rows[c("x", "fit", "lower", "upper")]
#'
#' @family statistics
#' @export
Analyze_Fit <- Analyze_Fit

#' Test the association between two categories
#'
#' Cross-tabulates two categorical variables and tests whether they are
#' associated.
#'
#' Each method is the base R function, called with R's defaults:
#'
#' | `strMethod` | R function |
#' |---|---|
#' | `"chisq"` | [stats::chisq.test()], with the continuity correction on a two-by-two table. |
#' | `"fisher"` | [stats::fisher.test()], on a table of any size R will accept. On a two-by-two table it also estimates the odds ratio, with its interval. |
#'
#' For `"chisq"`, each cell of `rows` carries the expected count that
#' `chisq.test()` returned and is flagged where it is below 5, the count at
#' which `chisq.test()` itself warns. When any cell is flagged, `notes` says
#' how many, and R's warning is in `warnings`.
#'
#' @inheritParams Analyze_GroupDifference
#' @param strRowCol,strColCol `character` Names of the two categorical columns:
#'   the rows and the columns of the table.
#' @param strMethod `character` The test: `"chisq"` or `"fisher"`. Default:
#'   `"chisq"`.
#' @param chrRowGroups,chrColGroups `character` The categories to keep, in
#'   order, for the rows and for the columns. Participants in any other
#'   category are dropped and counted. On a two-by-two table the order sets
#'   which way round the odds ratio is. Default: `NULL`, every category
#'   present, in sorted order.
#' @param nConfLevel `numeric` Confidence level of the odds ratio's interval.
#'   Default: `0.95`.
#' @param nMinGroup `numeric` The smallest category the test is computed for.
#'   If any row or column of the table totals fewer participants, the result
#'   has `status` `"too_small"` and no numbers. Default: `nMinGroupDefault`,
#'   which is 5. See [StatisticsResult].
#'
#' @return The fixed result described in [StatisticsResult]. Here `counts` is
#'   the number of participants in the table; `estimates` has the odds ratio
#'   for `"fisher"` on a two-by-two table and no rows otherwise; and `rows` is
#'   the table in long form, one row per cell, with the columns `row`, `col`,
#'   `n`, `expected` and `small_expected` (the last two `NA` for `"fisher"`).
#'
#' @examples
#' lResult <- Analyze_Contingency(Synthetic_Participants, "ARM", "RESPONSE")
#' lResult$method
#' lResult$p_value
#' lResult$rows
#'
#' Analyze_Contingency(Synthetic_Participants, "ARM", "RESPONSE", strMethod = "fisher")$estimates
#'
#' @family statistics
#' @export
Analyze_Contingency <- Analyze_Contingency

#' Compare survival between groups
#'
#' Tests whether the time to an event differs between two or more groups of
#' participants, and reports each group's median survival with its interval
#' and, for two groups, the hazard ratio with its interval.
#'
#' Each part is the survival package's own function:
#'
#' | Part | R function |
#' |---|---|
#' | The test | [survival::survdiff()], the log-rank test, for two or more groups. |
#' | Median survival and its interval | [survival::survfit()] with `conf.type = "log-log"`, the interval safety.viz draws as the band of its Kaplan-Meier curve. |
#' | The hazard ratio and its interval | [survival::coxph()], when there are exactly two groups. |
#'
#' The hazard ratio is the hazard in the first group over the hazard in the
#' second, so the second group is the reference: with
#' `chrGroups = c("High", "Low")` a ratio above 1 means events come sooner in
#' the high group.
#'
#' @section Two p-values, kept apart:
#' `p_value` is the log-rank test's. The Cox model has p-values of its own, and
#' the one that goes with the hazard ratio's interval is the Wald test's. It is
#' in `rows` as `hr_p_value`, labelled by `hr_test`, and is a different test
#' from the log-rank test: the two are usually close and need not agree.
#'
#' @section Censor flag or event flag:
#' The outcome is a time and a flag, and the flag can be written either way
#' round. Say which by the argument used: `strCensorCol` names a column that is
#' 1 for a censored time and 0 for an event, as ADaM's `CNSR` is;
#' `strEventCol` names a column that is 1 (or `TRUE`) for an event and 0 (or
#' `FALSE`) for a censored time. Exactly one must be given, and a column
#' holding anything but 0 and 1 is refused. The first of `notes` states which
#' value was read as an event, and `rows` gives the events in each group, so a
#' flag read the wrong way round shows.
#'
#' @inheritParams Analyze_GroupDifference
#' @param strTimeCol `character` Name of the numeric column holding the time to
#'   the event or to censoring.
#' @param strCensorCol `character` Name of a censor flag column: 1 for a
#'   censored time, 0 for an event. Default: `NULL`.
#' @param strEventCol `character` Name of an event flag column: 1 for an event,
#'   0 for a censored time. Default: `NULL`.
#' @param chrGroups `character` The groups to compare, in order; the hazard
#'   ratio is the first over the second. Participants in any other group are
#'   dropped and counted. Default: `NULL`, every group present, in sorted
#'   order.
#' @param nMinGroup `numeric` The smallest group the comparison is computed
#'   for, counted in participants, not events. If any group has fewer, the
#'   result has `status` `"too_small"` and no numbers. Default:
#'   `nMinGroupDefault`, which is 5. See [StatisticsResult].
#'
#' @return The fixed result described in [StatisticsResult]. Here `test` is
#'   `"logrank"`; `counts` is a named list of group to participants used;
#'   `estimates` has a row named `"Median"` per group and, for two groups, a
#'   row named `"Hazard ratio"`; and `rows` has one row per group, with the
#'   columns `group`, `n`, `events`, `median`, `lower`, `upper`, `level`, and,
#'   filled on the first group's row when there are two groups,
#'   `hazard_ratio`, `hr_lower`, `hr_upper`, `hr_p_value` and `hr_test`. A
#'   median or a bound that the curve or its band never reaches is `NA`.
#'
#' @examples
#' # Event-free survival by Baseline CRP, above against below its median
#' dfCRP <- Synthetic_Results[
#'   Synthetic_Results$TEST == "CRP" & Synthetic_Results$VISIT == "Baseline",
#' ]
#' dfFrame <- merge(Synthetic_Outcomes, dfCRP[c("USUBJID", "STRESN")])
#' dfFrame$Level <- ifelse(dfFrame$STRESN > stats::median(dfFrame$STRESN), "High", "Low")
#'
#' lResult <- Analyze_Survival(
#'   dfFrame, "AVAL", "Level",
#'   strCensorCol = "CNSR", chrGroups = c("High", "Low")
#' )
#' lResult$p_value
#' lResult$estimates
#' lResult$rows[c("group", "n", "events", "median")]
#'
#' @family statistics
#' @export
Analyze_Survival <- Analyze_Survival

#' Screen many biomarkers with one comparison
#'
#' Runs one comparison, chosen once, on every biomarker and returns one row
#' per biomarker: a unit-free estimate with its interval, a p-value, and that
#' p-value adjusted across the rows.
#'
#' The data stay one row per participant, with one column per biomarker.
#' Each row of the screen is the matching single function's answer for that
#' biomarker, so its unadjusted p-value is the one the single chart prints:
#'
#' | `strComparison` | Estimate | p-value from | Needs |
#' |---|---|---|---|
#' | `"difference"` | The standardised difference between two groups. | [Analyze_GroupDifference()], the Welch t-test. | `strGroupCol` |
#' | `"correlation"` | The correlation with one fixed variable. | [Analyze_Correlation()]. | `strWithCol` |
#' | `"hazard"` | The hazard ratio, high against low. | [Analyze_Survival()], the log-rank test. | `strTimeCol` and a flag column |
#'
#' The adjustment is [stats::p.adjust()] across the rows that have a p-value.
#' A biomarker that could not be computed has its own `status` and `reason`,
#' has no p-value, and is left out of the adjustment; `notes` says how many
#' rows the adjustment covered, and each adjusted row carries that number as
#' `adjusted_over`. The default is Benjamini-Hochberg, which controls the
#' share of false leads among the rows picked out, the usual aim of a screen;
#' Holm, which guards against any false lead at all, is stricter.
#'
#' The rows come back in the order the columns were named. Sorting is the
#' caller's.
#'
#' @section The standardised difference:
#' This is the one statistic the package computes itself rather than handing
#' to an existing function, to avoid a heavy dependency. It is Hedges' g: the
#' difference in means, first group minus second, divided by the pooled
#' standard deviation, times the exact small-sample correction. Its interval
#' is the noncentral t interval for the two-sample t statistic with pooled
#' variance, put on the same scale. It agrees with `effectsize::hedges_g()`,
#' and the package's tests check that it does.
#'
#' Its p-value is not computed from it: it is [stats::t.test()]'s, Welch,
#' which does not assume the equal variances that the pooled standard
#' deviation does.
#'
#' @section High against low:
#' For `"hazard"`, each biomarker is split at its median among the
#' participants who can be used, those with a value, a time and a flag. A
#' value above the median is high and a value on the median or below it is
#' low. The hazard ratio is high over low, its interval is the Cox model's and
#' the p-value is the log-rank test's. Other cuts are not offered here.
#'
#' @inheritParams Analyze_Survival
#' @param chrCols `character` Names of the numeric biomarker columns, one row
#'   of the screen each.
#' @param strComparison `character` The comparison: `"difference"`,
#'   `"correlation"` or `"hazard"`. Default: `"difference"`.
#' @param strGroupCol `character` For `"difference"`: name of the column
#'   holding each participant's group. Default: `NULL`.
#' @param chrGroups `character` For `"difference"`: the two groups, in order;
#'   the difference is the first minus the second. Default: `NULL`, the two
#'   groups present, in sorted order.
#' @param strWithCol `character` For `"correlation"`: name of the numeric
#'   column every biomarker is correlated with. Default: `NULL`.
#' @param strCorMethod `character` For `"correlation"`: `"pearson"` or
#'   `"spearman"`. Default: `"pearson"`.
#' @param strTimeCol `character` For `"hazard"`: name of the numeric column
#'   holding the time to the event or to censoring. Default: `NULL`.
#' @param strPAdjust `character` The adjustment across the rows, one of
#'   [stats::p.adjust.methods]. Default: `"BH"`.
#' @param nMinGroup `numeric` The smallest group, or number of complete pairs,
#'   a row is computed for. A biomarker below it has `status` `"too_small"`
#'   in its row and no numbers. Default: `nMinGroupDefault`, which is 5. See
#'   [StatisticsResult].
#'
#' @return The fixed result described in [StatisticsResult]. Here `test` is
#'   the comparison; `p_value` is `NA` and `estimates` and `statistic` have no
#'   rows; `counts` is a named list of biomarker to participants used; and
#'   `rows` has one row per biomarker, with the columns `biomarker`, `counts`,
#'   `n_1` and `n_2` (the two groups, or high and low), `events`, `dropped`,
#'   `estimate`, `lower`, `upper`, `level`, `method`, `statistic`,
#'   `p_unadjusted`, `p_value` (adjusted), `adjustment`, `adjusted_over`,
#'   `status`, `reason` and `warning`. The result's own `status` is `"ok"`
#'   when any row is.
#'
#' @examples
#' # Every biomarker's change from Baseline to Week 4, Treatment against Placebo
#' dfFrame <- Synthetic_Participants
#' chrBiomarkers <- unique(Synthetic_Results$TEST)
#' for (strBiomarker in chrBiomarkers) {
#'   dfOne <- Synthetic_Results[Synthetic_Results$TEST == strBiomarker, ]
#'   dfBaseline <- dfOne[dfOne$VISIT == "Baseline", ]
#'   dfWeek4 <- dfOne[dfOne$VISIT == "Week 4", ]
#'   dfFrame[[strBiomarker]] <- dfWeek4$STRESN[match(dfFrame$USUBJID, dfWeek4$USUBJID)] -
#'     dfBaseline$STRESN[match(dfFrame$USUBJID, dfBaseline$USUBJID)]
#' }
#'
#' lResult <- Analyze_Screen(
#'   dfFrame, chrBiomarkers, "difference",
#'   strGroupCol = "ARM", chrGroups = c("Treatment", "Placebo")
#' )
#' dfRows <- lResult$rows[order(lResult$rows$p_value), ]
#' head(dfRows[c("biomarker", "estimate", "lower", "upper", "p_unadjusted", "p_value")], 3)
#'
#' @family statistics
#' @export
Analyze_Screen <- Analyze_Screen
