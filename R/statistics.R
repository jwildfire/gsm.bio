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
#' | `rows` | A data frame for a function's many-row results: pairwise comparisons, per-group correlations, the pairs of a matrix, the cells of a table. Its columns are given on each function's page. No rows when there are none. |
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
#' runs as it is in a bare R session with only the stats package attached.
#'
#' @name StatisticsResult
#' @aliases statistics-result
#' @seealso [Analyze_GroupDifference()], [Analyze_Correlation()],
#'   [Analyze_CorrelationMatrix()], [Analyze_Contingency()]
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
