#' Group Comparison Table
#'
#' The statistics of the view the group comparison chart opens on, as a table:
#' one row per panel the chart tests, with R's method, each estimate and its
#' interval, the counts, the p-value and its note, written as the chart prints
#' them. The numbers are [Analyze_GroupDifference()]'s on the rows the chart
#' draws in each panel, as [Widget_GroupComparison()] stores them; with
#' `pairwise = TRUE` each pair of groups has a row of its own beneath its panel.
#'
#' @section Display rules:
#' A p-value is written to three decimals, `p < 0.001` below that and
#' `p > 0.999` above, never with stars. Every row is labelled exploratory, with
#' its adjustment named when it has one. An estimate is written to four
#' significant digits with its confidence interval. A statistic R did not
#' compute has no p-value, and its note is R's reason.
#'
#' @section Titles and footnotes:
#' The settings `title`, `subtitle` and `footnotes` are text with named
#' placeholders, as for `Visualize_GroupComparison()`: `{measure}`, `{visits}`,
#' `{value}`, `{group}`, `{n}`, `{filters}`, `{date}` and `{version}`. They are
#' the table's attributes `title`, `subtitle` and `footnotes`, the last footnote
#' always the table's own.
#'
#' @inheritParams Widget_GroupComparison
#' @param lSettings `list` bio.viz group comparison settings, as
#'   [Widget_GroupComparison()] takes them, and `title`, `subtitle` and
#'   `footnotes`. A table is of one biomarker, so `start_value` must name it.
#'   Default: `list()`.
#'
#' @return A `data.frame` of text: `Visit` (and `Panel` with `panel_by`, and
#'   `Pair` with `pairwise`), then `Statistic`, `Method`, `Estimate`, `Counts`,
#'   `p-value` and `Note`. Its attributes are `title`, `subtitle`,
#'   `footnotes`, `results` (R's answers) and `result_of` (which answer each
#'   row is of). [Write_RTF()] writes it to RTF.
#'
#' @examples
#' Table_GroupComparison(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     start_value = "IL-6",
#'     visits = c("Week 4", "Week 8"),
#'     value_type = "change",
#'     baseline_visits = "Baseline",
#'     group_by = "ARM"
#'   )
#' )
#'
#' @seealso `Visualize_GroupComparison()` and [Widget_GroupComparison()], the
#'   same view as a figure and as a widget.
#' @family tables
#' @export
Table_GroupComparison <- function(dfResults, dfParticipants = NULL, lSettings = list()) {
  lTitles <- Table_Inputs(dfResults, dfParticipants, lSettings)
  lConfig <- GroupComparison_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- GroupComparison_State(dfResults, dfParticipants, lConfig)
  if (is.null(lState$measure)) {
    Core_Stop("Table_GroupComparison() is of one biomarker: name one the results table has with the setting 'start_value'")
  }
  lModel <- GroupComparison_Panels(dfResults, dfParticipants, lConfig, lState)
  lAnswers <- Chart_Answer(
    GroupComparison_Requests(dfResults, dfParticipants, lConfig, lState),
    list(Analyze_GroupDifference = Analyze_GroupDifference)
  )
  if (length(lAnswers) == 0L) {
    Core_Stop("Table_GroupComparison(): the chart tests nothing at these settings")
  }
  bPanels <- !is.null(lState$panel_by)
  lRows <- list()
  nResultOf <- integer(0)
  for (iAnswer in seq_along(lAnswers)) {
    lResult <- lAnswers[[iAnswer]]
    lValue <- lResult$value
    dfRow <- data.frame(Visit = if (is.null(lResult$dataId$visit)) "Baseline" else lResult$dataId$visit, stringsAsFactors = FALSE)
    if (bPanels) dfRow$Panel <- lResult$dataId$panel
    dfRow <- cbind(dfRow, Table_Row(lValue, "Test of the groups"))
    lRows[[length(lRows) + 1L]] <- dfRow
    nResultOf <- c(nResultOf, iAnswer)
    # Each pair, beneath its panel.
    dfPairs <- lValue$rows
    if (is.data.frame(dfPairs) && nrow(dfPairs) > 0L && all(c("group_1", "group_2") %in% names(dfPairs))) {
      for (iPair in seq_len(nrow(dfPairs))) {
        lPair <- as.list(dfPairs[iPair, ])
        lPairValue <- list(
          status = lPair$status, reason = lPair$reason, method = lPair$method, p_value = lPair$p_value,
          adjustment = lPair$adjustment, counts = stats::setNames(list(lPair$n_1, lPair$n_2), c(lPair$group_1, lPair$group_2)),
          estimates = data.frame(
            name = "Difference in means", group = paste(lPair$group_1, "-", lPair$group_2),
            estimate = lPair$estimate, lower = lPair$lower, upper = lPair$upper, level = lPair$level, stringsAsFactors = FALSE
          )
        )
        dfPairRow <- dfRow
        dfPairRow[names(Table_Row(lPairValue))] <- Table_Row(lPairValue, paste(lPair$group_1, "and", lPair$group_2))
        lRows[[length(lRows) + 1L]] <- dfPairRow
        nResultOf <- c(nResultOf, iAnswer)
      }
    }
  }
  dfRows <- do.call(rbind, lRows)

  chrVisits <- if (lState$value_type == "baseline") character(0) else unique(unlist(lapply(lModel$panels, `[[`, "visit")))
  chrIds <- unique(unlist(lapply(lModel$panels, function(lPanel) Core_Text(lPanel$records[[lConfig$id_col]]))))
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      measure = lState$measure, visits = paste(chrVisits, collapse = ", "),
      value = chrOutputValueLabels[[lState$value_type]], group = Output_GroupLabel(lState$group_by, lConfig),
      n = length(chrIds)
    )
  )
  Table_Make(dfRows, lTitles, lValues, lapply(lAnswers, `[[`, "value"), nResultOf)
}
