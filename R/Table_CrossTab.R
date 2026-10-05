#' Cross-Tabulation Table
#'
#' R's test of the table the cross-tabulation opens on, as a table of one row:
#' its method, the counts, the p-value and its note, written as the chart
#' prints them. The numbers are [Analyze_Contingency()]'s on the rows the chart
#' tabulates, by the test the settings open on.
#'
#' @inheritSection Table_GroupComparison Display rules
#'
#' @section Titles and footnotes:
#' As for `Visualize_CrossTab()`: `{rows}`, `{columns}`, `{n}`, `{filters}`,
#' `{date}` and `{version}`.
#'
#' @inheritParams Widget_CrossTab
#' @param lSettings `list` bio.viz cross-tabulation settings, as
#'   [Widget_CrossTab()] takes them, and `title`, `subtitle` and `footnotes`.
#'   Default: `list()`.
#'
#' @return A `data.frame` of text: `Statistic`, `Method`, `Estimate`, `Counts`,
#'   `p-value` and `Note`, with the attributes of [Table_GroupComparison()].
#'
#' @examples
#' Table_CrossTab(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(row_by = "ARM", col_by = "RESPONSE")
#' )
#'
#' @seealso `Visualize_CrossTab()` and [Widget_CrossTab()].
#' @family tables
#' @export
Table_CrossTab <- function(dfResults, dfParticipants = NULL, lSettings = list()) {
  lTitles <- Table_Inputs(dfResults, dfParticipants, lSettings)
  lConfig <- CrossTab_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- CrossTab_State(dfResults, dfParticipants, lConfig)
  Chart_StopIfNobody("Table_CrossTab", dfResults, dfParticipants, lConfig, lState$filters)
  lRequests <- CrossTab_Requests(dfResults, dfParticipants, lConfig, lState)
  if (length(lRequests) == 0L) {
    lTable <- CrossTab_Table(dfResults, dfParticipants, lConfig, lState)
    strWhy <- if (is.null(lConfig$statistic)) {
      "the setting 'statistic' is NULL, which asks R for no test"
    } else if (identical(lState$test, "none")) {
      "the setting test = 'none' asks R for no test"
    } else if (identical(lTable$filtered, 0L)) {
      "no participant passes the filters"
    } else if (lTable$total == 0L) {
      "no participant has a category both ways"
    } else if (length(lTable$row_levels) < 2L) {
      "the rows have only one category, and a test of a table needs two or more each way"
    } else {
      "the columns have only one category, and a test of a table needs two or more each way"
    }
    Core_Stop("Table_CrossTab() has no statistic to show: ", strWhy)
  }
  lAnswers <- Chart_Answer(lRequests, list(Analyze_Contingency = Analyze_Contingency))
  dfRows <- do.call(rbind, lapply(lAnswers, function(lResult) Table_Row(CrossTab_Oriented(lResult), "Test of the table")))
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      rows = Output_GroupLabel(lState$row_by, lConfig), columns = Output_GroupLabel(lState$col_by, lConfig),
      n = lAnswers[[1]]$rows
    )
  )
  Table_Make(dfRows, lTitles, lValues, lapply(lAnswers, `[[`, "value"), seq_along(lAnswers))
}
