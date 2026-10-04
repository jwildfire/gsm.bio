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
  lRequests <- CrossTab_Requests(dfResults, dfParticipants, lConfig, lState)
  if (length(lRequests) == 0L) {
    Core_Stop("Table_CrossTab(): the chart tests nothing at these settings")
  }
  lAnswers <- Chart_Answer(lRequests, list(Analyze_Contingency = Analyze_Contingency))
  dfRows <- do.call(rbind, lapply(lAnswers, function(lResult) Table_Row(lResult$value, "Test of the table")))
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      rows = Output_GroupLabel(lState$row_by, lConfig), columns = Output_GroupLabel(lState$col_by, lConfig),
      n = lAnswers[[1]]$rows
    )
  )
  Table_Make(dfRows, lTitles, lValues, lapply(lAnswers, `[[`, "value"), seq_along(lAnswers))
}
