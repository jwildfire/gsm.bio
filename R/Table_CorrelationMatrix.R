#' Correlation Matrix Table
#'
#' The coefficients of the grid the correlation matrix opens on, as a table:
#' one row per pair of variables, with R's coefficient and its interval and the
#' pair's own count. A matrix reports no p-values, so none is written. The
#' numbers are [Analyze_CorrelationMatrix()]'s on the frame the chart hands R.
#'
#' @inheritSection Table_GroupComparison Display rules
#'
#' @section Titles and footnotes:
#' As for `Visualize_CorrelationMatrix()`: `{heading}`, `{variables}`,
#' `{visit}`, `{value}`, `{n}`, `{filters}`, `{date}` and `{version}`.
#'
#' @inheritParams Widget_CorrelationMatrix
#' @param lSettings `list` bio.viz correlation matrix settings, as
#'   [Widget_CorrelationMatrix()] takes them, and `title`, `subtitle` and
#'   `footnotes`. Default: `list()`.
#'
#' @return A `data.frame` of text: `Variable 1` and `Variable 2`, then
#'   `Statistic`, `Method`, `Estimate`, `Counts`, `p-value` and `Note`, with the
#'   attributes of [Table_GroupComparison()].
#'
#' @examples
#' Table_CorrelationMatrix(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(visit = "Baseline")
#' )
#'
#' @seealso `Visualize_CorrelationMatrix()` and [Widget_CorrelationMatrix()].
#' @family tables
#' @export
Table_CorrelationMatrix <- function(dfResults, dfParticipants = NULL, lSettings = list()) {
  lTitles <- Table_Inputs(dfResults, dfParticipants, lSettings)
  lConfig <- CorrelationMatrix_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- CorrelationMatrix_State(dfResults, dfParticipants, lConfig)
  lVariables <- CorrelationMatrix_Variables(dfResults, lConfig, lState)
  lRequests <- CorrelationMatrix_Requests(dfResults, dfParticipants, lConfig, lState)
  if (length(lRequests) == 0L) {
    Core_Stop("Table_CorrelationMatrix(): there is no grid at these settings: it needs two variables or more, with values")
  }
  lAnswer <- Chart_Answer(lRequests, list(Analyze_CorrelationMatrix = Analyze_CorrelationMatrix))[[1]]
  lValue <- lAnswer$value
  chrLabels <- vapply(lVariables, function(lAxis) if (lState$mode == "visits") lAxis$visit else lAxis$measure, character(1))
  names(chrLabels) <- paste0("v", seq_along(lVariables))
  dfPairs <- lValue$rows
  strNote <- "No p-value: a matrix reports each coefficient, its interval and its pair count."
  dfRows <- do.call(rbind, lapply(seq_len(nrow(dfPairs)), function(iPair) {
    lPair <- as.list(dfPairs[iPair, ])
    bShown <- identical(lPair$status, "ok") && !is.na(lPair$estimate)
    data.frame(
      `Variable 1` = chrLabels[[lPair$x]], `Variable 2` = chrLabels[[lPair$y]],
      Statistic = "Correlation", Method = lValue$method,
      Estimate = if (bShown) Table_Estimate(c(list(name = "Coefficient", group = NA), lPair[c("estimate", "lower", "upper", "level")]), FALSE) else "",
      Counts = paste("n =", lPair$counts), `p-value` = "",
      Note = if (bShown) strNote else if (is.na(lPair$reason)) "Not computed." else lPair$reason,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }))
  strValue <- lState$value_type
  chrValueWords <- c(raw = "Result", baseline = "Baseline value", chrOutputValueLabels[c("change", "fold_change", "percent_change")])
  strHeading <- if (lState$mode == "visits") {
    paste0(lState$measure, ": ", tolower(chrValueWords[[strValue]]), ", visit against visit")
  } else {
    paste0(chrValueWords[[strValue]], if (strValue == "baseline") "" else paste0(" at ", lState$visit), ", biomarker against biomarker")
  }
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      heading = strHeading, variables = length(lVariables), visit = if (is.null(lState$visit)) "" else lState$visit,
      value = chrOutputValueLabels[[strValue]], n = lAnswer$rows
    )
  )
  Table_Make(dfRows, lTitles, lValues, list(lValue), rep(1L, nrow(dfRows)), strOf = "variables")
}
