#' Association Scatter Table
#'
#' The statistics of the view the association scatter opens on, as a table:
#' for each panel, R's correlation coefficient and, when `fit` asks for one, its
#' fitted line, each a row with its method, estimates and intervals, counts,
#' p-value and note, written as the chart prints them. The numbers are
#' [Analyze_Correlation()]'s and [Analyze_Fit()]'s on the rows the chart draws.
#'
#' @inheritSection Table_GroupComparison Display rules
#'
#' @section Titles and footnotes:
#' As for `Visualize_AssociationScatter()`: `{x}`, `{y}`, `{n}`, `{filters}`,
#' `{date}` and `{version}`.
#'
#' @inheritParams Widget_AssociationScatter
#' @param lSettings `list` bio.viz association scatter settings, as
#'   [Widget_AssociationScatter()] takes them, and `title`, `subtitle` and
#'   `footnotes`. Default: `list()`.
#'
#' @return A `data.frame` of text: `Panel` with `panel_by`, then `Statistic`,
#'   `Method`, `Estimate`, `Counts`, `p-value` and `Note`, with the attributes
#'   of [Table_GroupComparison()].
#'
#' @examples
#' Table_AssociationScatter(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     x = list(measure = "TNF-alpha", visit = "Baseline"),
#'     y = list(measure = "IL-10", visit = "Baseline"),
#'     fit = "linear"
#'   )
#' )
#'
#' @seealso `Visualize_AssociationScatter()` and [Widget_AssociationScatter()].
#' @family tables
#' @export
Table_AssociationScatter <- function(dfResults, dfParticipants = NULL, lSettings = list()) {
  lTitles <- Table_Inputs(dfResults, dfParticipants, lSettings)
  lConfig <- AssociationScatter_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- AssociationScatter_State(dfResults, dfParticipants, lConfig)
  if (is.null(lState$x) || is.null(lState$y)) {
    Core_Stop("Table_AssociationScatter(): the tables have no two variables to relate")
  }
  lRequests <- AssociationScatter_Requests(dfResults, dfParticipants, lConfig, lState)
  if (length(lRequests) == 0L) {
    Core_Stop("Table_AssociationScatter(): the chart asks R nothing at these settings")
  }
  lAnswers <- Chart_Answer(lRequests, list(Analyze_Correlation = Analyze_Correlation, Analyze_Fit = Analyze_Fit))
  # A coefficient is named as the chart names it.
  chrNames <- c(cor = paste0("Pearson", intToUtf8(0x2019L), "s r"), rho = paste0("Spearman", intToUtf8(0x2019L), "s rho"))
  bPanels <- !is.null(lState$panel_by)
  dfRows <- do.call(rbind, lapply(lAnswers, function(lResult) {
    dfRow <- Table_Row(lResult$value, if (lResult$name == "Analyze_Fit") "Fitted line" else "Correlation", chrNames)
    if (bPanels) cbind(data.frame(Panel = lResult$dataId$panel, stringsAsFactors = FALSE), dfRow) else dfRow
  }))
  lNumbers <- lapply(Chart_Numbers(dfResults, dfParticipants, lConfig), function(strCol) {
    lFound <- Filter(function(lSpec) identical(lSpec$value_col, strCol), lConfig$numbers)
    if (length(lFound) > 0L) lFound[[1]] else list(value_col = strCol, label = strCol)
  })
  nDrawn <- sum(vapply(Filter(function(lResult) lResult$name == "Analyze_Correlation", lAnswers), `[[`, integer(1), "rows"))
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      x = Output_AxisTitle(dfResults, lConfig, lState$x, lNumbers), y = Output_AxisTitle(dfResults, lConfig, lState$y, lNumbers),
      n = nDrawn
    )
  )
  Table_Make(dfRows, lTitles, lValues, lapply(lAnswers, `[[`, "value"), seq_along(lAnswers))
}
