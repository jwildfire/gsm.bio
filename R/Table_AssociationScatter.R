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
  Chart_StopIfNobody("Table_AssociationScatter", dfResults, dfParticipants, lConfig, lState$filters)
  if (is.null(lState$x) || is.null(lState$y)) {
    Core_Stop("Table_AssociationScatter(): the tables have no two variables to relate")
  }
  lRequests <- AssociationScatter_Requests(dfResults, dfParticipants, lConfig, lState)
  if (length(lRequests) == 0L) {
    strWhy <- if (is.null(lConfig$statistic) && !lState$fit %in% chrAssociationScatterFits) {
      "the setting 'statistic' is NULL and there is no fitted line, which asks R for nothing"
    } else if (AssociationScatter_FlatAtBaseline(lState$x, dfResults, lConfig) || AssociationScatter_FlatAtBaseline(lState$y, dfResults, lConfig)) {
      "a change from baseline at the baseline visit is the same for everyone, so there is nothing to relate"
    } else {
      "no participant has both variables, after the filters"
    }
    Core_Stop("Table_AssociationScatter() has no statistic to show: ", strWhy)
  }
  lAnswers <- Chart_Answer(lRequests, list(Analyze_Correlation = Analyze_Correlation, Analyze_Fit = Analyze_Fit))
  # A coefficient is named as the chart names it.
  chrNames <- c(cor = paste0("Pearson", intToUtf8(0x2019L), "s r"), rho = paste0("Spearman", intToUtf8(0x2019L), "s rho"))
  bPanels <- !is.null(lState$panel_by)
  dfRows <- do.call(rbind, lapply(lAnswers, function(lResult) {
    lValue <- lResult$value
    # A line's estimates as the chart lists them: the slope, the intercept,
    # and R-squared, which R gives among its statistics.
    if (lResult$name == "Analyze_Fit" && is.data.frame(lValue$estimates) && nrow(lValue$estimates) > 0L) {
      dfEstimates <- lValue$estimates[order(match(lValue$estimates$name, c("Slope", "Intercept"))), , drop = FALSE]
      nRSquared <- lValue$statistic$value[lValue$statistic$name == "r.squared"]
      if (length(nRSquared) == 1L && !is.na(nRSquared)) {
        dfEstimates <- rbind(dfEstimates, data.frame(
          name = "R-squared", group = NA_character_, estimate = nRSquared, lower = NA_real_, upper = NA_real_, level = NA_real_,
          stringsAsFactors = FALSE
        ))
      }
      lValue$estimates <- dfEstimates
    }
    dfRow <- Table_Row(lValue, if (lResult$name == "Analyze_Fit") "Fitted line" else "Correlation", chrNames)
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
