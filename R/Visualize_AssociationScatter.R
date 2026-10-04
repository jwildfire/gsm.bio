#' Association Scatter Figure
#'
#' A static ggplot2 figure of the view the association scatter opens on: one
#' point per participant with a variable on each axis, a panel per level of
#' `panel_by`, R's fitted line and its band when `fit` asks for one, and R's
#' correlation coefficient of each panel printed under the figure. It is the
#' figure [Widget_AssociationScatter()] draws in the browser, from the same
#' settings; the coefficient is [Analyze_Correlation()]'s and the line
#' [Analyze_Fit()]'s, on the same rows.
#'
#' @section Titles and footnotes:
#' The settings `title`, `subtitle` and `footnotes` are text with named
#' placeholders, as for [Visualize_GroupComparison()]. This figure fills `{x}`
#' and `{y}` (the axes, as their titles read), `{n}` (the participants drawn),
#' `{filters}`, `{date}` and `{version}`.
#'
#' @inheritParams Widget_AssociationScatter
#' @param lSettings `list` bio.viz association scatter settings, as
#'   [Widget_AssociationScatter()] takes them, and `title`, `subtitle` and
#'   `footnotes`. Default: `list()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   Visualize_AssociationScatter(
#'     Synthetic_Results,
#'     Synthetic_Participants,
#'     lSettings = list(
#'       x = list(measure = "TNF-alpha", visit = "Baseline"),
#'       y = list(measure = "IL-10", visit = "Baseline"),
#'       fit = "linear",
#'       title = "{y} against {x}"
#'     )
#'   )
#' }
#'
#' @seealso [Widget_AssociationScatter()], its interactive twin.
#' @family figures
#' @export
Visualize_AssociationScatter <- function(dfResults, dfParticipants = NULL, lSettings = list()) {
  lTitles <- Figure_Inputs("Visualize_AssociationScatter", dfResults, dfParticipants, lSettings)
  lConfig <- AssociationScatter_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- AssociationScatter_State(dfResults, dfParticipants, lConfig)
  if (is.null(lState$x) || is.null(lState$y)) {
    Core_Stop("Visualize_AssociationScatter(): the tables have no two variables to draw against each other")
  }
  lPanels <- AssociationScatter_Panels(dfResults, dfParticipants, lConfig, lState)
  if (length(lPanels) == 0L) {
    Core_Stop("Visualize_AssociationScatter(): no participant has both variables to draw at these settings")
  }
  lAnswers <- Chart_Answer(
    AssociationScatter_Requests(dfResults, dfParticipants, lConfig, lState),
    list(Analyze_Correlation = Analyze_Correlation, Analyze_Fit = Analyze_Fit)
  )
  PanelOf <- function(lResult) if (is.null(lResult$dataId$panel)) "" else lResult$dataId$panel

  dfPoints <- do.call(rbind, lapply(lPanels, function(lPanel) {
    data.frame(
      id = Core_Text(lPanel$records[[lConfig$id_col]]), x = lPanel$records$x, y = lPanel$records$y,
      colour = if (is.null(lState$color_by)) "All" else Core_Text(lPanel$records$color),
      panel = if (is.null(lPanel$panel)) "" else lPanel$panel, stringsAsFactors = FALSE
    )
  }))
  # R's line, on the scale it was computed on, drawn back on the axes'.
  Back <- function(nValue, strScale) if (strScale == "log") 10^nValue else nValue
  dfLines <- do.call(rbind, lapply(Filter(function(lResult) lResult$name == "Analyze_Fit", lAnswers), function(lResult) {
    dfRows <- lResult$value$rows
    if (is.null(dfRows) || nrow(dfRows) == 0L) {
      return(NULL)
    }
    data.frame(
      x = Back(dfRows$x, lState$x_scale), fit = Back(dfRows$fit, lState$y_scale),
      lower = Back(dfRows$lower, lState$y_scale), upper = Back(dfRows$upper, lState$y_scale),
      colour = if (is.null(lState$color_by)) "All" else Core_Text(dfRows$group),
      panel = PanelOf(lResult), stringsAsFactors = FALSE
    )
  }))

  # Each panel's coefficient and line, as the statistics line prints them: R's
  # sentence, then each estimate, a coefficient by the name the chart gives it.
  chrCoefficients <- enc2utf8(c(cor = "Pearson\u2019s r", rho = "Spearman\u2019s rho"))
  chrStatistics <- unlist(lapply(lAnswers, function(lResult) {
    strLead <- if (is.null(lResult$dataId$panel)) "" else paste0(lResult$dataId$panel, ": ")
    lValue <- lResult$value
    chrEstimates <- if (identical(lValue$status, "ok") && nrow(lValue$estimates) > 0L) {
      vapply(seq_len(nrow(lValue$estimates)), function(iRow) {
        lRow <- as.list(lValue$estimates[iRow, ])
        if (lRow$name %in% names(chrCoefficients)) lRow$name <- chrCoefficients[[lRow$name]]
        Output_EstimateText(lRow)
      }, character(1))
    } else {
      character(0)
    }
    paste0(strLead, paste(c(Output_StatisticText(lValue), chrEstimates), collapse = " "))
  }))

  chrColours <- Core_Levels(dfPoints$colour)
  dfPoints$colour <- factor(dfPoints$colour, levels = chrColours)
  lNumbers <- lapply(Chart_Numbers(dfResults, dfParticipants, lConfig), function(strCol) {
    lFound <- Filter(function(lSpec) identical(lSpec$value_col, strCol), lConfig$numbers)
    if (length(lFound) > 0L) lFound[[1]] else list(value_col = strCol, label = strCol)
  })
  strX <- Output_AxisTitle(dfResults, lConfig, lState$x, lNumbers)
  strY <- Output_AxisTitle(dfResults, lConfig, lState$y, lNumbers)
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(x = strX, y = strY, n = nrow(dfPoints))
  )
  lFilled <- Output_Titles(lTitles, lValues, lapply(lAnswers, `[[`, "value"))

  gg <- ggplot2::ggplot(dfPoints, Figure_Aes(x = "x", y = "y", colour = "colour"))
  if (!is.null(dfLines)) {
    dfLines$colour <- factor(dfLines$colour, levels = chrColours)
    gg <- gg +
      ggplot2::geom_ribbon(
        data = dfLines, Figure_Aes(x = "x", ymin = "lower", ymax = "upper", fill = "colour"),
        inherit.aes = FALSE, alpha = 0.15
      ) +
      ggplot2::geom_line(data = dfLines, Figure_Aes(x = "x", y = "fit", colour = "colour"), inherit.aes = FALSE, linewidth = 0.8)
  }
  gg <- gg +
    ggplot2::geom_point(alpha = 0.6, size = 1.4) +
    ggplot2::scale_colour_manual(values = Figure_Palette(length(chrColours)), guide = if (is.null(lState$color_by)) "none" else "legend") +
    ggplot2::scale_fill_manual(values = Figure_Palette(length(chrColours)), guide = "none") +
    ggplot2::labs(x = strX, y = strY, colour = if (is.null(lState$color_by)) NULL else Output_GroupLabel(lState$color_by, lConfig))
  if (!is.null(lState$panel_by)) {
    gg <- gg + ggplot2::facet_wrap("panel")
  }
  if (lState$x_scale == "log") gg <- gg + ggplot2::scale_x_log10()
  if (lState$y_scale == "log") gg <- gg + ggplot2::scale_y_log10()
  Figure_Finish(gg, lFilled, chrStatistics)
}
