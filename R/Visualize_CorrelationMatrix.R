#' Correlation Matrix Figure
#'
#' A static ggplot2 figure of the grid the correlation matrix opens on: one
#' cell per pair of variables, coloured by R's coefficient and labelled with it,
#' across biomarkers at one visit or across the visits of one biomarker. It is
#' the grid [Widget_CorrelationMatrix()] draws in the browser, from the same
#' settings, and its coefficients are [Analyze_CorrelationMatrix()]'s on the same
#' frame. A cell R did not compute, too few pairs among them, is left blank.
#'
#' @section Titles and footnotes:
#' The settings `title`, `subtitle` and `footnotes` are text with named
#' placeholders, as for [Visualize_GroupComparison()]. This figure fills
#' `{heading}` (what the grid is of), `{variables}` (how many), `{visit}`,
#' `{value}`, `{n}` (the participants in the frame), `{filters}`, `{date}` and
#' `{version}`.
#'
#' @inheritParams Widget_CorrelationMatrix
#' @param lSettings `list` bio.viz correlation matrix settings, as
#'   [Widget_CorrelationMatrix()] takes them, and `title`, `subtitle` and
#'   `footnotes`. Default: `list()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   Visualize_CorrelationMatrix(
#'     Synthetic_Results,
#'     Synthetic_Participants,
#'     lSettings = list(visit = "Baseline", title = "{heading}")
#'   )
#' }
#'
#' @seealso [Widget_CorrelationMatrix()], its interactive twin.
#' @family figures
#' @export
Visualize_CorrelationMatrix <- function(dfResults, dfParticipants = NULL, lSettings = list()) {
  lTitles <- Figure_Inputs("Visualize_CorrelationMatrix", dfResults, dfParticipants, lSettings)
  lConfig <- CorrelationMatrix_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- CorrelationMatrix_State(dfResults, dfParticipants, lConfig)
  lVariables <- CorrelationMatrix_Variables(dfResults, lConfig, lState)
  lRequests <- CorrelationMatrix_Requests(dfResults, dfParticipants, lConfig, lState)
  if (length(lRequests) == 0L) {
    Core_Stop("Visualize_CorrelationMatrix(): there is no grid to draw at these settings: it needs two variables or more, with values")
  }
  lAnswer <- Chart_Answer(lRequests, list(Analyze_CorrelationMatrix = Analyze_CorrelationMatrix))[[1]]
  lValue <- lAnswer$value

  # Each variable as the grid names it: a biomarker, or a visit of one.
  chrLabels <- vapply(lVariables, function(lAxis) if (lState$mode == "visits") lAxis$visit else lAxis$measure, character(1))
  names(chrLabels) <- paste0("v", seq_along(lVariables))
  dfRows <- lValue$rows
  bShown <- dfRows$status == "ok" & !is.na(dfRows$estimate)
  dfPairs <- data.frame(
    row = chrLabels[dfRows$x], col = chrLabels[dfRows$y], estimate = ifelse(bShown, dfRows$estimate, NA_real_),
    stringsAsFactors = FALSE
  )
  # Above the diagonal a pair is its coefficient, to two decimals, as the grid
  # writes it; below it, the same pair as a colour alone.
  dfCells <- rbind(
    data.frame(x = dfPairs$col, y = dfPairs$row, estimate = dfPairs$estimate, label = ifelse(bShown, Output_Coefficient(dfPairs$estimate), ""), stringsAsFactors = FALSE),
    data.frame(x = dfPairs$row, y = dfPairs$col, estimate = dfPairs$estimate, label = "", stringsAsFactors = FALSE)
  )
  dfCells$x <- factor(dfCells$x, levels = chrLabels)
  dfCells$y <- factor(dfCells$y, levels = rev(chrLabels))
  rownames(dfCells) <- NULL

  strValue <- lState$value_type
  strAt <- if (strValue == "baseline" || lState$mode == "visits") "" else paste0(" at ", lState$visit)
  chrValueWords <- c(raw = "Result", baseline = "Baseline value", chrOutputValueLabels[c("change", "fold_change", "percent_change")])
  strHeading <- if (lState$mode == "visits") {
    paste0(lState$measure, ": ", tolower(chrValueWords[[strValue]]), ", visit against visit")
  } else {
    paste0(chrValueWords[[strValue]], strAt, ", biomarker against biomarker")
  }
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      heading = strHeading, variables = length(lVariables), visit = if (is.null(lState$visit)) "" else lState$visit,
      value = chrOutputValueLabels[[strValue]], n = lAnswer$rows
    )
  )
  lFilled <- Output_Titles(lTitles, lValues, list(lValue), strOf = "variables")
  # As the grid's statistics line says it.
  chrStatistics <- paste0(
    lValue$method, ", pair by pair: ", sum(bShown), " pair", if (sum(bShown) == 1L) "" else "s", " of ", length(lVariables),
    " variables, each on the participants who have both of its values."
  )

  gg <- ggplot2::ggplot(dfCells, Figure_Aes(x = "x", y = "y", fill = "estimate")) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(Figure_Aes(label = "label"), size = 2.8) +
    ggplot2::scale_fill_gradient2(
      low = "#2563eb", mid = "#f7f7f7", high = "#dc2626", midpoint = 0, limits = c(-1, 1), na.value = "white",
      name = "Coefficient"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  gg <- Figure_Finish(gg, lFilled, chrStatistics)
  gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "right")
}
