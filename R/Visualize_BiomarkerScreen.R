#' Biomarker Screen Figure
#'
#' A static ggplot2 figure of the screen the biomarker screen opens on: one row
#' per biomarker, R's estimate and its interval on one shared axis, with the
#' estimate, the unadjusted p-value and the adjusted one printed beside each
#' row's name. The rows run from the largest estimate down, as the screen sorts
#' them by default. It is the screen [Widget_BiomarkerScreen()] draws in the
#' browser, from the same settings, and its rows are [Analyze_Screen()]'s on the
#' same frame. A hazard ratio is drawn on a logarithmic axis, with 1 marked.
#'
#' @section Titles and footnotes:
#' The settings `title`, `subtitle` and `footnotes` are text with named
#' placeholders, as for [Visualize_GroupComparison()]. This figure fills
#' `{heading}` (what the rows are), `{comparison}`, `{visit}`, `{endpoint}` (a
#' hazard ratio's endpoint), `{biomarkers}` (how many are screened), `{n}` (the
#' participants in the frame), `{filters}`, `{date}` and `{version}`.
#'
#' @inheritParams Widget_BiomarkerScreen
#' @param lSettings `list` bio.viz biomarker screen settings, as
#'   [Widget_BiomarkerScreen()] takes them, and `title`, `subtitle` and
#'   `footnotes`. Default: `list()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   Visualize_BiomarkerScreen(
#'     Synthetic_Results,
#'     Synthetic_Participants,
#'     lSettings = list(visit = "Week 4", value_type = "change", group_by = "ARM", title = "{heading}")
#'   )
#' }
#'
#' @seealso [Widget_BiomarkerScreen()], its interactive twin.
#' @family figures
#' @export
Visualize_BiomarkerScreen <- function(dfResults, dfParticipants = NULL, lSettings = list(), dfOutcomes = NULL) {
  lTitles <- Figure_Inputs("Visualize_BiomarkerScreen", dfResults, dfParticipants, lSettings, dfOutcomes)
  lConfig <- BiomarkerScreen_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  Widget_CheckOutcomes(dfOutcomes, lConfig)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- BiomarkerScreen_State(dfResults, dfParticipants, lConfig, dfOutcomes)
  lRequests <- BiomarkerScreen_Requests(dfResults, dfParticipants, lConfig, lState, dfOutcomes)
  if (length(lRequests) == 0L) {
    Core_Stop("Visualize_BiomarkerScreen(): there is no screen to draw at these settings")
  }
  lAnswer <- Chart_Answer(lRequests, list(Analyze_Screen = Analyze_Screen))[[1]]
  lValue <- lAnswer$value
  dfRows <- lValue$rows
  bHazard <- lState$comparison == "hazard"

  # Each row as the screen prints it: the estimate and its interval, and both
  # p-values, or R's reason for none.
  strAdjustment <- if (lState$adjustment %in% names(chrOutputAdjustments)) chrOutputAdjustments[[lState$adjustment]] else lState$adjustment
  chrSaid <- vapply(seq_len(nrow(dfRows)), function(iRow) {
    lRow <- as.list(dfRows[iRow, ])
    if (!identical(lRow$status, "ok") || is.na(lRow$estimate)) {
      return(if (is.na(lRow$reason)) "not computed" else lRow$reason)
    }
    strInterval <- if (is.na(lRow$lower) || is.na(lRow$upper)) "" else paste0(" (", Output_Figure(lRow$lower), " to ", Output_Figure(lRow$upper), ")")
    paste0(
      Output_Figure(lRow$estimate), strInterval, "; ", Output_P(lRow$p_unadjusted),
      if (is.na(lRow$p_value)) "" else paste0(", ", strAdjustment, " ", Output_P(lRow$p_value))
    )
  }, character(1))
  dfDrawn <- data.frame(
    biomarker = dfRows$biomarker, estimate = dfRows$estimate, lower = dfRows$lower, upper = dfRows$upper,
    said = chrSaid, stringsAsFactors = FALSE
  )
  dfDrawn$row <- paste0(dfDrawn$biomarker, "   ", dfDrawn$said)
  # Largest first, from the top; a row with no estimate last.
  iOrder <- order(is.na(dfDrawn$estimate), -dfDrawn$estimate, seq_len(nrow(dfDrawn)))
  dfDrawn$row <- factor(dfDrawn$row, levels = rev(dfDrawn$row[iOrder]))

  strValue <- lState$value_type
  chrValueWords <- c(raw = "Result", baseline = "Baseline value", chrOutputValueLabels[c("change", "fold_change", "percent_change")])
  strWords <- paste0(chrValueWords[[strValue]], if (strValue == "baseline") "" else paste0(" at ", lState$visit))
  dfEndpoints <- Chart_Endpoints(dfOutcomes, lConfig)
  strEndpoint <- if (bHazard) dfEndpoints$label[match(lState$endpoint, dfEndpoints$endpoint)] else ""
  strHeading <- if (lState$comparison == "difference") {
    paste0(strWords, ": ", if (length(lState$levels) == 2L) paste(lState$levels[1], "against", lState$levels[2]) else "two groups", ", standardised difference")
  } else if (bHazard) {
    paste0(strWords, ": hazard ratio, high against low, on ", strEndpoint)
  } else {
    paste0(strWords, ": correlation with ", BiomarkerScreen_VariableName(lState$with))
  }
  chrComparisons <- c(
    difference = "Difference between two groups", correlation = "Correlation with one variable",
    hazard = "Hazard ratio, high against low"
  )
  chrEstimates <- c(
    difference = enc2utf8("Standardised difference (Hedges\u2019 g)"),
    correlation = enc2utf8(if (lState$method == "spearman") "Spearman\u2019s rho" else "Pearson\u2019s r"),
    hazard = "Hazard ratio, High / Low"
  )
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      heading = strHeading, comparison = chrComparisons[[lState$comparison]],
      visit = if (is.null(lState$visit)) "" else lState$visit, endpoint = strEndpoint,
      biomarkers = nrow(dfRows), n = lAnswer$rows
    )
  )
  lFilled <- Output_Titles(lTitles, lValues, list(lValue), strOf = "biomarkers")
  # As the screen's statistics line says it, and what each row's estimate is.
  nOver <- suppressWarnings(max(dfRows$adjusted_over, na.rm = TRUE))
  nShown <- sum(dfRows$status == "ok" & !is.na(dfRows$estimate))
  chrStatistics <- c(
    paste0(
      lValue$method, ", one row per biomarker: ", nShown, " of ", nrow(dfRows), " computed.",
      if (is.finite(nOver) && nOver > 0) paste0(" The adjusted p-values are adjusted by ", strAdjustment, " across the ", nOver, " biomarker", if (nOver == 1) "" else "s", " that have a p-value.") else ""
    ),
    paste0(
      "Each row: ", chrEstimates[[lState$comparison]],
      if (lState$comparison == "difference" && length(lState$levels) == 2L) paste0(", ", lState$levels[1], " less ", lState$levels[2]) else "",
      ", with its interval, and its p-value unadjusted and adjusted."
    )
  )

  gg <- ggplot2::ggplot(dfDrawn, Figure_Aes(x = "estimate", y = "row")) +
    ggplot2::geom_vline(xintercept = if (bHazard) 1 else 0, linetype = "dashed", colour = "#52616f") +
    ggplot2::geom_errorbar(Figure_Aes(xmin = "lower", xmax = "upper"), width = 0, orientation = "y", colour = "#2563eb", na.rm = TRUE) +
    ggplot2::geom_point(colour = "#2563eb", size = 2, na.rm = TRUE) +
    ggplot2::labs(x = chrEstimates[[lState$comparison]], y = NULL) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 0))
  if (bHazard) {
    gg <- gg + ggplot2::scale_x_log10()
  }
  Figure_Finish(gg, lFilled, chrStatistics) + ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 0))
}
