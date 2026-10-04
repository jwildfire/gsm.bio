#' Group Comparison Figure
#'
#' A static ggplot2 figure of the view the group comparison chart opens on: one
#' biomarker's value across the levels of a category, as boxes with the
#' participants as points, one panel per visit, with R's test of each panel
#' printed in its heading. It is the figure [Widget_GroupComparison()] draws in
#' the browser, from the same settings, and its tests are
#' [Analyze_GroupDifference()]'s on the same rows.
#'
#' @section Titles and footnotes:
#' The settings `title`, `subtitle` and `footnotes` are text with named
#' placeholders, as the chart takes them: a name in braces is replaced by the
#' text of its value, and a name the figure does not have is left as written.
#' This figure fills `{measure}`, `{visits}`, `{value}`, `{group}`, `{n}` (the
#' participants drawn), `{filters}`, `{date}` and `{version}`. The last line
#' under the figure is always its own: the date it was drawn, by gsm.bio, and
#' R's method and counts behind each test printed, with the R and gsm.bio
#' versions that computed them.
#'
#' @inheritParams Widget_GroupComparison
#' @param lSettings `list` bio.viz group comparison settings, as
#'   [Widget_GroupComparison()] takes them, and `title`, `subtitle` and
#'   `footnotes`. A figure is of one biomarker, so `start_value` must name it.
#'   Default: `list()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   Visualize_GroupComparison(
#'     Synthetic_Results,
#'     Synthetic_Participants,
#'     lSettings = list(
#'       start_value = "IL-6",
#'       visits = c("Week 4", "Week 8"),
#'       value_type = "change",
#'       baseline_visits = "Baseline",
#'       group_by = "ARM",
#'       title = "{measure}: {value} by {group}",
#'       subtitle = "{n} participants, at {visits}"
#'     )
#'   )
#' }
#'
#' @seealso [Widget_GroupComparison()], its interactive twin, and
#'   [Analyze_GroupDifference()], which computes the tests.
#' @family figures
#' @export
Visualize_GroupComparison <- function(dfResults, dfParticipants = NULL, lSettings = list()) {
  lTitles <- Figure_Inputs("Visualize_GroupComparison", dfResults, dfParticipants, lSettings)
  lConfig <- GroupComparison_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- GroupComparison_State(dfResults, dfParticipants, lConfig)
  Chart_StopIfNobody("Visualize_GroupComparison", dfResults, dfParticipants, lConfig, lState$filters)
  if (is.null(lState$measure)) {
    Core_Stop("Visualize_GroupComparison() draws one biomarker: name one the results table has with the setting 'start_value'")
  }
  lModel <- GroupComparison_Panels(dfResults, dfParticipants, lConfig, lState)
  lAnswers <- Chart_Answer(
    GroupComparison_Requests(dfResults, dfParticipants, lConfig, lState),
    list(Analyze_GroupDifference = Analyze_GroupDifference)
  )

  # Each panel's heading: its visit and panel, and R's test of it.
  dfDrawn <- NULL
  chrHeadings <- character(0)
  chrTests <- character(0)
  for (lPanel in lModel$panels) {
    dfRecords <- lPanel$records
    if (nrow(dfRecords) == 0L) next
    strName <- paste(c(if (is.null(lPanel$visit)) "Baseline" else lPanel$visit, lPanel$panel), collapse = ", ")
    lAnswer <- Filter(function(lResult) {
      identical(lResult$dataId$visit, lPanel$visit) && identical(lResult$dataId$panel, lPanel$panel)
    }, lAnswers)
    strTest <- if (length(lAnswer) > 0L) Figure_GroupLines(lAnswer[[1]]$value) else NA_character_
    strHeading <- if (is.na(strTest[1])) strName else paste(c(strName, Figure_Wrap(strTest, 60L)), collapse = "\n")
    chrHeadings <- c(chrHeadings, strHeading)
    chrTests <- c(chrTests, strTest)
    dfDrawn <- rbind(dfDrawn, data.frame(
      id = Core_Text(dfRecords[[lConfig$id_col]]), y = dfRecords$y,
      x = if (is.null(lState$group_by)) "All" else Core_Text(dfRecords$x),
      heading = strHeading, stringsAsFactors = FALSE
    ))
  }
  if (is.null(dfDrawn)) {
    Core_Stop("Visualize_GroupComparison(): no participant has a value of ", lState$measure, " to draw at these settings")
  }
  chrLevels <- unique(dfDrawn$x)
  chrLevels <- if (Chart_IsCut(lState$group_by)) chrLevels else Core_Levels(chrLevels)
  if (Chart_IsCut(lState$group_by)) {
    lCut <- Chart_Cut(dfResults, dfParticipants, lConfig, lState$filters, lState$group_by)
    chrLevels <- lCut$labels[lCut$labels %in% dfDrawn$x]
  }
  dfDrawn$x <- factor(dfDrawn$x, levels = chrLevels)
  dfDrawn$heading <- factor(dfDrawn$heading, levels = chrHeadings)

  chrVisits <- if (lState$value_type == "baseline") character(0) else unique(unlist(lapply(lModel$panels, `[[`, "visit")))
  strYTitle <- Output_YTitle(dfResults, lConfig, lState$measure, lState$value_type, chrVisits)
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      measure = lState$measure, visits = paste(chrVisits, collapse = ", "),
      value = chrOutputValueLabels[[lState$value_type]], group = Output_GroupLabel(lState$group_by, lConfig),
      n = length(unique(dfDrawn$id))
    )
  )
  lFilled <- Output_Titles(lTitles, lValues, lapply(lAnswers, `[[`, "value"))

  gg <- ggplot2::ggplot(dfDrawn, Figure_Aes(x = "x", y = "y", colour = "x")) +
    ggplot2::geom_boxplot(outlier.shape = NA, fill = NA, width = 0.6) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.15, height = 0, seed = 1), alpha = 0.5, size = 1.2) +
    ggplot2::facet_wrap("heading") +
    ggplot2::scale_colour_manual(values = Figure_Palette(length(chrLevels)), guide = "none") +
    ggplot2::labs(x = Output_GroupLabel(lState$group_by, lConfig), y = strYTitle)
  if (lState$y_scale == "log") {
    gg <- gg + ggplot2::scale_y_log10()
  }
  Figure_Finish(gg, lFilled)
}

# What the chart prints under a panel for R's answer (bio.viz,
# src/group-comparison/statistic.js, `describeAnswer`): the test, each estimate
# that has an interval, and with pairs, their caption and each pair with its
# counts and adjusted p-value.
Figure_GroupLines <- function(lValue) {
  chrLines <- Output_StatisticText(lValue)
  bShown <- identical(lValue$status, "ok") && (is.null(lValue$reason) || is.na(lValue$reason))
  if (!bShown) {
    return(chrLines)
  }
  dfEstimates <- lValue$estimates
  if (is.data.frame(dfEstimates) && nrow(dfEstimates) > 0L) {
    bInterval <- !is.na(dfEstimates$lower) & !is.na(dfEstimates$upper)
    chrLines <- c(chrLines, vapply(which(bInterval), function(iRow) Output_EstimateText(as.list(dfEstimates[iRow, ])), character(1)))
  }
  dfPairs <- lValue$rows
  if (is.data.frame(dfPairs) && nrow(dfPairs) > 0L && "group_1" %in% names(dfPairs)) {
    bOk <- dfPairs$status == "ok" & is.na(dfPairs$reason)
    chrMethods <- unique(dfPairs$method[bOk])
    chrAdjustments <- unique(dfPairs$adjustment[bOk])
    strBy <- if (length(chrMethods) == 1L) paste0(", each by ", chrMethods) else if (length(chrMethods) > 1L) ", each by the test named with it" else ""
    strLabel <- if (length(chrAdjustments) == 1L) paste0(" ", Table_Note(list(adjustment = chrAdjustments))) else ""
    chrLines <- c(chrLines, paste0("Pairwise comparisons", strBy, ".", strLabel))
    chrLines <- c(chrLines, vapply(seq_len(nrow(dfPairs)), function(iPair) {
      lPair <- as.list(dfPairs[iPair, ])
      strSaid <- if (bOk[iPair] && !is.na(lPair$p_value)) Output_P(lPair$p_value) else if (is.na(lPair$reason)) "not computed" else lPair$reason
      paste0(lPair$group_1, " and ", lPair$group_2, " (n = ", lPair$n_1, ", ", lPair$n_2, "): ", strSaid)
    }, character(1)))
  }
  chrLines
}
