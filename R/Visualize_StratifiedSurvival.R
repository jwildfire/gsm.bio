#' Stratified Survival Figure
#'
#' A static ggplot2 figure of the curves the stratified survival chart opens
#' on: each group's Kaplan-Meier curve on one endpoint, with its confidence
#' band, a mark at each censored time, and R's log-rank test, medians and hazard
#' ratio printed under the figure. The groups, who is drawn and the test are
#' those of [Widget_StratifiedSurvival()] at the same settings, and the test is
#' [Analyze_Survival()]'s on the same rows. The curves and their bands are R's
#' own: [survival::survfit()] on those rows, with the log-log interval
#' Analyze_Survival() reports its medians by.
#'
#' @section Titles and footnotes:
#' The settings `title`, `subtitle` and `footnotes` are text with named
#' placeholders, as for [Visualize_GroupComparison()]. This figure fills
#' `{endpoint}` (by its label), `{group}` (what the groups are), `{n}` (the
#' participants drawn), `{filters}`, `{date}` and `{version}`.
#'
#' @inheritParams Widget_StratifiedSurvival
#' @param lSettings `list` bio.viz stratified survival settings, as
#'   [Widget_StratifiedSurvival()] takes them, and `title`, `subtitle` and
#'   `footnotes`. Default: `list()`.
#' @param dfOutcomes `data.frame` The outcomes table, as
#'   [Widget_StratifiedSurvival()] takes it. A figure needs one.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   Visualize_StratifiedSurvival(
#'     Synthetic_Results,
#'     Synthetic_Participants,
#'     lSettings = list(
#'       endpoint = "EFS",
#'       group_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
#'       title = "{endpoint} by {group}"
#'     ),
#'     dfOutcomes = Synthetic_Outcomes
#'   )
#' }
#'
#' @seealso [Widget_StratifiedSurvival()], its interactive twin.
#' @family figures
#' @export
Visualize_StratifiedSurvival <- function(dfResults, dfParticipants = NULL, lSettings = list(), dfOutcomes = NULL) {
  lTitles <- Figure_Inputs("Visualize_StratifiedSurvival", dfResults, dfParticipants, lSettings, dfOutcomes)
  if (is.null(dfOutcomes) || nrow(dfOutcomes) == 0L) {
    Core_Stop("Visualize_StratifiedSurvival() needs an outcomes table: give dfOutcomes, one row per participant and endpoint")
  }
  lConfig <- StratifiedSurvival_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  Widget_CheckOutcomes(dfOutcomes, lConfig)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- StratifiedSurvival_State(dfResults, dfParticipants, dfOutcomes, lConfig)
  Chart_StopIfNobody("Visualize_StratifiedSurvival", dfResults, dfParticipants, lConfig, lState$filters)
  lTable <- StratifiedSurvival_Table(dfResults, dfParticipants, dfOutcomes, lConfig, lState)
  if (nrow(lTable$records) == 0L) {
    Core_Stop("Visualize_StratifiedSurvival(): no participant has a group and an outcome for the endpoint at these settings")
  }
  lAnswers <- Chart_Answer(
    StratifiedSurvival_Requests(dfResults, dfParticipants, dfOutcomes, lConfig, lState),
    list(Analyze_Survival = Analyze_Survival)
  )

  # R's curves: survfit() on the rows drawn, a step from 1 at time 0.
  dfRecords <- lTable$records
  dfRecords$group <- factor(dfRecords$group, levels = lTable$levels)
  dfRecords <- dfRecords[!is.na(dfRecords$group), , drop = FALSE]
  lCurves <- lapply(lTable$levels, function(strLevel) {
    dfIn <- dfRecords[dfRecords$group == strLevel, , drop = FALSE]
    lFit <- survival::survfit(survival::Surv(dfIn$time, dfIn$event) ~ 1, conf.type = "log-log")
    data.frame(
      group = strLevel, time = c(0, lFit$time), surv = c(1, lFit$surv),
      lower = c(1, lFit$lower), upper = c(1, lFit$upper), censored = c(0L, lFit$n.censor), stringsAsFactors = FALSE
    )
  })
  dfCurves <- do.call(rbind, lCurves)
  # The band as steps: each estimate held until the next time.
  dfBand <- do.call(rbind, lapply(lCurves, function(dfCurve) {
    nTimes <- nrow(dfCurve)
    if (nTimes < 2L) {
      return(dfCurve[c("group", "time", "lower", "upper")])
    }
    iTime <- c(1L, rep(seq_len(nTimes)[-1L], each = 2L))
    iHeld <- c(1L, as.vector(rbind(seq_len(nTimes - 1L), seq_len(nTimes)[-1L])))
    data.frame(
      group = dfCurve$group[1], time = dfCurve$time[iTime], lower = dfCurve$lower[iHeld],
      upper = dfCurve$upper[iHeld], stringsAsFactors = FALSE
    )
  }))
  dfMarks <- dfCurves[dfCurves$censored > 0L, , drop = FALSE]
  dfCurves$group <- factor(dfCurves$group, levels = lTable$levels)
  dfBand$group <- factor(dfBand$group, levels = lTable$levels)
  dfMarks$group <- factor(dfMarks$group, levels = lTable$levels)

  # What the statistics line prints: the test, the medians in the legend's
  # order, and a cut's hazard ratio named high over low.
  chrStatistics <- unlist(lapply(lAnswers, function(lResult) {
    lValue <- lResult$value
    chrSaid <- Output_StatisticText(lValue)
    if (identical(lValue$status, "ok") && nrow(lValue$estimates) > 0L) {
      dfEstimates <- lValue$estimates
      bMedian <- dfEstimates$name == "Median"
      iMedians <- which(bMedian)[order(match(dfEstimates$group[bMedian], lTable$levels))]
      for (iRow in c(iMedians, which(!bMedian))) {
        lRow <- as.list(dfEstimates[iRow, ])
        if (identical(lRow$name, "Hazard ratio") && Chart_IsCut(lState$group_by)) lRow$name <- "Hazard ratio, high over low"
        chrSaid <- c(chrSaid, Output_EstimateText(lRow))
      }
    }
    chrSaid
  }))

  dfEndpoints <- Chart_Endpoints(dfOutcomes, lConfig)
  strEndpoint <- dfEndpoints$label[match(lState$endpoint, dfEndpoints$endpoint)]
  strGroup <- Output_GroupLabel(lState$group_by, lConfig)
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(endpoint = strEndpoint, group = strGroup, n = nrow(dfRecords))
  )
  lFilled <- Output_Titles(lTitles, lValues, lapply(lAnswers, `[[`, "value"))
  chrPalette <- stats::setNames(Figure_Palette(length(lTable$levels)), lTable$levels)
  chrLegend <- paste0(lTable$levels, " (n = ", as.integer(table(dfRecords$group)[lTable$levels]), ")")

  gg <- ggplot2::ggplot(dfCurves, Figure_Aes(x = "time", y = "surv", colour = "group")) +
    ggplot2::geom_ribbon(data = dfBand, Figure_Aes(x = "time", ymin = "lower", ymax = "upper", fill = "group"), inherit.aes = FALSE, alpha = 0.12, na.rm = TRUE) +
    ggplot2::geom_step(linewidth = 0.8) +
    ggplot2::geom_point(data = dfMarks, shape = 3, size = 1.6) +
    ggplot2::scale_colour_manual(values = chrPalette, labels = chrLegend) +
    ggplot2::scale_fill_manual(values = chrPalette, guide = "none") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = strEndpoint, y = "Kaplan-Meier estimate", colour = strGroup)
  Figure_Finish(gg, lFilled, chrStatistics)
}
