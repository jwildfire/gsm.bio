#' Stratified Survival Table
#'
#' R's test of the curves the stratified survival chart opens on, as a table:
#' a row for the log-rank test, with its counts and p-value, then a row for
#' each group's median survival, in the legend's order, and one for the hazard
#' ratio, each with its interval. A cut's hazard ratio is the higher group's
#' over the lower's, and is named so. The numbers are [Analyze_Survival()]'s on
#' the rows the chart draws.
#'
#' @inheritSection Table_GroupComparison Display rules
#'
#' @section Titles and footnotes:
#' As for `Visualize_StratifiedSurvival()`: `{endpoint}`, `{group}`, `{n}`,
#' `{filters}`, `{date}` and `{version}`.
#'
#' @inheritParams Widget_StratifiedSurvival
#' @param lSettings `list` bio.viz stratified survival settings, as
#'   [Widget_StratifiedSurvival()] takes them, and `title`, `subtitle` and
#'   `footnotes`. Default: `list()`.
#' @param dfOutcomes `data.frame` The outcomes table, as
#'   [Widget_StratifiedSurvival()] takes it. A table needs one.
#'
#' @return A `data.frame` of text: `Statistic`, `Method`, `Estimate`, `Counts`,
#'   `p-value` and `Note`, with the attributes of [Table_GroupComparison()].
#'
#' @examples
#' Table_StratifiedSurvival(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     endpoint = "EFS",
#'     group_by = list(measure = "CRP", visit = "Baseline", cut = "median")
#'   ),
#'   dfOutcomes = Synthetic_Outcomes
#' )
#'
#' @seealso `Visualize_StratifiedSurvival()` and [Widget_StratifiedSurvival()].
#' @family tables
#' @export
Table_StratifiedSurvival <- function(dfResults, dfParticipants = NULL, lSettings = list(), dfOutcomes = NULL) {
  lTitles <- Table_Inputs(dfResults, dfParticipants, lSettings, dfOutcomes)
  if (is.null(dfOutcomes) || nrow(dfOutcomes) == 0L) {
    Core_Stop("Table_StratifiedSurvival() needs an outcomes table: give dfOutcomes, one row per participant and endpoint")
  }
  lConfig <- StratifiedSurvival_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  Widget_CheckOutcomes(dfOutcomes, lConfig)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- StratifiedSurvival_State(dfResults, dfParticipants, dfOutcomes, lConfig)
  lTable <- StratifiedSurvival_Table(dfResults, dfParticipants, dfOutcomes, lConfig, lState)
  lRequests <- StratifiedSurvival_Requests(dfResults, dfParticipants, dfOutcomes, lConfig, lState)
  if (length(lRequests) == 0L) {
    strWhy <- if (is.null(lConfig$statistic)) {
      "the setting 'statistic' is NULL, which asks R for no test"
    } else if (identical(lTable$filtered, 0L)) {
      "no participant passes the filters"
    } else if (nrow(lTable$records) == 0L) {
      "no participant has both a group and an outcome for the endpoint"
    } else {
      "the participants drawn are all in one group, and the log-rank test compares two or more"
    }
    Core_Stop("Table_StratifiedSurvival() has no statistic to show: ", strWhy)
  }
  lResult <- Chart_Answer(lRequests, list(Analyze_Survival = Analyze_Survival))[[1]]
  lValue <- lResult$value

  # The test, with no estimate of its own; then each estimate as a row.
  lTest <- lValue
  lTest$estimates <- data.frame()
  dfRows <- Table_Row(lTest, if (is.na(lValue$method)) "Test of the curves" else lValue$method)
  dfEstimates <- lValue$estimates
  if (identical(lValue$status, "ok") && is.data.frame(dfEstimates) && nrow(dfEstimates) > 0L) {
    bMedian <- dfEstimates$name == "Median"
    iOrder <- c(which(bMedian)[order(match(dfEstimates$group[bMedian], lTable$levels))], which(!bMedian))
    for (iRow in iOrder) {
      lRow <- as.list(dfEstimates[iRow, ])
      # Each estimate by its own method, as R's notes name it: a median is
      # survfit()'s, the hazard ratio coxph()'s.
      strMethod <- if (identical(lRow$name, "Median")) "Kaplan-Meier, survfit() with the log-log interval" else "Cox proportional hazards, coxph()"
      if (identical(lRow$name, "Hazard ratio") && Chart_IsCut(lState$group_by)) lRow$name <- "Hazard ratio, high over low"
      dfRows <- rbind(dfRows, data.frame(
        Statistic = paste0(lRow$name, if (is.na(lRow$group)) "" else paste0(" (", lRow$group, ")")),
        Method = strMethod, Estimate = Table_Estimate(lRow, FALSE), Counts = "", `p-value` = "", Note = "",
        check.names = FALSE, stringsAsFactors = FALSE
      ))
    }
  }
  dfEndpoints <- Chart_Endpoints(dfOutcomes, lConfig)
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      endpoint = dfEndpoints$label[match(lState$endpoint, dfEndpoints$endpoint)],
      group = Output_GroupLabel(lState$group_by, lConfig), n = nrow(lTable$records)
    )
  )
  Table_Make(dfRows, lTitles, lValues, list(lValue), rep(1L, nrow(dfRows)))
}
