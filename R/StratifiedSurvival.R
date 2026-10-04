# What bio.viz's stratified survival chart draws and asks R, worked out in R.
#
# The chart draws a Kaplan-Meier curve for each group on one endpoint of the
# outcomes table, and asks its connection to R once for a test of the curves:
# the log-rank test, each group's median and the hazard ratio (bio.viz,
# docs/stratified-survival.md, "What R is asked"). The groups are a column, or
# a biomarker or a number cut by the shared cut rule. To ship R's answer with a
# page, Widget_StratifiedSurvival() has to know, before there is a page, who
# the chart draws, in which group, with which time and flag, and how it asks.
# This file follows the chart's own code for that (bio.viz,
# src/stratified-survival/): the settings R reads, what the controls open on,
# the participants, and the request (bio.viz's `survival_key`,
# tools/r-survival.R). What every chart shares is R/chart.R, the outcomes
# table's rules among it, and the cut rule is R/core.R.
#
# It computes no statistic: the rows come from Core_Frame() and the outcomes
# table, and the answer is Analyze_Survival() on them. Nothing here is
# exported.

# The settings of the chart that R reads, with the chart's own defaults. The
# chart has more (its listing, the at-risk strip, the profile); those pass
# through to the page untouched. tests/testthat/test-StratifiedSurvival.R holds
# these to the vendored bundle.
lStratifiedSurvivalDefaults <- list(
  id_col = "USUBJID",
  measure_col = "TEST",
  value_col = "STRESN",
  visit_col = "VISIT",
  visit_order_col = "VISITNUM",
  unit_col = "STRESU",
  participant_id_col = NULL,
  baseline_visits = NULL,
  baseline_stat = "mean",
  # The outcomes table's columns (lOutcomeDefaults), and the endpoint the chart
  # opens on.
  outcome_id_col = NULL,
  endpoint_col = "PARAMCD",
  endpoint_label_col = "PARAM",
  time_col = "AVAL",
  censor_col = "CNSR",
  event_col = NULL,
  endpoint = NULL,
  group_by = NULL,
  cuts = NULL,
  at_risk_times = NULL,
  measures = NULL,
  groups = NULL,
  max_levels = 12L,
  filters = NULL,
  statistic = "Analyze_Survival"
)

# The one R function the widget stores results of.
strStratifiedSurvivalStatistic <- "Analyze_Survival"

# The settings R reads, in full: the caller's over the chart's defaults, each
# checked, in the forms the functions below read them. An event column named
# alone is the flag, as the chart reads it.
StratifiedSurvival_Settings <- function(lSettings = list()) {
  lSettings <- Chart_FlaggedSettings(lSettings)
  lConfig <- Core_Overlay(
    lStratifiedSurvivalDefaults, lSettings[intersect(names(lSettings), names(lStratifiedSurvivalDefaults))]
  )
  IsName <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue))
  for (strKey in c("id_col", "measure_col", "value_col", "visit_col")) {
    if (!IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single column name (a character string)")
    }
  }
  for (strKey in c("visit_order_col", "unit_col", "participant_id_col")) {
    if (!is.null(lConfig[[strKey]]) && !IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single name (a character string), or NULL")
    }
  }
  Chart_CheckOutcomeSettings(lConfig)
  # The groups: a column, or a biomarker or a number cut into groups by the
  # shared cut rule.
  lConfig["group_by"] <- list(Chart_Grouping(lConfig$group_by, "group_by"))
  if (!is.null(lConfig$cuts)) {
    if (!is.list(lConfig$cuts) || is.data.frame(lConfig$cuts)) {
      Core_Stop("Setting 'cuts' must be a list of cut variables, or NULL")
    }
    lConfig$cuts <- lapply(seq_along(lConfig$cuts), function(iCut) {
      if (!is.list(lConfig$cuts[[iCut]])) {
        Core_Stop("Setting 'cuts[", iCut, "]' must be a cut variable: list(measure, visit, cut) or list(col, type = 'number', cut)")
      }
      Chart_Grouping(lConfig$cuts[[iCut]], paste0("cuts[", iCut, "]"))
    })
  }
  nTimes <- unlist(lConfig$at_risk_times)
  if (!is.null(lConfig$at_risk_times) &&
    !(is.numeric(nTimes) && length(nTimes) > 0L && all(is.finite(nTimes)) && all(nTimes >= 0) && all(diff(nTimes) > 0))) {
    Core_Stop("Setting 'at_risk_times' must be times, none below 0, in ascending order, or NULL")
  }
  if (!lConfig$baseline_stat %in% chrCoreBaselineStats) {
    Core_Stop("Setting 'baseline_stat' must be one of ", paste(chrCoreBaselineStats, collapse = ", "))
  }
  nMaxLevels <- lConfig$max_levels
  if (!(is.numeric(nMaxLevels) && length(nMaxLevels) == 1L && !is.na(nMaxLevels) && nMaxLevels >= 1 && nMaxLevels == round(nMaxLevels))) {
    Core_Stop("Setting 'max_levels' must be a whole number, one or more")
  }
  if (!is.null(lConfig$statistic) && !identical(lConfig$statistic, strStratifiedSurvivalStatistic)) {
    Core_Stop(
      "Setting 'statistic' must be '", strStratifiedSurvivalStatistic, "', the function the widget stores results of, ",
      "or NULL for no statistics line"
    )
  }
  for (strKey in c("baseline_visits", "measures")) {
    lConfig[strKey] <- list(Chart_Names(lConfig[[strKey]], strKey))
  }
  for (strKey in c("groups", "filters")) {
    lConfig[strKey] <- list(Chart_Fields(lConfig[[strKey]], strKey))
  }
  lConfig
}

# What the controls open on: the endpoint the settings name, where the outcomes
# table has it, or its first; the groups the settings name, where the tables
# have them (a cut variable is offered as it is named), or the first category
# column; and the filters.
StratifiedSurvival_State <- function(dfResults, dfParticipants, dfOutcomes, lConfig) {
  dfCategories <- Chart_Categories(dfResults, dfParticipants, lConfig)
  Chart_CheckCut(lConfig$group_by, "group_by", dfResults, dfParticipants, lConfig)
  for (iCut in seq_along(lConfig$cuts)) {
    Chart_CheckCut(lConfig$cuts[[iCut]], paste0("cuts[", iCut, "]"), dfResults, dfParticipants, lConfig)
  }
  xGroupBy <- if (Chart_IsCut(lConfig$group_by)) {
    lConfig$group_by
  } else if (!is.null(lConfig$group_by) && lConfig$group_by %in% dfCategories$value_col) {
    lConfig$group_by
  } else {
    Core_First(dfCategories$value_col)
  }
  if (length(xGroupBy) == 0L) xGroupBy <- NULL
  chrEndpoints <- Chart_Endpoints(dfOutcomes, lConfig)$endpoint
  strEndpoint <- if (!is.null(lConfig$endpoint) && lConfig$endpoint %in% chrEndpoints) {
    lConfig$endpoint
  } else {
    Core_First(chrEndpoints)
  }
  if (length(strEndpoint) == 0L) strEndpoint <- NULL
  list(
    endpoint = strEndpoint,
    group_by = xGroupBy,
    filters = Chart_Filters(dfParticipants, lConfig, dfCategories)
  )
}

# Who the chart draws (bio.viz, src/stratified-survival/structureData.js,
# `buildSurvival`): one record per participant the filters keep who has a group
# and an outcome to use for the endpoint, with the id, `group` as text (a
# cut's as its group's label), `time`, `flag` as the table holds it and
# `event`; the groups, a cut's low to high and a column's by name with numbers
# as numbers, those with someone in them; the cut, when the groups are one;
# who was left out for want of an outcome, by reason; and `filtered`, how many
# participants pass the filters, NULL with no participant table.
StratifiedSurvival_Table <- function(dfResults, dfParticipants, dfOutcomes, lConfig, lState) {
  lKept <- Chart_KeepFiltered(dfResults, dfParticipants, lConfig, lState$filters)
  nFiltered <- if (is.null(lKept$participants)) NULL else nrow(lKept$participants)
  lEmpty <- list(records = data.frame(), levels = character(0), cut = NULL, left = list(), filtered = nFiltered)
  if (nrow(lKept$results) == 0L || is.null(lState$group_by) || is.null(lState$endpoint)) {
    return(lEmpty)
  }
  # A cut's points are worked out on the participants the filters keep who
  # have a value of it and an outcome for the endpoint: the ones the curves are
  # drawn of, so a median cuts them in halves, as Analyze_Screen cuts a
  # biomarker for its hazard ratio.
  lCut <- if (Chart_IsCut(lState$group_by)) {
    strIdCol <- if (is.null(lKept$participants)) lConfig$id_col else if (is.null(lConfig$participant_id_col)) lConfig$id_col else lConfig$participant_id_col
    chrKept <- unique(Core_Text((if (is.null(lKept$participants)) lKept$results else lKept$participants)[[strIdCol]]))
    chrKept <- chrKept[!is.na(chrKept)]
    chrWithOutcome <- chrKept[is.na(Chart_Outcomes(dfOutcomes, lConfig, lState$endpoint, chrKept)$reason)]
    Chart_Cut(dfResults, dfParticipants, lConfig, lState$filters, lState$group_by, chrWithOutcome)
  } else {
    NULL
  }
  dfData <- Core_Frame(
    lKept$results, lKept$participants, list(group = Chart_GroupingVariable(lState$group_by)),
    Chart_CoreSettings(lConfig)
  )$data
  chrIds <- Core_Text(dfData[[lConfig$id_col]])
  dfOutcome <- Chart_Outcomes(dfOutcomes, lConfig, lState$endpoint, chrIds)
  bUsed <- is.na(dfOutcome$reason)
  dfRecords <- data.frame(id = chrIds[bUsed], stringsAsFactors = FALSE)
  names(dfRecords) <- lConfig$id_col
  dfRecords$group <- if (is.null(lCut)) Core_Text(dfData$group[bUsed]) else Core_CutGroups(dfData$group[bUsed], lCut$points)
  dfRecords$time <- dfOutcome$time[bUsed]
  dfRecords$flag <- dfOutcome$flag[bUsed]
  dfRecords$event <- dfOutcome$event[bUsed]
  chrLevels <- if (is.null(lCut)) Core_Levels(dfRecords$group) else lCut$labels[lCut$labels %in% dfRecords$group]
  chrLeft <- dfOutcome$reason[!bUsed]
  list(
    records = dfRecords, levels = chrLevels, cut = lCut,
    left = lapply(split(chrLeft, factor(chrLeft, levels = unique(chrLeft))), length),
    filtered = nFiltered
  )
}

# Whether a grouping is a cut biomarker that reads a baseline: its value is a
# baseline, or a change from one.
StratifiedSurvival_ReadsBaseline <- function(xBy) {
  Chart_IsCut(xBy) && !is.null(xBy$measure) && !identical(xBy$value, "raw")
}

# What the chart asks R for its curves: bio.viz's `survival_key` recipe
# (tools/r-survival.R). A cut's groups are handed to R high to low, so the
# hazard ratio is the higher group's over the lower's; a column's by code
# point. The baseline settings are in the identity only when a cut biomarker
# reads a baseline.
StratifiedSurvival_Key <- function(lTable, lView) {
  lDataId <- list(chart = "stratified-survival", endpoint = lView$endpoint, group_by = lView$group_by)
  if (StratifiedSurvival_ReadsBaseline(lView$group_by)) {
    if (!is.null(lView$baseline_visits)) lDataId$baseline_visits <- as.list(lView$baseline_visits)
    lDataId$baseline_stat <- lView$baseline_stat
  }
  if (length(lView$filters) > 0L) {
    lDataId$filters <- lapply(lView$filters, as.list)
  }
  lArgs <- list(strTimeCol = "time", strGroupCol = "group")
  if (lView$flag == "censor") lArgs$strCensorCol <- "censor" else lArgs$strEventCol <- "event"
  lArgs$chrGroups <- as.list(if (Chart_IsCut(lView$group_by)) rev(lTable$levels) else Core_SortText(lTable$levels))
  list(name = lView$statistic, args = lArgs, dataId = lDataId, rows = nrow(lTable$records))
}

# The request the chart makes for its curves, or none: with no statistic,
# nobody through the filters, no one with a group and an outcome, or fewer than
# two groups. R is handed the id, `time`, `group` and the flag, under the name
# the arguments give it.
StratifiedSurvival_Requests <- function(dfResults, dfParticipants, dfOutcomes, lConfig, lState) {
  if (is.null(lConfig$statistic)) {
    return(list())
  }
  lTable <- StratifiedSurvival_Table(dfResults, dfParticipants, dfOutcomes, lConfig, lState)
  if (identical(lTable$filtered, 0L) || nrow(lTable$records) == 0L || length(lTable$levels) < 2L) {
    return(list())
  }
  lFlag <- Chart_FlagOf(lConfig)
  dfData <- lTable$records[c(lConfig$id_col, "time", "group")]
  dfData[[lFlag$field]] <- lTable$records$flag
  lKey <- StratifiedSurvival_Key(lTable, list(
    statistic = lConfig$statistic, endpoint = lState$endpoint, group_by = lState$group_by, flag = lFlag$field,
    baseline_visits = lConfig$baseline_visits, baseline_stat = lConfig$baseline_stat,
    filters = Chart_FiltersInForce(lState$filters)
  ))
  list(c(lKey, list(data = dfData)))
}

# The stored results a page ships: R's answer for the curves the settings open
# on.
StratifiedSurvival_StoredResults <- function(dfResults, dfParticipants, dfOutcomes, lConfig) {
  if (nrow(dfResults) == 0L || is.null(dfOutcomes) || nrow(dfOutcomes) == 0L) {
    return(list())
  }
  lState <- StratifiedSurvival_State(dfResults, dfParticipants, dfOutcomes, lConfig)
  Chart_Answer(
    StratifiedSurvival_Requests(dfResults, dfParticipants, dfOutcomes, lConfig, lState),
    list(Analyze_Survival = Analyze_Survival)
  )
}
