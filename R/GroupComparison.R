# What bio.viz's group comparison chart draws and asks R, worked out in R.
#
# The chart draws at three levels (bio.viz, src/group-comparison/level.js):
# every biomarker, a trend tile each, which asks R for nothing; one biomarker
# over time, every visit it has in one picture, which asks R for the test at
# every visit in one request; and one biomarker's visits, a panel per visit
# chosen, which asks R for one test per panel. It finds a stored result by the
# function's name, its arguments and the identity of the rows together
# (bio.viz, docs/group-comparison.md, "What R is asked"). To ship R's answers
# with a page, Widget_GroupComparison() has to know, before there is a page,
# what the chart will draw at each level and what it will ask. This file
# follows the chart's own code for that (bio.viz, src/group-comparison/): what
# the controls offer, what they open on, which visits are drawn, the level,
# the panels, and the two requests.
#
# It computes no statistic. The rows come from Core_Frame(); the answer for a
# panel is Analyze_GroupDifference() on its rows, and the answer under one
# biomarker over time is Analyze_GroupDifferenceBy() on the rows of every visit
# tested. Nothing here is exported.

# The settings of the chart that R reads, with the chart's own defaults. The
# chart has more (how it draws, its listing, the participant profile); those
# pass through to the page untouched. tests/testthat/test-GroupComparison.R
# holds these defaults to the vendored bundle's.
lGroupComparisonDefaults <- list(
  id_col = "USUBJID",
  measure_col = "TEST",
  value_col = "STRESN",
  visit_col = "VISIT",
  visit_order_col = "VISITNUM",
  unit_col = "STRESU",
  participant_id_col = NULL,
  baseline_visits = NULL,
  baseline_stat = "mean",
  start_value = NULL,
  visits = NULL,
  value_type = "raw",
  group_by = NULL,
  levels = NULL,
  color_by = NULL,
  panel_by = NULL,
  y_scale = "linear",
  time_mark = "box",
  measures = NULL,
  groups = NULL,
  max_levels = 12L,
  filters = NULL,
  unscheduled_visits = FALSE,
  unscheduled_visit_pattern = "/unscheduled|early termination/i",
  unscheduled_visit_values = NULL,
  tile_summary = "median",
  tile_min_spread = 1.25,
  statistic = "Analyze_GroupDifference",
  test = "t",
  pairwise = FALSE,
  statistic_by_visit = "Analyze_GroupDifferenceBy",
  visit_adjustment = "none",
  studyday_col = NULL,
  normal_col_high = NULL,
  normal_col_low = NULL
)

chrGroupComparisonTests <- c("t", "wilcoxon", "anova", "kruskal", "none")

# What one biomarker over time is drawn as, what a trend tile's line goes
# through, and how R adjusts the p-values across the visits, by the names
# `p.adjust()` gives the adjustments.
chrGroupComparisonTimeMarks <- c("box", "mean_se", "median_iqr")
chrGroupComparisonTileSummaries <- c("median", "mean")
chrGroupComparisonAdjustments <- c("none", "holm", "BH")

# The levels the chart draws at, by the names it gives them.
chrGroupComparisonLevels <- c("biomarkers", "over-time", "visits")

# The R functions the widget stores results of: the test under one panel, and
# the test under every visit of one biomarker over time.
strGroupComparisonStatistic <- "Analyze_GroupDifference"
strGroupComparisonByVisit <- "Analyze_GroupDifferenceBy"

# The settings R reads, in full: the caller's over the chart's defaults, each
# checked, in the forms the functions below read them.
GroupComparison_Settings <- function(lSettings = list()) {
  lConfig <- Core_Overlay(lGroupComparisonDefaults, lSettings[intersect(names(lSettings), names(lGroupComparisonDefaults))])
  IsName <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue))
  for (strKey in c("id_col", "measure_col", "value_col", "visit_col")) {
    if (!IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single column name (a character string)")
    }
  }
  for (strKey in c(
    "visit_order_col", "unit_col", "participant_id_col", "start_value", "color_by",
    "studyday_col", "normal_col_high", "normal_col_low"
  )) {
    if (!is.null(lConfig[[strKey]]) && !IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single name (a character string), or NULL")
    }
  }
  Choice <- function(strKey, chrChoices) {
    if (!IsName(lConfig[[strKey]]) || !lConfig[[strKey]] %in% chrChoices) {
      Core_Stop("Setting '", strKey, "' must be one of ", paste(chrChoices, collapse = ", "))
    }
  }
  # The groups and the panels: a column, or a biomarker or a number cut into
  # groups by the shared cut rule.
  for (strKey in c("group_by", "panel_by")) {
    lConfig[strKey] <- list(Chart_Grouping(lConfig[[strKey]], strKey))
  }
  Choice("value_type", chrCoreValueTypes)
  Choice("y_scale", c("linear", "log"))
  Choice("baseline_stat", chrCoreBaselineStats)
  Choice("test", chrGroupComparisonTests)
  Choice("time_mark", chrGroupComparisonTimeMarks)
  Choice("tile_summary", chrGroupComparisonTileSummaries)
  Choice("visit_adjustment", chrGroupComparisonAdjustments)
  for (strKey in c("pairwise", "unscheduled_visits")) {
    if (!(is.logical(lConfig[[strKey]]) && length(lConfig[[strKey]]) == 1L && !is.na(lConfig[[strKey]]))) {
      Core_Stop("Setting '", strKey, "' must be TRUE or FALSE")
    }
  }
  nSpread <- lConfig$tile_min_spread
  if (!(is.numeric(nSpread) && length(nSpread) == 1L && is.finite(nSpread) && nSpread >= 0)) {
    Core_Stop(
      "Setting 'tile_min_spread' must be a number, zero or more: how many standard deviations of the results at the ",
      "baseline visit a tile's value axis spans at the least"
    )
  }
  nMaxLevels <- lConfig$max_levels
  if (!(is.numeric(nMaxLevels) && length(nMaxLevels) == 1L && !is.na(nMaxLevels) && nMaxLevels >= 1 && nMaxLevels == round(nMaxLevels))) {
    Core_Stop("Setting 'max_levels' must be a whole number, one or more")
  }
  if (!is.null(lConfig$statistic) && !identical(lConfig$statistic, strGroupComparisonStatistic)) {
    Core_Stop(
      "Setting 'statistic' must be '", strGroupComparisonStatistic, "', the function the widget stores results of, ",
      "or NULL for no statistics line"
    )
  }
  if (!is.null(lConfig$statistic_by_visit) && !identical(lConfig$statistic_by_visit, strGroupComparisonByVisit)) {
    Core_Stop(
      "Setting 'statistic_by_visit' must be '", strGroupComparisonByVisit, "', the function the widget stores the tests ",
      "under one biomarker's visits of, or NULL for no test under the visits"
    )
  }
  # No visit named is every visit, and no level named is none drawn, as bio.viz
  # reads them. A list of no unscheduled visits is a list: it names none, and
  # the pattern is then not read.
  for (strKey in c("baseline_visits", "visits", "levels", "measures", "unscheduled_visit_values")) {
    lConfig[strKey] <- list(Chart_Names(
      lConfig[[strKey]], strKey,
      bEmpty = strKey %in% c("visits", "levels", "unscheduled_visit_values")
    ))
  }
  # The pattern is read only when no visit is named outright, and then it has
  # to be one R reads as a browser does (R/core.R).
  strPattern <- lConfig$unscheduled_visit_pattern
  if (!is.null(strPattern) && !IsName(strPattern)) {
    Core_Stop(
      "Setting 'unscheduled_visit_pattern' must be a regular expression written as text, \"/source/flags\" or a plain ",
      "source, or NULL for none"
    )
  }
  if (!is.null(strPattern) && is.null(lConfig$unscheduled_visit_values) && is.null(Core_ReadPattern(strPattern))) {
    Core_Stop(
      "Setting 'unscheduled_visit_pattern' is a regular expression a browser reads, and R reads only the patterns that ",
      "mean the same in both: words of letters, digits and spaces set side by side with |, with or without the flag i, ",
      "as \"/unscheduled|early termination/i\" is. Name the unscheduled visits in 'unscheduled_visit_values' instead, ",
      "which is read the same everywhere"
    )
  }
  for (strKey in c("groups", "filters")) {
    lConfig[strKey] <- list(Chart_Fields(lConfig[[strKey]], strKey))
  }
  lConfig
}

# The visits the chart has, and the ones it opens on: the visits in the
# setting `visits` that the table has, or, when the setting names none, every
# visit. They are the visits the chart draws: an unscheduled visit is among
# them only when the settings switch unscheduled visits on (Chart_Unscheduled).
GroupComparison_Visits <- function(dfResults, lConfig, lDrawn = Chart_Unscheduled(dfResults, lConfig)) {
  chrAll <- Core_Visits(lDrawn$results, Chart_CoreSettings(lConfig))
  chrAsked <- lConfig$visits[lConfig$visits %in% chrAll]
  list(all = chrAll, start = if (length(chrAsked) > 0L) chrAsked else chrAll)
}

# The visits one biomarker has values at, in visit order, of the visits the
# chart draws: the ones the Visit control offers while that biomarker is open,
# and the ones it has a panel or a place over time at.
GroupComparison_MeasureVisits <- function(dfDrawn, lConfig, strMeasure) {
  if (is.null(strMeasure) || is.na(strMeasure) || nrow(dfDrawn) == 0L) {
    return(character(0))
  }
  dfRows <- dfDrawn[Core_Text(dfDrawn[[lConfig$measure_col]]) %in% strMeasure, , drop = FALSE]
  if (nrow(dfRows) == 0L) character(0) else Core_Visits(dfRows, Chart_CoreSettings(lConfig))
}

# Which level the chart draws for what the controls are set to (bio.viz,
# src/group-comparison/level.js): every biomarker when none is chosen; one
# biomarker over time when every visit it has is chosen, it has two or more,
# and the value has a visit, which a baseline value does not; and otherwise
# that biomarker's visits, a panel each.
GroupComparison_Level <- function(lState, chrOffered) {
  if (is.null(lState$measure) || is.na(lState$measure)) {
    return("biomarkers")
  }
  if (lState$value_type == "baseline" || length(chrOffered) < 2L) {
    return("visits")
  }
  if (all(chrOffered %in% lState$visits)) "over-time" else "visits"
}

# The visits that are drawn, of the visits chosen. For a change, a fold change
# or a percent change from baseline, the baseline visit itself is left out when
# it is the only baseline visit: there every participant's value is the same by
# definition, and there is nothing to compare. With several baseline visits
# every visit is drawn.
GroupComparison_VisitsDrawn <- function(chrVisits, strValueType, chrBaselineVisits) {
  bRelative <- strValueType %in% c("change", "fold_change", "percent_change")
  if (!bRelative || length(chrBaselineVisits) != 1L) {
    return(chrVisits)
  }
  chrVisits[chrVisits != chrBaselineVisits]
}

# What the chart opens on: the settings, where the tables have what they name.
# The biomarker is the one `start_value` names when the table has it, and
# otherwise NULL, which is every biomarker, a trend tile each: the tiles print
# no test and ask R for nothing.
GroupComparison_State <- function(dfResults, dfParticipants, lConfig) {
  dfCategories <- Chart_Categories(dfResults, dfParticipants, lConfig)
  chrMeasures <- Chart_Measures(dfResults, lConfig)
  Has <- function(strColumn) !is.null(strColumn) && !Chart_IsCut(strColumn) && strColumn %in% dfCategories$value_col
  # A cut variable the settings name is offered as it is: of a biomarker or a
  # column the tables have.
  Chart_CheckCut(lConfig$group_by, "group_by", dfResults, dfParticipants, lConfig)
  Chart_CheckCut(lConfig$panel_by, "panel_by", dfResults, dfParticipants, lConfig)
  list(
    measure = if (!is.null(lConfig$start_value) && lConfig$start_value %in% chrMeasures) lConfig$start_value else NULL,
    visits = GroupComparison_Visits(dfResults, lConfig)$start,
    value_type = lConfig$value_type,
    group_by = if (Chart_IsCut(lConfig$group_by)) {
      lConfig$group_by
    } else if (Has(lConfig$group_by)) {
      lConfig$group_by
    } else if (nrow(dfCategories) > 0L) {
      dfCategories$value_col[1L]
    } else {
      NULL
    },
    levels = lConfig$levels,
    color_by = if (Has(lConfig$color_by)) lConfig$color_by else NULL,
    panel_by = if (Chart_IsCut(lConfig$panel_by) || Has(lConfig$panel_by)) lConfig$panel_by else NULL,
    y_scale = lConfig$y_scale,
    test = lConfig$test,
    pairwise = lConfig$pairwise,
    visit_adjustment = lConfig$visit_adjustment,
    filters = Chart_Filters(dfParticipants, lConfig, dfCategories)
  )
}

# The test to ask for: the one chosen when it fits the number of groups drawn,
# and otherwise its counterpart of the same kind. `none` stays `none`. With
# fewer than two groups no test fits, and the answer is NULL.
GroupComparison_FitTest <- function(strTest, nGroups) {
  if (strTest == "none") {
    return("none")
  }
  chrOffered <- if (nGroups == 2L) c("t", "wilcoxon") else if (nGroups > 2L) c("anova", "kruskal") else character(0)
  if (length(chrOffered) == 0L) {
    return(NULL)
  }
  if (strTest %in% chrOffered) {
    return(strTest)
  }
  c(t = "anova", anova = "t", wilcoxon = "kruskal", kruskal = "wilcoxon")[[strTest]]
}

# The panels the chart draws in one view, each with its rows: one per visit,
# and one per level of the panel column, as the chart's `buildPanels` makes
# them. The visits are the ones chosen that the biomarker has values at, of the
# visits the chart draws. Returns a list with `panels`, each a list of `visit`,
# `panel` (the level of the panel column, or NULL) and `records` (the panel's
# rows, one per participant: the id, `y`, `x`, and `color` and `panel` when
# set); `groups`, how many levels of the group are drawn; `levels`, those
# levels; and `baseline_visits`, the baseline visit of a change.
#
# `bKeepBaseline` keeps the one baseline visit of a change among the visits,
# as the picture of one biomarker over time does: it is drawn there, where
# every group starts, and is not tested. `lDrawn` is the results the chart
# draws, for a caller that has worked them out already.
GroupComparison_Panels <- function(dfResults, dfParticipants, lConfig, lState, bKeepBaseline = FALSE,
                                   lDrawn = Chart_Unscheduled(dfResults, lConfig)) {
  lCore <- Chart_CoreSettings(lConfig)
  strIdCol <- lConfig$id_col
  # Unscheduled visits are set aside before anything is listed or framed, so
  # they are not a panel and not the baseline a change is measured from.
  dfResults <- lDrawn$results

  # The filters choose participants; the results of the others are set aside
  # before the frame is made, so they are not counted as missing from it.
  lKept <- Chart_KeepFiltered(dfResults, dfParticipants, lConfig, lState$filters)
  dfKept <- lKept$participants
  dfRows <- lKept$results

  bNeedsVisit <- lState$value_type != "baseline"
  # A change at the one baseline visit is the same for everyone, so that visit
  # is not drawn: there is nothing in it to compare.
  chrBaselineVisits <- if (lState$value_type %in% c("change", "fold_change", "percent_change")) {
    if (is.null(lCore$baseline_visits)) Core_First(Core_Visits(dfRows, lCore)) else lCore$baseline_visits
  } else {
    character(0)
  }
  # A visit the biomarker has no value at has no panel: it keeps its place
  # among the visits chosen for another biomarker.
  chrAsked <- lState$visits[lState$visits %in% GroupComparison_MeasureVisits(dfResults, lConfig, lState$measure)]
  chrDrawn <- if (bKeepBaseline) chrAsked else GroupComparison_VisitsDrawn(chrAsked, lState$value_type, chrBaselineVisits)
  lVisits <- if (bNeedsVisit) as.list(chrDrawn) else list(NULL)
  # A cut variable's points are worked out once, on every participant the
  # filters keep who has a value of it, whether or not they have a value to
  # draw: so the groups are the same in every visit's panel.
  lCuts <- list()
  for (strField in c("x", "panel")) {
    xBy <- if (strField == "x") lState$group_by else lState$panel_by
    if (Chart_IsCut(xBy)) {
      lCuts[[strField]] <- Chart_Cut(dfResults, dfParticipants, lConfig, lState$filters, xBy)
    }
  }
  lFramed <- lapply(lVisits, function(strVisit) {
    lVariables <- list(y = if (bNeedsVisit) {
      list(measure = lState$measure, visit = strVisit, value = lState$value_type)
    } else {
      list(measure = lState$measure, value = "baseline")
    })
    if (!is.null(lState$group_by)) lVariables$x <- Chart_GroupingVariable(lState$group_by)
    if (!is.null(lState$color_by)) lVariables$color <- list(col = lState$color_by)
    if (!is.null(lState$panel_by)) lVariables$panel <- Chart_GroupingVariable(lState$panel_by)
    dfData <- Core_Frame(dfRows, dfKept, lVariables, lCore)$data
    # A cut variable's number, as its group's label.
    for (strField in names(lCuts)) {
      dfData[[strField]] <- Core_CutGroups(dfData[[strField]], lCuts[[strField]]$points)
    }
    # A logarithmic axis has no place for zero or less.
    if (lState$y_scale == "log") {
      dfData <- dfData[dfData$y > 0, , drop = FALSE]
    }
    dfData
  })

  # A cut's groups, low to high, those with someone in them.
  CutLevels <- function(strField) {
    chrIn <- unlist(lapply(lFramed, function(dfData) dfData[[strField]]))
    lCuts[[strField]]$labels[lCuts[[strField]]$labels %in% chrIn]
  }
  # The levels drawn are the ones in the rows of every panel together.
  chrLevels <- if (is.null(lState$group_by)) {
    character(0)
  } else if (!is.null(lCuts$x)) {
    CutLevels("x")
  } else {
    Core_Levels(unlist(lapply(lFramed, function(dfData) Core_Text(dfData$x))))
  }
  # Every group a cut makes is drawn: the levels are for a column.
  chrShown <- if (is.null(lState$levels) || !is.null(lCuts$x)) chrLevels else chrLevels[chrLevels %in% lState$levels]
  lPanelLevels <- if (is.null(lState$panel_by)) {
    list(NULL)
  } else if (!is.null(lCuts$panel)) {
    as.list(CutLevels("panel"))
  } else {
    as.list(Core_Levels(unlist(lapply(lFramed, function(dfData) Core_Text(dfData$panel)))))
  }

  lPanels <- list()
  for (iVisit in seq_along(lVisits)) {
    dfData <- lFramed[[iVisit]]
    for (strPanel in lPanelLevels) {
      bIn <- if (is.null(lState$group_by)) rep(TRUE, nrow(dfData)) else Core_Text(dfData$x) %in% chrShown
      if (!is.null(strPanel)) {
        bIn <- bIn & Core_Text(dfData$panel) == strPanel
      }
      dfRecords <- dfData[bIn, , drop = FALSE]
      rownames(dfRecords) <- NULL
      lPanels[[length(lPanels) + 1L]] <- list(
        visit = lVisits[[iVisit]], panel = strPanel, records = dfRecords,
        # A cut's groups in this panel's rows, low to high.
        cut_groups = if (is.null(lCuts$x)) NULL else chrShown[chrShown %in% dfRecords$x]
      )
    }
  }
  list(
    panels = lPanels, groups = if (is.null(lState$group_by)) 0L else length(chrShown),
    levels = chrShown, baseline_visits = chrBaselineVisits
  )
}

# What the chart asks R for one panel: the function, the arguments and the
# identity of the rows, with the number of rows. This is bio.viz's
# `group_comparison_key()` recipe (docs/group-comparison.md, "Stored results,
# from R"). A member of the identity that is not set is left out, and a member
# that is a list is an unnamed list, so it is written as a JSON array whatever
# its length.
GroupComparison_Key <- function(dfRecords, lView) {
  chrGroups <- Core_SortText(unique(Core_Text(dfRecords$x)))
  lDataId <- list(chart = "group-comparison", measure = lView$measure, value_type = lView$value_type)
  if (!is.null(lView$visit)) lDataId$visit <- lView$visit
  if (!is.null(lView$baseline_visits)) lDataId$baseline_visits <- as.list(lView$baseline_visits)
  lDataId$baseline_stat <- lView$baseline_stat
  if (!is.null(lView$group_by)) lDataId$group_by <- lView$group_by
  lDataId$groups <- as.list(chrGroups)
  if (!is.null(lView$color_by)) lDataId$color_by <- lView$color_by
  if (!is.null(lView$panel_by)) {
    lDataId$panel_by <- lView$panel_by
    lDataId$panel <- lView$panel
  }
  if (length(lView$filters) > 0L) {
    lDataId$filters <- lapply(lView$filters, as.list)
  }
  if (identical(lView$y_scale, "log")) lDataId$positive_only <- TRUE
  # The rows were framed with unscheduled visits among the results: a baseline
  # found among them need not be the one found without them.
  if (isTRUE(lView$unscheduled_visits)) lDataId$unscheduled_visits <- TRUE
  lArgs <- list(
    strValueCol = "y",
    strGroupCol = "x",
    strMethod = lView$test,
    # Pairs exist only among more than two groups.
    bPairwise = isTRUE(lView$pairwise) && length(chrGroups) > 2L
  )
  # A cut's groups are handed to R low to high, the order they are drawn in, so
  # R names them in that order and a difference is the lower group less the
  # higher. A column's are left to R, which sorts them as the identity does.
  if (!is.null(lView$cut_groups)) {
    lArgs$chrGroups <- as.list(lView$cut_groups)
  }
  list(name = lView$statistic, args = lArgs, dataId = lDataId, rows = nrow(dfRecords))
}

# Every request the chart makes of one biomarker's visits, a panel each: one
# per panel that has a test. Each is the key, with `data`, the panel's rows.
GroupComparison_Requests <- function(dfResults, dfParticipants, lConfig, lState,
                                     lDrawn = Chart_Unscheduled(dfResults, lConfig)) {
  if (is.null(lConfig$statistic) || is.null(lState$group_by) || is.null(lState$measure) || is.na(lState$measure)) {
    return(list())
  }
  if (lState$value_type != "baseline" && length(lState$visits) == 0L) {
    return(list())
  }
  lModel <- GroupComparison_Panels(dfResults, dfParticipants, lConfig, lState, lDrawn = lDrawn)
  strTest <- GroupComparison_FitTest(lState$test, lModel$groups)
  if (is.null(strTest) || strTest == "none") {
    return(list())
  }
  lRequests <- list()
  for (lPanel in lModel$panels) {
    # A test compares two or more groups: a panel with fewer asks R nothing.
    if (length(unique(Core_Text(lPanel$records$x))) < 2L) {
      next
    }
    lKey <- GroupComparison_Key(lPanel$records, list(
      statistic = lConfig$statistic, test = strTest, pairwise = lState$pairwise,
      measure = lState$measure, value_type = lState$value_type, visit = lPanel$visit,
      baseline_visits = lConfig$baseline_visits, baseline_stat = lConfig$baseline_stat,
      group_by = lState$group_by, color_by = lState$color_by, panel_by = lState$panel_by, panel = lPanel$panel,
      cut_groups = lPanel$cut_groups,
      filters = Chart_FiltersInForce(lState$filters), y_scale = lState$y_scale,
      unscheduled_visits = lDrawn$drawn
    ))
    lRequests[[length(lRequests) + 1L]] <- c(lKey, list(data = lPanel$records))
  }
  lRequests
}

# What the chart asks R under one biomarker over time: the function, the
# arguments and the identity of the rows, with the number of rows. This is
# bio.viz's `group_comparison_by_visit_key()` recipe (docs/group-comparison.md,
# "Stored results, from R"), and the chart's own `overTimeRequest`
# (src/group-comparison/statistic.js). The rows are long, one per participant
# and visit tested. The identity has no colour and no panel: the picture takes
# neither.
GroupComparison_OverTimeKey <- function(dfRows, lView) {
  chrGroups <- Core_SortText(unique(Core_Text(dfRows$x)))
  lDataId <- list(chart = "group-comparison", measure = lView$measure, value_type = lView$value_type)
  lDataId$visits <- as.list(lView$visits)
  if (!is.null(lView$baseline_visits)) lDataId$baseline_visits <- as.list(lView$baseline_visits)
  lDataId$baseline_stat <- lView$baseline_stat
  if (!is.null(lView$group_by)) lDataId$group_by <- lView$group_by
  lDataId$groups <- as.list(chrGroups)
  if (length(lView$filters) > 0L) {
    lDataId$filters <- lapply(lView$filters, as.list)
  }
  if (identical(lView$y_scale, "log")) lDataId$positive_only <- TRUE
  if (isTRUE(lView$unscheduled_visits)) lDataId$unscheduled_visits <- TRUE
  lArgs <- list(
    strValueCol = "y",
    strGroupCol = "x",
    strByCol = "visit",
    strMethod = lView$test,
    # The visits in visit order: R would sort their names otherwise.
    chrBy = as.list(lView$visits),
    strPAdjust = lView$visit_adjustment
  )
  # A cut's groups are handed to R low to high: every group drawn, at any
  # visit, the same at every visit. A column's are left to R.
  if (!is.null(lView$cut_groups)) {
    lArgs$chrGroups <- as.list(lView$cut_groups)
  }
  list(name = lView$statistic_by_visit, args = lArgs, dataId = lDataId, rows = nrow(dfRows))
}

# The request the chart makes under one biomarker over time, once for each
# adjustment named: the key, with `data`, the rows of every visit tested, each
# the row the single-visit view hands R for that visit with the visit named in
# `visit`. The baseline visit of a change is drawn and not tested, so its rows
# are not sent. An empty list when the chart draws another level for what the
# controls are set to, or asks R for nothing there.
GroupComparison_OverTimeRequests <- function(dfResults, dfParticipants, lConfig, lState,
                                             chrAdjustments = lState$visit_adjustment,
                                             lDrawn = Chart_Unscheduled(dfResults, lConfig)) {
  # The row of tests is there when both functions are named.
  if (is.null(lConfig$statistic) || is.null(lConfig$statistic_by_visit) || is.null(lState$group_by)) {
    return(list())
  }
  chrOffered <- GroupComparison_MeasureVisits(lDrawn$results, lConfig, lState$measure)
  if (GroupComparison_Level(lState, chrOffered) != "over-time") {
    return(list())
  }
  # The picture takes no second grouping and no panels.
  lPicture <- lState
  lPicture["color_by"] <- list(NULL)
  lPicture["panel_by"] <- list(NULL)
  lModel <- GroupComparison_Panels(dfResults, dfParticipants, lConfig, lPicture, bKeepBaseline = TRUE, lDrawn = lDrawn)
  strTest <- GroupComparison_FitTest(lState$test, lModel$groups)
  if (is.null(strTest) || strTest == "none") {
    return(list())
  }
  chrVisits <- unlist(lapply(lModel$panels, function(lPanel) lPanel$visit))
  chrTested <- GroupComparison_VisitsDrawn(chrVisits, lState$value_type, lModel$baseline_visits)
  lTested <- Filter(function(lPanel) lPanel$visit %in% chrTested, lModel$panels)
  dfRows <- do.call(rbind, lapply(lTested, function(lPanel) {
    dfRecords <- lPanel$records
    dfRecords$visit <- rep(lPanel$visit, nrow(dfRecords))
    dfRecords
  }))
  rownames(dfRows) <- NULL
  lapply(chrAdjustments, function(strAdjustment) {
    lKey <- GroupComparison_OverTimeKey(dfRows, list(
      statistic_by_visit = lConfig$statistic_by_visit, test = strTest, visit_adjustment = strAdjustment,
      measure = lState$measure, value_type = lState$value_type, visits = chrTested,
      baseline_visits = lConfig$baseline_visits, baseline_stat = lConfig$baseline_stat,
      group_by = lState$group_by,
      cut_groups = if (Chart_IsCut(lState$group_by)) lModel$levels else NULL,
      filters = Chart_FiltersInForce(lState$filters), y_scale = lState$y_scale,
      unscheduled_visits = lDrawn$drawn
    ))
    c(lKey, list(data = dfRows))
  })
}

# The stored results a page ships. The trend tiles ask R for nothing, so
# nothing is stored for them. For each biomarker the Biomarker control offers,
# the page holds R's answer to what the chart asks at the two levels a reader
# reaches from a tile, at the widget's settings:
#
# - one biomarker over time: the test under every visit, in one request,
#   unadjusted, and under the adjustment the setting `visit_adjustment` names
#   when it names one, so the switch between the two is answered either way;
# - one visit alone, for each visit the biomarker has: the panel a click on
#   that visit opens, or its panels when the settings name a panel column;
# - and, when the settings name some of the visits, the panels the page opens
#   that biomarker on.
#
# Each is the request the chart makes with `value`, what the statistics
# function returned for the request's rows, and `data`, those rows.
GroupComparison_StoredResults <- function(dfResults, dfParticipants, lConfig) {
  if (is.null(lConfig$statistic) || nrow(dfResults) == 0L) {
    return(list())
  }
  lDrawn <- Chart_Unscheduled(dfResults, lConfig)
  chrAll <- GroupComparison_Visits(dfResults, lConfig, lDrawn)$all
  # What the controls open on is the same for every biomarker but the biomarker.
  lOpening <- GroupComparison_State(dfResults, dfParticipants, lConfig)
  # The p-values as R gives them, and as the setting has R adjust them.
  chrAdjustments <- unique(c("none", lConfig$visit_adjustment))
  lRequests <- list()
  for (strMeasure in Chart_Measures(dfResults, lConfig)) {
    lState <- lOpening
    lState$measure <- strMeasure
    chrOffered <- GroupComparison_MeasureVisits(lDrawn$results, lConfig, strMeasure)
    # Over time, as the page opens the biomarker when it opens it so, and as
    # All in the Visit control and the way back from a visit lead to it: every
    # visit the biomarker has is chosen, in visit order.
    lEvery <- lState
    lEvery$visits <- chrAll[chrAll %in% c(lState$visits, chrOffered)]
    for (lView in list(lState, lEvery)) {
      lRequests <- c(lRequests, GroupComparison_OverTimeRequests(dfResults, dfParticipants, lConfig, lView, chrAdjustments, lDrawn))
    }
    # The panels the page opens the biomarker on, when it opens it on some of
    # its visits, or on a baseline value, which has no visit.
    if (GroupComparison_Level(lState, chrOffered) == "visits") {
      lRequests <- c(lRequests, GroupComparison_Requests(dfResults, dfParticipants, lConfig, lState, lDrawn))
    }
    # Each visit alone, as a click on it over time opens it.
    if (lState$value_type != "baseline") {
      for (strVisit in chrOffered) {
        lOne <- lState
        lOne$visits <- strVisit
        lRequests <- c(lRequests, GroupComparison_Requests(dfResults, dfParticipants, lConfig, lOne, lDrawn))
      }
    }
  }
  Chart_Answer(lRequests, list(
    Analyze_GroupDifference = Analyze_GroupDifference,
    Analyze_GroupDifferenceBy = Analyze_GroupDifferenceBy
  ))
}
