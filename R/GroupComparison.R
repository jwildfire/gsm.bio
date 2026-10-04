# What bio.viz's group comparison chart draws and asks R, worked out in R.
#
# The chart asks its connection to R for one test per panel, and finds a stored
# result by the function's name, its arguments and the identity of the panel's
# rows together (bio.viz, docs/group-comparison.md, "What R is asked"). To ship
# R's answers with a page, Widget_GroupComparison() has to know, before there is
# a page, which panels the chart will draw and what it will ask for each. This
# file follows the chart's own code for that (bio.viz, src/group-comparison/):
# what the controls offer, what they open on, the panels, and the request.
#
# It computes no statistic. The rows of a panel come from Core_Frame(), and the
# answer for a panel is Analyze_GroupDifference() on those rows. Nothing here is
# exported.

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
  measures = NULL,
  groups = NULL,
  max_levels = 12L,
  filters = NULL,
  statistic = "Analyze_GroupDifference",
  test = "t",
  pairwise = FALSE,
  studyday_col = NULL,
  normal_col_high = NULL,
  normal_col_low = NULL
)

chrGroupComparisonTests <- c("t", "wilcoxon", "anova", "kruskal", "none")

# The one R function the widget stores results of.
strGroupComparisonStatistic <- "Analyze_GroupDifference"

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
  if (!(is.logical(lConfig$pairwise) && length(lConfig$pairwise) == 1L && !is.na(lConfig$pairwise))) {
    Core_Stop("Setting 'pairwise' must be TRUE or FALSE")
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
  # No visit named is every visit, and no level named is none drawn, as bio.viz
  # reads them.
  for (strKey in c("baseline_visits", "visits", "levels", "measures")) {
    lConfig[strKey] <- list(Chart_Names(lConfig[[strKey]], strKey, bEmpty = strKey %in% c("visits", "levels")))
  }
  for (strKey in c("groups", "filters")) {
    lConfig[strKey] <- list(Chart_Fields(lConfig[[strKey]], strKey))
  }
  lConfig
}

# The visits the Visit control offers, and the ones it opens on: the visits in
# the setting `visits` that the table has, or, when the setting names none,
# every visit.
GroupComparison_Visits <- function(dfResults, lConfig) {
  chrAll <- Core_Visits(dfResults, Chart_CoreSettings(lConfig))
  chrAsked <- lConfig$visits[lConfig$visits %in% chrAll]
  list(all = chrAll, start = if (length(chrAsked) > 0L) chrAsked else chrAll)
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
# otherwise NULL, which is the overview of every biomarker: the overview prints
# no test and asks R for nothing.
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
# them. Returns a list with `panels`, each a list of `visit`, `panel` (the
# level of the panel column, or NULL) and `records` (the panel's rows, one per
# participant: the id, `y`, `x`, and `color` and `panel` when set), and
# `groups`, how many levels of the group are drawn.
GroupComparison_Panels <- function(dfResults, dfParticipants, lConfig, lState) {
  lCore <- Chart_CoreSettings(lConfig)
  strIdCol <- lConfig$id_col

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
  chrDrawn <- GroupComparison_VisitsDrawn(lState$visits, lState$value_type, chrBaselineVisits)
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
  list(panels = lPanels, groups = if (is.null(lState$group_by)) 0L else length(chrShown))
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

# Every request the chart makes in one view: one per panel that has a test.
# Each is the key, with `data`, the panel's rows.
GroupComparison_Requests <- function(dfResults, dfParticipants, lConfig, lState) {
  if (is.null(lConfig$statistic) || is.null(lState$group_by) || is.null(lState$measure) || is.na(lState$measure)) {
    return(list())
  }
  if (lState$value_type != "baseline" && length(lState$visits) == 0L) {
    return(list())
  }
  lModel <- GroupComparison_Panels(dfResults, dfParticipants, lConfig, lState)
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
      filters = Chart_FiltersInForce(lState$filters), y_scale = lState$y_scale
    ))
    lRequests[[length(lRequests) + 1L]] <- c(lKey, list(data = lPanel$records))
  }
  lRequests
}

# The stored results a page ships: for each biomarker the Biomarker control
# offers, R's answer for each panel the chart draws when that biomarker is
# opened at the widget's settings. Each is the request the chart makes for the
# panel with `value`, what Analyze_GroupDifference() returned for the panel's
# rows, and `data`, those rows.
#
# The chart opens on an overview of every biomarker unless `start_value` names
# one; the overview prints no test, and a reader opens a biomarker from it. So
# the results are stored for every biomarker, whichever the page opens on.
GroupComparison_StoredResults <- function(dfResults, dfParticipants, lConfig) {
  if (is.null(lConfig$statistic) || nrow(dfResults) == 0L) {
    return(list())
  }
  # What the controls open on is the same for every biomarker but the biomarker.
  lOpening <- GroupComparison_State(dfResults, dfParticipants, lConfig)
  lRequests <- list()
  for (strMeasure in Chart_Measures(dfResults, lConfig)) {
    lState <- lOpening
    lState$measure <- strMeasure
    lRequests <- c(lRequests, GroupComparison_Requests(dfResults, dfParticipants, lConfig, lState))
  }
  Chart_Answer(lRequests, list(Analyze_GroupDifference = Analyze_GroupDifference))
}
