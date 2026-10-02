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

# A column name, or list(value_col, label), as list(value_col, label, ...).
GroupComparison_Field <- function(xSpec, strSetting) {
  if (is.character(xSpec) && length(xSpec) == 1L && !is.na(xSpec) && nzchar(trimws(xSpec))) {
    return(list(value_col = xSpec, label = xSpec))
  }
  strCol <- if (is.list(xSpec)) xSpec$value_col else NULL
  if (is.character(strCol) && length(strCol) == 1L && !is.na(strCol) && nzchar(trimws(strCol))) {
    if (!(is.character(xSpec$label) && length(xSpec$label) == 1L && nzchar(trimws(xSpec$label)))) {
      xSpec$label <- strCol
    }
    return(xSpec)
  }
  Core_Stop("Setting '", strSetting, "' holds something that is not a column name or list(value_col, label)")
}

# A setting that lists columns: one name, several names, one spec or a list of
# specs, as a list of specs. NULL stays NULL.
GroupComparison_Fields <- function(xValue, strSetting) {
  if (is.null(xValue)) {
    return(NULL)
  }
  lEntries <- if (is.character(xValue)) {
    as.list(xValue)
  } else if (is.list(xValue) && !is.null(xValue$value_col)) {
    list(xValue)
  } else {
    xValue
  }
  lapply(lEntries, GroupComparison_Field, strSetting = strSetting)
}

# A setting that lists names: one or several, as distinct text. NULL stays NULL.
GroupComparison_Names <- function(xValue, strSetting) {
  if (is.null(xValue)) {
    return(NULL)
  }
  xValues <- unlist(xValue)
  if (length(xValues) == 0L || !(is.character(xValues) || is.numeric(xValues)) || anyNA(xValues) || !all(nzchar(trimws(as.character(xValues))))) {
    Core_Stop("Setting '", strSetting, "' must be a name, or several names")
  }
  unique(Core_Text(xValues))
}

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
    "visit_order_col", "unit_col", "participant_id_col", "start_value", "group_by", "color_by", "panel_by",
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
  for (strKey in c("baseline_visits", "visits", "levels", "measures")) {
    lConfig[strKey] <- list(GroupComparison_Names(lConfig[[strKey]], strKey))
  }
  for (strKey in c("groups", "filters")) {
    lConfig[strKey] <- list(GroupComparison_Fields(lConfig[[strKey]], strKey))
  }
  lConfig
}

# The settings the frame reads, taken from the chart's.
GroupComparison_CoreSettings <- function(lConfig) {
  lConfig[c(
    "id_col", "measure_col", "value_col", "visit_col", "visit_order_col", "participant_id_col",
    "baseline_visits", "baseline_stat"
  )]
}

# The biomarkers the Biomarker control offers: the configured list in its
# order, keeping the ones the table has, or every biomarker in the table by
# name.
GroupComparison_Measures <- function(dfResults, lConfig) {
  chrPresent <- Core_Levels(dfResults[[lConfig$measure_col]])
  if (is.null(lConfig$measures)) {
    return(chrPresent)
  }
  chrListed <- lConfig$measures[lConfig$measures %in% chrPresent]
  if (length(chrListed) > 0L) chrListed else chrPresent
}

# The visits the Visit control offers, and the ones it opens on: the visits in
# the setting `visits` that the table has, or, when the setting names none,
# every visit.
GroupComparison_Visits <- function(dfResults, lConfig) {
  chrAll <- Core_Visits(dfResults, GroupComparison_CoreSettings(lConfig))
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

# The columns that can make a group, a colour or a panel: columns that hold a
# category, which is to say at most `max_levels` different values. With a
# participant table: its columns, other than the id. Without one, and after
# them: the columns carried on the results rows, other than the ones the
# settings map, that hold one value for each participant. The setting `groups`,
# when given, is the list, and nothing is worked out.
GroupComparison_Categories <- function(dfResults, dfParticipants, lConfig) {
  if (!is.null(lConfig$groups)) {
    return(data.frame(
      value_col = vapply(lConfig$groups, function(lSpec) lSpec$value_col, character(1)),
      table = "given", stringsAsFactors = FALSE
    ))
  }
  FewEnough <- function(chrText) {
    nLevels <- length(unique(chrText))
    nLevels > 0L && nLevels <= lConfig$max_levels
  }
  chrColumns <- character(0)
  chrTables <- character(0)
  if (!is.null(dfParticipants) && nrow(dfParticipants) > 0L) {
    strIdCol <- if (is.null(lConfig$participant_id_col)) lConfig$id_col else lConfig$participant_id_col
    for (strName in setdiff(names(dfParticipants), strIdCol)) {
      xColumn <- dfParticipants[[strName]]
      if (FewEnough(Core_Text(xColumn)[!Core_IsBlank(xColumn)])) {
        chrColumns <- c(chrColumns, strName)
        chrTables <- c(chrTables, "participants")
      }
    }
  }
  chrMapped <- unlist(lConfig[c(
    "id_col", "measure_col", "value_col", "visit_col", "visit_order_col", "unit_col", "studyday_col",
    "normal_col_high", "normal_col_low"
  )])
  chrRowId <- Core_Text(dfResults[[lConfig$id_col]])
  for (strName in setdiff(names(dfResults), c(chrMapped, chrColumns))) {
    xColumn <- dfResults[[strName]]
    bFilled <- !Core_IsBlank(xColumn)
    chrText <- Core_Text(xColumn)[bFilled]
    chrId <- chrRowId[bFilled]
    # One value for each participant, or it is not a participant-level column.
    bConstant <- all(chrText == chrText[match(chrId, chrId)])
    if (bConstant && FewEnough(chrText[!duplicated(chrId)])) {
      chrColumns <- c(chrColumns, strName)
      chrTables <- c(chrTables, "results")
    }
  }
  data.frame(value_col = chrColumns, table = chrTables, stringsAsFactors = FALSE)
}

# What each filter opens on. There are filters only with a participant table:
# they choose participants. The setting `filters`, when given, is the list,
# kept to the columns the participant table has; otherwise every category
# column of the participant table is a filter. A filter opens on its `start`,
# and one with none lets every participant through. Returns a named list of
# column to the value, or values, the filter opens on; NULL for a filter that
# opens on all.
GroupComparison_Filters <- function(dfParticipants, lConfig, dfCategories) {
  if (is.null(dfParticipants) || nrow(dfParticipants) == 0L) {
    return(list())
  }
  lSpecs <- if (!is.null(lConfig$filters)) {
    Filter(function(lSpec) lSpec$value_col %in% names(dfParticipants), lConfig$filters)
  } else {
    lapply(dfCategories$value_col[dfCategories$table == "participants"], function(strCol) list(value_col = strCol))
  }
  lState <- list()
  for (lSpec in lSpecs) {
    xStart <- unlist(lSpec$start)
    bStarted <- length(xStart) > 0L && !(length(xStart) == 1L && (is.na(xStart) || identical(as.character(xStart), "")))
    lState[lSpec$value_col] <- list(if (!bStarted) {
      NULL
    } else if (isTRUE(lSpec$multiple)) {
      Core_Text(xStart)
    } else {
      Core_Text(xStart)[1L]
    })
  }
  lState
}

# What the chart opens on: the settings, where the tables have what they name.
# The biomarker is the one `start_value` names when the table has it, and
# otherwise NULL, which is the overview of every biomarker: the overview prints
# no test and asks R for nothing.
GroupComparison_State <- function(dfResults, dfParticipants, lConfig) {
  dfCategories <- GroupComparison_Categories(dfResults, dfParticipants, lConfig)
  chrMeasures <- GroupComparison_Measures(dfResults, lConfig)
  Has <- function(strColumn) !is.null(strColumn) && strColumn %in% dfCategories$value_col
  list(
    measure = if (!is.null(lConfig$start_value) && lConfig$start_value %in% chrMeasures) lConfig$start_value else NULL,
    visits = GroupComparison_Visits(dfResults, lConfig)$start,
    value_type = lConfig$value_type,
    group_by = if (Has(lConfig$group_by)) {
      lConfig$group_by
    } else if (nrow(dfCategories) > 0L) {
      dfCategories$value_col[1L]
    } else {
      NULL
    },
    levels = lConfig$levels,
    color_by = if (Has(lConfig$color_by)) lConfig$color_by else NULL,
    panel_by = if (Has(lConfig$panel_by)) lConfig$panel_by else NULL,
    y_scale = lConfig$y_scale,
    test = lConfig$test,
    pairwise = lConfig$pairwise,
    filters = GroupComparison_Filters(dfParticipants, lConfig, dfCategories)
  )
}

# The filters in force, each as the values it lets through, as text sorted by
# code point. A filter set to all is not in force and is left out.
GroupComparison_FiltersInForce <- function(lFilters) {
  lInForce <- list()
  for (strColumn in names(lFilters)) {
    chrValues <- lFilters[[strColumn]]
    if (length(chrValues) > 0L) {
      lInForce[[strColumn]] <- sort(unique(Core_Text(chrValues)), method = "radix")
    }
  }
  lInForce
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
  lCore <- GroupComparison_CoreSettings(lConfig)
  strIdCol <- lConfig$id_col

  # The filters choose participants; the results of the others are set aside
  # before the frame is made, so they are not counted as missing from it.
  dfKept <- if (!is.null(dfParticipants) && nrow(dfParticipants) > 0L) dfParticipants else NULL
  dfRows <- dfResults
  if (!is.null(dfKept)) {
    strParticipantIdCol <- if (is.null(lConfig$participant_id_col)) strIdCol else lConfig$participant_id_col
    for (strColumn in names(lState$filters)) {
      chrSelection <- lState$filters[[strColumn]]
      if (length(chrSelection) > 0L) {
        # A participant with nothing in the column is compared as the chart
        # compares it, by the text of nothing.
        chrText <- Core_Text(dfKept[[strColumn]])
        chrText[is.na(chrText)] <- "null"
        dfKept <- dfKept[chrText %in% chrSelection, , drop = FALSE]
      }
    }
    dfRows <- dfResults[Core_Text(dfResults[[strIdCol]]) %in% Core_Text(dfKept[[strParticipantIdCol]]), , drop = FALSE]
  }

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
  lFramed <- lapply(lVisits, function(strVisit) {
    lVariables <- list(y = if (bNeedsVisit) {
      list(measure = lState$measure, visit = strVisit, value = lState$value_type)
    } else {
      list(measure = lState$measure, value = "baseline")
    })
    if (!is.null(lState$group_by)) lVariables$x <- list(col = lState$group_by)
    if (!is.null(lState$color_by)) lVariables$color <- list(col = lState$color_by)
    if (!is.null(lState$panel_by)) lVariables$panel <- list(col = lState$panel_by)
    dfData <- Core_Frame(dfRows, dfKept, lVariables, lCore)$data
    # A logarithmic axis has no place for zero or less.
    if (lState$y_scale == "log") {
      dfData <- dfData[dfData$y > 0, , drop = FALSE]
    }
    dfData
  })

  # The levels drawn are the ones in the rows of every panel together.
  chrLevels <- if (is.null(lState$group_by)) {
    character(0)
  } else {
    Core_Levels(unlist(lapply(lFramed, function(dfData) Core_Text(dfData$x))))
  }
  chrShown <- if (is.null(lState$levels)) chrLevels else chrLevels[chrLevels %in% lState$levels]
  lPanelLevels <- if (is.null(lState$panel_by)) {
    list(NULL)
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
      lPanels[[length(lPanels) + 1L]] <- list(visit = lVisits[[iVisit]], panel = strPanel, records = dfRecords)
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
  chrGroups <- sort(unique(Core_Text(dfRecords$x)), method = "radix")
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
  list(
    name = lView$statistic,
    args = list(
      strValueCol = "y",
      strGroupCol = "x",
      strMethod = lView$test,
      # Pairs exist only among more than two groups.
      bPairwise = isTRUE(lView$pairwise) && length(chrGroups) > 2L
    ),
    dataId = lDataId,
    rows = nrow(dfRecords)
  )
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
      filters = GroupComparison_FiltersInForce(lState$filters), y_scale = lState$y_scale
    ))
    lRequests[[length(lRequests) + 1L]] <- c(lKey, list(data = lPanel$records))
  }
  lRequests
}

# A key as text, the same for two keys that differ only in the order their
# members were written: how the chart's connection tells stored results apart.
GroupComparison_KeyText <- function(xValue) {
  if (is.list(xValue)) {
    if (is.null(names(xValue))) {
      return(paste0("[", paste(vapply(xValue, GroupComparison_KeyText, character(1)), collapse = ","), "]"))
    }
    chrNames <- sort(names(xValue), method = "radix")
    return(paste0("{", paste0(chrNames, ":", vapply(xValue[chrNames], GroupComparison_KeyText, character(1)), collapse = ","), "}"))
  }
  paste0(typeof(xValue), "(", paste(as.character(xValue), collapse = ","), ")")
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
  lStored <- list()
  for (strMeasure in GroupComparison_Measures(dfResults, lConfig)) {
    lState <- lOpening
    lState$measure <- strMeasure
    for (lRequest in GroupComparison_Requests(dfResults, dfParticipants, lConfig, lState)) {
      strKey <- GroupComparison_KeyText(lRequest[c("name", "args", "dataId")])
      if (is.null(lStored[[strKey]])) {
        lRequest$value <- do.call(Analyze_GroupDifference, c(list(lRequest$data), lRequest$args))
        lStored[[strKey]] <- lRequest
      }
    }
  }
  unname(lStored)
}
