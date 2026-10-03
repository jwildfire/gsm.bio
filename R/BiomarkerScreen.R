# What bio.viz's biomarker screen draws and asks R, worked out in R.
#
# The screen asks its connection to R once, for every row, and a click on a row
# opens that biomarker's own chart in place, the group comparison for a
# difference or the association scatter for a correlation, which asks for its
# own statistics through the same connection (bio.viz,
# docs/biomarker-screen.md, "What R is asked" and "A row opens its chart"). To
# ship R's answers with a page, Widget_BiomarkerScreen() has to know, before
# there is a page, the screen the chart will draw and the chart each of its rows
# opens. This file follows the chart's own code for that (bio.viz,
# src/biomarker-screen/): the rows, what the chart opens on, the frame and the
# one request; and what the screen hands the chart a row opens, whose own rules
# are R/GroupComparison.R and R/AssociationScatter.R. What every chart shares is
# R/chart.R.
#
# It computes no statistic. The frame comes from Core_Frame(), one column per
# biomarker and none of them required, so R counts who each row has; the answer
# is Analyze_Screen() on that frame. Nothing here is exported.

# The settings of the chart that R reads, with the chart's own defaults. The
# chart has more (the order of its rows, how many a page shows); those pass
# through to the page untouched. tests/testthat/test-BiomarkerScreen.R holds
# these defaults to the vendored bundle's.
lBiomarkerScreenDefaults <- list(
  id_col = "USUBJID",
  measure_col = "TEST",
  value_col = "STRESN",
  visit_col = "VISIT",
  visit_order_col = "VISITNUM",
  unit_col = "STRESU",
  participant_id_col = NULL,
  baseline_visits = NULL,
  baseline_stat = "mean",
  comparison = "difference",
  visit = NULL,
  value_type = "raw",
  group_by = NULL,
  levels = NULL,
  with = NULL,
  method = "pearson",
  adjustment = "BH",
  measures = NULL,
  groups = NULL,
  numbers = NULL,
  max_levels = 12L,
  filters = NULL,
  statistic = "Analyze_Screen",
  group_comparison = NULL,
  association_scatter = NULL
)

# The R function the widget stores the screen's result of.
strBiomarkerScreenStatistic <- "Analyze_Screen"

# The settings the screen sets on the chart a row opens, over what the page set
# for that chart (bio.viz, src/biomarker-screen.js, `openRow`).
lBiomarkerScreenCarried <- list(
  group_comparison = c(
    "start_value", "visits", "value_type", "group_by", "levels", "test",
    "connection", "waiting_note", "filters", "back"
  ),
  association_scatter = c("x", "y", "method", "connection", "waiting_note", "filters", "back")
)
lBiomarkerScreenCarriedWords <- list(
  group_comparison = "its biomarker, its visit, its value type, its two groups and Welch's test",
  association_scatter = "its biomarker and its variable, its method"
)

chrBiomarkerScreenValueWords <- c(
  raw = "Result", baseline = "Baseline value", change = "Change from baseline",
  fold_change = "Fold change from baseline", percent_change = "Percent change from baseline"
)

# The settings R reads, in full: the caller's over the chart's defaults, each
# checked, in the forms the functions below read them.
BiomarkerScreen_Settings <- function(lSettings = list()) {
  lConfig <- Core_Overlay(
    lBiomarkerScreenDefaults, lSettings[intersect(names(lSettings), names(lBiomarkerScreenDefaults))]
  )
  IsName <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue))
  for (strKey in c("id_col", "measure_col", "value_col", "visit_col")) {
    if (!IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single column name (a character string)")
    }
  }
  for (strKey in c("visit_order_col", "unit_col", "participant_id_col", "group_by", "visit")) {
    if (!is.null(lConfig[[strKey]]) && !IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single name (a character string), or NULL")
    }
  }
  Choice <- function(strKey, chrChoices) {
    if (!IsName(lConfig[[strKey]]) || !lConfig[[strKey]] %in% chrChoices) {
      Core_Stop("Setting '", strKey, "' must be one of ", paste(chrChoices, collapse = ", "))
    }
  }
  Choice("comparison", c("difference", "correlation"))
  Choice("value_type", chrCoreValueTypes)
  Choice("method", chrAssociationScatterMethods)
  Choice("adjustment", c("BH", "holm"))
  Choice("baseline_stat", chrCoreBaselineStats)
  nMaxLevels <- lConfig$max_levels
  if (!(is.numeric(nMaxLevels) && length(nMaxLevels) == 1L && !is.na(nMaxLevels) && nMaxLevels >= 1 && nMaxLevels == round(nMaxLevels))) {
    Core_Stop("Setting 'max_levels' must be a whole number, one or more")
  }
  if (!is.null(lConfig$statistic) && !identical(lConfig$statistic, strBiomarkerScreenStatistic)) {
    Core_Stop(
      "Setting 'statistic' must be '", strBiomarkerScreenStatistic, "', the function the widget stores results of, ",
      "or NULL for no rows"
    )
  }
  for (strKey in c("group_comparison", "association_scatter")) {
    lOpened <- lConfig[[strKey]]
    if (!is.null(lOpened) && (!is.list(lOpened) || is.data.frame(lOpened) ||
      (length(lOpened) > 0L && (is.null(names(lOpened)) || !all(nzchar(names(lOpened))))))) {
      Core_Stop("Setting '", strKey, "' must be a named list of settings for the chart a row opens, or NULL")
    }
    # What the screen carries across is the screen's to set.
    chrCarried <- intersect(names(lOpened), lBiomarkerScreenCarried[[strKey]])
    if (length(chrCarried) > 0L) {
      Core_Stop(
        "Setting '", strKey, "' cannot name ", paste0("'", chrCarried, "'", collapse = ", "),
        ": the screen hands the chart a row opens ", lBiomarkerScreenCarriedWords[[strKey]],
        ", its filters, the connection and the way back"
      )
    }
  }
  lConfig["levels"] <- list(Chart_Names(lConfig$levels, "levels"))
  if (!is.null(lConfig$levels) && length(lConfig$levels) != 2L) {
    Core_Stop("Setting 'levels' must name two groups, the first and the second, or be NULL")
  }
  lConfig["with"] <- list(AssociationScatter_Axis(lConfig$with, "with"))
  for (strKey in c("baseline_visits", "measures")) {
    lConfig[strKey] <- list(Chart_Names(lConfig[[strKey]], strKey))
  }
  for (strKey in c("groups", "numbers", "filters")) {
    lConfig[strKey] <- list(Chart_Fields(lConfig[[strKey]], strKey))
  }
  lConfig
}

# A variable by name, as a column of the frame is named: a participant-level
# column by its name, a biomarker by its name and visit, with its value type
# where it is not the result.
BiomarkerScreen_VariableName <- function(lAxis) {
  if (!is.null(lAxis$col)) {
    return(lAxis$col)
  }
  if (lAxis$value == "baseline") {
    return(paste0(lAxis$measure, ", baseline value"))
  }
  if (lAxis$value == "raw") {
    return(paste(lAxis$measure, "at", lAxis$visit))
  }
  paste0(lAxis$measure, ", ", tolower(chrBiomarkerScreenValueWords[[lAxis$value]]), " at ", lAxis$visit)
}

# What the controls open on: the settings, where the tables have what they name.
BiomarkerScreen_State <- function(dfResults, dfParticipants, lConfig) {
  chrMeasures <- Chart_Measures(dfResults, lConfig)
  chrVisits <- Core_Visits(dfResults, Chart_CoreSettings(lConfig))
  dfCategories <- Chart_Categories(dfResults, dfParticipants, lConfig)
  chrNumbers <- Chart_Numbers(dfResults, dfParticipants, lConfig)
  # The columns of groups a difference can compare: those with two groups or more.
  chrGroupColumns <- Filter(function(strCol) {
    length(Chart_ColumnLevels(dfResults, dfParticipants, strCol)) >= 2L
  }, dfCategories$value_col)
  strGroupBy <- if (!is.null(lConfig$group_by) && lConfig$group_by %in% chrGroupColumns) {
    lConfig$group_by
  } else {
    Core_First(unlist(chrGroupColumns))
  }
  if (length(strGroupBy) == 0L) strGroupBy <- NULL
  # The two groups: the ones chosen when the column has both, else its first two.
  chrOffered <- if (is.null(strGroupBy)) character(0) else Chart_ColumnLevels(dfResults, dfParticipants, strGroupBy)
  chrLevels <- if (!is.null(lConfig$levels) && all(lConfig$levels %in% chrOffered)) {
    lConfig$levels
  } else if (length(chrOffered) >= 2L) {
    chrOffered[1:2]
  } else {
    character(0)
  }
  lOffered <- list(measures = chrMeasures, visits = chrVisits, numbers = chrNumbers)
  lWith <- if (AssociationScatter_Offered(lConfig$with, lOffered)) {
    lConfig$with
  } else if (length(chrNumbers) > 0L) {
    list(col = chrNumbers[1L])
  } else if (length(chrMeasures) > 0L && length(chrVisits) > 0L) {
    list(measure = chrMeasures[1L], value = "raw", visit = chrVisits[1L])
  } else {
    NULL
  }
  list(
    comparison = lConfig$comparison,
    visit = if (!is.null(lConfig$visit) && lConfig$visit %in% chrVisits) lConfig$visit else Core_First(chrVisits),
    value_type = lConfig$value_type,
    group_by = strGroupBy,
    levels = chrLevels,
    with = lWith,
    method = lConfig$method,
    adjustment = lConfig$adjustment,
    filters = Chart_Filters(dfParticipants, lConfig, dfCategories),
    measures = chrMeasures
  )
}

# The biomarkers the screen has rows for, in the Biomarker list's order, each
# at the one visit with the one value type, as the settings write a variable.
# A biomarker that is the variable every row is correlated with is not a row of
# its own. None, with no screen to draw: a change at the one baseline visit, a
# difference with no column of two groups, a correlation with nothing to
# correlate with.
BiomarkerScreen_Rows <- function(dfResults, lConfig, lState) {
  strValue <- lState$value_type
  lCore <- Chart_CoreSettings(lConfig)
  chrBaseline <- if (strValue %in% c("change", "fold_change", "percent_change")) {
    if (is.null(lCore$baseline_visits)) Core_First(Core_Visits(dfResults, lCore)) else lCore$baseline_visits
  } else {
    character(0)
  }
  if (length(chrBaseline) == 1L && identical(chrBaseline, lState$visit)) {
    return(list())
  }
  if (lState$comparison == "difference") {
    if (is.null(lState$group_by) || length(lState$levels) != 2L) {
      return(list())
    }
  } else if (is.null(lState$with)) {
    return(list())
  }
  lRows <- list()
  for (strMeasure in lState$measures) {
    lAxis <- list(measure = strMeasure, value = strValue)
    if (strValue != "baseline") lAxis$visit <- lState$visit
    if (lState$comparison == "correlation" && identical(lAxis, lState$with)) {
      next
    }
    lRows[[strMeasure]] <- lAxis
  }
  lRows
}

# The frame the screen hands R: one row per participant who has at least one
# of the biomarkers, after the filters, with the id, one column per biomarker
# named by the biomarker, NA where the participant has no value, and beside them
# the column of groups for a difference or the variable every row is correlated
# with, named as BiomarkerScreen_VariableName() names it. None of the variables
# is required: R counts who each row has.
BiomarkerScreen_Frame <- function(dfResults, dfParticipants, lConfig, lState, lRows) {
  lKept <- Chart_KeepFiltered(dfResults, dfParticipants, lConfig, lState$filters)
  if (length(lRows) == 0L || nrow(lKept$results) == 0L) {
    return(NULL)
  }
  strExtra <- if (lState$comparison == "difference") lState$group_by else BiomarkerScreen_VariableName(lState$with)
  lExtra <- if (lState$comparison == "difference") list(col = lState$group_by) else AssociationScatter_Variable(lState$with)
  # A column of the frame named as another would be two columns of one name.
  chrNames <- c(lConfig$id_col, names(lRows), strExtra)
  if (anyDuplicated(chrNames) > 0L) {
    return(NULL)
  }
  lVariables <- c(lapply(lRows, AssociationScatter_Variable), stats::setNames(list(lExtra), strExtra))
  dfData <- Core_Frame(
    lKept$results, lKept$participants, lVariables,
    c(Chart_CoreSettings(lConfig), list(required = character(0)))
  )$data
  # A participant with none of the biomarkers gives no row anything.
  dfData <- dfData[rowSums(!is.na(dfData[names(lRows)])) > 0L, , drop = FALSE]
  rownames(dfData) <- NULL
  list(data = dfData, extra = strExtra)
}

# What the chart asks R for the screen: one request, whatever the number of
# rows. This is bio.viz's `biomarker_screen_key()` recipe
# (docs/biomarker-screen.md, "Stored results, from R").
BiomarkerScreen_Request <- function(dfFrame, lView) {
  lDataId <- list(chart = "biomarker-screen", value_type = lView$value_type)
  if (!is.null(lView$visit)) lDataId$visit <- lView$visit
  if (!is.null(lView$baseline_visits)) lDataId$baseline_visits <- as.list(lView$baseline_visits)
  lDataId$baseline_stat <- lView$baseline_stat
  if (identical(lView$comparison, "correlation")) lDataId$with <- lView$with
  if (length(lView$filters) > 0L) {
    lDataId$filters <- lapply(lView$filters, as.list)
  }
  lArgs <- list(chrCols = as.list(lView$biomarkers), strComparison = lView$comparison)
  if (identical(lView$comparison, "difference")) {
    lArgs$strGroupCol <- lView$group_by
    lArgs$chrGroups <- as.list(lView$groups)
  } else {
    lArgs$strWithCol <- lView$with_name
    lArgs$strCorMethod <- lView$method
  }
  lArgs$strPAdjust <- lView$adjustment
  list(name = lView$statistic, args = lArgs, dataId = lDataId, rows = nrow(dfFrame), data = dfFrame)
}

# The request the chart makes for its screen in one view, or none when there is
# no screen to draw.
BiomarkerScreen_Requests <- function(dfResults, dfParticipants, lConfig, lState) {
  if (is.null(lConfig$statistic) || nrow(dfResults) == 0L || length(lState$measures) == 0L) {
    return(list())
  }
  lRows <- BiomarkerScreen_Rows(dfResults, lConfig, lState)
  lFrame <- BiomarkerScreen_Frame(dfResults, dfParticipants, lConfig, lState, lRows)
  if (is.null(lFrame) || nrow(lFrame$data) == 0L) {
    return(list())
  }
  list(BiomarkerScreen_Request(lFrame$data, list(
    statistic = lConfig$statistic, comparison = lState$comparison, value_type = lState$value_type,
    visit = if (lState$value_type == "baseline") NULL else lState$visit,
    baseline_visits = lConfig$baseline_visits, baseline_stat = lConfig$baseline_stat,
    with = lState$with, with_name = lFrame$extra, filters = Chart_FiltersInForce(lState$filters),
    biomarkers = names(lRows), group_by = lState$group_by, groups = lState$levels,
    method = lState$method, adjustment = lState$adjustment
  )))
}

# The settings the screen hands the chart a row opens: its column and baseline
# settings, `unit_col`, `measures`, `max_levels` and `groups` (and `numbers`
# for the scatter); then what the page set for that chart; then what the screen
# carries across. For a difference the group comparison opens on the biomarker,
# at the screen's one visit, with its value type, its column of groups and its
# two groups, and Welch's test. For a correlation the association scatter opens
# with the biomarker along the bottom and the fixed variable up the side, with
# the same method. Both get the filters as they are set; the connection and the
# way back are the page's, and ask R for nothing.
BiomarkerScreen_OpenedSettings <- function(lConfig, lState, lFilterSpecs, strBiomarker, lAxis) {
  lSettings <- c(Chart_CoreSettings(lConfig), lConfig[c("unit_col", "measures", "max_levels", "groups")])
  bDifference <- lState$comparison == "difference"
  if (!bDifference) {
    lSettings["numbers"] <- list(lConfig$numbers)
  }
  lPage <- if (bDifference) lConfig$group_comparison else lConfig$association_scatter
  for (strName in names(lPage)) {
    lSettings[strName] <- list(lPage[[strName]])
  }
  if (bDifference) {
    lSettings$start_value <- strBiomarker
    lSettings["visits"] <- list(if (lState$value_type == "baseline") NULL else lState$visit)
    lSettings$value_type <- lState$value_type
    lSettings$group_by <- lState$group_by
    lSettings$levels <- lState$levels
    lSettings$test <- "t"
  } else {
    lSettings$x <- lAxis
    lSettings$y <- lState$with
    lSettings$method <- lState$method
  }
  lSettings$filters <- lapply(lFilterSpecs, function(lSpec) {
    lSpec["start"] <- list(lState$filters[[lSpec$value_col]])
    lSpec$all <- TRUE
    lSpec
  })
  lSettings
}

# What the chart a row opens asks R when it opens, for one row.
BiomarkerScreen_OpenedRequests <- function(dfResults, dfParticipants, lConfig, lState, lFilterSpecs, strBiomarker, lAxis) {
  lSettings <- BiomarkerScreen_OpenedSettings(lConfig, lState, lFilterSpecs, strBiomarker, lAxis)
  if (lState$comparison == "difference") {
    lOpened <- GroupComparison_Settings(lSettings)
    GroupComparison_Requests(dfResults, dfParticipants, lOpened, GroupComparison_State(dfResults, dfParticipants, lOpened))
  } else {
    lOpened <- AssociationScatter_Settings(lSettings)
    AssociationScatter_Requests(dfResults, dfParticipants, lOpened, AssociationScatter_State(dfResults, dfParticipants, lOpened))
  }
}

# The stored results a page ships: R's answer for the screen the settings open
# on, and, for every row of it, exactly what the chart that row opens asks of R
# when it opens, keyed as that chart keys it.
BiomarkerScreen_StoredResults <- function(dfResults, dfParticipants, lConfig) {
  if (nrow(dfResults) == 0L) {
    return(list())
  }
  lState <- BiomarkerScreen_State(dfResults, dfParticipants, lConfig)
  lRequests <- BiomarkerScreen_Requests(dfResults, dfParticipants, lConfig, lState)
  if (length(lRequests) > 0L) {
    lFilterSpecs <- Chart_FilterSpecs(dfResults, dfParticipants, lConfig)
    lRows <- BiomarkerScreen_Rows(dfResults, lConfig, lState)
    for (strBiomarker in names(lRows)) {
      lRequests <- c(lRequests, BiomarkerScreen_OpenedRequests(
        dfResults, dfParticipants, lConfig, lState, lFilterSpecs, strBiomarker, lRows[[strBiomarker]]
      ))
    }
  }
  Chart_Answer(lRequests, list(
    Analyze_Screen = Analyze_Screen, Analyze_GroupDifference = Analyze_GroupDifference,
    Analyze_Correlation = Analyze_Correlation, Analyze_Fit = Analyze_Fit
  ))
}
