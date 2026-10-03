# What bio.viz's association scatter draws and asks R, worked out in R.
#
# The chart asks its connection to R for one correlation coefficient per panel
# and, when its fitted line is a linear fit or a smooth, one line per panel, and
# finds a stored result by the function's name, its arguments and the identity
# of the panel's rows together (bio.viz, docs/association-scatter.md, "What R is
# asked"). To ship R's answers with a page, Widget_AssociationScatter() has to
# know, before there is a page, which rows the chart will draw in each panel and
# how it will ask for them. This file follows the chart's own code for that
# (bio.viz, src/association-scatter/): the variables an axis can take, what the
# chart opens on, the panels, and the two requests. What every chart shares is
# in R/chart.R.
#
# It computes no statistic. The rows of a panel come from Core_Frame(), with x
# and y as two variables, and the answers are Analyze_Correlation() and
# Analyze_Fit() on those rows. On a logarithmic axis R is handed the base-10
# logarithm of the value, as the chart hands it: the logarithm is taken here, in
# R, from the value itself. Nothing here is exported.

# The settings of the chart that R reads, with the chart's own defaults. The
# chart has more (its listing, the participant profile); those pass through to
# the page untouched. tests/testthat/test-AssociationScatter.R holds these
# defaults to the vendored bundle's.
lAssociationScatterDefaults <- list(
  id_col = "USUBJID",
  measure_col = "TEST",
  value_col = "STRESN",
  visit_col = "VISIT",
  visit_order_col = "VISITNUM",
  unit_col = "STRESU",
  participant_id_col = NULL,
  baseline_visits = NULL,
  baseline_stat = "mean",
  x = NULL,
  y = NULL,
  color_by = NULL,
  panel_by = NULL,
  x_scale = "linear",
  y_scale = "linear",
  fit = "none",
  measures = NULL,
  numbers = NULL,
  groups = NULL,
  max_levels = 12L,
  filters = NULL,
  statistic = "Analyze_Correlation",
  method = "pearson",
  fit_statistic = "Analyze_Fit",
  studyday_col = NULL,
  normal_col_high = NULL,
  normal_col_low = NULL
)

# The coefficients and the lines R can be asked for. `none` and `identity` are
# lines too, and ask R for nothing: y = x is not an estimate.
chrAssociationScatterMethods <- c("pearson", "spearman")
chrAssociationScatterFits <- c("linear", "smooth")

# The R functions the widget stores results of: the coefficient's, and the
# fitted line's.
strAssociationScatterStatistic <- "Analyze_Correlation"
strAssociationScatterFitStatistic <- "Analyze_Fit"

# An axis as the settings write a variable, in full: list(measure, value,
# visit), without `visit` for a baseline value, or list(col). It is what the
# chart writes into the identity of the rows. NULL stays NULL.
AssociationScatter_Axis <- function(lSpec, strSetting) {
  if (is.null(lSpec)) {
    return(NULL)
  }
  if (!is.list(lSpec) || is.data.frame(lSpec)) {
    Core_Stop(
      "Setting '", strSetting, "' must be a variable: list(measure, visit, value) for a biomarker at a visit, ",
      "or list(col) for a participant-level number; or NULL"
    )
  }
  # A column on an axis is a number, so `type` need not be written.
  if (!is.null(lSpec$col)) {
    lSpec$type <- "number"
  }
  lVariable <- tryCatch(Core_Variable(lSpec), error = function(cndError) {
    Core_Stop("Setting '", strSetting, "': ", conditionMessage(cndError))
  })
  if (lVariable$kind == "column") {
    return(list(col = lVariable$col))
  }
  lAxis <- list(measure = lVariable$measure, value = lVariable$value)
  if (!is.null(lVariable$visit)) {
    lAxis$visit <- lVariable$visit
  }
  lAxis
}

# An axis as the frame takes a variable. A column on an axis is read as a
# number: a participant whose value is not one is left out, and counted.
AssociationScatter_Variable <- function(lAxis) {
  if (!is.null(lAxis$col)) {
    return(list(col = lAxis$col, type = "number"))
  }
  if (lAxis$value == "baseline") {
    return(list(measure = lAxis$measure, value = "baseline"))
  }
  list(measure = lAxis$measure, visit = lAxis$visit, value = lAxis$value)
}

# The settings R reads, in full: the caller's over the chart's defaults, each
# checked, in the forms the functions below read them.
AssociationScatter_Settings <- function(lSettings = list()) {
  lConfig <- Core_Overlay(
    lAssociationScatterDefaults, lSettings[intersect(names(lSettings), names(lAssociationScatterDefaults))]
  )
  IsName <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue))
  for (strKey in c("id_col", "measure_col", "value_col", "visit_col")) {
    if (!IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single column name (a character string)")
    }
  }
  for (strKey in c(
    "visit_order_col", "unit_col", "participant_id_col", "color_by", "panel_by", "studyday_col",
    "normal_col_high", "normal_col_low"
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
  Choice("x_scale", c("linear", "log"))
  Choice("y_scale", c("linear", "log"))
  Choice("fit", c("none", "identity", chrAssociationScatterFits))
  Choice("method", chrAssociationScatterMethods)
  Choice("baseline_stat", chrCoreBaselineStats)
  nMaxLevels <- lConfig$max_levels
  if (!(is.numeric(nMaxLevels) && length(nMaxLevels) == 1L && !is.na(nMaxLevels) && nMaxLevels >= 1 && nMaxLevels == round(nMaxLevels))) {
    Core_Stop("Setting 'max_levels' must be a whole number, one or more")
  }
  if (!is.null(lConfig$statistic) && !identical(lConfig$statistic, strAssociationScatterStatistic)) {
    Core_Stop(
      "Setting 'statistic' must be '", strAssociationScatterStatistic, "', the function the widget stores results of, ",
      "or NULL for no statistics line"
    )
  }
  if (!is.null(lConfig$fit_statistic) && !identical(lConfig$fit_statistic, strAssociationScatterFitStatistic)) {
    Core_Stop(
      "Setting 'fit_statistic' must be '", strAssociationScatterFitStatistic, "', the function the widget stores results of, ",
      "or NULL for no linear fit and no smooth"
    )
  }
  for (strKey in c("x", "y")) {
    lConfig[strKey] <- list(AssociationScatter_Axis(lConfig[[strKey]], strKey))
  }
  for (strKey in c("baseline_visits", "measures")) {
    lConfig[strKey] <- list(Chart_Names(lConfig[[strKey]], strKey))
  }
  for (strKey in c("numbers", "groups", "filters")) {
    lConfig[strKey] <- list(Chart_Fields(lConfig[[strKey]], strKey))
  }
  lConfig
}

# Whether the tables can draw an axis: its biomarker and its visit are in the
# results table, or its column is one of the numbers offered.
AssociationScatter_Offered <- function(lAxis, lOffered) {
  if (is.null(lAxis)) {
    return(FALSE)
  }
  if (!is.null(lAxis$col)) {
    return(lAxis$col %in% lOffered$numbers)
  }
  if (!lAxis$measure %in% lOffered$measures) {
    return(FALSE)
  }
  lAxis$value == "baseline" || lAxis$visit %in% lOffered$visits
}

# The two axes the chart opens on. Each is the one the settings name, when the
# tables can draw it. Otherwise: on x, the first biomarker at the first visit;
# on y, the second biomarker at the first visit, or with one biomarker the same
# biomarker at the second visit, or with one visit the first participant-level
# number, or the same variable as x.
AssociationScatter_OpeningAxes <- function(lConfig, lOffered) {
  At <- function(strMeasure, strVisit) list(measure = strMeasure, value = "raw", visit = strVisit)
  Named <- function(strKey) {
    if (AssociationScatter_Offered(lConfig[[strKey]], lOffered)) lConfig[[strKey]] else NULL
  }
  chrMeasures <- lOffered$measures
  chrVisits <- lOffered$visits
  chrNumbers <- lOffered$numbers
  lFirst <- if (length(chrMeasures) > 0L && length(chrVisits) > 0L) {
    At(chrMeasures[1L], chrVisits[1L])
  } else if (length(chrNumbers) > 0L) {
    list(col = chrNumbers[1L])
  } else {
    NULL
  }
  lX <- Named("x")
  if (is.null(lX)) {
    lX <- lFirst
  }
  lY <- Named("y")
  if (is.null(lY) && !is.null(lX)) {
    lY <- if (length(chrMeasures) > 1L && length(chrVisits) > 0L) {
      At(chrMeasures[2L], chrVisits[1L])
    } else if (length(chrMeasures) > 0L && length(chrVisits) > 1L) {
      At(chrMeasures[1L], chrVisits[2L])
    } else if (length(chrNumbers) > 0L) {
      list(col = chrNumbers[1L])
    } else {
      lX
    }
  }
  list(x = lX, y = lY)
}

# What the chart opens on: the settings, where the tables have what they name.
AssociationScatter_State <- function(dfResults, dfParticipants, lConfig) {
  dfCategories <- Chart_Categories(dfResults, dfParticipants, lConfig)
  Has <- function(strColumn) !is.null(strColumn) && strColumn %in% dfCategories$value_col
  lAxes <- AssociationScatter_OpeningAxes(lConfig, list(
    measures = Chart_Measures(dfResults, lConfig),
    visits = Core_Visits(dfResults, Chart_CoreSettings(lConfig)),
    numbers = Chart_Numbers(dfResults, dfParticipants, lConfig)
  ))
  list(
    x = lAxes$x,
    y = lAxes$y,
    color_by = if (Has(lConfig$color_by)) lConfig$color_by else NULL,
    panel_by = if (Has(lConfig$panel_by)) lConfig$panel_by else NULL,
    x_scale = lConfig$x_scale,
    y_scale = lConfig$y_scale,
    fit = lConfig$fit,
    method = lConfig$method,
    filters = Chart_Filters(dfParticipants, lConfig, dfCategories)
  )
}

# Whether an axis is a change, a fold change or a percent change read at the
# one baseline visit, where it is the same for every participant by definition.
# There is nothing to relate there, and the chart draws nothing and asks nothing.
AssociationScatter_FlatAtBaseline <- function(lAxis, dfResults, lConfig) {
  if (!is.null(lAxis$col) || !lAxis$value %in% c("change", "fold_change", "percent_change")) {
    return(FALSE)
  }
  lCore <- Chart_CoreSettings(lConfig)
  chrBaseline <- if (is.null(lCore$baseline_visits)) Core_First(Core_Visits(dfResults, lCore)) else lCore$baseline_visits
  length(chrBaseline) == 1L && identical(chrBaseline, lAxis$visit)
}

# The panels the chart draws in one view, each with its rows: one per level of
# the panel column, or one in all, as the chart's `buildScatter` makes them.
# Returns a list of panels, each a list of `panel` (the level of the panel
# column, or NULL) and `records` (the panel's rows, one per participant: the id,
# `x`, `y`, and `color` and `panel` when set, the values themselves).
AssociationScatter_Panels <- function(dfResults, dfParticipants, lConfig, lState) {
  # The filters choose participants; the results of the others are set aside
  # before the frame is made, so they are not counted as missing from it.
  lKept <- Chart_KeepFiltered(dfResults, dfParticipants, lConfig, lState$filters)
  if (nrow(lKept$results) == 0L) {
    return(list())
  }
  lVariables <- list(x = AssociationScatter_Variable(lState$x), y = AssociationScatter_Variable(lState$y))
  if (!is.null(lState$color_by)) lVariables$color <- list(col = lState$color_by)
  if (!is.null(lState$panel_by)) lVariables$panel <- list(col = lState$panel_by)
  dfData <- Core_Frame(lKept$results, lKept$participants, lVariables, Chart_CoreSettings(lConfig))$data
  # A logarithmic axis has no place for zero or less.
  if (lState$x_scale == "log") {
    dfData <- dfData[dfData$x > 0, , drop = FALSE]
  }
  if (lState$y_scale == "log") {
    dfData <- dfData[dfData$y > 0, , drop = FALSE]
  }
  if (nrow(dfData) == 0L) {
    return(list())
  }
  rownames(dfData) <- NULL
  if (is.null(lState$panel_by)) {
    return(list(list(panel = NULL, records = dfData)))
  }
  lapply(Core_Levels(dfData$panel), function(strPanel) {
    dfRecords <- dfData[Core_Text(dfData$panel) == strPanel, , drop = FALSE]
    rownames(dfRecords) <- NULL
    list(panel = strPanel, records = dfRecords)
  })
}

# The identity of one panel's rows: what was drawn, by the settings' own names.
# This is bio.viz's `association_scatter_id()` recipe
# (docs/association-scatter.md, "Stored results, from R"). A member that is not
# set is left out, and a member that is a list is an unnamed list, so it is
# written as a JSON array whatever its length. The scale is part of it: a
# result computed on a logarithm never answers for the values themselves.
AssociationScatter_Id <- function(dfRows, lView) {
  lDataId <- list(chart = "association-scatter", x = lView$x, y = lView$y)
  if (!is.null(lView$baseline_visits)) lDataId$baseline_visits <- as.list(lView$baseline_visits)
  lDataId$baseline_stat <- lView$baseline_stat
  if (!is.null(lView$color_by)) {
    lDataId$color_by <- lView$color_by
    lDataId$groups <- as.list(sort(unique(Core_Text(dfRows$color)), method = "radix"))
  }
  if (!is.null(lView$panel_by)) {
    lDataId$panel_by <- lView$panel_by
    lDataId$panel <- lView$panel
  }
  if (length(lView$filters) > 0L) {
    lDataId$filters <- lapply(lView$filters, as.list)
  }
  if (identical(lView$x_scale, "log")) lDataId$x_scale <- "log"
  if (identical(lView$y_scale, "log")) lDataId$y_scale <- "log"
  lDataId
}

# What the chart asks R for one panel: `strName` is the function and
# `strMethod` its method, a coefficient's (`pearson`, `spearman`) or a fitted
# line's (`linear`, `smooth`). The two are asked the same way, on the same rows
# and under the same identity: bio.viz's `association_scatter_key()` and
# `association_scatter_fit_key()`.
AssociationScatter_Key <- function(dfRows, lView, strName, strMethod) {
  lArgs <- list(strXCol = "x", strYCol = "y", strMethod = strMethod)
  # Within each colour as well, when there is a colour.
  if (!is.null(lView$color_by)) lArgs$strGroupCol <- "color"
  list(name = strName, args = lArgs, dataId = AssociationScatter_Id(dfRows, lView), rows = nrow(dfRows))
}

# Every request the chart makes in one view: for each panel with a participant
# in it, its coefficient and, when the fitted line is a linear fit or a smooth,
# its line. Each is the key, with `data`, the rows R is handed: on a logarithmic
# axis the base-10 logarithm of the value, taken here from the value itself.
AssociationScatter_Requests <- function(dfResults, dfParticipants, lConfig, lState) {
  if (nrow(dfResults) == 0L || is.null(lState$x) || is.null(lState$y)) {
    return(list())
  }
  if (AssociationScatter_FlatAtBaseline(lState$x, dfResults, lConfig) ||
    AssociationScatter_FlatAtBaseline(lState$y, dfResults, lConfig)) {
    return(list())
  }
  bLine <- lState$fit %in% chrAssociationScatterFits && !is.null(lConfig$fit_statistic)
  lRequests <- list()
  for (lPanel in AssociationScatter_Panels(dfResults, dfParticipants, lConfig, lState)) {
    dfRows <- lPanel$records
    if (lState$x_scale == "log") dfRows$x <- log10(dfRows$x)
    if (lState$y_scale == "log") dfRows$y <- log10(dfRows$y)
    lView <- list(
      x = lState$x, y = lState$y, baseline_visits = lConfig$baseline_visits, baseline_stat = lConfig$baseline_stat,
      color_by = lState$color_by, panel_by = lState$panel_by, panel = lPanel$panel,
      filters = Chart_FiltersInForce(lState$filters), x_scale = lState$x_scale, y_scale = lState$y_scale
    )
    if (!is.null(lConfig$statistic)) {
      lKey <- AssociationScatter_Key(dfRows, lView, lConfig$statistic, lState$method)
      lRequests[[length(lRequests) + 1L]] <- c(lKey, list(data = dfRows))
    }
    if (bLine) {
      lKey <- AssociationScatter_Key(dfRows, lView, lConfig$fit_statistic, lState$fit)
      lRequests[[length(lRequests) + 1L]] <- c(lKey, list(data = dfRows))
    }
  }
  lRequests
}

# The stored results a page ships: R's answers for the view the widget's
# settings open on, for each of its panels. Beside the coefficient and the
# fitted line the settings open on, the other coefficient and the other line
# are stored too: the Method control and the Fitted line control change what is
# asked of the same rows and nothing else, so they are the two a reader of a
# saved page can move and still be answered. Another variable, colour, panel,
# filter or scale is other rows, and is not computed.
AssociationScatter_StoredResults <- function(dfResults, dfParticipants, lConfig) {
  lOpening <- AssociationScatter_State(dfResults, dfParticipants, lConfig)
  lRequests <- list()
  for (strMethod in unique(c(lOpening$method, chrAssociationScatterMethods))) {
    for (strFit in unique(c(intersect(lOpening$fit, chrAssociationScatterFits), chrAssociationScatterFits))) {
      lState <- lOpening
      lState$method <- strMethod
      lState$fit <- strFit
      lRequests <- c(lRequests, AssociationScatter_Requests(dfResults, dfParticipants, lConfig, lState))
    }
  }
  Chart_Answer(lRequests, list(Analyze_Correlation = Analyze_Correlation, Analyze_Fit = Analyze_Fit))
}
