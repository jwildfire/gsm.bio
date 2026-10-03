# What bio.viz's correlation matrix draws and asks R, worked out in R.
#
# The grid asks its connection to R once, for every coefficient of the grid,
# and a click on a cell opens the association scatter for that pair in place,
# which asks for its own statistics through the same connection (bio.viz,
# docs/correlation-matrix.md, "What R is asked" and "A cell opens the
# scatter"). To ship R's answers with a page, Widget_CorrelationMatrix() has to
# know, before there is a page, the grid the chart will draw and the scatter
# each of its cells opens. This file follows the chart's own code for that
# (bio.viz, src/correlation-matrix/): the grid's variables, what the chart opens
# on, the frame and the one request; and what the grid hands the scatter, whose
# own rules are R/AssociationScatter.R. What every chart shares is R/chart.R.
#
# It computes no statistic. The frame comes from Core_Frame(), one column per
# variable and none of them required, so a participant missing some of the
# values is kept with a gap and R counts each pair's complete rows; the answer
# is Analyze_CorrelationMatrix() on that frame. Nothing here is exported.

# The settings of the chart that R reads, with the chart's own defaults. The
# chart has more (how it draws); those pass through to the page untouched.
# tests/testthat/test-CorrelationMatrix.R holds these defaults to the vendored
# bundle's.
lCorrelationMatrixDefaults <- list(
  id_col = "USUBJID",
  measure_col = "TEST",
  value_col = "STRESN",
  visit_col = "VISIT",
  visit_order_col = "VISITNUM",
  unit_col = "STRESU",
  participant_id_col = NULL,
  baseline_visits = NULL,
  baseline_stat = "mean",
  mode = "biomarkers",
  visit = NULL,
  biomarkers = NULL,
  measure = NULL,
  visits = NULL,
  value_type = "raw",
  limit = 12L,
  measures = NULL,
  max_levels = 12L,
  filters = NULL,
  statistic = "Analyze_CorrelationMatrix",
  method = "pearson",
  min_pairs = NULL,
  scatter = NULL
)

# The R function the widget stores the grid's result of.
strCorrelationMatrixStatistic <- "Analyze_CorrelationMatrix"

# The settings R reads, in full: the caller's over the chart's defaults, each
# checked, in the forms the functions below read them.
CorrelationMatrix_Settings <- function(lSettings = list()) {
  lConfig <- Core_Overlay(
    lCorrelationMatrixDefaults, lSettings[intersect(names(lSettings), names(lCorrelationMatrixDefaults))]
  )
  IsName <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue))
  IsWhole <- function(xValue, nLeast) {
    is.numeric(xValue) && length(xValue) == 1L && !is.na(xValue) && xValue >= nLeast && xValue == round(xValue)
  }
  for (strKey in c("id_col", "measure_col", "value_col", "visit_col")) {
    if (!IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single column name (a character string)")
    }
  }
  for (strKey in c("visit_order_col", "unit_col", "participant_id_col", "visit", "measure")) {
    if (!is.null(lConfig[[strKey]]) && !IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single name (a character string), or NULL")
    }
  }
  Choice <- function(strKey, chrChoices) {
    if (!IsName(lConfig[[strKey]]) || !lConfig[[strKey]] %in% chrChoices) {
      Core_Stop("Setting '", strKey, "' must be one of ", paste(chrChoices, collapse = ", "))
    }
  }
  Choice("mode", c("biomarkers", "visits"))
  Choice("value_type", chrCoreValueTypes)
  Choice("method", chrAssociationScatterMethods)
  Choice("baseline_stat", chrCoreBaselineStats)
  if (!IsWhole(lConfig$max_levels, 1)) {
    Core_Stop("Setting 'max_levels' must be a whole number, one or more")
  }
  # A grid has pairs only from two variables up.
  if (!IsWhole(lConfig$limit, 2)) {
    Core_Stop("Setting 'limit' must be a whole number, two or more")
  }
  # The minimum is R's to apply and R's to default: NULL sends R nothing.
  nMinPairs <- lConfig$min_pairs
  if (!is.null(nMinPairs)) {
    if (!(is.numeric(nMinPairs) && length(nMinPairs) == 1L && is.finite(nMinPairs) && nMinPairs > 0)) {
      Core_Stop("Setting 'min_pairs' must be a number above zero, or NULL for R's own minimum")
    }
    # Written as JSON writes it: a whole number is a whole number.
    if (nMinPairs == round(nMinPairs)) {
      lConfig$min_pairs <- as.integer(nMinPairs)
    }
  }
  if (!is.null(lConfig$statistic) && !identical(lConfig$statistic, strCorrelationMatrixStatistic)) {
    Core_Stop(
      "Setting 'statistic' must be '", strCorrelationMatrixStatistic, "', the function the widget stores results of, ",
      "or NULL for no coefficients"
    )
  }
  if (!is.null(lConfig$scatter)) {
    if (!is.list(lConfig$scatter) || is.data.frame(lConfig$scatter) ||
      (length(lConfig$scatter) > 0L && (is.null(names(lConfig$scatter)) || !all(nzchar(names(lConfig$scatter)))))) {
      Core_Stop("Setting 'scatter' must be a named list of settings for the association scatter, or NULL")
    }
    # What the grid carries across is the grid's to set.
    chrCarried <- intersect(names(lConfig$scatter), c("x", "y", "method", "filters", "connection", "back"))
    if (length(chrCarried) > 0L) {
      Core_Stop(
        "Setting 'scatter' cannot name ", paste0("'", chrCarried, "'", collapse = ", "),
        ": the grid hands the scatter its pair, its method, its filters, the connection and the way back"
      )
    }
  }
  for (strKey in c("baseline_visits", "biomarkers", "visits", "measures")) {
    lConfig[strKey] <- list(Chart_Names(lConfig[[strKey]], strKey))
  }
  lConfig["filters"] <- list(Chart_Fields(lConfig$filters, "filters"))
  lConfig
}

# What the controls open on: the settings, where the tables have what they name.
CorrelationMatrix_State <- function(dfResults, dfParticipants, lConfig) {
  chrMeasures <- Chart_Measures(dfResults, lConfig)
  chrVisits <- Core_Visits(dfResults, Chart_CoreSettings(lConfig))
  # NULL is every one the control offers.
  Among <- function(chrChosen, chrList) {
    chrKept <- chrChosen[chrChosen %in% chrList]
    if (length(chrKept) > 0L) chrKept else NULL
  }
  dfCategories <- Chart_Categories(dfResults, dfParticipants, lConfig[setdiff(names(lConfig), "groups")])
  list(
    mode = lConfig$mode,
    visit = if (!is.null(lConfig$visit) && lConfig$visit %in% chrVisits) lConfig$visit else Core_First(chrVisits),
    biomarkers = Among(lConfig$biomarkers, chrMeasures),
    measure = if (!is.null(lConfig$measure) && lConfig$measure %in% chrMeasures) lConfig$measure else Core_First(chrMeasures),
    visits = Among(lConfig$visits, chrVisits),
    value_type = lConfig$value_type,
    method = lConfig$method,
    min_pairs = lConfig$min_pairs,
    filters = Chart_Filters(dfParticipants, lConfig, dfCategories),
    offered = list(measures = chrMeasures, visits = chrVisits)
  )
}

# The variables the grid draws, in order, each as the settings write a
# variable: list(measure, value, visit), without `visit` for a baseline value.
# Across biomarkers they are the biomarkers chosen at one visit; across the
# visits of one biomarker, the visits chosen, in visit order, leaving out the
# one baseline visit of a change, a fold change or a percent change, where the
# value is the same for everyone. No more than `limit` are drawn. Fewer than two
# is no grid, and an empty list.
CorrelationMatrix_Variables <- function(dfResults, lConfig, lState) {
  strValue <- lState$value_type
  lCore <- Chart_CoreSettings(lConfig)
  chrBaseline <- if (strValue %in% c("change", "fold_change", "percent_change")) {
    if (is.null(lCore$baseline_visits)) Core_First(Core_Visits(dfResults, lCore)) else lCore$baseline_visits
  } else {
    character(0)
  }
  Flat <- function(strVisit) length(chrBaseline) == 1L && identical(chrBaseline, strVisit)
  At <- function(strMeasure, strVisit) {
    lAxis <- list(measure = strMeasure, value = strValue)
    if (strValue != "baseline") lAxis$visit <- strVisit
    lAxis
  }
  if (lState$mode == "visits") {
    # A baseline value has no visit: there is nothing to relate across visits.
    if (strValue == "baseline" || is.null(lState$measure)) {
      return(list())
    }
    chrOffered <- lState$offered$visits
    chrChosen <- if (is.null(lState$visits)) chrOffered else chrOffered[chrOffered %in% lState$visits]
    chrDrawn <- chrChosen[!vapply(chrChosen, Flat, logical(1))]
    lVariables <- lapply(CorrelationMatrix_First(chrDrawn, lConfig$limit), function(strVisit) At(lState$measure, strVisit))
  } else {
    if (strValue != "baseline" && (is.null(lState$visit) || Flat(lState$visit))) {
      return(list())
    }
    chrOffered <- lState$offered$measures
    chrChosen <- if (is.null(lState$biomarkers)) chrOffered else chrOffered[chrOffered %in% lState$biomarkers]
    lVariables <- lapply(CorrelationMatrix_First(chrChosen, lConfig$limit), function(strMeasure) At(strMeasure, lState$visit))
  }
  if (length(lVariables) < 2L) list() else unname(lVariables)
}

# The first `n` of several.
CorrelationMatrix_First <- function(xValues, n) {
  xValues[seq_len(min(as.integer(n), length(xValues)))]
}

# The frame the grid hands R: one row per participant who has at least one of
# the grid's values, after the filters, with the id and one column per
# variable, named v1, v2, ... in the grid's order, NA where the participant has
# no value. None of the variables is required, so a participant missing some of
# them is kept: which participants a pair has in common is R's to count.
CorrelationMatrix_Frame <- function(dfResults, dfParticipants, lConfig, lState, lVariables) {
  lKept <- Chart_KeepFiltered(dfResults, dfParticipants, lConfig, lState$filters)
  chrNames <- paste0("v", seq_along(lVariables))
  if (nrow(lKept$results) == 0L || length(lVariables) < 2L) {
    return(NULL)
  }
  lFrameVariables <- stats::setNames(lapply(lVariables, AssociationScatter_Variable), chrNames)
  dfData <- Core_Frame(
    lKept$results, lKept$participants, lFrameVariables,
    c(Chart_CoreSettings(lConfig), list(required = character(0)))
  )$data
  # A participant with none of the grid's values gives no pair anything.
  dfData <- dfData[rowSums(!is.na(dfData[chrNames])) > 0L, , drop = FALSE]
  rownames(dfData) <- NULL
  dfData
}

# What the chart asks R for the grid: one request, whatever the number of
# cells. This is bio.viz's `correlation_matrix_key()` recipe
# (docs/correlation-matrix.md, "Stored results, from R"). The minimum is sent
# only when the settings set one; with none, the key has none and R's own
# default applies.
CorrelationMatrix_Request <- function(dfFrame, lView) {
  lDataId <- list(chart = "correlation-matrix", variables = unname(lView$variables))
  if (!is.null(lView$baseline_visits)) lDataId$baseline_visits <- as.list(lView$baseline_visits)
  lDataId$baseline_stat <- lView$baseline_stat
  if (length(lView$filters) > 0L) {
    lDataId$filters <- lapply(lView$filters, as.list)
  }
  lArgs <- list(chrCols = as.list(paste0("v", seq_along(lView$variables))), strMethod = lView$method)
  if (!is.null(lView$min_pairs)) lArgs$nMinPairs <- lView$min_pairs
  list(name = lView$statistic, args = lArgs, dataId = lDataId, rows = nrow(dfFrame), data = dfFrame)
}

# The request the chart makes for its grid in one view, or none when there is
# no grid: fewer than two variables, no participant with a value, or no
# function named.
CorrelationMatrix_Requests <- function(dfResults, dfParticipants, lConfig, lState) {
  if (is.null(lConfig$statistic) || nrow(dfResults) == 0L) {
    return(list())
  }
  lVariables <- CorrelationMatrix_Variables(dfResults, lConfig, lState)
  dfFrame <- CorrelationMatrix_Frame(dfResults, dfParticipants, lConfig, lState, lVariables)
  if (is.null(dfFrame) || nrow(dfFrame) == 0L) {
    return(list())
  }
  list(CorrelationMatrix_Request(dfFrame, list(
    statistic = lConfig$statistic, method = lState$method, min_pairs = lState$min_pairs, variables = lVariables,
    baseline_visits = lConfig$baseline_visits, baseline_stat = lConfig$baseline_stat,
    filters = Chart_FiltersInForce(lState$filters)
  )))
}

# The settings the grid hands the association scatter a cell opens: the
# grid's column and baseline settings, `unit_col`, `measures` and `max_levels`;
# then what the page set for the scatter in `scatter`; then what the grid
# carries across: the pair (the column's variable on x, the row's on y), the
# method, and each filter opening on what the grid's is set to. The connection
# and the way back are the page's, and ask R for nothing.
CorrelationMatrix_ScatterSettings <- function(lConfig, lState, lFilterSpecs, lX, lY) {
  lSettings <- c(
    Chart_CoreSettings(lConfig),
    lConfig[c("unit_col", "measures", "max_levels")]
  )
  for (strName in names(lConfig$scatter)) {
    lSettings[strName] <- list(lConfig$scatter[[strName]])
  }
  lSettings$x <- lX
  lSettings$y <- lY
  lSettings$method <- lState$method
  lSettings$filters <- lapply(lFilterSpecs, function(lSpec) {
    lSpec["start"] <- list(lState$filters[[lSpec$value_col]])
    lSpec$all <- TRUE
    lSpec
  })
  lSettings
}

# The stored results a page ships: R's answer for the grid the settings open
# on, and, for every cell of it, exactly what the association scatter asks when
# a click on that cell opens it, keyed as the scatter keys it. A cell on either
# side of the diagonal opens its pair, with the column's variable on x, so each
# pair is two scatters, one each way round.
CorrelationMatrix_StoredResults <- function(dfResults, dfParticipants, lConfig) {
  if (nrow(dfResults) == 0L) {
    return(list())
  }
  lState <- CorrelationMatrix_State(dfResults, dfParticipants, lConfig)
  lRequests <- CorrelationMatrix_Requests(dfResults, dfParticipants, lConfig, lState)
  lVariables <- CorrelationMatrix_Variables(dfResults, lConfig, lState)
  if (length(lRequests) > 0L) {
    lFilterSpecs <- Chart_FilterSpecs(dfResults, dfParticipants, lConfig)
    for (iRow in seq_along(lVariables)) {
      for (iColumn in seq_along(lVariables)) {
        if (iRow == iColumn) next
        lScatter <- AssociationScatter_Settings(CorrelationMatrix_ScatterSettings(
          lConfig, lState, lFilterSpecs, lVariables[[iColumn]], lVariables[[iRow]]
        ))
        lRequests <- c(lRequests, AssociationScatter_Requests(
          dfResults, dfParticipants, lScatter, AssociationScatter_State(dfResults, dfParticipants, lScatter)
        ))
      }
    }
  }
  Chart_Answer(lRequests, list(
    Analyze_CorrelationMatrix = Analyze_CorrelationMatrix, Analyze_Correlation = Analyze_Correlation,
    Analyze_Fit = Analyze_Fit
  ))
}
