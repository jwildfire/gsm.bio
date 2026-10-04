# bio.viz's chart specifications (bio.viz#68; docs/output.md, "Specifications";
# src/shared/specification.js), read in R: what a chart draws, written as JSON
# data, so gsm.bio's batch runner can draw the same view as a static figure
# and a table. tests/testthat/test-batch.R holds the reader to bio.viz's own
# reader, run on the same specifications from the copied bundle: the format's
# rules case by case, and every setting of every chart given each of 21 values.
#
# Nothing in a specification is ever evaluated. It is read by jsonlite as
# data (text, numbers, true, false, null, lists and objects), and every value
# stays that: a title or a setting that looks like code is text. Nothing here
# calls eval(), parse() or anything that runs text.
#
# How the reader differs from bio.viz's, each on purpose:
# - The format's rules, and the shape of a list or object setting, are
#   refused in bio.viz's words. A value of a setting is checked by the chart's
#   own rules in R (R/GroupComparison.R and the files beside it) and, for the
#   settings only the browser reads, by the rules bio.viz's charts check them
#   by; a refusal of a value says it in R's words, which are not always
#   bio.viz's. Which values are accepted is the same.
# - A chart's `statistic`, and the scatter's `fit_statistic`, name the R
#   function the page asks. bio.viz takes any name; gsm.bio computes each
#   chart's statistics with one Analyze_*() function, and refuses any other
#   name with a sentence.
# - bio.viz's reader checks the depth of a specification given as an object
#   and not one given as text (bio.viz#74); gsm.bio checks both.

strSpecFormat <- "bio.viz specification"
nSpecVersion <- 1L
chrSpecPageSettings <- c("connection", "back")
chrSpecNoColumn <- c("__proto__", "constructor", "prototype")
nSpecMostNested <- 64L
chrSpecCharts <- c(
  "group-comparison", "association-scatter", "correlation-matrix", "biomarker-screen", "cross-tab", "stratified-survival"
)

# jsonlite reads JSON, and the batch runner needs it; it is suggested, not
# imported, as the figures' ggplot2 is.
Spec_HasJsonlite <- function() {
  requireNamespace("jsonlite", quietly = TRUE)
}

Spec_NeedJsonlite <- function(strFunction) {
  if (!Spec_HasJsonlite()) {
    stop(strFunction, "() reads specifications with jsonlite, which is not installed. Install it with install.packages(\"jsonlite\").", call. = FALSE)
  }
  invisible(NULL)
}

# Each chart's settings, by name, as the copied schema lists them
# (inst/specification/specification.schema.json, written by bio.viz from each
# chart's own defaults).
Spec_SettingNames <- function() {
  Spec_NeedJsonlite("Spec_SettingNames")
  strSchema <- system.file("specification", "specification.schema.json", package = "gsm.bio")
  lSchema <- jsonlite::read_json(strSchema, simplifyVector = FALSE)
  lNames <- list()
  for (lCase in lSchema$allOf) {
    strChart <- lCase[["if"]]$properties$chart$const
    lNames[[strChart]] <- unlist(lCase[["then"]]$properties$settings$propertyNames$enum)
  }
  lNames[chrSpecCharts]
}

# Whether a value read from JSON is an object (a named list) or a list (an
# unnamed one).
Spec_IsObject <- function(xValue) is.list(xValue) && !is.null(names(xValue))
Spec_IsArray <- function(xValue) is.list(xValue) && is.null(names(xValue))
Spec_IsText <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && !Core_IsBlank(xValue)

# A value as JSON writes it, for a sentence: JSON.stringify's, and `undefined`
# for a member that is not there.
Spec_Said <- function(xValue, bPresent = TRUE) {
  if (!bPresent) {
    return("undefined")
  }
  as.character(jsonlite::toJSON(xValue, auto_unbox = TRUE, null = "null", digits = NA))
}

# The apostrophe bio.viz's sentences use, made from its code point so it is
# the same character in every locale.
Spec_Apostrophe <- function() intToUtf8(0x2019L)

Spec_Refuse <- function(...) {
  stop(paste0(...), call. = FALSE)
}

# How a specification read by jsonlite with its simplifying, fromJSON()'s
# default, is read as data instead.
strSpecSimplified <- paste(
  "jsonlite simplifies JSON when it reads it with fromJSON()'s default: a list of specifications becomes",
  "a data frame, and a list of values a vector. Read the specifications with jsonlite::read_json() or",
  "fromJSON(simplifyVector = FALSE), or give Run_Specifications() the file or the text."
)

# A JSON object read with a member twice keeps the last, as JavaScript's
# JSON.parse() does; and nesting deeper than any chart's settings is refused.
# A data frame, or a vector of several values, is JSON simplified, which a
# specification read as data never holds.
Spec_Tidy <- function(xValue, nDepth = 0L) {
  if (is.data.frame(xValue) || (is.atomic(xValue) && length(xValue) != 1L && !is.null(xValue))) {
    Spec_Refuse("the specification holds ", if (is.data.frame(xValue)) "a data frame" else "a vector of several values", ". ", strSpecSimplified)
  }
  if (!is.list(xValue)) {
    return(xValue)
  }
  if (nDepth > nSpecMostNested) {
    Spec_Refuse("the specification is nested more than ", nSpecMostNested, " deep, which no chart", Spec_Apostrophe(), "s settings are.")
  }
  if (!is.null(names(xValue))) {
    xValue <- xValue[!duplicated(names(xValue), fromLast = TRUE)]
  }
  for (iItem in seq_along(xValue)) {
    xItem <- Spec_Tidy(xValue[[iItem]], nDepth + 1L)
    if (is.null(xItem)) xValue[iItem] <- list(NULL) else xValue[[iItem]] <- xItem
  }
  xValue
}

# Text read as JSON, as data.
Spec_FromText <- function(strText) {
  Spec_NeedJsonlite("Spec_Read")
  tryCatch(
    jsonlite::parse_json(strText, simplifyVector = FALSE),
    error = function(cndError) {
      Spec_Refuse("a specification given as text must be JSON: ", trimws(gsub("\\s+", " ", conditionMessage(cndError))), ".")
    }
  )
}

# The specifications a batch is given: a JSON file, JSON text, or the list
# jsonlite reads one as; a JSON array of specifications or one specification.
# Each comes back as data, to be read on its own.
Spec_Parse <- function(xSpecifications) {
  Spec_NeedJsonlite("Run_Specifications")
  if (is.character(xSpecifications) && length(xSpecifications) == 1L && !is.na(xSpecifications)) {
    strText <- if (!grepl("^\\s*[\\[{]", xSpecifications) && file.exists(xSpecifications)) {
      paste(readLines(xSpecifications, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    } else {
      xSpecifications
    }
    xSpecifications <- Spec_FromText(strText)
  }
  if (is.data.frame(xSpecifications)) {
    Spec_Refuse("xSpecifications is a data frame. ", strSpecSimplified)
  }
  if (!is.list(xSpecifications)) {
    Spec_Refuse("xSpecifications must be a JSON file, JSON text or a list of specifications as jsonlite::read_json() reads them")
  }
  lSpecs <- if (Spec_IsObject(xSpecifications)) list(xSpecifications) else xSpecifications
  if (length(lSpecs) == 0L) {
    Spec_Refuse("xSpecifications holds no specification: a list of specifications is a JSON array of them")
  }
  lSpecs
}

#' @noRd
Spec_Read <- function(xSpecification) {
  lSpec <- if (is.character(xSpecification) && length(xSpecification) == 1L) Spec_FromText(xSpecification) else xSpecification
  lSpec <- Spec_Tidy(lSpec)
  if (!Spec_IsObject(lSpec)) {
    Spec_Refuse("a specification is an object: { format, chart, settings, filters }.")
  }
  if (!identical(lSpec$format, strSpecFormat)) {
    Spec_Refuse("this is not a bio.viz specification: its `format` must be \"", strSpecFormat, "\".")
  }
  xVersion <- lSpec[["format_version"]]
  if (!(is.numeric(xVersion) && length(xVersion) == 1L && !is.na(xVersion) && xVersion == nSpecVersion)) {
    Spec_Refuse(
      "this specification is of format version ", Spec_Said(xVersion, "format_version" %in% names(lSpec)),
      ", and this version of bio.viz reads version ", nSpecVersion, "."
    )
  }
  chrKnown <- c("format", "format_version", "bio_viz_version", "chart", "settings", "filters")
  chrExtra <- setdiff(names(lSpec), chrKnown)
  if (length(chrExtra) > 0L) {
    Spec_Refuse("a specification has no `", chrExtra[1], "`: it holds ", paste(chrKnown, collapse = ", "), ".")
  }
  if (!Spec_IsText(lSpec$bio_viz_version)) {
    Spec_Refuse("a specification", Spec_Apostrophe(), "s `bio_viz_version` is text, the version that wrote it.")
  }
  lNames <- Spec_SettingNames()
  if (!Spec_IsText(lSpec$chart) || !lSpec$chart %in% names(lNames)) {
    Spec_Refuse(
      "this specification names the chart ", Spec_Said(lSpec$chart, "chart" %in% names(lSpec)), ", which bio.viz does not have. ",
      "Its charts are ", paste(names(lNames), collapse = ", "), "."
    )
  }
  lSettings <- if ("settings" %in% names(lSpec)) lSpec$settings else structure(list(), names = character(0))
  if (!Spec_IsObject(lSettings)) {
    Spec_Refuse("a specification", Spec_Apostrophe(), "s `settings` is an object of settings.")
  }
  chrUnknown <- names(lSettings)[!names(lSettings) %in% lNames[[lSpec$chart]] | names(lSettings) %in% chrSpecPageSettings]
  if (length(chrUnknown) > 0L) {
    Spec_Refuse(
      "this specification of the ", lSpec$chart, " chart, written by bio.viz ", lSpec$bio_viz_version, ", holds ",
      paste0("`", chrUnknown, "`", collapse = ", "), ", which ", if (length(chrUnknown) == 1L) "is not a setting" else "are not settings",
      " of that chart in this version."
    )
  }
  if ("filters" %in% names(lSettings) && !is.null(lSettings$filters) && !Spec_IsArray(lSettings$filters)) {
    Spec_Refuse("the setting `filters` of a specification is a list of filters, or null.")
  }
  # A setting that names a column by a name no column may have.
  for (strKey in names(lSettings)) {
    xValue <- lSettings[[strKey]]
    if (is.character(xValue) && length(xValue) == 1L && xValue %in% chrSpecNoColumn) {
      Spec_Refuse("`", strKey, "` names `", xValue, "`, which is no column", Spec_Apostrophe(), "s name.")
    }
  }
  lFilters <- if ("filters" %in% names(lSpec)) lSpec$filters else list()
  if (!Spec_IsArray(lFilters)) {
    Spec_Refuse("a specification", Spec_Apostrophe(), "s `filters` is a list of { column, operator, values }.")
  }
  lRead <- lSettings
  chrColumns <- vapply(lFilters, function(lFilter) if (Spec_IsObject(lFilter) && Spec_IsText(lFilter$column)) lFilter$column else NA_character_, character(1))
  for (iFilter in seq_along(lFilters)) {
    lFilter <- lFilters[[iFilter]]
    strWhere <- paste("filter", iFilter)
    if (!Spec_IsObject(lFilter)) {
      Spec_Refuse(strWhere, " must be { column, operator, values }.")
    }
    chrOther <- setdiff(names(lFilter), c("column", "operator", "values"))
    if (length(chrOther) > 0L) {
      Spec_Refuse(strWhere, " has `", chrOther[1], "`: a filter is { column, operator, values }.")
    }
    if (!Spec_IsText(lFilter$column)) {
      Spec_Refuse(strWhere, " must name its column.")
    }
    if (lFilter$column %in% chrSpecNoColumn) {
      Spec_Refuse(strWhere, " is on `", lFilter$column, "`, which is no column", Spec_Apostrophe(), "s name.")
    }
    iBefore <- match(lFilter$column, chrColumns)
    if (iBefore < iFilter) {
      Spec_Refuse(strWhere, " is on ", lFilter$column, ", which filter ", iBefore, " is already on: a column is filtered once.")
    }
    if (!identical(lFilter$operator, "in")) {
      Spec_Refuse(
        strWhere, ", on ", lFilter$column, ", has the operator ", Spec_Said(lFilter$operator, "operator" %in% names(lFilter)),
        "; the operators are \"in\": in, the values it lets through."
      )
    }
    lValues <- lFilter$values
    bValues <- Spec_IsArray(lValues) && all(vapply(lValues, function(xValue) {
      (is.character(xValue) || is.numeric(xValue)) && length(xValue) == 1L && !is.na(xValue)
    }, logical(1)))
    if (!bValues) {
      Spec_Refuse(strWhere, ", on ", lFilter$column, ", must list the values it lets through: text or numbers.")
    }
    # A value is compared as text: 35 and "35" are one value.
    chrValues <- vapply(lValues, function(xValue) Core_Text(xValue), character(1))
    if (anyDuplicated(chrValues) > 0L) {
      Spec_Refuse(strWhere, ", on ", lFilter$column, ", names ", chrValues[anyDuplicated(chrValues)], " twice: a value is listed once.")
    }
    # The filter laid onto the chart's own, as where it starts.
    lSpecs <- if (is.null(lRead$filters)) list() else lRead$filters
    iAt <- which(vapply(lSpecs, function(xSpec) {
      if (is.character(xSpec)) identical(xSpec, lFilter$column) else Spec_IsObject(xSpec) && identical(xSpec$value_col, lFilter$column)
    }, logical(1)))[1]
    lStarted <- if (!is.na(iAt) && Spec_IsObject(lSpecs[[iAt]])) lSpecs[[iAt]] else list(value_col = lFilter$column)
    lStarted["start"] <- list(if (length(chrValues) == 1L) chrValues else chrValues)
    if (length(chrValues) != 1L) lStarted$multiple <- TRUE
    if (!is.na(iAt)) lSpecs[[iAt]] <- lStarted else lSpecs[[length(lSpecs) + 1L]] <- lStarted
    lRead$filters <- lSpecs
  }
  list(
    chart = lSpec$chart, settings = lRead, version = lSpec$bio_viz_version,
    filters = lapply(lFilters, function(lFilter) list(column = lFilter$column, values = vapply(lFilter$values, Core_Text, character(1))))
  )
}

# The shapes bio.viz's charts hold their list and object settings to
# (bio.viz, src/shared/settings.js `textList` and `fieldList`, and each chart's
# own check), by chart. A name list is a name or a list of names (text or
# numbers), and is empty only where the chart takes no value as none; a field
# list is a column's name or { value_col, label }, or a list of them; a chart a
# row opens is an object of its settings.
lSpecNameLists <- list(
  "group-comparison" = c("baseline_visits", "visits", "levels", "measures"),
  "association-scatter" = c("baseline_visits", "measures"),
  "correlation-matrix" = c("baseline_visits", "biomarkers", "visits", "measures"),
  "biomarker-screen" = c("levels", "baseline_visits", "measures"),
  "cross-tab" = c("baseline_visits", "measures"),
  "stratified-survival" = c("baseline_visits", "measures")
)
lSpecEmptyNameLists <- list(
  "group-comparison" = c("visits", "levels"),
  "correlation-matrix" = c("biomarkers", "visits")
)
lSpecFieldLists <- list(
  "group-comparison" = c("groups", "filters", "details", "profile_details"),
  "association-scatter" = c("numbers", "groups", "filters", "details", "profile_details"),
  "correlation-matrix" = "filters",
  "biomarker-screen" = c("groups", "numbers", "filters"),
  "cross-tab" = c("groups", "filters", "details", "profile_details"),
  "stratified-survival" = c("groups", "filters", "details", "profile_details")
)
lSpecNested <- list(
  "correlation-matrix" = c(scatter = "`scatter` must be an object of settings for the association scatter, or null."),
  "biomarker-screen" = c(
    group_comparison = "`group_comparison` must be an object of settings for the chart a row opens, or null.",
    association_scatter = "`association_scatter` must be an object of settings for the chart a row opens, or null.",
    stratified_survival = "`stratified_survival` must be an object of settings for the chart a row opens, or null."
  )
)
lSpecStatistics <- list(
  "group-comparison" = c(statistic = strGroupComparisonStatistic),
  "association-scatter" = c(statistic = strAssociationScatterStatistic, fit_statistic = strAssociationScatterFitStatistic),
  "correlation-matrix" = c(statistic = strCorrelationMatrixStatistic),
  "biomarker-screen" = c(statistic = strBiomarkerScreenStatistic),
  "cross-tab" = c(statistic = strCrossTabStatistic),
  "stratified-survival" = c(statistic = strStratifiedSurvivalStatistic)
)

# Settings held to bio.viz's shapes, in its words, before the chart's own rules
# in R read them. Of the settings given only; a default is always its shape.
Spec_CheckShapes <- function(strChart, lSettings) {
  Has <- function(strKey) strKey %in% names(lSettings) && !is.null(lSettings[[strKey]])
  IsEntry <- function(xValue) Spec_IsText(xValue) || (is.numeric(xValue) && length(xValue) == 1L && !is.na(xValue))
  AsList <- function(xValue) if (Spec_IsArray(xValue)) xValue else list(xValue)
  for (strKey in lSpecNameLists[[strChart]]) {
    if (!Has(strKey)) next
    lList <- AsList(lSettings[[strKey]])
    bEmpty <- strKey %in% lSpecEmptyNameLists[[strChart]]
    if ((length(lList) == 0L && !bEmpty) || !all(vapply(lList, IsEntry, logical(1)))) {
      Spec_Refuse("`", strKey, "` must be a name, or a list of names.")
    }
  }
  for (strKey in lSpecFieldLists[[strChart]]) {
    if (!Has(strKey)) next
    bFields <- all(vapply(AsList(lSettings[[strKey]]), function(xEntry) {
      Spec_IsText(xEntry) || (Spec_IsObject(xEntry) && Spec_IsText(xEntry$value_col))
    }, logical(1)))
    if (!bFields) Spec_Refuse("`", strKey, "` holds something that is not a column name or { value_col, label }.")
  }
  if (Has("footnotes")) {
    bTexts <- all(vapply(AsList(lSettings$footnotes), function(xEntry) is.character(xEntry) && length(xEntry) == 1L && !is.na(xEntry), logical(1)))
    if (!bTexts) Spec_Refuse("`footnotes` must be text, or a list of texts, or null for none.")
  }
  chrNested <- lSpecNested[[strChart]]
  for (strKey in names(chrNested)) {
    if (Has(strKey) && !Spec_IsObject(lSettings[[strKey]])) Spec_Refuse(chrNested[[strKey]])
  }
  if (strChart %in% c("cross-tab", "stratified-survival") && Has("cuts") && !Spec_IsArray(lSettings$cuts)) {
    Spec_Refuse("`cuts` must be a list of cut variables, or null.")
  }
  if (strChart == "stratified-survival" && Has("at_risk_times")) {
    lTimes <- lSettings$at_risk_times
    bTimes <- Spec_IsArray(lTimes) && length(lTimes) > 0L &&
      all(vapply(lTimes, function(xTime) is.numeric(xTime) && length(xTime) == 1L && is.finite(xTime) && xTime >= 0, logical(1)))
    if (bTimes && length(lTimes) > 1L) bTimes <- all(diff(unlist(lTimes)) > 0)
    if (!bTimes) Spec_Refuse("`at_risk_times` must be a list of times, none below 0, in ascending order, or null.")
  }
  # Where gsm.bio differs from bio.viz on purpose: a chart's statistic names
  # the R function the page asks, and bio.viz takes any name. gsm.bio computes
  # each chart's statistics with one Analyze_*() function, and refuses another.
  chrStatistics <- lSpecStatistics[[strChart]]
  for (strKey in names(chrStatistics)) {
    xValue <- lSettings[[strKey]]
    if (Has(strKey) && Spec_IsText(xValue) && !identical(xValue, chrStatistics[[strKey]])) {
      Spec_Refuse(
        "`", strKey, "` names ", xValue, ", which gsm.bio does not compute: gsm.bio computes the ", strChart,
        " chart", Spec_Apostrophe(), "s ", if (strKey == "fit_statistic") "fit" else "statistics", " with ", chrStatistics[[strKey]],
        " only, so `", strKey, "` is \"", chrStatistics[[strKey]], "\" or null."
      )
    }
  }
  invisible(lSettings)
}

# The settings a specification holds, checked as the chart checks them: by the
# chart's own rules in R, and the settings only the browser reads by the rules
# bio.viz checks them by. Returns the settings, unchanged; refuses with a
# sentence.
Spec_Check <- function(lRead) {
  lSettings <- lRead$settings
  fnSettings <- list(
    "group-comparison" = GroupComparison_Settings, "association-scatter" = AssociationScatter_Settings,
    "correlation-matrix" = CorrelationMatrix_Settings, "biomarker-screen" = BiomarkerScreen_Settings,
    "cross-tab" = CrossTab_Settings, "stratified-survival" = StratifiedSurvival_Settings
  )[[lRead$chart]]
  Spec_CheckShapes(lRead$chart, lSettings)
  fnSettings(lSettings)
  Output_CheckTitles(Core_Overlay(lOutputTitleDefaults, lSettings[intersect(names(lSettings), names(lOutputTitleDefaults))]))
  Has <- function(strKey) strKey %in% names(lSettings)
  IsWhole <- function(xValue, nLeast) is.numeric(xValue) && length(xValue) == 1L && !is.na(xValue) && xValue == round(xValue) && xValue >= nLeast
  IsFlag <- function(xValue) is.logical(xValue) && length(xValue) == 1L && !is.na(xValue)
  Choice <- function(strKey, chrChoices) {
    if (Has(strKey) && !(is.character(lSettings[[strKey]]) && length(lSettings[[strKey]]) == 1L && lSettings[[strKey]] %in% chrChoices)) {
      Core_Stop("Setting '", strKey, "' must be one of ", paste(chrChoices, collapse = ", "))
    }
  }
  if (Has("profile") && !IsFlag(lSettings$profile)) Core_Stop("Setting 'profile' must be TRUE or FALSE")
  if (Has("downloads") && !IsFlag(lSettings$downloads)) Core_Stop("Setting 'downloads' must be TRUE or FALSE")
  if (Has("waiting_note") && !is.null(lSettings$waiting_note) && !Spec_IsText(lSettings$waiting_note)) {
    Core_Stop("Setting 'waiting_note' must be a sentence, or NULL for none")
  }
  if (Has("png_scale") && !(is.numeric(lSettings$png_scale) && length(lSettings$png_scale) == 1L && !is.na(lSettings$png_scale) &&
    lSettings$png_scale >= 1 && lSettings$png_scale <= 4)) {
    Core_Stop("Setting 'png_scale' must be a number from 1 to 4: image pixels per CSS pixel")
  }
  for (strKey in c("page_size", "overview_limit", "limit")) {
    if (Has(strKey) && !IsWhole(lSettings[[strKey]], 1)) Core_Stop("Setting '", strKey, "' must be a whole number, one or more")
  }
  if (Has("page") && !IsWhole(lSettings$page, 0)) Core_Stop("Setting 'page' must be a whole number, from 0")
  Choice("mark", c("box", "violin", "points"))
  Choice("sort", c("estimate", "name", "adjusted"))
  Choice("view", c("grid", "scatters"))
  for (strKey in c("details", "profile_details")) {
    if (Has(strKey)) Chart_Fields(lSettings[[strKey]], strKey)
  }
  for (strKey in c("studyday_col", "normal_col_high", "normal_col_low")) {
    if (Has(strKey) && !is.null(lSettings[[strKey]]) && !Spec_IsText(lSettings[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be the name of a column, or NULL")
    }
  }
  if (Has("measures")) Chart_Names(lSettings$measures, "measures")
  invisible(lSettings)
}

# A specification read, and its settings checked as the chart checks them.
Spec_ReadChecked <- function(xSpecification) {
  lRead <- Spec_Read(xSpecification)
  Spec_Check(lRead)
  lRead
}

# A cut variable or a variable that is a biomarker, which a chart's biomarker
# setting may hold.
Spec_IsBiomarker <- function(xValue) Spec_IsObject(xValue) && is.character(xValue$measure) && length(xValue$measure) == 1L

# One specification across every biomarker (docs/output.md, "Which setting
# holds the biomarker each chart draws"): the setting that holds the
# biomarker takes each biomarker the chart offers in turn. A chart already of
# every biomarker (the screen, a matrix of biomarkers) or with no biomarker to
# take (a cross-tabulation of two columns) is one view. Returns the read
# specifications, named by their biomarker, or NA for the one view.
Spec_Expand <- function(lRead, dfResults, dfParticipants = NULL) {
  lSettings <- lRead$settings
  lConfig <- list(
    measure_col = if (is.null(lSettings$measure_col)) "TEST" else lSettings$measure_col,
    measures = if (is.null(lSettings$measures)) NULL else Chart_Names(lSettings$measures, "measures")
  )
  chrBiomarkers <- Chart_Measures(dfResults, lConfig)
  One <- stats::setNames(list(lRead), NA_character_)
  Each <- function(fnSet, chrSkip = character(0)) {
    chrEach <- setdiff(chrBiomarkers, chrSkip)
    stats::setNames(lapply(chrEach, function(strBiomarker) {
      lOne <- lRead
      lOne$settings <- fnSet(lSettings, strBiomarker)
      lOne
    }), chrEach)
  }
  Measure <- function(strKey) function(lGiven, strBiomarker) {
    lGiven[[strKey]]$measure <- strBiomarker
    lGiven
  }
  # Explicit cut points are the specification's, and stay as they are for
  # every biomarker; each view says so. A named cut (a median, tertiles) is
  # worked out on each biomarker's own values.
  Noted <- function(lViews, strKey) {
    xCut <- lSettings[[strKey]]$cut
    if (is.list(xCut) || is.numeric(xCut)) {
      strCut <- paste(vapply(unlist(xCut), Core_Text, character(1)), collapse = ", ")
      for (strBiomarker in names(lViews)) {
        lViews[[strBiomarker]]$note <- paste0("The explicit cut ", strCut, " is applied to ", strBiomarker, " as it is.")
      }
    }
    lViews
  }
  # The scatter's other axis keeps its biomarker out of the views only where
  # the two would be one variable: at the same visit.
  SameVisit <- function(lOne, lOther) identical(lOne$visit, lOther$visit) && identical(lOne$value, lOther$value)
  switch(lRead$chart,
    "group-comparison" = Each(function(lGiven, strBiomarker) {
      lGiven$start_value <- strBiomarker
      lGiven
    }),
    "association-scatter" = if (Spec_IsBiomarker(lSettings$x)) {
      Each(Measure("x"), if (Spec_IsBiomarker(lSettings$y) && SameVisit(lSettings$x, lSettings$y)) lSettings$y$measure else character(0))
    } else if (Spec_IsBiomarker(lSettings$y)) {
      Each(Measure("y"))
    } else {
      One
    },
    "correlation-matrix" = if (identical(lSettings$mode, "visits")) {
      Each(function(lGiven, strBiomarker) {
        lGiven$measure <- strBiomarker
        lGiven
      })
    } else {
      One
    },
    "cross-tab" = if (Spec_IsBiomarker(lSettings$row_by)) {
      Noted(Each(Measure("row_by")), "row_by")
    } else if (Spec_IsBiomarker(lSettings$col_by)) {
      Noted(Each(Measure("col_by")), "col_by")
    } else {
      One
    },
    "stratified-survival" = if (Spec_IsBiomarker(lSettings$group_by)) Noted(Each(Measure("group_by")), "group_by") else One,
    One
  )
}
