# bio.viz's chart specifications (bio.viz#68; docs/output.md, "Specifications";
# src/shared/specification.js), read in R: what a chart draws, written as JSON
# data, so gsm.bio's batch runner can draw the same view as a static figure
# and a table. The reader makes every check bio.viz's reader makes, in its
# words, and the settings it returns are the ones bio.viz's reader returns:
# tests/testthat/test-batch.R holds it to bio.viz's own reader, run on the same
# specifications from the copied bundle.
#
# Nothing in a specification is ever evaluated. It is read by jsonlite as
# data (text, numbers, true, false, null, lists and objects), and every value
# stays that: a title or a setting that looks like code is text. Nothing here
# calls eval(), parse() or anything that runs text.
#
# The one way the reader differs from bio.viz's: a value of a setting is
# checked by the chart's own rules in R (R/GroupComparison.R and the files
# beside it) and, for the settings only the browser reads (how a chart draws
# its listing, its downloads), by the rules bio.viz's charts check them by. A
# refusal of a value says it in R's words, which are not always bio.viz's.

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

# A JSON object read with a member twice keeps the last, as JavaScript's
# JSON.parse() does; and nesting deeper than any chart's settings is refused.
Spec_Tidy <- function(xValue, nDepth = 0L) {
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
  if (!is.list(xSpecifications)) {
    Spec_Refuse("xSpecifications must be a JSON file, JSON text or a list of specifications as jsonlite reads them")
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
  switch(lRead$chart,
    "group-comparison" = Each(function(lGiven, strBiomarker) {
      lGiven$start_value <- strBiomarker
      lGiven
    }),
    "association-scatter" = if (Spec_IsBiomarker(lSettings$x)) {
      Each(Measure("x"), if (Spec_IsBiomarker(lSettings$y)) lSettings$y$measure else character(0))
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
    "cross-tab" = if (Spec_IsBiomarker(lSettings$row_by)) Each(Measure("row_by")) else if (Spec_IsBiomarker(lSettings$col_by)) Each(Measure("col_by")) else One,
    "stratified-survival" = if (Spec_IsBiomarker(lSettings$group_by)) Each(Measure("group_by")) else One,
    One
  )
}
