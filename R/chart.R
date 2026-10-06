# What every chart's controls offer, read from the tables, and what every chart
# does before a frame is made and when it asks R: bio.viz's src/shared/, in R.
#
# A widget stores R's answers ahead of time, so it has to know, before there is
# a page, what the chart's controls will open on and how the chart will ask.
# The rules here are the ones every bio.viz chart shares: which biomarkers and
# columns a control offers, what a filter opens on, which participants the
# filters keep, and how two requests are told apart. Each chart's own rules are
# in its own file (R/GroupComparison.R, R/AssociationScatter.R). Nothing here
# computes a statistic, and nothing is exported.

# A column name, or list(value_col, label), as list(value_col, label, ...).
Chart_Field <- function(xSpec, strSetting) {
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
Chart_Fields <- function(xValue, strSetting) {
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
  lapply(lEntries, Chart_Field, strSetting = strSetting)
}

# A setting that lists names: one or several, as distinct text. NULL stays NULL.
# With `bEmpty`, a list of none is none, an empty vector, as bio.viz takes it
# for the settings it reads that way (`textList(..., { empty: true })`).
Chart_Names <- function(xValue, strSetting, bEmpty = FALSE) {
  if (is.null(xValue)) {
    return(NULL)
  }
  xValues <- unlist(xValue)
  if (bEmpty && length(xValues) == 0L) {
    return(character(0))
  }
  if (length(xValues) == 0L || !(is.character(xValues) || is.numeric(xValues)) || anyNA(xValues) || !all(nzchar(trimws(as.character(xValues))))) {
    Core_Stop("Setting '", strSetting, "' must be a name, or several names")
  }
  unique(Core_Text(xValues))
}

# The settings the frame reads, taken from the chart's.
Chart_CoreSettings <- function(lConfig) {
  lConfig[c(
    "id_col", "measure_col", "value_col", "visit_col", "visit_order_col", "participant_id_col",
    "baseline_visits", "baseline_stat"
  )]
}

# The results a chart draws, by the unscheduled-visit rule (R/core.R): for a
# chart that has the rule's settings, the rows at scheduled visits, or every
# row when `unscheduled_visits` switches the others on. A chart without the
# settings draws every row. Returns `results`, the rows drawn; `visits`, the
# unscheduled visits that have a result to draw, in visit order, which is what
# the chart's switch brings back; and `drawn`, whether the rows drawn hold
# unscheduled visits, which a request's identity then says.
Chart_Unscheduled <- function(dfResults, lConfig) {
  if (!"unscheduled_visits" %in% names(lConfig)) {
    return(list(results = dfResults, visits = character(0), drawn = FALSE))
  }
  lFound <- Core_Scheduled(dfResults, lConfig[c("visit_col", names(lCoreUnscheduledDefaults))])
  chrVisits <- character(0)
  if (length(lFound$visits) > 0L) {
    chrAll <- Core_Visits(dfResults, Chart_CoreSettings(lConfig))
    chrVisits <- chrAll[chrAll %in% lFound$visits]
  }
  bShown <- isTRUE(lConfig$unscheduled_visits)
  list(
    results = if (bShown) dfResults else lFound$results,
    visits = chrVisits,
    drawn = bShown && length(chrVisits) > 0L
  )
}

# The biomarkers the Biomarker control offers: the configured list in its
# order, keeping the ones the table has, or every biomarker in the table by
# name.
Chart_Measures <- function(dfResults, lConfig) {
  chrPresent <- Core_Levels(dfResults[[lConfig$measure_col]])
  if (is.null(lConfig$measures)) {
    return(chrPresent)
  }
  chrListed <- lConfig$measures[lConfig$measures %in% chrPresent]
  if (length(chrListed) > 0L) chrListed else chrPresent
}

# The columns that can make a group, a colour or a panel: columns that hold a
# category, which is to say at most `max_levels` different values. With a
# participant table: its columns, other than the id. Without one, and after
# them: the columns carried on the results rows, other than the ones the
# settings map, that hold one value for each participant. The setting `groups`,
# when given, is the list, and nothing is worked out.
Chart_Categories <- function(dfResults, dfParticipants, lConfig) {
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
# column of the participant table is a filter, and a filter on the
# participant's id is none. A filter opens on its `start` when the data has it,
# and otherwise lets every participant through, unless it is set `all = FALSE`,
# when it opens on its first value. tests/testthat/test-chart.R holds this to
# the safety.viz kit the widgets ship. Returns a named list of
# column to the value, or values, the filter opens on; NULL for a filter that
# opens on all, and an empty vector for one that opens on none.
Chart_Filters <- function(dfParticipants, lConfig, dfCategories) {
  if (is.null(dfParticipants) || nrow(dfParticipants) == 0L) {
    return(list())
  }
  lSpecs <- if (!is.null(lConfig$filters)) {
    Filter(function(lSpec) lSpec$value_col %in% names(dfParticipants), lConfig$filters)
  } else {
    lapply(dfCategories$value_col[dfCategories$table == "participants"], function(strCol) list(value_col = strCol))
  }
  # What each filter starts on, as the kit's `normalizeFilterSpec` and
  # `initFilterState` give it: its `start` as text, the first of several for a
  # filter of one value, or NULL. Two specs on one column are one state, the
  # later spec's.
  lState <- list()
  for (lSpec in lSpecs) {
    xStart <- unlist(lSpec$start)
    bStarted <- length(xStart) > 0L && !(length(xStart) == 1L && (is.na(xStart) || identical(as.character(xStart), "")))
    chrStart <- if (bStarted) Core_Text(xStart) else NULL
    # A filter of several values whose `start` is an empty list opens on no
    # value, and lets nobody through, as bio.viz's `startFilters` opens it.
    if (isTRUE(lSpec$multiple) && !is.null(lSpec$start) && length(lSpec$start) == 0L) chrStart <- character(0)
    lState[lSpec$value_col] <- list(if (isTRUE(lSpec$multiple) || is.null(chrStart)) chrStart else chrStart[1L])
  }
  # The participant's id is no filter: it gets no control, and so no restriction.
  strIdCol <- if (is.null(lConfig$participant_id_col)) lConfig$id_col else lConfig$participant_id_col
  lState[[strIdCol]] <- NULL
  lSpecs <- Filter(function(lSpec) !identical(lSpec$value_col, strIdCol), lSpecs)
  # Each filter reconciled in turn with the values its control offers, as
  # bio.viz's `addFilterControls` calls the kit's `reconcileFilters`: a start
  # the data lacks is dropped, and the filter opens on All, or, with
  # `all = FALSE`, on its first value; several values keep the ones the data
  # has, and open on All when it has none. The values are listed only when
  # they decide something. A filter that opens on no value keeps it.
  for (lSpec in lSpecs) {
    strColumn <- lSpec$value_col
    chrSelected <- lState[[strColumn]]
    bAll <- !identical(lSpec$all, FALSE)
    if (isTRUE(lSpec$multiple)) {
      if (length(chrSelected) > 0L) {
        chrSelected <- chrSelected[chrSelected %in% Chart_FilterValues(dfParticipants, strColumn)]
        if (length(chrSelected) == 0L) chrSelected <- NULL
      }
    } else {
      chrSelected <- if (is.null(chrSelected)) NULL else chrSelected[1L]
      if (!is.null(chrSelected) || !bAll) {
        chrValues <- Chart_FilterValues(dfParticipants, strColumn)
        if (!is.null(chrSelected) && !chrSelected %in% chrValues) chrSelected <- NULL
        if (is.null(chrSelected) && !bAll && length(chrValues) > 0L) chrSelected <- chrValues[1L]
      }
    }
    lState[strColumn] <- list(chrSelected)
  }
  lState
}

# The values a filter's control offers, as bio.viz lists them: every distinct
# value of the participant table's column as text, other than a missing value
# and empty text (text that is only white space is a value), sorted by name with
# numbers as numbers.
Chart_FilterValues <- function(dfParticipants, strColumn) {
  chrText <- Core_Text(dfParticipants[[strColumn]])
  Core_SortWith(unique(chrText[!is.na(chrText) & chrText != ""]), Core_NaturalCompare)
}

# The filters in force, each as the values it lets through, as text sorted by
# code point. A filter set to all is not in force and is left out.
Chart_FiltersInForce <- function(lFilters) {
  lInForce <- list()
  for (strColumn in names(lFilters)) {
    chrValues <- lFilters[[strColumn]]
    if (length(chrValues) > 0L) {
      lInForce[[strColumn]] <- Core_SortText(unique(Core_Text(chrValues)))
    }
  }
  lInForce
}

# The tables the filters leave. A filter chooses participants: the ones
# filtered out are set aside with their results before a frame is made, so they
# are not counted as missing from it. With no participant table there is nothing
# to filter, and the tables come back as they are. `lFilters` is what each
# filter is set to, by its column, as Chart_Filters() gives it: NULL lets
# everyone through, and an empty vector nobody. Returns a list of
# `results` and `participants`, which is NULL when there is no participant table.
Chart_KeepFiltered <- function(dfResults, dfParticipants, lConfig, lFilters) {
  if (is.null(dfParticipants) || nrow(dfParticipants) == 0L) {
    return(list(results = dfResults, participants = NULL))
  }
  strIdCol <- lConfig$id_col
  strParticipantIdCol <- if (is.null(lConfig$participant_id_col)) strIdCol else lConfig$participant_id_col
  dfKept <- dfParticipants
  for (strColumn in names(lFilters)) {
    chrSelection <- lFilters[[strColumn]]
    if (!is.null(chrSelection)) {
      # A participant with nothing in the column is compared as the chart
      # compares it, by the text of nothing.
      chrText <- Core_Text(dfKept[[strColumn]])
      chrText[is.na(chrText)] <- "null"
      dfKept <- dfKept[chrText %in% chrSelection, , drop = FALSE]
    }
  }
  list(
    results = dfResults[Core_Text(dfResults[[strIdCol]]) %in% Core_Text(dfKept[[strParticipantIdCol]]), , drop = FALSE],
    participants = dfKept
  )
}

# How many participants the filters keep: the participant table's rows they
# keep, or, with no participant table, the participants the results hold.
Chart_Passing <- function(dfResults, dfParticipants, lConfig, lFilters) {
  lKept <- Chart_KeepFiltered(dfResults, dfParticipants, lConfig, lFilters)
  if (is.null(lKept$participants)) {
    chrIds <- Core_Text(lKept$results[[lConfig$id_col]])
    return(length(unique(chrIds[!is.na(chrIds)])))
  }
  nrow(lKept$participants)
}

# A figure or table of nobody: when the filters keep no participant, bio.viz's
# chart says "No participant passes the filters." before any other reason it
# has nothing to draw, and so does R.
Chart_StopIfNobody <- function(strWho, dfResults, dfParticipants, lConfig, lFilters) {
  if (!is.null(dfParticipants) && nrow(dfParticipants) > 0L && Chart_Passing(dfResults, dfParticipants, lConfig, lFilters) == 0L) {
    Core_Stop(strWho, "(): no participant passes the filters")
  }
  invisible(NULL)
}

# The participant-level numbers a chart can take as a variable: columns in which every value
# that is written is a number, and that hold more than one different value.
# With a participant table: its columns, other than the id. Without one, and
# after them: the columns carried on the results rows, other than the ones the
# settings map and the participant table's own, that hold one value for each
# participant. The setting `numbers`, when given, is the list, and nothing is
# worked out.
Chart_Numbers <- function(dfResults, dfParticipants, lConfig) {
  if (!is.null(lConfig$numbers)) {
    return(vapply(lConfig$numbers, function(lSpec) lSpec$value_col, character(1)))
  }
  IsNumbers <- function(xValues) {
    xWritten <- xValues[!Core_IsBlank(xValues)]
    nWritten <- Core_Number(xWritten)
    !anyNA(nWritten) && length(unique(nWritten)) > 1L
  }
  chrColumns <- character(0)
  chrTaken <- character(0)
  if (!is.null(dfParticipants) && nrow(dfParticipants) > 0L) {
    strIdCol <- if (is.null(lConfig$participant_id_col)) lConfig$id_col else lConfig$participant_id_col
    chrTaken <- setdiff(names(dfParticipants), strIdCol)
    for (strName in chrTaken) {
      if (IsNumbers(dfParticipants[[strName]])) {
        chrColumns <- c(chrColumns, strName)
      }
    }
  }
  chrMapped <- unlist(lConfig[c(
    "id_col", "measure_col", "value_col", "visit_col", "visit_order_col", "unit_col", "studyday_col",
    "normal_col_high", "normal_col_low"
  )])
  chrRowId <- Core_Text(dfResults[[lConfig$id_col]])
  for (strName in setdiff(names(dfResults), c(chrMapped, chrTaken))) {
    xColumn <- dfResults[[strName]]
    bFilled <- !Core_IsBlank(xColumn)
    chrText <- Core_Text(xColumn)[bFilled]
    chrId <- chrRowId[bFilled]
    # One value for each participant, or it is not a participant-level column.
    bConstant <- all(chrText == chrText[match(chrId, chrId)])
    if (bConstant && IsNumbers(xColumn[bFilled][!duplicated(chrId)])) {
      chrColumns <- c(chrColumns, strName)
    }
  }
  chrColumns
}

# The filters a chart shows, as the chart a cell or a row opens is handed them:
# their specs, as the setting `filters` gives them, kept to the columns the
# participant table has, or the participant table's category columns. There
# are filters only with a participant table.
Chart_FilterSpecs <- function(dfResults, dfParticipants, lConfig) {
  if (is.null(dfParticipants) || nrow(dfParticipants) == 0L) {
    return(list())
  }
  if (!is.null(lConfig$filters)) {
    return(Filter(function(lSpec) lSpec$value_col %in% names(dfParticipants), lConfig$filters))
  }
  dfCategories <- Chart_Categories(dfResults, dfParticipants, lConfig)
  lapply(dfCategories$value_col[dfCategories$table == "participants"], function(strCol) {
    list(value_col = strCol, label = strCol)
  })
}

# Every level of a column, read from the table that holds it: the participant
# table when it has the column, and otherwise the results rows.
Chart_ColumnLevels <- function(dfResults, dfParticipants, strColumn) {
  if (!is.null(dfParticipants) && strColumn %in% names(dfParticipants)) {
    return(Core_Levels(dfParticipants[[strColumn]]))
  }
  if (!strColumn %in% names(dfResults)) {
    return(character(0))
  }
  Core_Levels(dfResults[[strColumn]])
}

# A setting that makes groups (bio.viz, src/shared/cut.js, `checkGrouping`): a
# column's name, NULL, or a biomarker or a number cut into groups, which must
# carry a cut and comes back as the settings write it (Core_WrittenCut()).
Chart_Grouping <- function(xValue, strSetting) {
  if (is.null(xValue)) {
    return(NULL)
  }
  if (is.list(xValue) && !is.data.frame(xValue)) {
    if (is.null(xValue$cut)) {
      Core_Stop(
        "Setting '", strSetting, "' is a variable with no cut. A biomarker or a number makes groups only when it ",
        "is cut: add cut = 'median', 'tertiles', 'quartiles' or the cut points."
      )
    }
    lVariable <- tryCatch(Core_Variable(xValue), error = function(cndError) {
      Core_Stop("Setting '", strSetting, "': ", conditionMessage(cndError))
    })
    return(Core_WrittenCut(lVariable))
  }
  if (!(is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue)))) {
    Core_Stop("Setting '", strSetting, "' must be a single name (a character string), a cut variable, or NULL")
  }
  xValue
}

# Whether a setting that makes groups holds a cut variable rather than a column.
Chart_IsCut <- function(xBy) is.list(xBy)

# A grouping as the frame takes it: the cut variable, or the column.
Chart_GroupingVariable <- function(xBy) if (Chart_IsCut(xBy)) xBy else list(col = xBy)

# A cut variable a chart is given must be of a biomarker the results table has,
# or of a column a table has: one that is not is refused when the tables are
# read, naming the setting.
Chart_CheckCut <- function(xBy, strSetting, dfResults, dfParticipants, lConfig) {
  if (!Chart_IsCut(xBy)) {
    return(invisible(NULL))
  }
  if (!is.null(xBy$measure) && !xBy$measure %in% Core_Text(dfResults[[lConfig$measure_col]])) {
    Core_Stop("Setting '", strSetting, "' cuts the biomarker '", xBy$measure, "', which the results table does not have.")
  }
  if (!is.null(xBy$col) && !xBy$col %in% c(names(dfResults), names(dfParticipants))) {
    Core_Stop("Setting '", strSetting, "' cuts the column '", xBy$col, "', which neither table has.")
  }
  invisible(NULL)
}

# The cut of one variable on a chart's tables (bio.viz, src/shared/cut.js,
# `cutOf`): its value for each participant the filters keep, one each, and the
# points and groups of the cut rule, worked out on those with a value. Returns
# Core_CutPoints()'s list with `spec`, the variable as the settings write it,
# and `ids` and `values`, each participant's. `chrOnly`, when given, keeps the
# participants the cut is worked out on to those ids: the survival chart cuts
# only the participants with an outcome, the ones it draws.
Chart_Cut <- function(dfResults, dfParticipants, lConfig, lFilters, xBy, chrOnly = NULL) {
  lKept <- Chart_KeepFiltered(dfResults, dfParticipants, lConfig, lFilters)
  if (!is.null(chrOnly)) {
    if (is.null(lKept$participants)) {
      lKept$results <- lKept$results[Core_Text(lKept$results[[lConfig$id_col]]) %in% chrOnly, , drop = FALSE]
    } else {
      strParticipantIdCol <- if (is.null(lConfig$participant_id_col)) lConfig$id_col else lConfig$participant_id_col
      lKept$participants <- lKept$participants[Core_Text(lKept$participants[[strParticipantIdCol]]) %in% chrOnly, , drop = FALSE]
    }
  }
  dfData <- Core_Frame(
    lKept$results, lKept$participants, list(v = xBy),
    c(Chart_CoreSettings(lConfig), list(required = character(0)))
  )$data
  lVariable <- Core_Variable(xBy)
  c(
    Core_CutPoints(dfData$v, lVariable$cut),
    list(spec = Core_WrittenCut(lVariable), ids = Core_Text(dfData[[lConfig$id_col]]), values = dfData$v)
  )
}

# A key as text, the same for two keys that differ only in the order their
# members were written: how the chart's connection tells stored results apart.
Chart_KeyText <- function(xValue) {
  if (is.list(xValue)) {
    if (is.null(names(xValue))) {
      return(paste0("[", paste(vapply(xValue, Chart_KeyText, character(1)), collapse = ","), "]"))
    }
    chrNames <- Core_SortText(names(xValue))
    return(paste0("{", paste0(chrNames, ":", vapply(xValue[chrNames], Chart_KeyText, character(1)), collapse = ","), "}"))
  }
  paste0(typeof(xValue), "(", paste(as.character(xValue), collapse = ","), ")")
}

# R's answers for a list of requests, each stored once: a request is the key
# (`name`, `args`, `dataId`, `rows`) with `data`, the rows R is handed. Two
# requests with the same name, arguments and identity are one stored result.
# `lFunctions` names the statistics functions a request may call.
Chart_Answer <- function(lRequests, lFunctions) {
  lStored <- list()
  for (lRequest in lRequests) {
    strKey <- Chart_KeyText(lRequest[c("name", "args", "dataId")])
    if (is.null(lStored[[strKey]])) {
      lRequest$value <- do.call(lFunctions[[lRequest$name]], c(list(lRequest$data), lRequest$args))
      lStored[[strKey]] <- lRequest
    }
  }
  unname(lStored)
}

# ---- Outcomes ---------------------------------------------------------------------
#
# The outcomes table the survival chart, and the screen's hazard rows, read
# (bio.viz, src/shared/outcomes.js): one row per participant and endpoint, with
# a time and a flag, read either way round, censored (ADaM's CNSR, 1 =
# censored) or an event (1 = event). Exactly one of the two is named.

# The outcome settings, with the chart's defaults. The endpoint is the one the
# chart opens on: NULL means the first.
lOutcomeDefaults <- list(
  outcome_id_col = NULL,
  endpoint_col = "PARAMCD",
  endpoint_label_col = "PARAM",
  time_col = "AVAL",
  censor_col = "CNSR",
  event_col = NULL,
  endpoint = NULL
)

# Why a participant's outcome is left out, in the chart's words.
chrOutcomeLeftOut <- c(
  none = "No outcome for the endpoint",
  several = "More than one outcome row for the endpoint",
  missing = "Time or flag is missing or not a number",
  flag = "Flag is not 0 or 1",
  negative = "Time is negative"
)

# The settings as given, with an event column named alone read as the flag:
# the default censor column is then none (the chart's `flaggedSettings`).
Chart_FlaggedSettings <- function(lSettings) {
  if (!is.null(lSettings[["event_col"]]) && !"censor_col" %in% names(lSettings)) {
    lSettings["censor_col"] <- list(NULL)
  }
  lSettings
}

# The outcome settings refused as the chart refuses them.
Chart_CheckOutcomeSettings <- function(lConfig) {
  IsName <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue))
  for (strKey in c("outcome_id_col", "endpoint_label_col", "censor_col", "event_col")) {
    if (!is.null(lConfig[[strKey]]) && !IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single column name (a character string), or NULL")
    }
  }
  for (strKey in c("endpoint_col", "time_col")) {
    if (!IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single column name (a character string)")
    }
  }
  if (is.null(lConfig[["censor_col"]]) == is.null(lConfig[["event_col"]])) {
    Core_Stop(
      "Name exactly one of 'censor_col' (1 = censored, as ADaM's CNSR) and 'event_col' (1 = event); ",
      "give the other as NULL"
    )
  }
  if (!is.null(lConfig[["endpoint"]]) && !IsName(lConfig[["endpoint"]])) {
    Core_Stop("Setting 'endpoint' must be the name of an endpoint, or NULL for the first")
  }
  invisible(NULL)
}

# The flag column and what R is told it is: `censor` or `event`.
Chart_FlagOf <- function(lConfig) {
  if (!is.null(lConfig$censor_col)) list(col = lConfig$censor_col, field = "censor") else list(col = lConfig$event_col, field = "event")
}

# The outcomes table must have the columns the settings name; an empty table is
# no outcomes table, and is not checked. `strTable` is what the table is called
# when it is refused.
Chart_CheckOutcomes <- function(dfOutcomes, lConfig, strTable = "the outcomes table") {
  if (is.null(dfOutcomes) || nrow(dfOutcomes) == 0L) {
    return(invisible(NULL))
  }
  lFlag <- Chart_FlagOf(lConfig)
  strIdKey <- if (is.null(lConfig$outcome_id_col)) "id_col" else "outcome_id_col"
  chrKeys <- c("endpoint_col", strIdKey, "time_col", paste0(lFlag$field, "_col"))
  chrColumns <- c(lConfig$endpoint_col, lConfig[[strIdKey]], lConfig$time_col, lFlag$col)
  for (iNeeded in seq_along(chrKeys)) {
    if (!chrColumns[iNeeded] %in% names(dfOutcomes)) {
      Core_Stop(strTable, " has no column '", chrColumns[iNeeded], "' (setting '", chrKeys[iNeeded], "').")
    }
  }
  invisible(NULL)
}

# The endpoints the table has, by name with numbers as numbers, each with the
# first label written for it (or its name).
Chart_Endpoints <- function(dfOutcomes, lConfig) {
  if (is.null(dfOutcomes) || nrow(dfOutcomes) == 0L) {
    return(data.frame(endpoint = character(0), label = character(0), stringsAsFactors = FALSE))
  }
  chrEndpoints <- Core_Levels(dfOutcomes[[lConfig$endpoint_col]])
  chrText <- Core_Text(dfOutcomes[[lConfig$endpoint_col]])
  chrLabels <- vapply(chrEndpoints, function(strEndpoint) {
    strLabelCol <- lConfig$endpoint_label_col
    if (is.null(strLabelCol) || !strLabelCol %in% names(dfOutcomes)) {
      return(strEndpoint)
    }
    xLabels <- dfOutcomes[[strLabelCol]]
    iFound <- which(!is.na(chrText) & chrText == strEndpoint & !Core_IsBlank(xLabels))
    if (length(iFound) == 0L) strEndpoint else Core_Text(xLabels[iFound[1L]])
  }, character(1), USE.NAMES = FALSE)
  data.frame(endpoint = chrEndpoints, label = chrLabels, stringsAsFactors = FALSE)
}

# A time or a flag as the chart reads it: a number, or text that reads as one;
# a logical, TRUE or FALSE, or text written "TRUE", "true", "FALSE" or
# "false", is 1 or 0, as Analyze_Survival takes a logical flag. Otherwise NA.
Chart_OutcomeNumber <- function(xValue) {
  if (is.logical(xValue)) {
    return(as.numeric(xValue))
  }
  nValue <- Core_Number(xValue)
  if (is.character(xValue) || is.factor(xValue)) {
    chrText <- as.character(xValue)
    nValue[chrText %in% c("TRUE", "true")] <- 1
    nValue[chrText %in% c("FALSE", "false")] <- 0
  }
  nValue
}

# Each participant's outcome for one endpoint, as the chart reads it
# (`outcomesOf`): one row per id asked for, with the time, the flag and whether
# it is an event; or, where there is none to use, NA for each and the reason,
# in the chart's words.
Chart_Outcomes <- function(dfOutcomes, lConfig, strEndpoint, chrIds) {
  nIds <- length(chrIds)
  dfRead <- data.frame(
    time = rep(NA_real_, nIds), flag = rep(NA_real_, nIds), event = rep(NA, nIds),
    reason = rep(unname(chrOutcomeLeftOut["none"]), nIds), stringsAsFactors = FALSE
  )
  if (is.null(dfOutcomes) || nrow(dfOutcomes) == 0L || nIds == 0L) {
    return(dfRead)
  }
  strIdCol <- if (is.null(lConfig$outcome_id_col)) lConfig$id_col else lConfig$outcome_id_col
  lFlag <- Chart_FlagOf(lConfig)
  chrEndpoint <- Core_Text(dfOutcomes[[lConfig$endpoint_col]])
  bOf <- !is.na(chrEndpoint) & chrEndpoint == strEndpoint & !Core_IsBlank(dfOutcomes[[strIdCol]])
  dfOf <- dfOutcomes[bOf, , drop = FALSE]
  chrOfIds <- Core_Text(dfOf[[strIdCol]])
  nCount <- tabulate(match(chrOfIds, chrIds), nbins = nIds)
  iFirst <- match(chrIds, chrOfIds)
  nTime <- Chart_OutcomeNumber(dfOf[[lConfig$time_col]])[iFirst]
  nFlag <- Chart_OutcomeNumber(dfOf[[lFlag$col]])[iFirst]
  strReason <- ifelse(
    nCount == 0L, chrOutcomeLeftOut[["none"]],
    ifelse(
      nCount > 1L, chrOutcomeLeftOut[["several"]],
      ifelse(
        is.na(nTime) | is.na(nFlag), chrOutcomeLeftOut[["missing"]],
        ifelse(!nFlag %in% c(0, 1), chrOutcomeLeftOut[["flag"]], ifelse(nTime < 0, chrOutcomeLeftOut[["negative"]], NA_character_))
      )
    )
  )
  bUsed <- is.na(strReason)
  dfRead$reason <- strReason
  dfRead$time[bUsed] <- nTime[bUsed]
  dfRead$flag[bUsed] <- nFlag[bUsed]
  dfRead$event[bUsed] <- if (lFlag$field == "censor") nFlag[bUsed] == 0 else nFlag[bUsed] == 1
  dfRead
}
