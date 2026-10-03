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
Chart_Names <- function(xValue, strSetting) {
  if (is.null(xValue)) {
    return(NULL)
  }
  xValues <- unlist(xValue)
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
# opens on all.
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
  # they decide something.
  for (lSpec in lSpecs) {
    strColumn <- lSpec$value_col
    chrSelected <- lState[[strColumn]]
    bAll <- !identical(lSpec$all, FALSE)
    if (isTRUE(lSpec$multiple)) {
      if (!is.null(chrSelected)) {
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
# filter is set to, by its column, as Chart_Filters() gives it. Returns a list of
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
    if (length(chrSelection) > 0L) {
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
