# bio.viz's core, in R: named variables resolved to one row per participant.
#
# A bio.viz chart resolves its variables in the page, with `BioViz.core.frame`.
# A widget that ships R's answers with the page has to resolve the same
# variables in R first, so that R computes on the rows the chart will draw. This
# file is that second copy, and the only rule gsm.bio writes twice. It follows
# bio.viz's docs/core.md and src/core/frame.js line for line, and
# tests/testthat/test-Core_Frame.R holds what it returns to frames written by
# the core of the vendored bundle itself.
#
# Everything here is arithmetic on a participant's own results: a difference, a
# ratio, the mean of a participant's baseline visits. Nothing is estimated or
# tested, and nothing is summarised across participants. Nothing is exported.

# Why a participant is not in the frame, in the order bio.viz lists them.
chrCoreDropped <- c(
  NOT_IN_PARTICIPANT_TABLE = "Not in the participant table",
  NO_RESULT = "No result at the visit",
  MISSING_RESULT = "Result at the visit is missing or not a number",
  NO_BASELINE = "No baseline result",
  MISSING_BASELINE = "Baseline result is missing or not a number",
  ZERO_BASELINE = "Baseline is zero",
  NEGATIVE_BASELINE = "Baseline is negative",
  EMPTY_COLUMN = "Column is empty",
  VARYING_COLUMN = "Column has more than one value for the participant",
  NOT_A_NUMBER = "Column value is not a number"
)

chrCoreValueTypes <- c("raw", "baseline", "change", "fold_change", "percent_change")
chrCoreBaselineStats <- c("mean", "min", "max", "first")

lCoreDefaults <- list(
  id_col = "USUBJID",
  measure_col = "TEST",
  value_col = "STRESN",
  visit_col = "VISIT",
  visit_order_col = "VISITNUM",
  participant_id_col = NULL,
  baseline_visits = NULL,
  baseline_stat = "mean"
)

Core_Stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

# Lays the settings given over the defaults. A setting given as NULL stays in
# the list as NULL, which `modifyList()` would drop.
Core_Overlay <- function(lDefaults, lGiven) {
  for (strName in names(lGiven)) {
    lDefaults[strName] <- list(lGiven[[strName]])
  }
  lDefaults
}

# A value as the text JavaScript's `String()` gives it, which is how the chart
# compares the levels of a category. Text is itself; a whole number has no
# decimal point and no exponent; any other number is the shortest text that
# reads back as the same number; a logical is `true` or `false`. A missing
# value is NA.
Core_Text <- function(xValue) {
  if (is.character(xValue)) {
    return(xValue)
  }
  if (is.factor(xValue)) {
    return(as.character(xValue))
  }
  if (is.logical(xValue)) {
    return(ifelse(is.na(xValue), NA_character_, ifelse(xValue, "true", "false")))
  }
  if (is.numeric(xValue)) {
    chrText <- rep(NA_character_, length(xValue))
    bFinite <- is.finite(xValue)
    bWhole <- bFinite & xValue == round(xValue) & abs(xValue) < 1e15
    chrText[bWhole] <- sprintf("%.0f", xValue[bWhole])
    for (iValue in which(bFinite & !bWhole)) {
      for (iDigits in 15:17) {
        strText <- sprintf("%.*g", iDigits, xValue[iValue])
        if (identical(as.numeric(strText), as.numeric(xValue[iValue]))) break
      }
      chrText[iValue] <- strText
    }
    return(chrText)
  }
  as.character(xValue)
}

# Nothing was written in the cell: a missing value, or text that is empty or
# only white space.
Core_IsBlank <- function(xValue) {
  if (is.character(xValue) || is.factor(xValue)) {
    chrText <- as.character(xValue)
    return(is.na(chrText) | !nzchar(trimws(chrText)))
  }
  is.na(xValue)
}

# A finite number, or text that reads as one; otherwise NA. A table read from a
# CSV file holds its numbers as text, and " 7 " is 7.
Core_Number <- function(xValue) {
  if (is.factor(xValue)) {
    xValue <- as.character(xValue)
  }
  nValue <- if (is.character(xValue)) {
    suppressWarnings(as.numeric(trimws(xValue)))
  } else if (is.numeric(xValue)) {
    as.numeric(xValue)
  } else {
    rep(NA_real_, length(xValue))
  }
  nValue[!is.finite(nValue)] <- NA_real_
  nValue
}

# Text as UTF-8: text R holds marked as Latin-1 is converted, and other text is
# left as the bytes it is. Text R holds unmarked is usually UTF-8 read in a
# session whose locale is not, and converting it from that locale would spoil it.
Core_Utf8 <- function(chrText) {
  bLatin1 <- !is.na(chrText) & Encoding(chrText) == "latin1"
  chrText[bLatin1] <- enc2utf8(chrText[bLatin1])
  chrText
}

# Text as its UTF-8 bytes, which is the order R sorts in: by code point, the
# same in every session whatever its locale.
Core_Bytes <- function(strText) {
  charToRaw(Core_Utf8(strText))
}

# Which of two byte strings comes first: -1, 0 or 1.
Core_CompareBytes <- function(rawFirst, rawSecond) {
  nShared <- min(length(rawFirst), length(rawSecond))
  if (nShared > 0L) {
    nDifference <- as.integer(rawFirst[seq_len(nShared)]) - as.integer(rawSecond[seq_len(nShared)])
    iAt <- which(nDifference != 0L)
    if (length(iAt) > 0L) {
      return(sign(nDifference[iAt[1L]]))
    }
  }
  sign(length(rawFirst) - length(rawSecond))
}

# Text sorted by code point, as the chart sorts the parts of a key and the
# values of a filter in force. `sort(method = "radix")` gives the same order in
# a UTF-8 session, and refuses text that is not ASCII in a session that is not.
Core_SortText <- function(chrText) {
  chrBytes <- Core_Utf8(chrText)
  Encoding(chrBytes) <- "bytes"
  chrText[order(chrBytes, method = "radix")]
}

# Whether one name comes before another, by name, with numbers inside a name
# counted as numbers, so that `Week 2` comes before `Week 12`, and the letters
# A to Z read as a to z. Anything else is compared by its code point, the same
# in every session whatever its locale. The chart orders with the reader's
# browser's collation, which agrees with this for names made of ASCII letters,
# digits and spaces, and not always for anything else: a letter with an
# accent (the browser puts É with E, this puts it after Z) or punctuation
# (each has its own order). It is used to order what is shown, to find the
# first visit when the table has no visit-order column, and to find the first
# value an `all = FALSE` filter opens on. Where a stored result rests on it the
# widget names R's choice to the chart outright: the baseline visits
# (Widget_NameBaseline()) and that first value (Widget_NameFilters()).
Core_NaturalCompare <- function(strFirst, strSecond) {
  Parts <- function(strText) {
    regmatches(strText, gregexpr("[0-9]+|[^0-9]+", strText, useBytes = TRUE))[[1]]
  }
  Lower <- function(rawText) {
    bUpper <- rawText >= as.raw(0x41) & rawText <= as.raw(0x5a)
    rawText[bUpper] <- as.raw(as.integer(rawText[bUpper]) + 32L)
    rawText
  }
  IsDigits <- function(strPart) grepl("^[0-9]", strPart, useBytes = TRUE)
  # A letter, of any alphabet: an ASCII letter, or anything past ASCII.
  IsLetter <- function(strPart) {
    rawFirst <- Core_Bytes(strPart)[1L]
    grepl("^[A-Za-z]", strPart, useBytes = TRUE) || rawFirst >= as.raw(0x80)
  }
  chrFirst <- Parts(Core_Utf8(strFirst))
  chrSecond <- Parts(Core_Utf8(strSecond))
  for (iPart in seq_len(min(length(chrFirst), length(chrSecond)))) {
    strA <- chrFirst[iPart]
    strB <- chrSecond[iPart]
    bDigitsA <- IsDigits(strA)
    bDigitsB <- IsDigits(strB)
    if (bDigitsA && bDigitsB) {
      nDifference <- as.numeric(strA) - as.numeric(strB)
      if (nDifference != 0) {
        return(sign(nDifference))
      }
    } else if (bDigitsA != bDigitsB) {
      # A digit sorts before a letter and after a space or punctuation.
      iSign <- if (IsLetter(if (bDigitsA) strB else strA)) -1 else 1
      return(if (bDigitsA) iSign else -iSign)
    } else {
      iOrder <- Core_CompareBytes(Lower(Core_Bytes(strA)), Lower(Core_Bytes(strB)))
      if (iOrder != 0) {
        return(iOrder)
      }
    }
  }
  if (length(chrFirst) != length(chrSecond)) {
    return(sign(length(chrFirst) - length(chrSecond)))
  }
  iOrder <- Core_CompareBytes(Core_Bytes(strFirst), Core_Bytes(strSecond))
  # The same letters in another case: lower case first.
  -iOrder
}

# The first of several names, or none when there is none.
Core_First <- function(chrNames) {
  chrNames[seq_len(min(1L, length(chrNames)))]
}

# Sorts names with a comparison, keeping the order of names that compare equal.
Core_SortWith <- function(chrNames, Compare) {
  for (iNext in seq_along(chrNames)[-1]) {
    strNext <- chrNames[iNext]
    iAt <- iNext - 1L
    while (iAt >= 1L && Compare(chrNames[iAt], strNext) > 0) {
      chrNames[iAt + 1L] <- chrNames[iAt]
      iAt <- iAt - 1L
    }
    chrNames[iAt + 1L] <- strNext
  }
  chrNames
}

# Distinct values that are not blank, as text, sorted by name with numbers as
# numbers.
Core_Levels <- function(xValue) {
  chrText <- unique(Core_Text(xValue)[!Core_IsBlank(xValue)])
  Core_SortWith(chrText, Core_NaturalCompare)
}

# The core's settings in full: the caller's over the defaults, checked. Beside
# the settings a chart maps (lCoreDefaults), the frame takes `required`: the
# names of the variables a participant must have to be in the frame, NULL for
# all of them. A chart sets it, not a page: the correlation matrix keeps a
# participant who has only some of its variables.
Core_Settings <- function(lSettings = list()) {
  if (!is.list(lSettings) || is.data.frame(lSettings)) {
    Core_Stop("settings must be a list")
  }
  lDefaults <- c(lCoreDefaults, list(required = NULL))
  chrUnknown <- setdiff(names(lSettings), names(lDefaults))
  if (length(chrUnknown) > 0L) {
    Core_Stop("`", chrUnknown[1], "` is not a setting of the frame. Its settings are ", paste(names(lDefaults), collapse = ", "), ".")
  }
  lConfig <- Core_Overlay(lDefaults, lSettings)
  if (!is.null(lConfig$required)) {
    chrRequired <- as.character(unlist(lConfig$required))
    if (anyNA(chrRequired) || !all(nzchar(trimws(chrRequired)))) {
      Core_Stop("`required` must be a list of the names of variables, or NULL for all of them.")
    }
    lConfig$required <- unique(chrRequired)
  }
  IsName <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue))
  for (strKey in c("id_col", "measure_col", "value_col", "visit_col")) {
    if (!IsName(lConfig[[strKey]])) {
      Core_Stop("`", strKey, "` must be the name of a column.")
    }
  }
  for (strKey in c("visit_order_col", "participant_id_col")) {
    if (!is.null(lConfig[[strKey]]) && !IsName(lConfig[[strKey]])) {
      Core_Stop("`", strKey, "` must be the name of a column, or NULL.")
    }
  }
  if (!is.null(lConfig$baseline_visits)) {
    chrVisits <- unlist(lConfig$baseline_visits)
    if (!is.character(chrVisits) || length(chrVisits) == 0L || anyNA(chrVisits) || !all(nzchar(trimws(chrVisits)))) {
      Core_Stop("`baseline_visits` must be a name, or several names, and none of them empty.")
    }
    lConfig$baseline_visits <- unique(chrVisits)
  }
  if (!IsName(lConfig$baseline_stat) || !lConfig$baseline_stat %in% chrCoreBaselineStats) {
    Core_Stop("`baseline_stat` must be one of ", paste(chrCoreBaselineStats, collapse = ", "), ".")
  }
  lConfig
}

# A variable in full: `list(kind = "measure", measure, visit, value)` for a
# biomarker at a visit, or `list(kind = "column", col, type)` for a column.
Core_Variable <- function(lSpec) {
  IsName <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue))
  if (!is.list(lSpec) || is.data.frame(lSpec)) {
    Core_Stop("a variable must be a list: list(measure, visit, value) for a biomarker at a visit, or list(col) for a column.")
  }
  chrUnknown <- setdiff(names(lSpec), c("kind", "measure", "visit", "value", "col", "type"))
  if (length(chrUnknown) > 0L) {
    Core_Stop("a variable has a member that is not known: ", paste(chrUnknown, collapse = ", "), ".")
  }
  bMeasure <- !is.null(lSpec$measure)
  bColumn <- !is.null(lSpec$col)
  if (bMeasure == bColumn) {
    Core_Stop("a variable must name a biomarker (`measure`) or a column (`col`), and it names ", if (bMeasure) "both" else "neither", ".")
  }
  if (bColumn) {
    if (!IsName(lSpec$col)) {
      Core_Stop("`col` must be the name of a column.")
    }
    if (!is.null(lSpec$visit) || !is.null(lSpec$value)) {
      Core_Stop("a column takes no `visit` and no `value`.")
    }
    if (!is.null(lSpec$type) && !identical(lSpec$type, "number")) {
      Core_Stop("`type` can only be 'number', to read the column as a number.")
    }
    return(list(kind = "column", col = lSpec$col, type = lSpec$type))
  }
  if (!IsName(lSpec$measure)) {
    Core_Stop("`measure` must be the name of a biomarker.")
  }
  strValue <- if (is.null(lSpec$value)) "raw" else lSpec$value
  if (!IsName(strValue) || !strValue %in% chrCoreValueTypes) {
    Core_Stop("`value` must be one of ", paste(chrCoreValueTypes, collapse = ", "), ".")
  }
  if (strValue == "baseline") {
    if (!is.null(lSpec$visit)) {
      Core_Stop("a baseline value is read at the baseline visits named in settings: it takes no `visit`.")
    }
    return(list(kind = "measure", measure = lSpec$measure, visit = NULL, value = strValue))
  }
  if (!IsName(lSpec$visit)) {
    Core_Stop("a variable on a biomarker must name its visit.")
  }
  list(kind = "measure", measure = lSpec$measure, visit = lSpec$visit, value = strValue)
}

Core_NeedColumn <- function(dfTable, strColumn, strSetting, strTable) {
  if (!strColumn %in% names(dfTable)) {
    Core_Stop("the ", strTable, " table has no column `", strColumn, "` (`", strSetting, "`).")
  }
}

# The visits of a results table, in visit order: by the visit-order column when
# the table has one, otherwise by name. Only visits with at least one usable
# result are listed. The first of them is the baseline visit when settings name
# none.
Core_Visits <- function(dfResults, lSettings = list()) {
  lConfig <- Core_Settings(lSettings)
  if (nrow(dfResults) == 0L) {
    return(character(0))
  }
  Core_NeedColumn(dfResults, lConfig$visit_col, "visit_col", "results")
  Core_NeedColumn(dfResults, lConfig$value_col, "value_col", "results")
  bOrdered <- !is.null(lConfig$visit_order_col) && lConfig$visit_order_col %in% names(dfResults)
  bUsable <- !Core_IsBlank(dfResults[[lConfig$visit_col]]) & !is.na(Core_Number(dfResults[[lConfig$value_col]]))
  chrVisit <- Core_Text(dfResults[[lConfig$visit_col]])[bUsable]
  chrVisits <- unique(chrVisit)
  # The order of a visit is the one its first usable row carries.
  nOrder <- if (bOrdered) {
    Core_Number(dfResults[[lConfig$visit_order_col]])[bUsable][match(chrVisits, chrVisit)]
  } else {
    rep(NA_real_, length(chrVisits))
  }
  names(nOrder) <- chrVisits
  Core_SortWith(chrVisits, function(strFirst, strSecond) {
    nFirst <- nOrder[[strFirst]]
    nSecond <- nOrder[[strSecond]]
    if (!is.na(nFirst) && !is.na(nSecond) && nFirst != nSecond) {
      return(sign(nFirst - nSecond))
    }
    Core_NaturalCompare(strFirst, strSecond)
  })
}

# Resolves named variables to one row per participant.
#
# dfResults       the results table, one row per participant, biomarker and
#                 visit; or a wide table, one row per participant
# dfParticipants  the participant table, one row per participant, or NULL
# lVariables      the variables, each under the name its column is to have:
#                 list(y = list(measure, visit, value), x = list(col))
# lSettings       column names and how the baseline is found
#
# Returns a list: `data`, a data frame with one row per participant, holding
# the participant's id under the name of the id column and one column per
# variable; `id_col`; `participants`, how many participants were seen, which
# equals the rows of `data` plus the counts in `dropped`; `dropped`, a data
# frame of `reason`, `variable` and `n`; and `baseline_visits`, the baseline
# visits used, or NULL when no variable needed a baseline.
#
# A participant for whom a variable cannot be worked out is left out and
# counted once, under the first such variable in the order the variables were
# given. Rows that were read and not used, which the chart also counts, are not
# counted here: no statistic rests on them.
Core_Frame <- function(dfResults, dfParticipants = NULL, lVariables, lSettings = list()) {
  lConfig <- Core_Settings(lSettings)
  if (!is.data.frame(dfResults)) {
    Core_Stop("`dfResults` must be a data frame, one row per result.")
  }
  if (!is.null(dfParticipants) && !is.data.frame(dfParticipants)) {
    Core_Stop("`dfParticipants` must be a data frame, one row per participant, or NULL.")
  }
  if (!is.list(lVariables) || length(lVariables) == 0L || is.null(names(lVariables)) || !all(nzchar(names(lVariables)))) {
    Core_Stop("the variables must be a named list, each under the name of its column.")
  }
  strIdCol <- lConfig$id_col
  if (strIdCol %in% names(lVariables)) {
    Core_Stop("a variable cannot be named `", strIdCol, "`: that column holds the participant's id.")
  }
  lNamed <- lapply(lVariables, Core_Variable)
  chrRequired <- if (is.null(lConfig$required)) names(lNamed) else lConfig$required
  for (strName in chrRequired) {
    if (!strName %in% names(lNamed)) {
      Core_Stop("`required` names `", strName, "`, which is not one of the variables.")
    }
  }
  bMeasure <- vapply(lNamed, function(lVariable) lVariable$kind == "measure", logical(1))
  bNeedsBaseline <- any(vapply(lNamed[bMeasure], function(lVariable) lVariable$value != "raw", logical(1)))

  # The columns the variables need, each checked before a row is read.
  Core_NeedColumn(dfResults, strIdCol, "id_col", "results")
  if (any(bMeasure)) {
    Core_NeedColumn(dfResults, lConfig$measure_col, "measure_col", "results")
    Core_NeedColumn(dfResults, lConfig$visit_col, "visit_col", "results")
    Core_NeedColumn(dfResults, lConfig$value_col, "value_col", "results")
  }
  strParticipantIdCol <- if (is.null(lConfig$participant_id_col)) strIdCol else lConfig$participant_id_col
  if (!is.null(dfParticipants)) {
    Core_NeedColumn(dfParticipants, strParticipantIdCol, "participant_id_col", "participant")
  }
  # A column is read from the participant table when that table has it, and
  # otherwise from the results rows.
  for (lVariable in lNamed[!bMeasure]) {
    bInParticipants <- !is.null(dfParticipants) && lVariable$col %in% names(dfParticipants)
    if (!bInParticipants && !lVariable$col %in% names(dfResults)) {
      Core_Stop(
        "no table has the column `", lVariable$col, "`: it is not in the ",
        if (!is.null(dfParticipants)) "participant table or the " else "", "results table."
      )
    }
  }

  # The baseline visits: the ones named in settings, or the first visit.
  chrBaselineVisits <- NULL
  if (bNeedsBaseline) {
    chrBaselineVisits <- lConfig$baseline_visits
    if (is.null(chrBaselineVisits)) {
      chrBaselineVisits <- Core_First(Core_Visits(dfResults, lSettings))
    }
  }

  # The results rows of each participant: a row with no id is not used.
  dfRows <- dfResults[!Core_IsBlank(dfResults[[strIdCol]]), , drop = FALSE]
  chrRowId <- Core_Text(dfRows[[strIdCol]])

  # Who the frame is about: the participant table's participants, in its order,
  # when there is one; otherwise everyone with a row of results, as first seen.
  chrIds <- unique(chrRowId)
  nNotInTable <- 0L
  dfWho <- NULL
  if (!is.null(dfParticipants)) {
    dfWho <- dfParticipants[!Core_IsBlank(dfParticipants[[strParticipantIdCol]]), , drop = FALSE]
    # A later row for a participant already in the table is not used.
    dfWho <- dfWho[!duplicated(Core_Text(dfWho[[strParticipantIdCol]])), , drop = FALSE]
    chrWho <- Core_Text(dfWho[[strParticipantIdCol]])
    nNotInTable <- sum(!chrIds %in% chrWho)
    chrIds <- chrWho
  }
  xIdValue <- if (is.null(dfWho)) {
    dfRows[[strIdCol]][match(chrIds, chrRowId)]
  } else {
    dfWho[[strParticipantIdCol]]
  }
  if (is.factor(xIdValue)) {
    xIdValue <- as.character(xIdValue)
  }

  # One value per participant, with the reason where there is none.
  Found <- function(xValue, chrReason) {
    list(value = xValue, reason = chrReason)
  }

  if (any(bMeasure)) {
    chrRowMeasure <- Core_Text(dfRows[[lConfig$measure_col]])
    chrRowVisit <- Core_Text(dfRows[[lConfig$visit_col]])
    nRowValue <- Core_Number(dfRows[[lConfig$value_col]])
  }

  # The result at a visit: the first usable result, in the table's order, among
  # the participant's rows for the biomarker and the visit.
  ResultAt <- function(strMeasure, strVisit) {
    bAt <- !is.na(chrRowMeasure) & chrRowMeasure == strMeasure & !is.na(chrRowVisit) & chrRowVisit == strVisit
    chrAtId <- chrRowId[bAt]
    nAtValue <- nRowValue[bAt]
    bUsable <- !is.na(nAtValue)
    nValue <- nAtValue[bUsable][match(chrIds, chrAtId[bUsable])]
    chrReason <- rep(NA_character_, length(chrIds))
    chrReason[is.na(nValue)] <- chrCoreDropped[["MISSING_RESULT"]]
    chrReason[!chrIds %in% chrAtId] <- chrCoreDropped[["NO_RESULT"]]
    Found(nValue, chrReason)
  }

  # One result per baseline visit that has one, brought to a single value.
  BaselineOf <- function(strMeasure) {
    lFound <- lapply(chrBaselineVisits, function(strVisit) ResultAt(strMeasure, strVisit))
    nCount <- rep(0L, length(chrIds))
    nValue <- rep(NA_real_, length(chrIds))
    bMissing <- rep(FALSE, length(chrIds))
    for (lAt in lFound) {
      bHas <- !is.na(lAt$value)
      bFirst <- bHas & nCount == 0L
      bLater <- bHas & nCount > 0L
      nValue[bFirst] <- lAt$value[bFirst]
      nValue[bLater] <- switch(lConfig$baseline_stat,
        # Added one visit at a time, as the chart adds them.
        mean = nValue[bLater] + lAt$value[bLater],
        min = pmin(nValue[bLater], lAt$value[bLater]),
        max = pmax(nValue[bLater], lAt$value[bLater]),
        first = nValue[bLater]
      )
      nCount <- nCount + bHas
      bMissing <- bMissing | (!is.na(lAt$reason) & lAt$reason == chrCoreDropped[["MISSING_RESULT"]])
    }
    if (lConfig$baseline_stat == "mean") {
      nValue <- ifelse(nCount > 0L, nValue / nCount, NA_real_)
    }
    chrReason <- rep(NA_character_, length(chrIds))
    chrReason[nCount == 0L] <- chrCoreDropped[["NO_BASELINE"]]
    chrReason[nCount == 0L & bMissing] <- chrCoreDropped[["MISSING_BASELINE"]]
    Found(nValue, chrReason)
  }

  MeasureValue <- function(lVariable) {
    if (lVariable$value == "raw") {
      return(ResultAt(lVariable$measure, lVariable$visit))
    }
    if (lVariable$value == "baseline") {
      return(BaselineOf(lVariable$measure))
    }
    # The visit is looked at before the baseline: a participant missing both
    # is counted under the visit.
    lAt <- ResultAt(lVariable$measure, lVariable$visit)
    lBaseline <- BaselineOf(lVariable$measure)
    chrReason <- ifelse(is.na(lAt$reason), lBaseline$reason, lAt$reason)
    nAt <- lAt$value
    nBaseline <- lBaseline$value
    if (lVariable$value == "change") {
      return(Found(ifelse(is.na(chrReason), nAt - nBaseline, NA_real_), chrReason))
    }
    # A ratio to a baseline of zero does not exist, and one to a negative
    # baseline has no meaning as a fold or a percent change.
    bOk <- is.na(chrReason)
    chrReason[bOk & nBaseline == 0] <- chrCoreDropped[["ZERO_BASELINE"]]
    chrReason[bOk & nBaseline < 0] <- chrCoreDropped[["NEGATIVE_BASELINE"]]
    nValue <- if (lVariable$value == "fold_change") {
      nAt / nBaseline
    } else {
      (100 * (nAt - nBaseline)) / nBaseline
    }
    Found(ifelse(is.na(chrReason), nValue, NA_real_), chrReason)
  }

  ColumnValue <- function(lVariable) {
    strCol <- lVariable$col
    chrReason <- rep(NA_character_, length(chrIds))
    if (!is.null(dfWho) && strCol %in% names(dfWho)) {
      xValue <- dfWho[[strCol]]
      chrReason[Core_IsBlank(xValue)] <- chrCoreDropped[["EMPTY_COLUMN"]]
    } else {
      # On the results rows a participant's rows must agree: the column holds
      # one value for the participant, and an empty cell beside a filled one
      # counts as that one value.
      xColumn <- dfRows[[strCol]]
      bFilled <- !Core_IsBlank(xColumn)
      chrFilledId <- chrRowId[bFilled]
      chrFilledText <- Core_Text(xColumn)[bFilled]
      xValue <- xColumn[bFilled][match(chrIds, chrFilledId)]
      bVaries <- chrIds %in% unique(chrFilledId[chrFilledText != chrFilledText[match(chrFilledId, chrFilledId)]])
      chrReason[!chrIds %in% chrFilledId] <- chrCoreDropped[["EMPTY_COLUMN"]]
      chrReason[bVaries] <- chrCoreDropped[["VARYING_COLUMN"]]
    }
    if (is.factor(xValue)) {
      xValue <- as.character(xValue)
    }
    if (identical(lVariable$type, "number")) {
      nValue <- Core_Number(xValue)
      chrReason[is.na(chrReason) & is.na(nValue)] <- chrCoreDropped[["NOT_A_NUMBER"]]
      xValue <- nValue
    }
    xValue[!is.na(chrReason)] <- NA
    Found(xValue, chrReason)
  }

  lFound <- lapply(lNamed, function(lVariable) {
    if (lVariable$kind == "measure") MeasureValue(lVariable) else ColumnValue(lVariable)
  })

  # The first required variable that cannot be worked out, in the order the
  # variables were given, is the reason the participant is left out. A variable
  # that is not required is left as NA, which R reads as missing.
  chrLeftOutBy <- rep(NA_character_, length(chrIds))
  chrLeftOutFor <- rep(NA_character_, length(chrIds))
  for (strName in intersect(names(lFound), chrRequired)) {
    bNow <- is.na(chrLeftOutBy) & !is.na(lFound[[strName]]$reason)
    chrLeftOutBy[bNow] <- strName
    chrLeftOutFor[bNow] <- lFound[[strName]]$reason[bNow]
  }
  bKept <- is.na(chrLeftOutBy)

  dfData <- data.frame(xIdValue[bKept], stringsAsFactors = FALSE)
  names(dfData) <- strIdCol
  for (strName in names(lFound)) {
    dfData[[strName]] <- lFound[[strName]]$value[bKept]
  }
  rownames(dfData) <- NULL

  # Counts in a fixed order: by variable as given, then by reason as listed.
  dfDropped <- data.frame(reason = character(0), variable = character(0), n = integer(0), stringsAsFactors = FALSE)
  if (nNotInTable > 0L) {
    dfDropped[1L, ] <- list(chrCoreDropped[["NOT_IN_PARTICIPANT_TABLE"]], NA_character_, nNotInTable)
  }
  for (strName in names(lFound)) {
    for (strReason in chrCoreDropped) {
      nLeftOut <- sum(!bKept & chrLeftOutBy == strName & chrLeftOutFor == strReason, na.rm = TRUE)
      if (nLeftOut > 0L) {
        dfDropped[nrow(dfDropped) + 1L, ] <- list(strReason, strName, nLeftOut)
      }
    }
  }
  rownames(dfDropped) <- NULL

  list(
    data = dfData,
    id_col = strIdCol,
    participants = length(chrIds) + nNotInTable,
    dropped = dfDropped,
    baseline_visits = chrBaselineVisits
  )
}
