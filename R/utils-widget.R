# What every gsm.bio widget shares: how it is sized, and how R's answers are
# written so that a page reads them as the chart's connection expects.

#' Default sizing policy for gsm.bio widgets
#'
#' A bio.viz chart lays out as a normal document (controls beside the chart,
#' the statistics line and the listing beneath), so the widgets default to the
#' full container width rather than htmlwidgets' fixed 960px. This is the
#' policy gsm.safety's widgets use.
#'
#' @return An [htmlwidgets::sizingPolicy()].
#'
#' @keywords internal
#' @noRd
WidgetSizingPolicy <- function() {
  htmlwidgets::sizingPolicy(
    defaultWidth = "100%",
    browser.defaultWidth = "100%",
    viewer.defaultWidth = "100%",
    knitr.defaultWidth = "100%"
  )
}

#' Write an R value in the shape a page reads it
#'
#' Turns what a statistics function returned into plain nested lists that
#' become JSON in the shape bio.viz's connection gives a result computed in the
#' browser (bio.viz, docs/r-connection.md, "What R receives and returns"), so a
#' stored result reads the same as a live one:
#'
#' | R value | Becomes | In the page |
#' |---|---|---|
#' | data frame, at any depth | an unnamed list of rows, each a named list | an array of row objects |
#' | named list, named vector | a named list | an object |
#' | unnamed list | an unnamed list | an array, whatever its length |
#' | unnamed vector of length one | itself | a single value |
#' | unnamed vector of any other length | an unnamed list | an array |
#' | factor | text | text |
#' | `NA`, `NULL` | `NULL` | `null` |
#'
#' Nothing is left for the JSON writer to decide: a named vector's names are
#' kept by making it a list, and a table has one shape whatever its size.
#'
#' @param xValue The value: a statistics result, or any member of one.
#'
#' @return The value as nested lists, single values and `NULL`.
#'
#' @keywords internal
#' @noRd
StoredValue <- function(xValue) {
  if (is.null(xValue)) {
    return(NULL)
  }
  if (is.data.frame(xValue)) {
    return(lapply(seq_len(nrow(xValue)), function(iRow) {
      lapply(xValue, function(xColumn) StoredValue(xColumn[[iRow]]))
    }))
  }
  if (is.factor(xValue)) {
    xValue <- as.character(xValue)
  }
  bNamed <- !is.null(names(xValue)) && length(xValue) > 0L && all(nzchar(names(xValue)))
  if (is.list(xValue)) {
    lValue <- lapply(xValue, StoredValue)
    return(if (bNamed) lValue else unname(lValue))
  }
  if (!bNamed && length(xValue) == 1L) {
    return(if (is.na(xValue)) NULL else xValue)
  }
  lValue <- lapply(seq_along(xValue), function(iValue) if (is.na(xValue[[iValue]])) NULL else unname(xValue[iValue]))
  if (bNamed) stats::setNames(lValue, names(xValue)) else lValue
}

#' Which R computed a page's stored results
#'
#' A stored result is the answer of the R that built the widget, and R's own
#' answer can differ between versions (see [StatisticsResult]). The page says
#' which R answered, from this record.
#'
#' @return A named list: `r_version`, `gsm_bio_version`, `platform` and
#'   `computed_at`, the time in UTC as ISO 8601 text.
#'
#' @keywords internal
#' @noRd
StoredResultsProvenance <- function() {
  list(
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    gsm_bio_version = unname(getNamespaceVersion("gsm.bio")),
    platform = R.version$platform,
    computed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Check what every widget is given
#'
#' The tables, the settings list and the debug switch, refused with a message
#' naming the argument. The setting `connection` is the widget's to make: the
#' chart's connection to R is made in the page from the results the widget
#' stores.
#'
#' @keywords internal
#' @noRd
Widget_CheckInputs <- function(dfResults, dfParticipants, lSettings, bDebug) {
  if (!is.data.frame(dfResults)) {
    stop("dfResults is not a data.frame", call. = FALSE)
  }
  if (!is.null(dfParticipants) && !is.data.frame(dfParticipants)) {
    stop("dfParticipants is not a data.frame or NULL", call. = FALSE)
  }
  if (!is.list(lSettings) || is.data.frame(lSettings)) {
    stop("lSettings must be a list, but not a data.frame", call. = FALSE)
  }
  if (length(lSettings) > 0L && (is.null(names(lSettings)) || !all(nzchar(names(lSettings))))) {
    stop("lSettings must name every setting", call. = FALSE)
  }
  if (!(is.logical(bDebug) && length(bDebug) == 1L && !is.na(bDebug))) {
    stop("bDebug is not a logical", call. = FALSE)
  }
  if ("connection" %in% names(lSettings)) {
    stop(
      "Setting 'connection' cannot be given: the widget makes the chart's connection to R from the results it stores",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Check that the tables have the columns the settings map
#'
#' @param lConfig `list` The chart's settings in full, as its `*_Settings()`
#'   function returns them.
#'
#' @keywords internal
#' @noRd
Widget_CheckColumns <- function(lConfig, dfResults, dfParticipants) {
  for (strKey in c("id_col", "measure_col", "value_col", "visit_col")) {
    if (!lConfig[[strKey]] %in% names(dfResults)) {
      stop("Column '", lConfig[[strKey]], "' (setting '", strKey, "') not found in dfResults", call. = FALSE)
    }
  }
  if (!is.null(dfParticipants)) {
    strParticipantIdCol <- if (is.null(lConfig$participant_id_col)) lConfig$id_col else lConfig$participant_id_col
    if (!strParticipantIdCol %in% names(dfParticipants)) {
      stop(
        "Column '", strParticipantIdCol, "' (setting '",
        if (is.null(lConfig$participant_id_col)) "id_col" else "participant_id_col", "') not found in dfParticipants",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Name the baseline visits to the chart outright
#'
#' R and the chart then cannot resolve the baseline differently, and every
#' stored result says which baseline it rests on. When the settings name no
#' baseline visit it is the first visit in visit order, which is what the chart
#' chooses.
#'
#' @return A list of `config`, the settings R reads, and `settings`, the
#'   settings the page is given, each with `baseline_visits` filled in.
#'
#' @keywords internal
#' @noRd
Widget_NameBaseline <- function(lConfig, lSettings, dfResults) {
  if (is.null(lConfig$baseline_visits)) {
    chrFirstVisit <- Core_First(Core_Visits(dfResults, Chart_CoreSettings(lConfig)))
    if (length(chrFirstVisit) == 1L) {
      lConfig$baseline_visits <- chrFirstVisit
      lSettings$baseline_visits <- chrFirstVisit
    }
  }
  list(config = lConfig, settings = lSettings)
}

#' Name the value an `all = FALSE` filter opens on to the chart outright
#'
#' A filter set `all = FALSE` with no start the data has opens on its first
#' value. R and the reader's browser can order a column's values differently
#' (a letter with an accent, punctuation), so R's first value is handed to the
#' chart as the filter's `start`, which the chart keeps: R computes on the
#' participants the chart draws, and every stored result is found. Every spec
#' on that column is named, so two specs on one column agree. Filters that name
#' no first value leave the settings as they were given.
#'
#' @return A list of `config`, the settings R reads, and `settings`, the
#'   settings the page is given.
#'
#' @keywords internal
#' @noRd
Widget_NameFilters <- function(lConfig, lSettings, dfResults, dfParticipants) {
  lUnchanged <- list(config = lConfig, settings = lSettings)
  if (is.null(lConfig$filters) || is.null(dfParticipants) || nrow(dfParticipants) == 0L) {
    return(lUnchanged)
  }
  lState <- Chart_Filters(dfParticipants, lConfig, NULL)
  chrNamed <- unique(unlist(lapply(lConfig$filters, function(lSpec) {
    strColumn <- lSpec$value_col
    if (identical(lSpec$all, FALSE) && !isTRUE(lSpec$multiple) && !is.null(lState[[strColumn]])) strColumn
  })))
  if (length(chrNamed) == 0L) {
    return(lUnchanged)
  }
  lSpecs <- lapply(lConfig$filters, function(lSpec) {
    if (lSpec$value_col %in% chrNamed) {
      lSpec$start <- lState[[lSpec$value_col]]
    }
    lSpec
  })
  lConfig$filters <- lSpecs
  lSettings$filters <- lSpecs
  list(config = lConfig, settings = lSettings)
}

#' Text as UTF-8, marked so, in tables, lists and factors
#'
#' A widget's tables, settings and stored results are written into its page as
#' JSON, and jsonlite writes text R holds unmarked and not ASCII as escapes
#' ("<c3><96>dem" for an O with an umlaut, then "dem") in a session whose locale is not UTF-8. So text is
#' marked UTF-8 before anything is computed or written: text held as its
#' UTF-8 bytes is marked so, text marked Latin-1 is converted, and other
#' unmarked text is converted from the session's own encoding. ASCII text,
#' numbers and logicals are left as they are.
#'
#' @keywords internal
#' @noRd
Widget_Utf8 <- function(xValue) {
  Mark <- function(chrText) {
    bWide <- !is.na(chrText) & grepl("[^ -~\t\n\v\f\r]", chrText, useBytes = TRUE)
    if (!any(bWide)) {
      return(chrText)
    }
    bUnknown <- bWide & Encoding(chrText) == "unknown"
    bBytes <- bUnknown & validUTF8(chrText)
    Encoding(chrText[bBytes]) <- "UTF-8"
    bConvert <- (bWide & Encoding(chrText) == "latin1") | (bUnknown & !bBytes)
    chrText[bConvert] <- enc2utf8(chrText[bConvert])
    chrText
  }
  if (is.factor(xValue)) {
    levels(xValue) <- Mark(levels(xValue))
    return(xValue)
  }
  if (is.character(xValue)) {
    return(Mark(xValue))
  }
  if (is.data.frame(xValue)) {
    xValue[] <- lapply(xValue, Widget_Utf8)
    names(xValue) <- Mark(names(xValue))
    return(xValue)
  }
  if (is.list(xValue)) {
    lNames <- names(xValue)
    xValue <- lapply(xValue, Widget_Utf8)
    if (!is.null(lNames)) names(xValue) <- Mark(lNames)
    return(xValue)
  }
  xValue
}

#' Make a widget from its tables, its settings and R's answers
#'
#' @param strName `character` The widget's name, which is its binding's.
#' @param lStored `list` The stored results, as [Chart_Answer()] returns them.
#'
#' @return An `htmlwidget` whose payload carries the tables, the settings, the
#'   stored results in the shape the chart's connection reads, and which R
#'   computed them.
#'
#' @keywords internal
#' @noRd
Widget_Create <- function(strName, dfResults, dfParticipants, lSettings, lStored, width, height, elementId, bDebug) {
  x <- list(
    dfResults = dfResults,
    dfParticipants = dfParticipants,
    lSettings = lSettings,
    bDebug = bDebug,
    bAutoWidth = is.null(width),
    bAutoHeight = is.null(height),
    lStatistics = list(
      computed_by = StoredResultsProvenance(),
      results = lapply(lStored, function(lResult) {
        list(
          name = lResult$name, args = lResult$args, dataId = lResult$dataId, rows = lResult$rows,
          value = StoredValue(lResult$value)
        )
      })
    )
  )
  htmlwidgets::createWidget(
    name = strName,
    x = Widget_Utf8(x),
    width = width,
    height = height,
    package = "gsm.bio",
    elementId = elementId,
    sizingPolicy = WidgetSizingPolicy()
  )
}
