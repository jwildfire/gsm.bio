# What every statistics table shares: its inputs, a row for one of R's
# answers written as the chart prints it, the table with its title, subtitle
# and footnotes, and the RTF writer.
#
# A table is of the view its chart opens on, worked out by the same rules
# (R/core.R, R/chart.R and the chart's own file), and every number in it is an
# Analyze_*() answer on the same rows, written by R/output.R's display rules:
# the method and the counts, the estimate and its interval to four significant
# digits, the p-value to three decimals with its bounds, labelled exploratory
# with its adjustment named, and no stars. Nothing here is exported but
# Write_RTF().

# The columns every table has, after the ones that say which statistic a row
# is.
chrTableColumns <- c("Statistic", "Method", "Estimate", "Counts", "p-value", "Note")

# The tables and the settings a table is given, checked as a widget's are,
# with the title, subtitle and footnotes checked as the chart checks them.
Table_Inputs <- function(dfResults, dfParticipants, lSettings, dfOutcomes = NULL) {
  Widget_CheckInputs(dfResults, dfParticipants, lSettings, FALSE)
  if (!is.null(dfOutcomes) && !is.data.frame(dfOutcomes)) {
    Widget_CheckOutcomes(dfOutcomes, NULL)
  }
  Output_CheckTitles(Core_Overlay(lOutputTitleDefaults, lSettings[intersect(names(lSettings), names(lOutputTitleDefaults))]))
}

# One estimate as a table cell: the line's words without the closing stop.
Table_Estimate <- function(lRow, bNamed = TRUE) {
  # In Perl's mode, so that text marked UTF-8 stays itself in any locale.
  strText <- sub("\\.$", "", Output_EstimateText(lRow), perl = TRUE)
  if (bNamed) strText else sub("^[^:]*: ", "", strText, perl = TRUE)
}

# The note a row carries: R's reason when the statistic was not computed, and
# otherwise the label every p-value carries.
Table_Note <- function(lValue) {
  Text <- function(xValue) if (is.character(xValue) && length(xValue) == 1L && !Core_IsBlank(xValue)) trimws(xValue) else NULL
  strReason <- Text(lValue$reason)
  if (identical(lValue$status, "error")) {
    return(paste0("R reported an error: ", if (is.null(strReason)) "no message" else strReason))
  }
  if (!is.null(strReason)) {
    return(strReason)
  }
  strAdjustment <- Text(lValue$adjustment)
  strAdjustment <- if (is.null(strAdjustment) || tolower(strAdjustment) == "none") {
    NULL
  } else if (strAdjustment %in% names(chrOutputAdjustments)) {
    chrOutputAdjustments[[strAdjustment]]
  } else {
    strAdjustment
  }
  if (is.null(strAdjustment)) "Exploratory, unadjusted." else paste0("Exploratory, adjusted (", strAdjustment, ").")
}

# One of R's answers as a row: its method, every estimate with its interval,
# its counts, its p-value by the display rules and its note. A statistic R did
# not compute has no p-value, and its note is R's reason. `chrNames` renames
# estimates, by R's name, as the chart names them.
Table_Row <- function(lValue, strStatistic = "", chrNames = character(0)) {
  dfEstimates <- lValue$estimates
  bComputed <- !identical(lValue$status, "error") && (is.null(lValue$reason) || is.na(lValue$reason))
  chrEstimates <- if (bComputed && is.data.frame(dfEstimates) && nrow(dfEstimates) > 0L) {
    vapply(seq_len(nrow(dfEstimates)), function(iRow) {
      lRow <- as.list(dfEstimates[iRow, ])
      if (lRow$name %in% names(chrNames)) lRow$name <- chrNames[[lRow$name]]
      Table_Estimate(lRow)
    }, character(1))
  } else {
    character(0)
  }
  nP <- lValue$p_value
  strCounts <- Output_LineCounts(lValue$counts)
  dfRow <- data.frame(
    Statistic = strStatistic,
    Method = if (is.character(lValue$method) && length(lValue$method) == 1L && !is.na(lValue$method)) lValue$method else "",
    Estimate = paste(chrEstimates, collapse = "; "),
    Counts = if (is.null(strCounts)) "" else strCounts,
    `p-value` = if (bComputed && is.numeric(nP) && length(nP) == 1L && !is.na(nP)) Output_P(nP) else "",
    Note = Table_Note(lValue),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  dfRow
}

# A table: the rows, each with the columns that say which statistic it is
# first, and as attributes its title, subtitle and footnotes (the table's own
# last), R's answers (`results`) and which answer each row is of (`result_of`).
Table_Make <- function(dfRows, lTitles, lValues, lAnswers, nResultOf, strOf = "groups") {
  rownames(dfRows) <- NULL
  lFilled <- Output_Titles(lTitles, lValues, lAnswers, strOf)
  dfRows[] <- lapply(dfRows, function(xColumn) Widget_Utf8(as.character(xColumn)))
  attr(dfRows, "title") <- lFilled$title
  attr(dfRows, "subtitle") <- lFilled$subtitle
  attr(dfRows, "footnotes") <- lFilled$footnotes
  attr(dfRows, "results") <- lAnswers
  attr(dfRows, "result_of") <- nResultOf
  dfRows
}

# r2rtf is suggested, not imported: the RTF writer needs it, and says so.
Table_HasR2rtf <- function() {
  requireNamespace("r2rtf", quietly = TRUE)
}

#' Write a Statistics Table to RTF
#'
#' Writes a table that a `Table_*()` function returned to a Rich Text Format
#' file, as a report takes it: the title and subtitle above, the table, and the
#' footnotes beneath, the table's own last. It is written by the r2rtf package,
#' which gsm.bio suggests and does not import; nothing is fetched, so it works
#' offline.
#'
#' @param dfTable `data.frame` A table from [Table_GroupComparison()] or one of
#'   the functions beside it, with its `title`, `subtitle` and `footnotes`.
#' @param strFile `character` The file to write, in a folder that exists. A
#'   file already there is replaced.
#' @param strOrientation `character` `"landscape"` (the default) or
#'   `"portrait"`.
#'
#' @return `strFile`, invisibly.
#'
#' @examples
#' if (requireNamespace("r2rtf", quietly = TRUE)) {
#'   dfTable <- Table_CrossTab(
#'     Synthetic_Results,
#'     Synthetic_Participants,
#'     lSettings = list(row_by = "ARM", col_by = "RESPONSE", title = "{rows} by {columns}")
#'   )
#'   Write_RTF(dfTable, tempfile(fileext = ".rtf"))
#' }
#'
#' @seealso [Table_GroupComparison()] and the functions beside it, which make
#'   the tables.
#' @family tables
#' @export
Write_RTF <- function(dfTable, strFile, strOrientation = "landscape") {
  if (!Table_HasR2rtf()) {
    stop(
      "Write_RTF() writes RTF with r2rtf, which is not installed. ",
      "Install it with install.packages(\"r2rtf\"); the tables themselves do not need it.",
      call. = FALSE
    )
  }
  if (!is.data.frame(dfTable)) {
    stop("dfTable is not a data.frame: give it a table that a Table_*() function returned", call. = FALSE)
  }
  if (nrow(dfTable) == 0L || ncol(dfTable) == 0L) {
    stop("dfTable has no rows: there is no statistic to write", call. = FALSE)
  }
  if (!(is.character(strFile) && length(strFile) == 1L && !is.na(strFile) && nzchar(strFile))) {
    stop("strFile must be the name of the file to write", call. = FALSE)
  }
  if (!dir.exists(dirname(strFile))) {
    stop("The folder '", dirname(strFile), "' does not exist: Write_RTF() writes into a folder that does", call. = FALSE)
  }
  if (!(is.character(strOrientation) && length(strOrientation) == 1L && strOrientation %in% c("landscape", "portrait"))) {
    stop("strOrientation must be \"landscape\" or \"portrait\"", call. = FALSE)
  }
  # Columns with nothing in them are left out of the page. Every text is
  # written as RTF, here: r2rtf is handed it as it is to go in the file.
  bFilled <- vapply(dfTable, function(xColumn) any(nzchar(as.character(xColumn))), logical(1))
  dfShown <- as.data.frame(lapply(dfTable[bFilled], function(xColumn) Table_RtfText(as.character(xColumn))), check.names = FALSE, stringsAsFactors = FALSE)
  # A wide column, the estimate or the note, takes more of the page.
  nWidths <- vapply(names(dfShown), function(strColumn) {
    if (strColumn %in% c("Estimate", "Note", "Statistic", "Method")) 3 else if (strColumn %in% c("Counts")) 2 else 1.4
  }, numeric(1))
  strTitle <- attr(dfTable, "title")
  strSubtitle <- attr(dfTable, "subtitle")
  chrFootnotes <- attr(dfTable, "footnotes")
  # r2rtf measures text on the current graphics device. It is given one of its
  # own, which writes no file, and the caller's device is current again after.
  nCaller <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  nOwn <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(nOwn)
    if (nCaller > 1L) grDevices::dev.set(nCaller)
  }, add = TRUE)
  lTable <- r2rtf::rtf_page(dfShown, orientation = strOrientation)
  if (!is.null(strTitle) || !is.null(strSubtitle)) {
    lTable <- r2rtf::rtf_title(
      lTable,
      title = Table_RtfText(if (is.null(strTitle)) "" else strTitle),
      subtitle = if (is.null(strSubtitle)) NULL else Table_RtfText(strSubtitle), text_convert = FALSE
    )
  }
  # r2rtf reads "|" as the column separator of a heading.
  lTable <- r2rtf::rtf_colheader(
    lTable, colheader = paste(Table_RtfText(gsub("|", "/", names(dfShown), fixed = TRUE)), collapse = " | "),
    col_rel_width = nWidths, text_convert = FALSE
  )
  lTable <- r2rtf::rtf_body(lTable, col_rel_width = nWidths, text_justification = "l", text_convert = FALSE)
  if (length(chrFootnotes) > 0L) {
    lTable <- r2rtf::rtf_footnote(lTable, footnote = Table_RtfText(chrFootnotes), text_convert = FALSE)
  }
  r2rtf::write_rtf(r2rtf::rtf_encode(lTable), strFile)
  invisible(strFile)
}

# Text as RTF writes it, whatever the session's locale: a backslash and a
# brace escaped, and each character that is not ASCII as RTF's own escape, \u,
# the character's code as a signed 16-bit number, and `?` for a reader that
# cannot show it. A character beyond the 16 bits is written as its two UTF-16
# halves.
Table_RtfText <- function(chrText) {
  vapply(Widget_Utf8(as.character(chrText)), function(strText) {
    if (is.na(strText)) {
      return("")
    }
    nCodes <- utf8ToInt(strText)
    paste(vapply(nCodes, function(nCode) {
      if (nCode == 92L) {
        return("\\\\")
      }
      if (nCode == 123L || nCode == 125L) {
        return(paste0("\\", intToUtf8(nCode)))
      }
      if (nCode < 128L) {
        return(intToUtf8(nCode))
      }
      nUnits <- if (nCode > 65535L) c(55296L + (nCode - 65536L) %/% 1024L, 56320L + (nCode - 65536L) %% 1024L) else nCode
      paste0("\\u", ifelse(nUnits > 32767L, nUnits - 65536L, nUnits), "?", collapse = "")
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}
