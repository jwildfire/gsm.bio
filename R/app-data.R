# The app's Data view: a reader's own files, read by R, and which column of
# each is which (#73).
#
# A file is read into the session's memory and nowhere else: nothing is
# written to the server beyond Shiny's own temporary copy of an upload, which
# goes when the session ends. A table is drawn only when every column the
# charts need has been named; R then renames those columns to gsm.bio's own
# names, so every chart runs on its default column settings.

# The tables a reader may load: what the Data view calls each, whether the
# charts need it, and the columns the charts read from it, each under the
# name gsm.bio knows it by with what a reader is asked for.
App_Tables <- function() {
  lColumns <- App_Columns()
  Named <- function(chrColumns, chrAsked) stats::setNames(chrAsked, chrColumns)
  list(
    results = list(
      label = "Results", needed = TRUE,
      what = "one row per participant, biomarker and visit",
      columns = Named(lColumns$results, c("Participant", "Biomarker", "Result, a number", "Visit", "Visit order, a number")),
      numbers = unlist(lCoreDefaults[c("value_col", "visit_order_col")], use.names = FALSE)
    ),
    participants = list(
      label = "Participants", needed = FALSE,
      what = "one row per participant; its other columns become the groups and filters",
      columns = Named(lColumns$participants, "Participant"),
      numbers = character(0)
    ),
    outcomes = list(
      label = "Outcomes", needed = FALSE,
      what = "one row per participant and endpoint, for the survival chart",
      columns = Named(lColumns$outcomes, c("Participant", "Endpoint code", "Endpoint name", "Time, a number", "Censor flag, 1 for censored")),
      numbers = unlist(lOutcomeDefaults[c("time_col", "censor_col")], use.names = FALSE)
    )
  )
}

# The file types the app reads, by extension.
chrAppFileTypes <- c(".csv", ".xpt", ".sas7bdat")

App_HasHaven <- function() {
  requireNamespace("haven", quietly = TRUE)
}

# A file's type, from its name: the extension in lower case, with its dot.
App_FileType <- function(strName) {
  strType <- tolower(regmatches(strName, regexpr("\\.[^.]*$", strName)))
  if (length(strType) == 0L) "" else strType
}

#' Read a file a reader chose
#'
#' @param strPath `character` Where the file is.
#' @param strName `character` What the reader called it: its type is read from
#'   this name.
#'
#' @return A data frame of base R columns. An error, with a sentence for the
#'   reader, for a type the app does not read, a file R cannot read, a file
#'   with no rows or no columns, and a SAS file when haven is not installed.
#'
#' @keywords internal
#' @noRd
App_ReadFile <- function(strPath, strName) {
  strType <- App_FileType(strName)
  if (!strType %in% chrAppFileTypes) {
    App_Stop("The app reads ", paste(chrAppFileTypes, collapse = ", "), " files, and ", strName, " is none of them.")
  }
  if (strType != ".csv" && !App_HasHaven()) {
    App_Stop(
      strName, " is a SAS file, which R reads with the haven package, and haven is not installed on this server. ",
      "Install it with install.packages(\"haven\"), or load the table as a .csv file."
    )
  }
  # What R warns of in a file it does read, such as a last line with no line
  # end, is not the reader's concern: the table is checked below.
  dfTable <- tryCatch(
    withCallingHandlers(
      switch(strType,
        .csv = utils::read.csv(
          strPath,
          check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM"
        ),
        .xpt = haven::read_xpt(strPath),
        .sas7bdat = haven::read_sas(strPath)
      ),
      warning = function(cndWarning) invokeRestart("muffleWarning")
    ),
    error = function(cndError) {
      App_Stop(strName, " could not be read as a ", strType, " file: ", conditionMessage(cndError))
    }
  )
  # A plain data frame of plain columns: what SAS said of a column beside its
  # values (a label, a format) is not a value.
  dfTable <- as.data.frame(dfTable, stringsAsFactors = FALSE)
  for (strColumn in names(dfTable)) {
    for (strNote in c("label", "format.sas", "format.spss", "format.stata", "width", "display_width")) {
      attr(dfTable[[strColumn]], strNote) <- NULL
    }
  }
  if (ncol(dfTable) == 0L || nrow(dfTable) == 0L) {
    App_Stop(strName, " has no ", if (ncol(dfTable) == 0L) "columns" else "rows", ".")
  }
  if (anyDuplicated(names(dfTable)) > 0L || any(!nzchar(names(dfTable)))) {
    App_Stop(strName, " has two columns of one name, or a column with no name: the app cannot tell them apart.")
  }
  dfTable
}

# The column of a file the app takes for one the charts need, before the
# reader says: the one of exactly that name, or none.
App_Guess <- function(chrColumns, strColumn) {
  if (strColumn %in% chrColumns) strColumn else ""
}

#' A reader's table, with the columns the charts need under gsm.bio's names
#'
#' @param dfTable `data.frame` The table as it was read.
#' @param chrChosen `character` The reader's column for each one the charts
#'   need, named by gsm.bio's name for it; `""` for one not yet said.
#' @param lTable `list` The table's entry in [App_Tables()].
#' @param strName `character` The file's name, for the sentences.
#'
#' @return The table with those columns renamed. A column of the file that
#'   already had one of gsm.bio's names, and was not the one chosen for it, is
#'   kept under that name with `_original` added. An error, with a sentence
#'   for the reader, when a column is not said, when one column is chosen
#'   twice, or when a column that must be a number is not.
#'
#' @keywords internal
#' @noRd
App_MapTable <- function(dfTable, chrChosen, lTable, strName) {
  chrNeed <- names(lTable$columns)
  chrChosen <- chrChosen[chrNeed]
  bUnsaid <- is.na(chrChosen) | !nzchar(chrChosen) | !chrChosen %in% names(dfTable)
  if (any(bUnsaid)) {
    App_Stop(
      "Say which column of ", strName, " is each of these, and the charts can be drawn: ",
      paste(tolower(sub(",.*$", "", lTable$columns[bUnsaid])), collapse = ", "), "."
    )
  }
  if (anyDuplicated(chrChosen) > 0L) {
    strTwice <- chrChosen[duplicated(chrChosen)][1]
    App_Stop("The column ", strTwice, " of ", strName, " is chosen twice. Each of the columns the charts need is a column of its own.")
  }
  for (strColumn in lTable$numbers) {
    xValues <- dfTable[[chrChosen[[strColumn]]]]
    if (!is.numeric(xValues)) {
      App_Stop(
        "The column ", chrChosen[[strColumn]], " of ", strName, " is chosen as ",
        tolower(sub(",.*$", "", lTable$columns[[strColumn]])), ", which is a number, and it holds text."
      )
    }
  }
  chrWas <- names(dfTable)
  chrNames <- chrWas
  # A column that already has one of gsm.bio's names and was not chosen for it
  # steps aside, so no two columns end with one name.
  for (iColumn in which(chrWas %in% chrNeed)) {
    if (!identical(chrChosen[[chrWas[iColumn]]], chrWas[iColumn])) {
      chrNames[iColumn] <- paste0(chrWas[iColumn], "_original")
    }
  }
  for (strColumn in chrNeed) {
    chrNames[chrWas == chrChosen[[strColumn]]] <- strColumn
  }
  names(dfTable) <- make.unique(chrNames, sep = "_")
  dfTable
}
