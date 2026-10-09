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

# R's words about a file name it by where the server keeps it: a temporary
# path, under a name the server gave it, which means nothing to a reader and
# is not theirs to see. The file's own name is put in its place, however the
# path is written.
App_SaidOf <- function(chrSaid, strPath, strName) {
  chrPaths <- unique(c(
    strPath, path.expand(strPath),
    normalizePath(strPath, mustWork = FALSE), normalizePath(strPath, winslash = "/", mustWork = FALSE)
  ))
  chrPaths <- chrPaths[!is.na(chrPaths) & nzchar(chrPaths)]
  for (strOne in chrPaths[order(-nchar(chrPaths))]) {
    chrSaid <- gsub(strOne, strName, chrSaid, fixed = TRUE)
  }
  chrSaid
}

# The one warning of R's a file is read through: that its last line has no
# line end, which R says of a short file and reads the line all the same.
# Every other warning is one a file can be read short under, with no error:
# a byte that is not in the file's encoding and a quote that is never closed
# both stop read.csv() where they are, and it returns the rows before them
# (#86). The warning is known by R's English words, so an R that speaks
# another language refuses such a file, which is the safe way to be wrong.
App_ReadThrough <- function(chrWarned) {
  grepl("^incomplete final line found", chrWarned)
}

#' Read a file a reader chose
#'
#' @param strPath `character` Where the file is.
#' @param strName `character` What the reader called it: its type is read from
#'   this name.
#'
#' @return A data frame of base R columns. An error, with a sentence for the
#'   reader, for a type the app does not read, a file R cannot read, a file R
#'   warned of as it read it, a file with no rows or no columns, and a SAS
#'   file when haven is not installed. A sentence names the file as the reader
#'   does, never by its path on the server.
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
  # What R warns of as it reads is kept: a file is not drawn on a warning.
  chrWarned <- character(0)
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
      warning = function(cndWarning) {
        chrWarned <<- c(chrWarned, conditionMessage(cndWarning))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(cndError) {
      App_Stop(strName, " could not be read as a ", strType, " file: ", App_SaidOf(conditionMessage(cndError), strPath, strName))
    }
  )
  chrWarned <- chrWarned[!App_ReadThrough(chrWarned)]
  if (length(chrWarned) > 0L) {
    App_Stop(
      strName, " was not loaded: R warned as it read the file, and a file R warns of may have been read short. ",
      "R said: ", paste(sub("[.]$", "", App_SaidOf(unique(chrWarned), strPath, strName)), collapse = "; "), ".",
      if (strType == ".csv") " The app reads a .csv file as comma-separated text in UTF-8."
    )
  }
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

# ---- What is loaded: the tables, a page of rows at a time (#80) --------------

# How many rows the viewer shows at a time, and how many of a file just chosen.
nAppViewRows <- 10L
nAppPreviewRows <- 5L

# A value as the viewer shows it: as R holds it, not rounded, and a missing
# one as NA.
App_Cell <- function(xValues) {
  chrValues <- as.character(xValues)
  chrValues[is.na(xValues)] <- "NA"
  chrValues
}

#' One page of a table's rows
#'
#' @param dfTable `data.frame` A table.
#' @param nPage `numeric` The page asked for. One below the first is the
#'   first, and one past the last is the last.
#' @param nRows `numeric` The rows of a page.
#'
#' @return A list: `page`, the page given; `pages`, how many there are; `from`
#'   and `to`, the first and last row of the page; and `rows`, those rows.
#'
#' @keywords internal
#' @noRd
App_ViewPage <- function(dfTable, nPage = 1L, nRows = nAppViewRows) {
  nAll <- nrow(dfTable)
  nPages <- max(1L, as.integer(ceiling(nAll / nRows)))
  nPage <- if (!is.numeric(nPage) || length(nPage) != 1L || is.na(nPage)) 1L else as.integer(nPage)
  nPage <- min(max(nPage, 1L), nPages)
  nFrom <- if (nAll == 0L) 0L else (nPage - 1L) * nRows + 1L
  nTo <- min(nPage * nRows, nAll)
  list(
    page = nPage, pages = nPages, from = nFrom, to = nTo,
    rows = if (nAll == 0L) dfTable else dfTable[seq.int(nFrom, nTo), , drop = FALSE]
  )
}

#' Rows of a table, as a table of the page
#'
#' @param dfRows `data.frame` The rows to show.
#' @param nFirst `numeric` The number of the first of them in its table.
#'
#' @return A tag: the rows under the table's own column names, each with its
#'   number. Every value is text of the page, never markup.
#'
#' @keywords internal
#' @noRd
App_RowsTable <- function(dfRows, nFirst = 1L) {
  lCells <- lapply(dfRows, App_Cell)
  shiny::tags$div(
    class = "gsm-bio-app-rows",
    shiny::tags$table(
      class = "gsm-bio-app-table",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("Row"),
        lapply(names(dfRows), function(strColumn) shiny::tags$th(strColumn))
      )),
      shiny::tags$tbody(lapply(seq_len(nrow(dfRows)), function(iRow) {
        shiny::tags$tr(
          shiny::tags$td(class = "gsm-bio-app-row", format(nFirst + iRow - 1L, big.mark = ",")),
          lapply(lCells, function(chrColumn) shiny::tags$td(chrColumn[iRow]))
        )
      }))
    )
  )
}

# The tables of a study that are there, by what the Data view calls each.
App_Loaded <- function(lStudy) {
  lTables <- App_Tables()
  chrThere <- names(lTables)[vapply(names(lTables), function(strTable) !is.null(lStudy[[strTable]]), logical(1))]
  stats::setNames(chrThere, vapply(chrThere, function(strTable) lTables[[strTable]]$label, character(1)))
}

# The viewer's side of the session: which table is shown, which page of it,
# and the page itself.
App_ViewServer <- function(input, output, session, rStudy) {
  rPage <- shiny::reactiveVal(1L)
  # The table shown: the one chosen, when the study has it; the results if not.
  rShown <- shiny::reactive({
    strChosen <- input$gsm_bio_view_table
    if (length(strChosen) == 1L && strChosen %in% App_Loaded(rStudy())) strChosen else "results"
  })
  # A tab for each table the study has, named with its rows. Other tables are
  # drawn: the tabs are written again for the ones there are, and the viewer
  # starts again at the results' first rows.
  output$gsm_bio_view_tabs <- shiny::renderUI({
    lStudy <- rStudy()
    lTables <- App_Tables()
    Tab <- function(strTable) {
      if (is.null(lStudy[[strTable]])) {
        return(NULL)
      }
      shiny::tabPanel(
        sprintf("%s, %s rows", lTables[[strTable]]$label, format(nrow(lStudy[[strTable]]), big.mark = ",")),
        value = strTable
      )
    }
    shiny::tabsetPanel(id = "gsm_bio_view_table", selected = "results", Tab("results"), Tab("participants"), Tab("outcomes"))
  })
  shiny::observeEvent(rStudy(), rPage(1L))
  shiny::observeEvent(rShown(), rPage(1L))
  Turn <- function(nBy) {
    rPage(App_ViewPage(rStudy()[[rShown()]], rPage() + nBy)$page)
  }
  shiny::observeEvent(input$gsm_bio_view_previous, Turn(-1L))
  shiny::observeEvent(input$gsm_bio_view_next, Turn(1L))
  output$gsm_bio_view <- shiny::renderUI({
    lStudy <- rStudy()
    strTable <- rShown()
    dfTable <- lStudy[[strTable]]
    lPage <- App_ViewPage(dfTable, rPage())
    shiny::tagList(
      shiny::tags$p(class = "gsm-bio-app-what", sprintf(
        "%s, from %s: %s rows, %s columns. Rows %s to %s, page %s of %s. NA is a missing value.",
        App_Tables()[[strTable]]$label, lStudy$source, format(nrow(dfTable), big.mark = ","), ncol(dfTable),
        format(lPage$from, big.mark = ","), format(lPage$to, big.mark = ","),
        format(lPage$page, big.mark = ","), format(lPage$pages, big.mark = ",")
      )),
      App_RowsTable(lPage$rows, lPage$from)
    )
  })
  invisible(NULL)
}
