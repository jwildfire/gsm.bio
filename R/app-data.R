# The app's Data view: a reader's own files, read by R, and which column of
# each is which (#73).
#
# A file is read into the session's memory and nowhere else: nothing is
# written to the server beyond Shiny's own temporary copy of an upload, which
# goes when the session ends. A table is drawn only when every column the
# charts need has been named; R then renames those columns to gsm.bio's own
# names, so every chart runs on its default column settings. What the page
# says of the files as a reader goes is at the foot of this file (#85): the
# rail's three steps, a column's tag, and the charts that are ready.

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
#
# It is read through only of a .csv file that does end without a line end
# (#97). R says the same words, and no others, of a file with a quote left
# open on its header or one of its first four rows, whether or not the file
# ends with a line end: it looks at a file's first five lines before it reads
# the rest, a quote that is never closed takes that look to the end of the
# file, and R reads the file short from there.
App_ReadThrough <- function(chrWarned) {
  grepl("^incomplete final line found", chrWarned)
}

# How much of a file is looked at at a time: a megabyte, so a file of the
# largest size the app accepts is never held whole to be counted.
nAppPiece <- 1048576L

# What a .csv file's own bytes say, which R's reader does not (#97): whether
# its double-quote characters come to an odd number, and whether it ends with
# a line end. In a .csv file a quote that opens a value closes it, and a quote
# inside a value is written twice, so a whole file has an even number; with an
# odd number one is never closed, and R reads on from it to the end of the
# file as one value. R's reader takes every quote as opening or closing one,
# so the count says exactly whether R ends inside a quote. A quote is one byte
# in UTF-8 and is part of no other character, so the bytes are counted as they
# are. An even number of stray quotes is not known this way.
App_CsvBytes <- function(strPath, nPiece = nAppPiece) {
  xFile <- file(strPath, open = "rb")
  on.exit(close(xFile), add = TRUE)
  bOdd <- FALSE
  xLast <- raw(0)
  repeat {
    xBytes <- readBin(xFile, what = "raw", n = nPiece)
    if (length(xBytes) == 0L) {
      break
    }
    if (sum(xBytes == as.raw(0x22)) %% 2L == 1L) {
      bOdd <- !bOdd
    }
    xLast <- xBytes[length(xBytes)]
  }
  list(odd = bOdd, ends = length(xLast) == 1L && (xLast == as.raw(0x0a) || xLast == as.raw(0x0d)))
}

# A .csv file as the app reads it: comma-separated text in UTF-8, with or
# without a byte-order mark, under the file's own column names.
App_ReadCsv <- function(strPath) {
  utils::read.csv(strPath, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
}

#' Read a file a reader chose
#'
#' @param strPath `character` Where the file is.
#' @param strName `character` What the reader called it: its type is read from
#'   this name.
#'
#' @return A data frame of base R columns. An error, with a sentence for the
#'   reader, for a type the app does not read, a file R cannot read, a file R
#'   warned of as it read it, a `.csv` file with a quote that is never closed,
#'   a file with no rows or no columns, and a SAS file when haven is not
#'   installed. A sentence names the file as the reader does, never by its
#'   path on the server.
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
        .csv = App_ReadCsv(strPath),
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
  Warned <- function(chrOf) {
    App_Stop(
      strName, " was not loaded: R warned as it read the file, and a file R warns of may have been read short. ",
      "R said: ", paste(sub("[.]$", "", App_SaidOf(unique(chrOf), strPath, strName)), collapse = "; "), ".",
      if (strType == ".csv") " The app reads a .csv file as comma-separated text in UTF-8."
    )
  }
  bLastLine <- App_ReadThrough(chrWarned)
  if (any(!bLastLine)) {
    Warned(chrWarned[!bLastLine])
  }
  if (strType == ".csv") {
    # What R did not warn of, or warned of only as a last line with no line
    # end: the file's own bytes say whether a quote in it is left open, and
    # whether its last line is what R says it is (#97).
    lBytes <- tryCatch(App_CsvBytes(strPath), error = function(cndError) {
      App_Stop(strName, " could not be read as a .csv file: ", App_SaidOf(conditionMessage(cndError), strPath, strName))
    })
    if (lBytes$odd) {
      App_Stop(
        strName, " was not loaded: a quote in it is never closed, so R would read it short. ",
        "The file has an odd number of double-quote characters, and in a .csv file a quote that opens a value closes it, and a quote inside a value is written twice."
      )
    }
    if (any(bLastLine) && lBytes$ends) {
      Warned(chrWarned)
    }
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

# ---- Whose file it is: only an upload is read (#97) --------------------------

# What a card is told when what it was sent is not one uploaded file. It is
# one sentence, the same whatever was sent, and holds nothing of what was sent.
strAppNotUploaded <- "Nothing was read: a card takes one file, uploaded with its own control, and the page sent something else."

# What a card calls a file the page gave no name for.
strAppNoName <- "A file with no name"

#' The path of an uploaded file, or a refusal
#'
#' A file input's value is not Shiny's alone to set. When an upload ends,
#' Shiny's server sets it, to a data frame whose `datapath` is where the server
#' wrote the file. But a page can set any input of its session, a file input
#' among them, with `Shiny.setInputValue()`, and then `datapath` is whatever
#' the page says: any path on the server, or a web address. R would read
#' either, and the card would show its rows. So a value is taken only when its
#' path is where Shiny writes an upload in this R process, and that is checked
#' here before anything else touches the path.
#'
#' Shiny writes an upload to `tempdir()/<id>/<n><extension>`: a folder of this
#' process's temporary folder named by twelve random bytes as 24 hexadecimal
#' digits, and in it each file under its number (`shiny:::FileUploadContext`,
#' `shiny:::FileUploadOperation`). The rule is that shape and no more: one
#' path, of a file that is there and is no folder, directly inside a folder of
#' that name, directly inside this process's temporary folder. The paths are
#' compared as the file system resolves them, so a path with `..` in it, a
#' link to a file elsewhere and a web address all fail. A reader cannot name
#' another reader's upload either: its folder's name is 96 random bits that
#' are never sent to a page.
#'
#' Should a later Shiny keep uploads somewhere else, every upload would be
#' refused: the tests that choose a file in a browser fail then, and this is
#' the rule to change.
#'
#' @param xChosen What a file input holds: anything a page can send.
#'
#' @return The file's path, as the file system resolves it. An error, with the
#'   one sentence of `strAppNotUploaded`, for anything else.
#'
#' @keywords internal
#' @noRd
App_Uploaded <- function(xChosen) {
  xPath <- if (is.list(xChosen)) xChosen[["datapath"]]
  if (!is.character(xPath) || length(xPath) != 1L || is.na(xPath) || !nzchar(xPath)) {
    App_Stop(strAppNotUploaded)
  }
  # Where the path leads, or nowhere: a path to nothing has no real form.
  strReal <- tryCatch(
    suppressWarnings(normalizePath(xPath, winslash = "/", mustWork = TRUE)),
    error = function(cndError) NA_character_
  )
  strRoot <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  bUploaded <- !is.na(strReal) &&
    identical(dirname(dirname(strReal)), strRoot) &&
    grepl("^[0-9a-f]{24}$", basename(dirname(strReal))) &&
    isFALSE(file.info(strReal, extra_cols = FALSE)$isdir)
  if (!bUploaded) {
    App_Stop(strAppNotUploaded)
  }
  strReal
}

# A chosen file's name, as the page sent it: text, and only ever text. Two
# files dropped on one card are named together; a value with no name that is
# text has a few words in its place.
App_ChosenName <- function(xChosen) {
  xName <- if (is.list(xChosen)) xChosen[["name"]]
  if (!is.character(xName) || length(xName) == 0L || anyNA(xName) || !all(nzchar(xName))) {
    return(strAppNoName)
  }
  paste(xName, collapse = ", ")
}

# What a card holds once a file input has a value: the file's name with its
# table, or with the sentence saying why nothing was read. Whatever the page
# sent, this returns: no value ends the session.
App_Take <- function(xChosen) {
  strName <- strAppNoName
  tryCatch(
    {
      strName <- App_ChosenName(xChosen)
      # The rule comes first: nothing is read from a path it has not passed.
      strPath <- App_Uploaded(xChosen)
      list(name = strName, table = App_ReadFile(strPath, strName))
    },
    error = function(cndError) list(name = strName, problem = conditionMessage(cndError))
  )
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

# How many digits a number is shown to: the fifteen that identify it. R holds
# a number to between fifteen and seventeen, and the two past fifteen are how
# 0.1 + 0.2 comes to be written 0.30000000000000004.
nAppDigits <- 15L

# A value as the viewer shows it: as R holds it, and a missing one as NA. A
# number is written in full, each value on its own and so never padded to the
# width of another, and never as 1e+05 (#85). The exception is a number that
# fifteen digits cannot write out: one of a thousand million million or more,
# whose last digits R does not hold, and one smaller than a part in that many,
# which would be a row of zeros. Those are left in R's scientific form.
App_Cell <- function(xValues) {
  if (!is.numeric(xValues)) {
    chrValues <- as.character(xValues)
    chrValues[is.na(xValues)] <- "NA"
    return(chrValues)
  }
  vapply(as.numeric(xValues), function(nValue) {
    if (is.na(nValue)) {
      return("NA")
    }
    if (!is.finite(nValue)) {
      return(as.character(nValue))
    }
    bInFull <- nValue == 0 || (abs(nValue) >= 1e-15 && abs(nValue) < 1e15)
    format(nValue, digits = nAppDigits, scientific = !bInFull, trim = TRUE)
  }, character(1), USE.NAMES = FALSE)
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
    xSet <- shiny::tabsetPanel(id = "gsm_bio_view_table", selected = "results", Tab("results"), Tab("participants"), Tab("outcomes"))
    # Shiny's page says which links are tabs, and which is chosen, of the tab
    # sets it is loaded with. These are written after, so R says it (#85): the
    # arrow keys then walk them as they walk the pills.
    App_Change(xSet, "ul", function(xList) {
      xList <- shiny::tagAppendAttributes(xList, role = "tablist")
      xList$children <- App_Change(xList$children, "li", function(xItem) {
        xItem <- shiny::tagAppendAttributes(xItem, role = "presentation")
        # The tab chosen as the tabs are written is the results', always.
        xItem$children <- App_Change(xItem$children, "a", function(xLink) {
          bChosen <- identical(shiny::tagGetAttribute(xLink, "data-value"), "results")
          shiny::tagAppendAttributes(xLink, role = "tab", `aria-selected` = if (bChosen) "true" else "false")
        })
        xItem
      })
      xList
    })
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

# ---- The Data page: what is left to do, and what is ready (#85) --------------

# A tag of a few letters beside a name: whether a table is needed, and whether
# a column has been said.
App_TagOf <- function(strKind, strSays) {
  shiny::tags$span(class = paste0("gsm-bio-app-tag gsm-bio-app-tag-", strKind), strSays)
}

# The tag of one column question: the file has a column of gsm.bio's own name,
# the reader has said which column it is, or they have still to say.
App_Tag <- function(strChosen, strColumn) {
  if (length(strChosen) != 1L || is.na(strChosen) || !nzchar(strChosen)) {
    App_TagOf("need", "say which")
  } else if (identical(strChosen, strColumn)) {
    App_TagOf("same", "same name")
  } else {
    App_TagOf("said", "said")
  }
}

# The file types the app reads, in a sentence.
App_TypesSaid <- function() {
  nTypes <- length(chrAppFileTypes)
  paste(paste(chrAppFileTypes[-nTypes], collapse = ", "), "or", chrAppFileTypes[nTypes])
}

# The columns of a chosen file a reader has still to say: the ones with no
# column chosen, or with one the file does not have.
App_Unsaid <- function(lFile, chrChosen, lTable) {
  chrNeed <- names(lTable$columns)
  chrChosen <- chrChosen[chrNeed]
  chrNeed[is.na(chrChosen) | !nzchar(chrChosen) | !chrChosen %in% names(lFile$table)]
}

#' What is left to do on the Data page
#'
#' @param lStudy `list` The tables the charts are drawn on now.
#' @param lFiles `list` The files a reader has chosen, by table: each its
#'   `name` with its `table`, or with the `problem` R had reading it.
#' @param lChosen `list` The reader's column for each one the charts need, by
#'   table.
#' @param bDrawn `logical` Whether the charts are drawn on these files as they
#'   are now.
#'
#' @return A list of the rail's three steps, `files`, `columns` and `charts`,
#'   each with its `state` (`"done"`, `"next"` or `"waiting"`), what it `says`
#'   and its `notes`; `charts` also names the chart to `open`, when one is
#'   ready. `drawn` is what the charts are drawn on now: whether they are
#'   `still` on it while other files are chosen, each of its `tables` in a few
#'   words, and a `note`.
#'
#' @keywords internal
#' @noRd
App_Steps <- function(lStudy, lFiles, lChosen, bDrawn) {
  lTables <- App_Tables()
  chrHeld <- intersect(names(lTables), names(lFiles)[!vapply(lFiles, is.null, logical(1))])
  bRead <- vapply(chrHeld, function(strTable) is.null(lFiles[[strTable]]$problem), logical(1))
  chrNames <- vapply(chrHeld, function(strTable) lFiles[[strTable]]$name, character(1))
  Step <- function(strState, strSays, chrNotes = character(0)) list(state = strState, says = strSays, notes = chrNotes)

  # 1. The files.
  if (length(chrHeld) == 0L) {
    lFilesStep <- Step("done", "none chosen", sprintf(
      "The charts are drawn on %s. Choose a results file to draw them on %s.",
      lStudy$source, if (is.null(lStudy$files)) "a study of your own" else "other files"
    ))
  } else {
    chrNotes <- vapply(chrHeld[!bRead], function(strTable) {
      if (lTables[[strTable]]$needed) {
        return(sprintf("%s was not read: choose another results file.", chrNames[[strTable]]))
      }
      strA <- if (identical(strTable, "outcomes")) "an" else "a"
      sprintf(
        "%s was not read: choose another file for the %s table, or remove it. The charts can be drawn without %s %s table.",
        chrNames[[strTable]], strTable, strA, strTable
      )
    }, character(1), USE.NAMES = FALSE)
    if (!"results" %in% chrHeld) {
      chrNotes <- c(chrNotes, "Choose a results file: the charts are drawn from the results table.")
    }
    lFilesStep <- Step(
      if (length(chrNotes) > 0L) "next" else "done",
      paste(c(chrNames, if (any(!bRead)) paste(sum(!bRead), "not read")), collapse = ", "),
      chrNotes
    )
  }

  # 2. The columns of the files R read.
  lUnsaid <- lapply(stats::setNames(chrHeld[bRead], chrHeld[bRead]), function(strTable) {
    chrChosen <- lChosen[[strTable]]
    App_Unsaid(lFiles[[strTable]], if (is.null(chrChosen)) character(0) else chrChosen, lTables[[strTable]])
  })
  nAsked <- sum(vapply(chrHeld[bRead], function(strTable) length(lTables[[strTable]]$columns), integer(1)))
  nUnsaid <- sum(lengths(lUnsaid))
  lColumnsStep <- if (nAsked == 0L) {
    Step("done", "nothing to say")
  } else if (nUnsaid == 0L) {
    Step("done", sprintf("%d of %d columns said", nAsked, nAsked))
  } else {
    chrLeft <- names(lUnsaid)[lengths(lUnsaid) > 0L]
    Step("next", sprintf("%d of %d columns still to say", nUnsaid, nAsked), vapply(chrLeft, function(strTable) {
      sprintf("In %s: %s.", chrNames[[strTable]], paste(tolower(sub(",.*$", "", lTables[[strTable]]$columns[lUnsaid[[strTable]]])), collapse = ", "))
    }, character(1), USE.NAMES = FALSE))
  }

  # 3. The charts: the ones drawn now, or the ones the button would draw.
  chrCharts <- names(chrAppCharts)
  bStill <- length(chrHeld) > 0L && !isTRUE(bDrawn)
  if (!bStill) {
    lLacks <- lapply(chrCharts, App_Lacks, lStudy)
    bReady <- vapply(lLacks, is.null, logical(1))
    lChartsStep <- Step(
      if (isTRUE(bDrawn) || !is.null(lStudy$files)) "done" else "next",
      sprintf("%d of %d charts ready", sum(bReady), length(chrCharts)),
      as.character(unlist(lLacks))
    )
    lChartsStep$open <- if (any(bReady)) chrCharts[bReady][1]
  } else if (lFilesStep$state != "done") {
    lChartsStep <- Step("waiting", "waiting on step 1")
  } else if (lColumnsStep$state != "done") {
    lChartsStep <- Step("waiting", "waiting on step 2")
  } else {
    # What the button would draw: a chart lacks a table no file is chosen for.
    lWould <- stats::setNames(lapply(names(lTables), function(strTable) if (strTable %in% chrHeld) TRUE), names(lTables))
    bWould <- vapply(chrCharts, function(strChart) is.null(App_Lacks(strChart, lWould)), logical(1))
    lChartsStep <- Step(
      "next", sprintf("%d of %d charts can be drawn", sum(bWould), length(chrCharts)),
      c("Press the button under the cards.", sprintf("%s needs an outcomes table.", chrAppCharts[chrCharts[!bWould]]))
    )
  }

  # What the charts are drawn on now.
  chrDrawn <- vapply(names(lTables), function(strTable) {
    dfTable <- lStudy[[strTable]]
    if (is.null(dfTable)) {
      "none"
    } else if (is.null(lStudy$files)) {
      sprintf("%s, %s", App_Count(nrow(dfTable), "row"), App_Count(ncol(dfTable), "column"))
    } else {
      sprintf("%s, %s", lStudy$files[[strTable]], App_Count(nrow(dfTable), "row"))
    }
  }, character(1))
  names(chrDrawn) <- vapply(lTables, function(lTable) lTable$label, character(1))
  list(
    files = lFilesStep, columns = lColumnsStep, charts = lChartsStep,
    drawn = list(
      still = bStill, tables = chrDrawn,
      note = if (bStill) "The charts stay on these tables until the button under the cards is pressed."
    )
  )
}

# The rail as it is written: the three steps, each with whether it is done in
# words a screen reader reads, and under them what the charts are drawn on.
App_Rail <- function(lSteps) {
  chrTitles <- c(files = "Choose files", columns = "Say which column is which", charts = "Draw the charts")
  chrStates <- c(done = "Done: ", `next` = "Next: ", waiting = "Waiting: ")
  Step <- function(strStep) {
    lStep <- lSteps[[strStep]]
    shiny::tags$li(
      class = paste0("gsm-bio-app-step gsm-bio-app-step-", lStep$state), `data-state` = lStep$state,
      shiny::tags$span(class = "gsm-bio-app-step-mark", `aria-hidden` = "true"),
      shiny::tags$div(
        class = "gsm-bio-app-step-body",
        shiny::tags$p(
          class = "gsm-bio-app-step-title",
          shiny::tags$span(class = "gsm-bio-app-unseen", chrStates[[lStep$state]]), chrTitles[[strStep]]
        ),
        shiny::tags$p(class = "gsm-bio-app-step-says", lStep$says),
        lapply(lStep$notes, function(strNote) shiny::tags$p(class = "gsm-bio-app-step-note", strNote)),
        # A link the page opens the chart with, not an input.
        if (!is.null(lStep$open)) {
          shiny::tags$a(
            class = "gsm-bio-app-open", href = "#", `data-gsm-bio-open` = lStep$open,
            paste("Open", tolower(chrAppCharts[[lStep$open]]))
          )
        }
      )
    )
  }
  lDrawn <- lSteps$drawn
  shiny::tagList(
    shiny::tags$ol(class = "gsm-bio-app-steps", lapply(names(chrTitles), Step)),
    shiny::tags$div(
      class = "gsm-bio-app-drawn",
      shiny::tags$p(class = "gsm-bio-app-kicker", if (lDrawn$still) "Drawn on, still" else "Drawn on"),
      shiny::tags$ul(lapply(names(lDrawn$tables), function(strLabel) {
        shiny::tags$li(
          class = if (identical(lDrawn$tables[[strLabel]], "none")) "gsm-bio-app-drawn-none",
          shiny::tags$span(class = "gsm-bio-app-dot", `aria-hidden` = "true"),
          shiny::tags$span(class = "gsm-bio-app-drawn-name", strLabel),
          shiny::tags$span(lDrawn$tables[[strLabel]])
        )
      })),
      if (!is.null(lDrawn$note)) shiny::tags$p(class = "gsm-bio-app-step-note", lDrawn$note)
    )
  )
}

# The files the button would draw, in a sentence: each by its name and the
# table it was chosen for, so a reader sees a file chosen earlier is among
# them before they press.
App_Will <- function(lFiles) {
  if (length(lFiles) == 0L) {
    return("No file is chosen: the button has nothing to draw.")
  }
  chrEach <- vapply(names(lFiles), function(strTable) {
    sprintf("%s (%s%s)", lFiles[[strTable]]$name, strTable, if (is.null(lFiles[[strTable]]$problem)) "" else ", not read")
  }, character(1))
  paste0(
    "These files: ", paste(chrEach, collapse = ", "), ".",
    if (is.null(lFiles$results)) " There is no results file among them."
  )
}

# What the page says once the charts are drawn: on which files, how many of
# the charts are ready, and each chart with what it draws, or with the table
# it lacks. A chart's name is a link the page opens it with.
App_Ready <- function(lStudy) {
  chrCharts <- names(chrAppCharts)
  lLacks <- stats::setNames(lapply(chrCharts, App_Lacks, lStudy), chrCharts)
  nReady <- sum(vapply(lLacks, is.null, logical(1)))
  shiny::tags$div(
    class = "gsm-bio-app-done",
    shiny::tags$p(sprintf(
      "The charts are drawn on %s. %s Open one here, or from the row of pills at the top of the page.",
      lStudy$source,
      if (nReady == length(chrCharts)) {
        sprintf("All %d charts are ready.", nReady)
      } else {
        sprintf("%d of the %d charts are ready.", nReady, length(chrCharts))
      }
    )),
    shiny::tags$ul(class = "gsm-bio-app-ready", lapply(chrCharts, function(strChart) {
      strLacks <- lLacks[[strChart]]
      shiny::tags$li(
        class = if (!is.null(strLacks)) "gsm-bio-app-ready-lacks",
        shiny::tags$a(href = "#", `data-gsm-bio-open` = strChart, chrAppCharts[[strChart]]),
        shiny::tags$span(if (is.null(strLacks)) chrAppWhat[[strChart]] else strLacks)
      )
    }))
  )
}
