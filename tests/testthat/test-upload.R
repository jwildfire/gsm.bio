# The app's Data view: a reader's own files, read by R, and which column of
# each is which (#73).
#
# Reading a file and renaming its columns are plain functions, tested first.
# Then the session, with no browser; then the page itself, in a headless
# browser, with a file of each type the app reads.

# The synthetic results under a reader's own column names, short enough for
# every file type, and the participants under theirs.
chrReaderResults <- c(USUBJID = "SUBJ", TEST = "MARKER", STRESN = "RESULT", VISIT = "VIS", VISITNUM = "VISORD")
dfReaderResults <- function() {
  dfTable <- Synthetic_Results
  names(dfTable)[match(names(chrReaderResults), names(dfTable))] <- unname(chrReaderResults)
  dfTable
}
dfReaderParticipants <- function() {
  dfTable <- Synthetic_Participants
  names(dfTable)[names(dfTable) == "USUBJID"] <- "SUBJ"
  dfTable
}

# A table written as a file of one of the three types.
strWritten <- function(dfTable, strType, strStem = "results") {
  strDir <- tempfile("gsm-bio-upload")
  dir.create(strDir)
  strFile <- file.path(strDir, paste0(strStem, strType))
  switch(strType,
    .csv = utils::write.csv(dfTable, strFile, row.names = FALSE, na = ""),
    .xpt = haven::write_xpt(dfTable, strFile),
    # haven's writer of this type is on its way out; what it writes, haven reads.
    .sas7bdat = suppressWarnings(haven::write_sas(dfTable, strFile))
  )
  strFile
}

# What a session is told when a reader chooses a file: the file is put where
# Shiny keeps an upload, as the app reads it from nowhere else (#97).
dfChosen <- function(strFile) {
  dfUploaded(strFile)
}

test_that("a .csv, a .xpt and a .sas7bdat file are each read as the table that was written, in plain columns (#73)", {
  skip_if_not_installed("haven")
  for (strType in chrAppFileTypes) {
    strFile <- strWritten(Synthetic_Results, strType)
    dfRead <- App_ReadFile(strFile, basename(strFile))
    expect_identical(class(dfRead), "data.frame", label = strType)
    expect_identical(names(dfRead), names(Synthetic_Results), label = strType)
    expect_identical(nrow(dfRead), nrow(Synthetic_Results), label = strType)
    expect_identical(dfRead$USUBJID, Synthetic_Results$USUBJID, label = strType)
    expect_identical(dfRead$TEST, Synthetic_Results$TEST, label = strType)
    expect_equal(dfRead$STRESN, Synthetic_Results$STRESN, tolerance = 0, label = strType)
    expect_equal(dfRead$VISITNUM, as.numeric(Synthetic_Results$VISITNUM), tolerance = 0, label = strType)
    # A missing result is missing, in every type.
    expect_identical(is.na(dfRead$STRESN), is.na(Synthetic_Results$STRESN), label = strType)
    for (strColumn in names(dfRead)) {
      expect_null(attributes(dfRead[[strColumn]]), label = paste(strType, strColumn, "attributes"))
    }
  }
  # The type is read from the name, whatever its case, and a file made by SAS
  # itself is read: the one haven ships.
  strUpper <- strWritten(Synthetic_Participants, ".csv")
  expect_identical(nrow(App_ReadFile(strUpper, "PARTICIPANTS.CSV")), nrow(Synthetic_Participants))
  dfIris <- App_ReadFile(system.file("examples", "iris.sas7bdat", package = "haven"), "iris.sas7bdat")
  expect_identical(dim(dfIris), c(150L, 5L))
  expect_null(attributes(dfIris$Sepal_Length))
})

test_that("a file the app does not read, or R cannot, is refused with a sentence that names it (#73)", {
  skip_if_not_installed("haven")
  strText <- tempfile(fileext = ".txt")
  writeLines("not a table", strText)
  expect_error(App_ReadFile(strText, "study.xlsx"), "The app reads .csv, .xpt, .sas7bdat files, and study.xlsx is none of them.", fixed = TRUE)
  expect_error(App_ReadFile(strText, "results"), "and results is none of them.", fixed = TRUE)
  expect_error(App_ReadFile(strText, "results.xpt"), "^results.xpt could not be read as a .xpt file: ")
  expect_error(App_ReadFile(strText, "results.sas7bdat"), "^results.sas7bdat could not be read as a .sas7bdat file: ")
  strEmpty <- tempfile(fileext = ".csv")
  file.create(strEmpty)
  expect_error(App_ReadFile(strEmpty, "empty.csv"), "^empty.csv could not be read as a .csv file: ")
  strHeader <- tempfile(fileext = ".csv")
  writeLines("USUBJID,TEST,STRESN", strHeader)
  expect_error(App_ReadFile(strHeader, "header.csv"), "header.csv has no rows.", fixed = TRUE)
  strTwice <- tempfile(fileext = ".csv")
  writeLines(c("TEST,TEST,STRESN", "CRP,CRP,1.5"), strTwice)
  expect_error(App_ReadFile(strTwice, "twice.csv"), "twice.csv has two columns of one name", fixed = TRUE)
  # A last line with no line end is R's to warn of, and no reason to refuse.
  strOpen <- tempfile(fileext = ".csv")
  cat("USUBJID,STRESN\nBIO-001,1.5", file = strOpen)
  expect_no_warning(dfOpen <- App_ReadFile(strOpen, "open.csv"))
  expect_identical(dfOpen, data.frame(USUBJID = "BIO-001", STRESN = 1.5))
})

test_that("without haven a SAS file is answered with a sentence naming the package, and a .csv file is read all the same (#73)", {
  local_mocked_bindings(App_HasHaven = function() FALSE)
  strFile <- tempfile(fileext = ".csv")
  utils::write.csv(Synthetic_Participants, strFile, row.names = FALSE)
  for (strName in c("results.xpt", "results.sas7bdat")) {
    expect_error(
      App_ReadFile(strFile, strName),
      paste0(
        strName, " is a SAS file, which R reads with the haven package, and haven is not installed on this server. ",
        "Install it with install.packages(\"haven\"), or load the table as a .csv file."
      ),
      fixed = TRUE
    )
  }
  expect_identical(nrow(App_ReadFile(strFile, "participants.csv")), nrow(Synthetic_Participants))
})

test_that("a table's columns are renamed to gsm.bio's names when every one is said, and not before: a column left unsaid, chosen twice, or of text where a number is needed is answered with a sentence (#73)", {
  lTable <- App_Tables()$results
  dfTable <- dfReaderResults()
  chrChosen <- chrReaderResults
  dfMapped <- App_MapTable(dfTable, chrChosen, lTable, "results.csv")
  expect_identical(names(dfMapped), names(Synthetic_Results))
  expect_identical(dfMapped, Synthetic_Results)
  # A table already under gsm.bio's names is itself.
  chrSame <- stats::setNames(names(lTable$columns), names(lTable$columns))
  expect_identical(App_MapTable(Synthetic_Results, chrSame, lTable, "results.csv"), Synthetic_Results)
  # A column that had one of the names, and was not the one chosen, steps aside.
  dfBoth <- Synthetic_Results
  dfBoth$PARAM <- paste("Marker", dfBoth$TEST)
  chrOther <- chrSame
  chrOther[["TEST"]] <- "PARAM"
  dfOther <- App_MapTable(dfBoth, chrOther, lTable, "results.csv")
  expect_identical(dfOther$TEST, dfBoth$PARAM)
  expect_identical(dfOther$TEST_original, dfBoth$TEST)
  expect_false("PARAM" %in% names(dfOther))
  expect_identical(anyDuplicated(names(dfOther)), 0L)

  chrUnsaid <- chrChosen
  chrUnsaid[c("TEST", "VISITNUM")] <- ""
  expect_error(
    App_MapTable(dfTable, chrUnsaid, lTable, "results.csv"),
    "Say which column of results.csv is each of these, and the charts can be drawn: biomarker, visit order.",
    fixed = TRUE
  )
  # A column the file does not have is one not said.
  chrAbsent <- chrChosen
  chrAbsent[["USUBJID"]] <- "USUBJID"
  expect_error(App_MapTable(dfTable, chrAbsent, lTable, "results.csv"), "each of these, and the charts can be drawn: participant.", fixed = TRUE)
  chrTwice <- chrChosen
  chrTwice[["VISITNUM"]] <- "RESULT"
  expect_error(App_MapTable(dfTable, chrTwice, lTable, "results.csv"), "The column RESULT of results.csv is chosen twice.", fixed = TRUE)
  chrText <- chrChosen
  chrText[["STRESN"]] <- "STRESU"
  expect_error(
    App_MapTable(dfTable, chrText, lTable, "results.csv"),
    "The column STRESU of results.csv is chosen as result, which is a number, and it holds text.",
    fixed = TRUE
  )
  # The columns asked for are the ones RunApp() checks a table for.
  expect_identical(lapply(App_Tables(), function(lOne) names(lOne$columns)), App_Columns())
  expect_identical(App_Guess(names(Synthetic_Results), "TEST"), "TEST")
  expect_identical(App_Guess(names(dfTable), "TEST"), "")
})

test_that("in a session a reader's results file is drawn once its columns are said, a table half said or unreadable changes nothing, and participants and outcomes are optional (#73)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("haven")
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  dfSmall <- dfReaderResults()
  dfSmall <- dfSmall[dfSmall$MARKER %in% c("CRP", "IL-6"), ]
  strResults <- strWritten(dfSmall, ".xpt")
  strParticipants <- strWritten(dfReaderParticipants(), ".csv", "participants")
  strOutcomes <- strWritten(Synthetic_Outcomes, ".sas7bdat", "outcomes")
  strBroken <- tempfile(fileext = ".xpt")
  writeLines("not a table", strBroken)
  Payload <- function(strJson) jsonlite::fromJSON(strJson, simplifyVector = FALSE)$x
  Html <- function(xOutput) as.character(xOutput$html)

  shiny::testServer(RunApp(), {
    nSynthetic <- nrow(Synthetic_Results)
    expect_identical(output$gsm_bio_source, "Drawn on the synthetic study that ships with gsm.bio.")
    expect_length(Payload(output$GroupComparison)$dfResults$USUBJID, nSynthetic)

    # The button with no file: a sentence, and nothing changes.
    session$setInputs(gsm_bio_apply = 1)
    expect_match(Html(output$gsm_bio_data_said), "Choose a results file first", fixed = TRUE)
    expect_match(Html(output$gsm_bio_data_said), "gsm-bio-app-problem", fixed = TRUE)

    # A file R cannot read: the sentence is under the file, and the button
    # says it again and draws nothing.
    session$setInputs(gsm_bio_file_results = dfChosen(strBroken))
    expect_match(Html(output$gsm_bio_columns_results), "could not be read as a .xpt file", fixed = TRUE)
    session$setInputs(gsm_bio_apply = 2)
    expect_match(Html(output$gsm_bio_data_said), "could not be read as a .xpt file", fixed = TRUE)
    expect_length(Payload(output$GroupComparison)$dfResults$USUBJID, nSynthetic)

    # A file that is read: a select for each column the charts need, none
    # filled in, because no column of the file has gsm.bio's name for it.
    session$setInputs(gsm_bio_file_results = dfChosen(strResults))
    strAsked <- Html(output$gsm_bio_columns_results)
    expect_match(strAsked, sprintf("results.xpt: %s rows, 6 columns. Which column is which?", format(nrow(dfSmall), big.mark = ",")), fixed = TRUE)
    for (strColumn in names(chrReaderResults)) {
      expect_match(strAsked, sprintf("id=\"gsm_bio_column_results_%s\"", strColumn), fixed = TRUE)
    }
    expect_match(strAsked, "<option value=\"\" selected>Not said yet</option>", fixed = TRUE)
    expect_false(grepl("<option value=\"MARKER\" selected>", strAsked, fixed = TRUE))

    # Half said: a sentence naming what is not, and the charts as they were.
    session$setInputs(
      gsm_bio_column_results_USUBJID = "SUBJ", gsm_bio_column_results_TEST = "MARKER", gsm_bio_column_results_STRESN = "RESULT",
      gsm_bio_column_results_VISIT = "", gsm_bio_column_results_VISITNUM = "", gsm_bio_apply = 3
    )
    expect_match(Html(output$gsm_bio_data_said), "Say which column of results.xpt is each of these, and the charts can be drawn: visit, visit order.", fixed = TRUE)
    expect_identical(output$gsm_bio_source, "Drawn on the synthetic study that ships with gsm.bio.")
    expect_length(Payload(output$GroupComparison)$dfResults$USUBJID, nSynthetic)

    # Every column said: the charts are drawn on the reader's table, under
    # gsm.bio's names, with no participants and so no survival chart.
    session$setInputs(gsm_bio_column_results_VISIT = "VIS", gsm_bio_column_results_VISITNUM = "VISORD", gsm_bio_apply = 4)
    expect_match(Html(output$gsm_bio_data_said), "The charts are drawn on results.xpt, loaded in this session.", fixed = TRUE)
    expect_identical(output$gsm_bio_source, "Drawn on results.xpt, loaded in this session.")
    for (strChart in setdiff(names(chrAppCharts), "StratifiedSurvival")) {
      lPayload <- Payload(output[[strChart]])
      expect_length(lPayload$dfResults$USUBJID, nrow(dfSmall))
      expect_setequal(unlist(unique(lPayload$dfResults$TEST)), c("CRP", "IL-6"))
      expect_null(lPayload$dfResults$MARKER, label = strChart)
      expect_null(lPayload$dfParticipants, label = strChart)
      expect_true(lPayload$lStatistics$served, label = strChart)
    }
    expect_match(Html(output$gsm_bio_place_StratifiedSurvival), "The stratified survival chart reads an outcomes table", fixed = TRUE)
    expect_error(output$StratifiedSurvival)

    # With participants and outcomes: both reach the charts, the outcomes
    # already under gsm.bio's names, so its columns are filled in.
    session$setInputs(gsm_bio_file_participants = dfChosen(strParticipants), gsm_bio_file_outcomes = dfChosen(strOutcomes))
    expect_match(Html(output$gsm_bio_columns_outcomes), "<option value=\"PARAMCD\" selected>PARAMCD</option>", fixed = TRUE)
    session$setInputs(
      gsm_bio_column_participants_USUBJID = "SUBJ",
      gsm_bio_column_outcomes_USUBJID = "USUBJID", gsm_bio_column_outcomes_PARAMCD = "PARAMCD", gsm_bio_column_outcomes_PARAM = "PARAM",
      gsm_bio_column_outcomes_AVAL = "AVAL", gsm_bio_column_outcomes_CNSR = "CNSR", gsm_bio_apply = 5
    )
    expect_identical(output$gsm_bio_source, "Drawn on results.xpt, participants.csv, outcomes.sas7bdat, loaded in this session.")
    lSurvival <- Payload(output$StratifiedSurvival)
    expect_length(lSurvival$dfOutcomes$USUBJID, nrow(Synthetic_Outcomes))
    expect_length(lSurvival$dfParticipants$USUBJID, nrow(Synthetic_Participants))
    expect_identical(unlist(lSurvival$dfParticipants$USUBJID), Synthetic_Participants$USUBJID)
    expect_match(Html(output$gsm_bio_place_StratifiedSurvival), "id=\"StratifiedSurvival\"", fixed = TRUE)
  })
})

test_that("in a browser a results file of each type the app reads is loaded, its columns said, and the group comparison drawn on it with the statistic the packaged study gives; a file over the size the app accepts is refused (#73)", {
  NeedApp()
  expect_true(requireNamespace("haven", quietly = TRUE), label = "haven is installed, to write and read the SAS files")
  lSettings <- list(start_value = "CRP", visits = "Week 4", group_by = "ARM")
  lSaved <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lSettings)
  lExpected <- lSaved$x$lStatistics$results
  lExpected <- stats::setNames(lExpected, vapply(lExpected, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1)))
  strParticipants <- strWritten(dfReaderParticipants(), ".csv", "participants")

  lApp <- lRunApp("RunApp(Synthetic_Results[Synthetic_Results$TEST == 'IL-6', ], lSettings = list(GroupComparison = list(start_value = 'CRP', visits = 'Week 4', group_by = 'ARM')))")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)
  Choose <- function(strTable, chrColumns) {
    for (strColumn in names(chrColumns)) {
      strId <- sprintf("gsm_bio_column_%s_%s", strTable, strColumn)
      expect_true(bWaitFor(lPage, sprintf("document.querySelector('#%s')", strId)), label = paste("the page asks for", strColumn))
      lPage$Evaluate(sprintf(
        "(() => { const node = document.querySelector('#%s'); node.value = '%s'; node.dispatchEvent(new Event('change', { bubbles: true })); return node.value; })()",
        strId, chrColumns[[strColumn]]
      ))
    }
  }
  strChart <- "(HTMLWidgets.find('#GroupComparison') && HTMLWidgets.find('#GroupComparison').chart())"
  for (strType in chrAppFileTypes) {
    strFile <- strWritten(dfReaderResults(), strType)
    lPage$Evaluate("document.querySelector('a[data-value=\"Data\"]').click()")
    lPage$Evaluate(sprintf("(() => { window.gsmBioWas = %s; return true; })()", strChart))
    lPage$Upload("#gsm_bio_file_results", strFile)
    expect_true(
      bWaitFor(lPage, sprintf("(document.querySelector('#gsm_bio_columns_results') || {}).textContent.includes('results%s: 11,472 rows')", strType)),
      label = paste(strType, "was read")
    )
    Choose("results", chrReaderResults)
    if (strType == chrAppFileTypes[1]) {
      lPage$Upload("#gsm_bio_file_participants", strParticipants)
      Choose("participants", c(USUBJID = "SUBJ"))
    }
    lPage$Evaluate("document.querySelector('#gsm_bio_apply').click()")
    strSource <- sprintf("results%s, participants.csv, loaded in this session", strType)
    expect_true(
      bWaitFor(lPage, sprintf("document.querySelector('#gsm_bio_source').textContent === 'Drawn on %s.'", strSource)),
      label = paste(strType, "is what the charts are drawn on")
    )
    lPage$Evaluate("document.querySelector('a[data-value=\"GroupComparison\"]').click()")
    expect_true(
      bWaitFor(lPage, sprintf(
        "%s && %s !== window.gsmBioWas && %s.statistics().length === 1 && %s.statistics()[0].answer",
        strChart, strChart, strChart, strChart
      )),
      label = paste(strType, "was drawn and answered")
    )
    lAsked <- lPage$Evaluate(sprintf("%s.statistics()[0]", strChart))
    strKey <- Chart_KeyText(lAsked[c("name", "args", "dataId")])
    expect_true(strKey %in% names(lExpected), label = paste(strType, "asks what the packaged study's chart asks"))
    expect_identical(lAsked$answer$form, "server", label = strType)
    expect_equal(lAsked$answer$value, lExpected[[strKey]]$value, tolerance = 1e-14, label = strType)
    expect_identical(lAsked$rows, lExpected[[strKey]]$rows, label = strType)
  }
  expect_identical(lPage$Errors(), character(0))
  lPage$Close()
  lApp$Stop()

  # An app that accepts a hundredth of a megabyte: the file is refused by
  # Shiny, in the page, and the charts stay on what they were drawn on.
  lSmall <- lRunApp("RunApp(nMaxUploadMB = 0.01)")
  on.exit(lSmall$Stop(), add = TRUE)
  lRefused <- lOpenPage(NULL, strAddress = lSmall$address)
  on.exit(lRefused$Close(), add = TRUE)
  lRefused$Evaluate("document.querySelector('a[data-value=\"Data\"]').click()")
  lRefused$Upload("#gsm_bio_file_results", strWritten(dfReaderResults(), ".csv"))
  expect_true(
    bWaitFor(lRefused, "/Maximum upload size exceeded/.test(document.querySelector('#gsm_bio_file_results_progress').textContent)"),
    label = "the page says the file is too large"
  )
  expect_false(isTRUE(lRefused$Evaluate("Boolean(document.querySelector('#gsm_bio_column_results_TEST'))")))
  expect_identical(lRefused$Evaluate("document.querySelector('#gsm_bio_source').textContent"), "Drawn on the synthetic study that ships with gsm.bio.")
})

# ---- A file R warns of as it reads it (#86) ----------------------------------

# The release review's two files, made here byte for byte: a results table of
# 100 rows whose row 50 holds what stops read.csv() there with a warning and
# no error. `latin1` has the micro sign as Latin-1 writes it, one byte that is
# not UTF-8; `quote` has a quote that is never closed.
strShortRead <- function(strWhich) {
  chrLines <- c("USUBJID,TEST,STRESN,VISIT,VISITNUM,NOTE", sprintf("S%03d,CRP,%d,Week 1,1,ok", 1:100, 1:100))
  chrLines[51] <- switch(strWhich,
    latin1 = "S050,CRP,50,Week 1,1,@mol/L",
    quote = "S050,CRP,50,Week 1,1,5\" tube"
  )
  xBytes <- charToRaw(paste0(paste(chrLines, collapse = "\n"), "\n"))
  xBytes[xBytes == charToRaw("@")] <- as.raw(0xb5)
  strDir <- tempfile("gsm-bio-short")
  dir.create(strDir)
  strFile <- file.path(strDir, paste0(strWhich, ".csv"))
  writeBin(xBytes, strFile)
  strFile
}

# What R says of a file as read.csv() reads it the way the app does: the rows
# it gives and the warnings it raises.
lReadPlainly <- function(strFile) {
  chrWarned <- character(0)
  dfRead <- withCallingHandlers(
    utils::read.csv(strFile, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM"),
    warning = function(cndWarning) {
      chrWarned <<- c(chrWarned, conditionMessage(cndWarning))
      invokeRestart("muffleWarning")
    }
  )
  list(rows = nrow(dfRead), warned = chrWarned)
}

# Every form the path of a file can be written in: no sentence for a reader
# holds any of them, or the directory, or the name the server gave the file.
ExpectNoPath <- function(strSaid, strFile, strLabel) {
  for (strPart in unique(c(strFile, normalizePath(strFile), dirname(strFile), dirname(normalizePath(strFile)), tempdir(), normalizePath(tempdir())))) {
    expect_false(grepl(strPart, strSaid, fixed = TRUE), label = paste(strLabel, "holds", strPart))
  }
}

# The sentence App_ReadFile() stops with.
strRefused <- function(strFile, strName) {
  tryCatch(
    {
      App_ReadFile(strFile, strName)
      NA_character_
    },
    error = function(cndError) conditionMessage(cndError)
  )
}

test_that("a .csv file R warns of as it reads it, a byte that is not UTF-8 or a quote never closed in row 50 of 100, gives a sentence with R's own words and no table (#86)", {
  lSaid <- list(
    latin1 = "invalid input found on input connection 'latin1.csv'",
    quote = "EOF within quoted string"
  )
  for (strWhich in names(lSaid)) {
    strFile <- strShortRead(strWhich)
    strName <- paste0(strWhich, ".csv")
    # The file is what the review found: R reads 50 of its 100 rows, and warns.
    lPlain <- lReadPlainly(strFile)
    expect_identical(lPlain$rows, 50L, label = paste(strWhich, "rows R reads"))
    expect_length(lPlain$warned, 1L)
    # No table, and a sentence: the file's name, that R warned, and R's words.
    strSaid <- strRefused(strFile, strName)
    expect_identical(
      strSaid,
      paste0(
        strName, " was not loaded: R warned as it read the file, and a file R warns of may have been read short. ",
        "R said: ", lSaid[[strWhich]], ". The app reads a .csv file as comma-separated text in UTF-8."
      ),
      label = strWhich
    )
    ExpectNoPath(strSaid, strFile, strWhich)
  }
  # A byte R reads as the end of a text is warned of too, though every row is read.
  strNul <- tempfile(fileext = ".csv")
  writeBin(c(charToRaw("A,B\n1,2\n3,"), as.raw(0), charToRaw("4\n5,6\n")), strNul)
  expect_match(strRefused(strNul, "nul.csv"), "nul.csv was not loaded: .* R said: line 3 appears to contain embedded nulls\\.")
})

test_that("a .csv file whose last line has no line end is read with all its rows, whether or not R warns of it, and so is one with Windows line ends or a byte-order mark (#86)", {
  # Three lines: R warns that the last is incomplete, and reads it.
  strShort <- tempfile(fileext = ".csv")
  cat("USUBJID,STRESN\nBIO-001,1.5\nBIO-002,2.5", file = strShort)
  lPlain <- lReadPlainly(strShort)
  expect_match(lPlain$warned, "^incomplete final line found by readTableHeader")
  expect_identical(lPlain$rows, 2L)
  expect_no_warning(dfShort <- App_ReadFile(strShort, "short.csv"))
  expect_identical(dfShort, data.frame(USUBJID = c("BIO-001", "BIO-002"), STRESN = c(1.5, 2.5)))
  # A hundred lines: R does not warn.
  strLong <- tempfile(fileext = ".csv")
  cat(paste(c("USUBJID,STRESN", sprintf("BIO-%03d,%d", 1:100, 1:100)), collapse = "\n"), file = strLong)
  expect_identical(lReadPlainly(strLong)$warned, character(0))
  dfLong <- App_ReadFile(strLong, "long.csv")
  expect_identical(nrow(dfLong), 100L)
  expect_identical(dfLong$STRESN, 1:100)
  # Windows line ends, after a byte-order mark.
  strWindows <- tempfile(fileext = ".csv")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("USUBJID,STRESN\r\nBIO-001,1.5\r\nBIO-002,2.5\r\n")), strWindows)
  expect_identical(App_ReadFile(strWindows, "windows.csv"), data.frame(USUBJID = c("BIO-001", "BIO-002"), STRESN = c(1.5, 2.5)))
})

test_that("an .xpt or .sas7bdat file R cannot read is named by its own name and never by where the server keeps it, and one haven warns of gives no table (#86)", {
  skip_if_not_installed("haven")
  strDir <- tempfile("gsm-bio-upload")
  dir.create(strDir)
  # Shiny keeps an upload under a name of its own: 0.xpt, in a temporary folder.
  strKept <- file.path(strDir, "0.xpt")
  writeLines("not a table", strKept)
  for (strName in c("results.xpt", "results.sas7bdat")) {
    strSaid <- strRefused(strKept, strName)
    strType <- App_FileType(strName)
    expect_match(strSaid, sprintf("^%s could not be read as a %s file: ", strName, strType), label = strName)
    # haven names the file it failed on: by the reader's name for it.
    expect_match(strSaid, sprintf(": Failed to parse %s: ", strName), fixed = TRUE, label = strName)
    ExpectNoPath(strSaid, strKept, strName)
    expect_false(grepl("0.xpt", strSaid, fixed = TRUE), label = paste(strName, "holds the server's name for the file"))
  }
  # haven warned of nothing in the files tried when this was written. Should
  # it, the file is refused as a .csv is: with the warning's words, less the path.
  strRead <- strWritten(Synthetic_Outcomes, ".xpt", "outcomes")
  local_mocked_bindings(
    read_xpt = function(file, ...) {
      warning("Some rows of ", normalizePath(file), " were not read", call. = FALSE)
      Synthetic_Outcomes
    },
    .package = "haven"
  )
  strWarned <- strRefused(strRead, "outcomes.xpt")
  expect_identical(
    strWarned,
    "outcomes.xpt was not loaded: R warned as it read the file, and a file R warns of may have been read short. R said: Some rows of outcomes.xpt were not read."
  )
})

test_that("in a session a .csv file R read short is said in the file's place and again on the button, and the charts are left as they were (#86)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("haven")
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  Payload <- function(strJson) jsonlite::fromJSON(strJson, simplifyVector = FALSE)$x
  Html <- function(xOutput) as.character(xOutput$html)
  lFiles <- list(latin1 = strShortRead("latin1"), quote = strShortRead("quote"))
  strBroken <- file.path(dirname(lFiles$latin1), "0.xpt")
  writeLines("not a table", strBroken)

  shiny::testServer(RunApp(), {
    nSynthetic <- nrow(Synthetic_Results)
    nApplied <- 0L
    for (strWhich in names(lFiles)) {
      dfShort <- dfChosen(lFiles[[strWhich]])
      session$setInputs(gsm_bio_file_results = dfShort)
      strPlace <- Html(output$gsm_bio_columns_results)
      expect_match(strPlace, "gsm-bio-app-problem", fixed = TRUE, label = strWhich)
      expect_match(strPlace, sprintf("%s.csv was not loaded: R warned as it read the file", strWhich), fixed = TRUE, label = strWhich)
      expect_match(strPlace, if (strWhich == "latin1") "invalid input found on input connection" else "EOF within quoted string", fixed = TRUE)
      # Nothing of the file is offered: no rows counted, no column to say.
      expect_false(grepl("50 rows", strPlace, fixed = TRUE), label = paste(strWhich, "counts the rows read"))
      expect_false(grepl("gsm_bio_column_results_", strPlace, fixed = TRUE), label = paste(strWhich, "asks for columns"))
      # Neither the file as it was written nor where the server keeps it.
      ExpectNoPath(strPlace, lFiles[[strWhich]], strWhich)
      ExpectNoPath(strPlace, dfShort$datapath, strWhich)
      # The columns said all the same, as a reader who had them from a file
      # before would have: the button says the problem and draws nothing.
      nApplied <- nApplied + 1L
      session$setInputs(
        gsm_bio_column_results_USUBJID = "USUBJID", gsm_bio_column_results_TEST = "TEST", gsm_bio_column_results_STRESN = "STRESN",
        gsm_bio_column_results_VISIT = "VISIT", gsm_bio_column_results_VISITNUM = "VISITNUM", gsm_bio_apply = nApplied
      )
      strButton <- Html(output$gsm_bio_data_said)
      expect_match(strButton, "gsm-bio-app-problem", fixed = TRUE, label = strWhich)
      expect_match(strButton, sprintf("%s.csv was not loaded: R warned as it read the file", strWhich), fixed = TRUE, label = strWhich)
      expect_identical(output$gsm_bio_source, "Drawn on the synthetic study that ships with gsm.bio.")
      for (strChart in setdiff(names(chrAppCharts), "StratifiedSurvival")) {
        expect_length(Payload(output[[strChart]])$dfResults$USUBJID, nSynthetic)
      }
    }
    # A SAS file that cannot be read: no path of the server in either place.
    dfBroken <- dfUploaded(strBroken, "results.xpt")
    expect_identical(basename(dfBroken$datapath), "0.xpt")
    session$setInputs(gsm_bio_file_results = dfBroken)
    strPlace <- Html(output$gsm_bio_columns_results)
    expect_match(strPlace, "results.xpt could not be read as a .xpt file: Failed to parse results.xpt", fixed = TRUE)
    ExpectNoPath(strPlace, dfBroken$datapath, "the file's place")
    expect_false(grepl("0.xpt", strPlace, fixed = TRUE))
    session$setInputs(gsm_bio_apply = nApplied + 1L)
    ExpectNoPath(Html(output$gsm_bio_data_said), dfBroken$datapath, "the button")
    expect_length(Payload(output$GroupComparison)$dfResults$USUBJID, nSynthetic)
  })
})

# ---- The Data page's layout: a rail, the viewer and a card for each table (#85)

# A page's words, without its markup.
strPageText <- function(strHtml) {
  trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", strHtml)))
}

# One part of a page, from the tag that opens it to the tag that closes it.
strPagePart <- function(strHtml, strOpens, strCloses) {
  strFlat <- gsub("\n\\s*", "", strHtml)
  chrPart <- regmatches(strFlat, regexpr(paste0(strOpens, ".*?", strCloses), strFlat, perl = TRUE))
  if (length(chrPart) == 0L) "" else chrPart
}

# The files of the design's three states: a results file with two columns
# under names of its own, a participants file under gsm.bio's, and a file of a
# type the app does not read.
lLayoutFiles <- function() {
  strDir <- tempfile("gsm-bio-layout")
  dir.create(strDir)
  dfResults <- Synthetic_Results
  names(dfResults)[match(c("USUBJID", "TEST"), names(dfResults))] <- c("SUBJID", "LBTEST")
  lFiles <- list(results = file.path(strDir, "lb.csv"), participants = file.path(strDir, "dm.csv"), outcomes = file.path(strDir, "notes.xlsx"))
  utils::write.csv(dfResults, lFiles$results, row.names = FALSE, na = "")
  utils::write.csv(Synthetic_Participants, lFiles$participants, row.names = FALSE, na = "")
  writeLines("not a table", lFiles$outcomes)
  lFiles
}
strNotRead <- "The app reads .csv, .xpt, .sas7bdat files, and notes.xlsx is none of them."
strUnsaid <- "Say which column of lb.csv is each of these, and the charts can be drawn: participant, biomarker."

test_that("the Data page opens as a rail beside the viewer and a card for each table: results tagged needed, participants and outcomes optional, each with its file control, and the button with its two places beside it (#85)", {
  skip_if_not_installed("shiny")
  strPage <- gsub("\n\\s*", "", as.character(App_Ui()))
  At <- function(strWhat) regexpr(strWhat, strPage, fixed = TRUE)[1]
  # The rail is first, then the viewer, the three cards in order, the button.
  chrOrder <- c(
    "<aside class=\"gsm-bio-app-rail\"", "<section class=\"gsm-bio-app-card gsm-bio-app-viewer\"",
    "id=\"gsm_bio_card_results\"", "id=\"gsm_bio_card_participants\"", "id=\"gsm_bio_card_outcomes\"", "id=\"gsm_bio_apply\""
  )
  nAt <- vapply(chrOrder, At, numeric(1))
  expect_true(all(nAt > 0))
  expect_identical(order(nAt), seq_along(nAt))
  # The viewer keeps its tabs, its rows and its two buttons, in a card.
  strViewer <- strPagePart(strPage, "<section class=\"gsm-bio-app-card gsm-bio-app-viewer\"", "</section>")
  for (strId in c("gsm_bio_view_tabs", "gsm_bio_view", "gsm_bio_view_previous", "gsm_bio_view_next")) {
    expect_match(strViewer, sprintf("id=\"%s\"", strId), fixed = TRUE)
  }
  # A card for each table: its name, whether the charts need it, what it is
  # and the files the app reads, a place to choose a file, and the place the
  # session writes the chosen file in.
  lTables <- App_Tables()
  for (strTable in names(lTables)) {
    strCard <- strPagePart(strPage, sprintf("<section class=\"gsm-bio-app-card gsm-bio-app-file\" id=\"gsm_bio_card_%s\"", strTable), "</section>")
    expect_true(nzchar(strCard), label = strTable)
    expect_match(strCard, sprintf("<h3[^>]*>%s</h3>", lTables[[strTable]]$label), label = strTable)
    strTag <- if (lTables[[strTable]]$needed) "<span class=\"gsm-bio-app-tag gsm-bio-app-tag-need\">needed</span>" else "<span class=\"gsm-bio-app-tag gsm-bio-app-tag-optional\">optional</span>"
    expect_match(strCard, strTag, fixed = TRUE, label = strTable)
    expect_match(strPageText(strCard), paste0(lTables[[strTable]]$what, "; a .csv, .xpt or .sas7bdat file"), fixed = TRUE, label = strTable)
    expect_match(strCard, sprintf("<input id=\"gsm_bio_file_%s\"[^>]*type=\"file\"", strTable), label = strTable)
    expect_match(strCard, sprintf("id=\"gsm_bio_columns_%s\" class=\"shiny-html-output\"", strTable), fixed = TRUE, label = strTable)
  }
  expect_match(strPage, ">Choose the results file<", fixed = TRUE)
  # The button, and beside it what it will draw and what R said of the last press.
  strDraw <- strPagePart(strPage, "<div class=\"gsm-bio-app-draw\"", "</div>\\s*</div>\\s*</div>")
  expect_match(strDraw, "id=\"gsm_bio_apply\"", fixed = TRUE)
  expect_lt(regexpr("id=\"gsm_bio_apply\"", strDraw, fixed = TRUE)[1], regexpr("id=\"gsm_bio_data_said\"", strDraw, fixed = TRUE)[1])
  expect_match(strDraw, "id=\"gsm_bio_data_files\"", fixed = TRUE)

  # The rail as the page is written, before a session answers: three steps,
  # what the charts are drawn on, and that a file is held for the session only.
  strRail <- strPagePart(strPage, "<aside class=\"gsm-bio-app-rail\"", "</aside>")
  expect_match(strRail, "id=\"gsm_bio_rail\" class=\"shiny-html-output\"", fixed = TRUE)
  expect_identical(
    strPageText(strRail),
    paste(
      "Workflow",
      "Done: Choose files none chosen The charts are drawn on the synthetic study that ships with gsm.bio. Choose a results file to draw them on a study of your own.",
      "Done: Say which column is which nothing to say",
      "Next: Draw the charts 6 of 6 charts ready Open group comparison",
      "Drawn on",
      "Results 11,472 rows, 6 columns Participants 200 rows, 6 columns",
      sprintf("Outcomes %s rows, %d columns", format(nrow(Synthetic_Outcomes), big.mark = ","), ncol(Synthetic_Outcomes)),
      "A file you choose is read by R on this server and held in this session's memory only. Nothing is kept when the session ends."
    )
  )
})

test_that("the rail's three steps count what is left: files chosen and not read, columns still to say, and the charts that are ready or wait on a step (#85)", {
  lTables <- App_Tables()
  dfResults <- Synthetic_Results[Synthetic_Results$TEST %in% c("CRP", "IL-6"), ]
  dfOwn <- dfResults
  names(dfOwn)[match(c("USUBJID", "TEST"), names(dfOwn))] <- c("SUBJID", "LBTEST")
  lSynthetic <- App_Study(NULL, NULL, NULL)
  # What a reader has said of a file's columns when they have said nothing:
  # the columns that already have gsm.bio's names.
  Guessed <- function(lFiles) {
    lapply(stats::setNames(names(lFiles), names(lFiles)), function(strTable) {
      if (is.null(lFiles[[strTable]]$table)) {
        return(NULL)
      }
      vapply(names(lTables[[strTable]]$columns), function(strColumn) App_Guess(names(lFiles[[strTable]]$table), strColumn), character(1))
    })
  }
  Steps <- function(lStudy, lFiles, lChosen = Guessed(lFiles), bDrawn = FALSE) App_Steps(lStudy, lFiles, lChosen, bDrawn)

  # As the app opens: nothing chosen, nothing to say, every chart ready.
  lOpens <- Steps(lSynthetic, list())
  expect_identical(lOpens$files[c("state", "says")], list(state = "done", says = "none chosen"))
  expect_identical(lOpens$columns[c("state", "says", "notes")], list(state = "done", says = "nothing to say", notes = character(0)))
  expect_identical(lOpens$charts[c("state", "says", "notes", "open")], list(state = "next", says = "6 of 6 charts ready", notes = character(0), open = "GroupComparison"))
  expect_false(lOpens$drawn$still)
  expect_identical(lOpens$drawn$tables[["Results"]], "11,472 rows, 6 columns")

  # A results file with two columns to say, a participants file with none,
  # and an outcomes file R could not read.
  lFiles <- list(
    results = list(name = "lb.xpt", table = dfOwn), participants = list(name = "dm.csv", table = Synthetic_Participants),
    outcomes = list(name = "notes.xlsx", problem = strNotRead)
  )
  lHalf <- Steps(lSynthetic, lFiles)
  expect_identical(lHalf$files$state, "next")
  expect_identical(lHalf$files$says, "lb.xpt, dm.csv, notes.xlsx, 1 not read")
  expect_identical(lHalf$files$notes, "notes.xlsx was not read: choose another file for the outcomes table, or remove it. The charts can be drawn without an outcomes table.")
  expect_identical(lHalf$columns[c("state", "says", "notes")], list(state = "next", says = "2 of 6 columns still to say", notes = "In lb.xpt: participant, biomarker."))
  expect_identical(lHalf$charts[c("state", "says")], list(state = "waiting", says = "waiting on step 1"))
  expect_null(lHalf$charts$open)
  # The charts stay where they were, and the rail says so.
  expect_true(lHalf$drawn$still)
  expect_identical(lHalf$drawn$tables[["Results"]], "11,472 rows, 6 columns")
  expect_match(lHalf$drawn$note, "The charts stay on these tables until", fixed = TRUE)

  # The unreadable file taken away: the columns are what is left.
  lFiles$outcomes <- NULL
  lAsked <- Steps(lSynthetic, lFiles)
  expect_identical(lAsked$files[c("state", "says", "notes")], list(state = "done", says = "lb.xpt, dm.csv", notes = character(0)))
  expect_identical(lAsked$columns$state, "next")
  expect_identical(lAsked$charts[c("state", "says")], list(state = "waiting", says = "waiting on step 2"))
  # Every column said: the button is what is left, and it would draw five charts.
  lChosen <- Guessed(lFiles)
  lChosen$results[c("USUBJID", "TEST")] <- c("SUBJID", "LBTEST")
  lSaid <- Steps(lSynthetic, lFiles, lChosen)
  expect_identical(lSaid$columns[c("state", "says", "notes")], list(state = "done", says = "6 of 6 columns said", notes = character(0)))
  expect_identical(lSaid$charts$state, "next")
  expect_identical(lSaid$charts$says, "5 of 6 charts can be drawn")
  expect_identical(lSaid$charts$notes, c("Press the button under the cards.", "Stratified survival needs an outcomes table."))
  # A column said that the file does not have is not said.
  lChosen$results[["TEST"]] <- "MARKER"
  expect_identical(Steps(lSynthetic, lFiles, lChosen)$columns$says, "1 of 6 columns still to say")

  # Drawn on the reader's files: every step done, five charts ready, and the
  # chart that is not says which table it lacks.
  lDrawn <- list(
    results = dfResults, participants = Synthetic_Participants, outcomes = NULL,
    source = "lb.xpt, dm.csv, loaded in this session", files = c(results = "lb.xpt", participants = "dm.csv")
  )
  lChosen$results[["TEST"]] <- "LBTEST"
  lDone <- Steps(lDrawn, lFiles, lChosen, bDrawn = TRUE)
  expect_identical(vapply(lDone[c("files", "columns", "charts")], function(lStep) lStep$state, character(1)), c(files = "done", columns = "done", charts = "done"))
  expect_identical(lDone$charts$says, "5 of 6 charts ready")
  expect_identical(lDone$charts$notes, App_Lacks("StratifiedSurvival", lDrawn))
  expect_identical(lDone$charts$open, "GroupComparison")
  expect_false(lDone$drawn$still)
  expect_identical(lDone$drawn$tables, c(
    Results = sprintf("lb.xpt, %s rows", format(nrow(dfResults), big.mark = ",")), Participants = "dm.csv, 200 rows", Outcomes = "none"
  ))

  # Files chosen with no results file among them: that is what is left.
  lNoResults <- Steps(lSynthetic, list(participants = list(name = "dm.csv", table = Synthetic_Participants)))
  expect_identical(lNoResults$files$state, "next")
  expect_identical(lNoResults$files$notes, "Choose a results file: the charts are drawn from the results table.")
  # A results file R could not read.
  lBad <- Steps(lSynthetic, list(results = list(name = "lb.xlsx", problem = "no")))
  expect_identical(lBad$files$says, "lb.xlsx, 1 not read")
  expect_identical(lBad$files$notes, "lb.xlsx was not read: choose another results file.")
  expect_identical(lBad$columns$says, "nothing to say")
})

test_that("a column question is tagged same name where the file's column has gsm.bio's name, say which where the reader has still to say, and said where they have; its select is one the page marks while it is unsaid (#85)", {
  skip_if_not_installed("shiny")
  expect_identical(as.character(App_Tag("USUBJID", "USUBJID")), "<span class=\"gsm-bio-app-tag gsm-bio-app-tag-same\">same name</span>")
  expect_identical(as.character(App_Tag("", "USUBJID")), "<span class=\"gsm-bio-app-tag gsm-bio-app-tag-need\">say which</span>")
  expect_identical(as.character(App_Tag("SUBJID", "USUBJID")), "<span class=\"gsm-bio-app-tag gsm-bio-app-tag-said\">said</span>")
  # An unsaid select is amber by a rule of the page's own style: it is a
  # select that must have a value, and has none.
  expect_match(strAppStyle, ".gsm-bio-app-ask select:invalid {", fixed = TRUE)
})

test_that("in a session the Data page counts what is left as a reader goes: a file chosen with two columns to say, a file R could not read reported in its own card, R's sentence at the button, and the charts that are ready once they are drawn (#85)", {
  skip_if_not_installed("shiny")
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  lFiles <- lLayoutFiles()
  # An output with nothing in it has no markup: its words are none.
  Html <- function(xOutput) paste(as.character(xOutput$html), collapse = "")
  shiny::testServer(RunApp(), {
    Rail <- function() strPageText(Html(output$gsm_bio_rail))
    expect_match(Rail(), "Done: Choose files none chosen", fixed = TRUE)
    expect_match(Rail(), "Next: Draw the charts 6 of 6 charts ready", fixed = TRUE)
    expect_identical(strPageText(Html(output$gsm_bio_data_files)), "No file is chosen: the button has nothing to draw.")

    # The three files chosen. The results file's card: its name, the sentence
    # with its rows and columns, a question for each column with its tag, and
    # its first rows; and a way to take the file away.
    session$setInputs(gsm_bio_file_results = dfChosen(lFiles$results), gsm_bio_file_participants = dfChosen(lFiles$participants), gsm_bio_file_outcomes = dfChosen(lFiles$outcomes))
    strCard <- Html(output$gsm_bio_columns_results)
    expect_match(strCard, "<span class=\"gsm-bio-app-chosen-name\">lb.csv</span>", fixed = TRUE)
    expect_match(strCard, "lb.csv: 11,472 rows, 6 columns. Which column is which?", fixed = TRUE)
    expect_match(strCard, "<button [^>]*id=\"gsm_bio_remove_results\"[^>]*>")
    expect_match(strCard, "The first 5 rows of lb.csv, as R read them:", fixed = TRUE)
    for (strColumn in names(App_Tables()$results$columns)) {
      # A question: from where it opens to the next question, or to the rows.
      strAsk <- strPagePart(strCard, sprintf("<div class=\"gsm-bio-app-ask\" data-column=\"%s\"", strColumn), "(?=<div class=\"gsm-bio-app-ask\"|<p class=\"gsm-bio-app-what\")")
      expect_match(strAsk, sprintf("<select [^>]*id=\"gsm_bio_column_results_%s\"[^>]*required", strColumn), label = strColumn)
      expect_match(strAsk, sprintf("id=\"gsm_bio_tag_results_%s\" class=\"shiny-html-output\"", strColumn), fixed = TRUE, label = strColumn)
      # The tag as the card is written, and as the session writes it after.
      strTag <- if (strColumn %in% c("USUBJID", "TEST")) "say which" else "same name"
      expect_match(strPageText(strAsk), paste0("^", sub(",.*$", "", App_Tables()$results$columns[[strColumn]])), label = strColumn)
      expect_match(strPageText(strAsk), paste0(strTag, "$"), label = strColumn)
      expect_identical(strPageText(Html(output[[paste0("gsm_bio_tag_results_", strColumn)]])), strTag, label = strColumn)
    }
    expect_match(Html(output$gsm_bio_tag_results_USUBJID), "gsm-bio-app-tag-need", fixed = TRUE)
    expect_match(Html(output$gsm_bio_tag_results_STRESN), "gsm-bio-app-tag-same", fixed = TRUE)
    expect_identical(strPageText(Html(output$gsm_bio_tag_participants_USUBJID)), "same name")
    # The file R could not read: R's sentence in the card it was chosen in,
    # with what to do about it and the way to take it away.
    strBad <- Html(output$gsm_bio_columns_outcomes)
    expect_match(strBad, sprintf("<p class=\"gsm-bio-app-problem\">%s</p>", strNotRead), fixed = TRUE)
    expect_match(strBad, "<button [^>]*id=\"gsm_bio_remove_outcomes\"[^>]*>")
    expect_match(strPageText(strBad), "Choose another file, or remove this one: the charts can be drawn without an outcomes table.", fixed = TRUE)
    expect_false(grepl("gsm_bio_column_outcomes_", strBad, fixed = TRUE))
    # The rail counts all of it, and the charts are still on the synthetic study.
    expect_match(Rail(), "Next: Choose files lb.csv, dm.csv, notes.xlsx, 1 not read notes.xlsx was not read", fixed = TRUE)
    expect_match(Rail(), "Next: Say which column is which 2 of 6 columns still to say In lb.csv: participant, biomarker.", fixed = TRUE)
    expect_match(Rail(), "Waiting: Draw the charts waiting on step 1", fixed = TRUE)
    expect_match(Rail(), "Drawn on, still Results 11,472 rows, 6 columns", fixed = TRUE)
    expect_identical(strPageText(Html(output$gsm_bio_data_files)), "These files: lb.csv (results), dm.csv (participants), notes.xlsx (outcomes, not read).")

    # The button pressed with two columns unsaid: R's sentence, as it was.
    session$setInputs(gsm_bio_apply = 1)
    expect_identical(Html(output$gsm_bio_data_said), sprintf("<p class=\"gsm-bio-app-problem\">%s</p>", strUnsaid))
    expect_identical(output$gsm_bio_source, "Drawn on the synthetic study that ships with gsm.bio.")

    # One column said: its tag, and the rail's count, follow.
    session$setInputs(gsm_bio_column_results_USUBJID = "SUBJID")
    expect_identical(Html(output$gsm_bio_tag_results_USUBJID), "<span class=\"gsm-bio-app-tag gsm-bio-app-tag-said\">said</span>")
    expect_match(Rail(), "1 of 6 columns still to say In lb.csv: biomarker.", fixed = TRUE)
    session$setInputs(gsm_bio_column_results_TEST = "LBTEST")
    expect_match(Rail(), "Done: Say which column is which 6 of 6 columns said", fixed = TRUE)
    # The file R could not read still holds the button, with R's sentence.
    session$setInputs(gsm_bio_apply = 2)
    expect_identical(Html(output$gsm_bio_data_said), sprintf("<p class=\"gsm-bio-app-problem\">%s</p>", strNotRead))
    expect_identical(output$gsm_bio_source, "Drawn on the synthetic study that ships with gsm.bio.")

    # Taken away, it holds nothing: its card is empty, the rail says the
    # button is what is left, and a press of the last sentence is gone.
    session$setInputs(gsm_bio_remove_outcomes = 1)
    expect_identical(strPageText(Html(output$gsm_bio_columns_outcomes)), "")
    expect_identical(strPageText(Html(output$gsm_bio_data_said)), "")
    expect_match(Rail(), "Done: Choose files lb.csv, dm.csv Done: Say which column is which 6 of 6 columns said Next: Draw the charts 5 of 6 charts can be drawn", fixed = TRUE)
    expect_identical(strPageText(Html(output$gsm_bio_data_files)), "These files: lb.csv (results), dm.csv (participants).")

    # Drawn: the page says so beside the button and lists the charts, each
    # with what it draws, and the one that lacks a table with which.
    session$setInputs(gsm_bio_apply = 3)
    expect_identical(output$gsm_bio_source, "Drawn on lb.csv, dm.csv, loaded in this session.")
    strDone <- Html(output$gsm_bio_data_said)
    expect_match(strDone, "<div class=\"gsm-bio-app-done\">", fixed = TRUE)
    expect_match(strPageText(strDone), "^The charts are drawn on lb.csv, dm.csv, loaded in this session\\. 5 of the 6 charts are ready\\. Open one here, or from the row of pills at the top of the page\\.")
    expect_false(grepl("from the list", strDone, fixed = TRUE))
    chrItems <- regmatches(gsub("\n\\s*", "", strDone), gregexpr("<li[^>]*>.*?</li>", gsub("\n\\s*", "", strDone), perl = TRUE))[[1]]
    expect_identical(
      vapply(chrItems, strPageText, character(1), USE.NAMES = FALSE),
      c(
        paste(chrAppCharts[1:5], chrAppWhat[names(chrAppCharts)[1:5]]),
        paste("Stratified survival", "The stratified survival chart reads an outcomes table, with a time and a censor flag for each participant, and this app has none.")
      )
    )
    # Each is a link the page opens its chart with, not an input.
    expect_identical(
      regmatches(strDone, gregexpr("data-gsm-bio-open=\"[^\"]*\"", strDone))[[1]],
      sprintf("data-gsm-bio-open=\"%s\"", names(chrAppCharts))
    )
    expect_false(grepl("<(input|select|button|textarea)|action-button", strDone))
    expect_match(Rail(), "Done: Choose files lb.csv, dm.csv Done: Say which column is which 6 of 6 columns said Done: Draw the charts 5 of 6 charts ready", fixed = TRUE)
    expect_match(Rail(), "Drawn on Results lb.csv, 11,472 rows Participants dm.csv, 200 rows Outcomes none", fixed = TRUE)
  })
})

test_that("in a session a file can be taken away: a second study's results are drawn alone once the first study's other files are removed, and until then the page names every file the button would draw (#85)", {
  skip_if_not_installed("shiny")
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  # The release review's two studies: A, the synthetic study in three files;
  # B, another study's results alone, with participants A does not have.
  dfB <- Synthetic_Results[Synthetic_Results$TEST == "CRP", ]
  dfB$USUBJID <- paste0("B-", dfB$USUBJID)
  lA <- list(
    results = strWritten(Synthetic_Results, ".csv", "A_results"), participants = strWritten(Synthetic_Participants, ".csv", "A_participants"),
    outcomes = strWritten(Synthetic_Outcomes, ".csv", "A_outcomes")
  )
  strB <- strWritten(dfB, ".csv", "B_results")
  strBroken <- file.path(dirname(strB), "notes.xlsx")
  writeLines("not a table", strBroken)
  Payload <- function(strJson) jsonlite::fromJSON(strJson, simplifyVector = FALSE)$x
  # An output with nothing in it has no markup: its words are none.
  Html <- function(xOutput) paste(as.character(xOutput$html), collapse = "")
  shiny::testServer(RunApp(), {
    Rail <- function() strPageText(Html(output$gsm_bio_rail))
    Will <- function() strPageText(Html(output$gsm_bio_data_files))
    session$setInputs(gsm_bio_file_results = dfChosen(lA$results), gsm_bio_file_participants = dfChosen(lA$participants), gsm_bio_file_outcomes = dfChosen(lA$outcomes))
    session$setInputs(
      gsm_bio_column_results_USUBJID = "USUBJID", gsm_bio_column_results_TEST = "TEST", gsm_bio_column_results_STRESN = "STRESN",
      gsm_bio_column_results_VISIT = "VISIT", gsm_bio_column_results_VISITNUM = "VISITNUM", gsm_bio_column_participants_USUBJID = "USUBJID",
      gsm_bio_column_outcomes_USUBJID = "USUBJID", gsm_bio_column_outcomes_PARAMCD = "PARAMCD", gsm_bio_column_outcomes_PARAM = "PARAM",
      gsm_bio_column_outcomes_AVAL = "AVAL", gsm_bio_column_outcomes_CNSR = "CNSR"
    )
    session$setInputs(gsm_bio_apply = 1)
    expect_identical(output$gsm_bio_source, "Drawn on A_results.csv, A_participants.csv, A_outcomes.csv, loaded in this session.")
    expect_identical(Will(), "These files: A_results.csv (results), A_participants.csv (participants), A_outcomes.csv (outcomes).")

    # Study B's results chosen, and nothing else touched: before the button
    # is pressed the page names the three files it would draw, in the place
    # beside the button, in the rail and in each file's own card.
    session$setInputs(gsm_bio_file_results = dfChosen(strB))
    expect_identical(Will(), "These files: B_results.csv (results), A_participants.csv (participants), A_outcomes.csv (outcomes).")
    expect_match(Rail(), "Choose files B_results.csv, A_participants.csv, A_outcomes.csv", fixed = TRUE)
    expect_match(Rail(), "Drawn on, still Results A_results.csv,", fixed = TRUE)
    expect_match(Html(output$gsm_bio_columns_participants), "<span class=\"gsm-bio-app-chosen-name\">A_participants.csv</span>", fixed = TRUE)
    expect_match(Html(output$gsm_bio_columns_outcomes), "<span class=\"gsm-bio-app-chosen-name\">A_outcomes.csv</span>", fixed = TRUE)
    # The confirmation of the last press is of the files drawn then: it is gone.
    expect_identical(strPageText(Html(output$gsm_bio_data_said)), "")

    # A's participants and outcomes taken away: B's results are what is left,
    # and the button draws them alone.
    session$setInputs(gsm_bio_remove_participants = 1)
    session$setInputs(gsm_bio_remove_outcomes = 1)
    expect_identical(Will(), "These files: B_results.csv (results).")
    expect_identical(strPageText(Html(output$gsm_bio_columns_participants)), "")
    expect_identical(strPageText(Html(output$gsm_bio_columns_outcomes)), "")
    session$setInputs(gsm_bio_apply = 2)
    expect_identical(output$gsm_bio_source, "Drawn on B_results.csv, loaded in this session.")
    lDrawn <- Payload(output$GroupComparison)
    expect_length(lDrawn$dfResults$USUBJID, nrow(dfB))
    expect_true(all(startsWith(unlist(lDrawn$dfResults$USUBJID), "B-")))
    expect_null(lDrawn$dfParticipants)
    expect_null(Payload(output$BiomarkerScreen)$dfOutcomes)
    expect_match(Html(output$gsm_bio_place_StratifiedSurvival), "The stratified survival chart reads an outcomes table", fixed = TRUE)
    expect_match(Rail(), "Drawn on Results B_results.csv, 956 rows Participants none Outcomes none", fixed = TRUE)

    # An optional file R could not read holds the button until it is taken
    # away, and then holds nothing.
    session$setInputs(gsm_bio_file_outcomes = dfChosen(strBroken))
    session$setInputs(gsm_bio_apply = 3)
    expect_match(Html(output$gsm_bio_data_said), strNotRead, fixed = TRUE)
    session$setInputs(gsm_bio_remove_outcomes = 2)
    session$setInputs(gsm_bio_apply = 4)
    expect_match(Html(output$gsm_bio_data_said), "The charts are drawn on B_results.csv, loaded in this session.", fixed = TRUE)

    # The results file taken away: the others are no study without one, and
    # the charts stay where they are.
    session$setInputs(gsm_bio_file_participants = dfChosen(lA$participants))
    session$setInputs(gsm_bio_remove_results = 1)
    expect_identical(Will(), "These files: A_participants.csv (participants). There is no results file among them.")
    expect_match(Rail(), "Next: Choose files A_participants.csv Choose a results file: the charts are drawn from the results table.", fixed = TRUE)
    session$setInputs(gsm_bio_apply = 5)
    expect_match(Html(output$gsm_bio_data_said), "Choose a results file first", fixed = TRUE)
    expect_identical(output$gsm_bio_source, "Drawn on B_results.csv, loaded in this session.")
  })
})

# The Data page walked through the design's states in a browser, with what is
# measured in each: how wide the page is against its window, and where the
# parts are. A width is measured against the room the page has, which is the
# window less a scroll bar where the browser draws one beside the page.
lWalkDataPage <- function(lPage, lFiles) {
  lSeen <- list()
  strLook <- "(() => {
    const box = (selector) => { const node = document.querySelector(selector); if (!node) return null; const at = node.getBoundingClientRect(); return { left: at.left, right: at.right, top: at.top, bottom: at.bottom, width: at.width }; };
    const words = (selector) => { const node = document.querySelector(selector); return node ? node.textContent.replace(/\\s+/g, ' ').trim() : null; };
    return {
      window: window.innerWidth, room: document.documentElement.clientWidth, wide: Math.max(document.documentElement.scrollWidth, document.body.scrollWidth),
      rail: box('.gsm-bio-app-rail'), cards: box('.gsm-bio-app-cards'), viewer: box('.gsm-bio-app-viewer'), button: box('#gsm_bio_apply'), said: box('#gsm_bio_data_said > *'),
      railSays: words('#gsm_bio_rail'), saidSays: words('#gsm_bio_data_said'), willSays: words('#gsm_bio_data_files'),
      steps: Array.from(document.querySelectorAll('#gsm_bio_rail .gsm-bio-app-step')).map((step) => step.dataset.state),
      asks: Array.from(document.querySelectorAll('#gsm_bio_columns_results .gsm-bio-app-ask')).map((ask) => { const select = ask.querySelector('select'); const style = getComputedStyle(select);
        return { column: ask.dataset.column, value: select.value, unsaid: select.matches(':invalid'), edge: style.borderTopColor, ground: style.backgroundColor, tag: ask.querySelector('.gsm-bio-app-tag').textContent, need: ask.querySelector('.gsm-bio-app-tag').classList.contains('gsm-bio-app-tag-need') }; }),
      outcomes: { card: box('#gsm_bio_card_outcomes'), problem: box('#gsm_bio_card_outcomes .gsm-bio-app-problem'), says: words('#gsm_bio_card_outcomes .gsm-bio-app-problem'), remove: Boolean(document.querySelector('#gsm_bio_remove_outcomes')),
                  file: document.querySelector('#gsm_bio_file_outcomes').value, named: document.querySelector('#gsm_bio_card_outcomes input[type=\"text\"]').value }
    };
  })()"
  Wait <- function(strCondition, strLabel) expect_true(bWaitFor(lPage, strCondition), label = strLabel)
  Say <- function(strColumn, strValue) {
    lPage$Evaluate(sprintf(
      "(() => { const node = document.querySelector('#gsm_bio_column_results_%s'); node.value = '%s'; node.dispatchEvent(new Event('change', { bubbles: true })); return node.value; })()",
      strColumn, strValue
    ))
  }
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"Data\"]').click()")
  Wait("document.querySelector('#gsm_bio_view table') && document.querySelector('#gsm_bio_rail .gsm-bio-app-step')", "the Data page is drawn")
  lSeen$opens <- lPage$Evaluate(strLook)

  # A results file with two columns to say, a participants file, and a file
  # R cannot read; then the button.
  lPage$Upload("#gsm_bio_file_results", lFiles$results)
  Wait("document.querySelector('#gsm_bio_column_results_VISITNUM')", "the results file's columns are asked for")
  lPage$Upload("#gsm_bio_file_participants", lFiles$participants)
  Wait("document.querySelector('#gsm_bio_column_participants_USUBJID')", "the participants file's column is asked for")
  lPage$Upload("#gsm_bio_file_outcomes", lFiles$outcomes)
  Wait("document.querySelector('#gsm_bio_card_outcomes .gsm-bio-app-problem')", "the unreadable file is reported")
  Wait("(document.querySelector('#gsm_bio_rail') || {}).textContent.includes('2 of 6 columns still to say')", "the rail counts the columns")
  lSeen$chosen <- lPage$Evaluate(strLook)
  lPage$Evaluate("document.querySelector('#gsm_bio_apply').click()")
  Wait("(document.querySelector('#gsm_bio_data_said') || {}).textContent.includes('Say which column of lb.csv')", "R's sentence is shown")
  lSeen$pressed <- lPage$Evaluate(strLook)

  # The unreadable file taken away with its Remove control.
  lPage$Evaluate("document.querySelector('#gsm_bio_remove_outcomes').click()")
  Wait("!document.querySelector('#gsm_bio_card_outcomes .gsm-bio-app-problem') && !document.querySelector('#gsm_bio_remove_outcomes')", "the unreadable file is gone from its card")
  lSeen$removed <- lPage$Evaluate(strLook)

  # The two columns said, and the button.
  Say("USUBJID", "SUBJID")
  Say("TEST", "LBTEST")
  Wait("(document.querySelector('#gsm_bio_rail') || {}).textContent.includes('6 of 6 columns said')", "the rail counts every column said")
  lSeen$said <- lPage$Evaluate(strLook)
  lPage$Evaluate("document.querySelector('#gsm_bio_apply').click()")
  Wait("document.querySelector('#gsm_bio_data_said .gsm-bio-app-done')", "the page says the charts are drawn")
  Wait("document.querySelector('#gsm_bio_source').textContent === 'Drawn on lb.csv, dm.csv, loaded in this session.'", "the chip follows")
  Wait("(document.querySelector('#gsm_bio_rail') || {}).textContent.includes('5 of 6 charts ready')", "the rail counts the charts")
  lSeen$applied <- lPage$Evaluate(strLook)
  lSeen
}

test_that("in a browser the Data page is a rail beside its cards: unsaid columns are amber and tagged, R's sentence is beside the button, an unreadable file is reported in its card and taken away there, and the charts that are ready open from the list (#85)", {
  NeedApp()
  lFiles <- lLayoutFiles()
  lApp <- lRunApp("RunApp()")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, nWidth = 1280L, nHeight = 900L, strAddress = lApp$address, chrBlocked = c("*fonts.googleapis.com*", "*fonts.gstatic.com*"))
  on.exit(lPage$Close(), add = TRUE)
  lSeen <- lWalkDataPage(lPage, lFiles)
  for (strState in names(lSeen)) {
    expect_lte(lSeen[[strState]]$wide, lSeen[[strState]]$room, label = paste(strState, "reaches no wider than the page's room"))
    expect_lte(lSeen[[strState]]$cards$right, lSeen[[strState]]$room, label = paste(strState, "has the cards inside the page's room"))
    # The rail is beside the cards, and both are inside the page.
    expect_lte(lSeen[[strState]]$rail$right, lSeen[[strState]]$cards$left, label = paste(strState, "has the rail beside the cards"))
    expect_lt(abs(lSeen[[strState]]$rail$top - lSeen[[strState]]$cards$top), 2, label = paste(strState, "has the rail level with the cards"))
    expect_gte(lSeen[[strState]]$cards$width, 900, label = paste(strState, "leaves the cards the width"))
  }
  # As it opens.
  expect_identical(unlist(lSeen$opens$steps), c("done", "done", "next"))
  expect_match(lSeen$opens$railSays, "6 of 6 charts ready", fixed = TRUE)
  expect_identical(lSeen$opens$willSays, "No file is chosen: the button has nothing to draw.")
  # Two columns to say: their selects are amber and tagged, the others are not.
  lAsks <- lSeen$chosen$asks
  expect_identical(vapply(lAsks, function(lAsk) lAsk$column, character(1)), names(App_Tables()$results$columns))
  bUnsaid <- vapply(lAsks, function(lAsk) lAsk$unsaid, logical(1))
  expect_identical(bUnsaid, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(vapply(lAsks, function(lAsk) lAsk$tag, character(1)), c("say which", "say which", "same name", "same name", "same name"))
  expect_identical(vapply(lAsks, function(lAsk) lAsk$need, logical(1)), bUnsaid)
  expect_identical(unique(vapply(lAsks[bUnsaid], function(lAsk) lAsk$edge, character(1))), "rgb(224, 164, 103)")
  expect_identical(unique(vapply(lAsks[bUnsaid], function(lAsk) lAsk$ground, character(1))), "rgb(255, 247, 237)")
  expect_false("rgb(224, 164, 103)" %in% vapply(lAsks[!bUnsaid], function(lAsk) lAsk$edge, character(1)))
  expect_identical(unlist(lSeen$chosen$steps), c("next", "next", "waiting"))
  expect_match(lSeen$chosen$railSays, "lb.csv, dm.csv, notes.xlsx, 1 not read", fixed = TRUE)
  expect_identical(lSeen$chosen$willSays, "These files: lb.csv (results), dm.csv (participants), notes.xlsx (outcomes, not read).")
  # The file R could not read: R's sentence, inside the card it was chosen in.
  expect_identical(lSeen$chosen$outcomes$says, strNotRead)
  expect_gte(lSeen$chosen$outcomes$problem$top, lSeen$chosen$outcomes$card$top)
  expect_lte(lSeen$chosen$outcomes$problem$bottom, lSeen$chosen$outcomes$card$bottom)
  expect_true(lSeen$chosen$outcomes$remove)
  expect_match(lSeen$chosen$outcomes$named, "notes.xlsx", fixed = TRUE)
  # The button pressed: R's sentence, as it was, beside the button.
  expect_identical(lSeen$pressed$saidSays, strUnsaid)
  expect_gte(lSeen$pressed$said$left, lSeen$pressed$button$right)
  expect_lt(lSeen$pressed$said$top, lSeen$pressed$button$bottom)
  expect_gt(lSeen$pressed$said$bottom, lSeen$pressed$button$top)
  # Taken away: the card is as it opened, the file control is empty, and the
  # sentence of the last press, which was of other files, is gone.
  expect_null(lSeen$removed$outcomes$says)
  expect_identical(lSeen$removed$outcomes$file, "")
  expect_identical(lSeen$removed$outcomes$named, "")
  expect_identical(lSeen$removed$willSays, "These files: lb.csv (results), dm.csv (participants).")
  expect_identical(unlist(lSeen$removed$steps), c("done", "next", "waiting"))
  # Said: nothing amber, every tag said or same name.
  expect_false(any(vapply(lSeen$said$asks, function(lAsk) lAsk$unsaid, logical(1))))
  expect_identical(vapply(lSeen$said$asks, function(lAsk) lAsk$tag, character(1)), c("said", "said", "same name", "same name", "same name"))
  expect_identical(unlist(lSeen$said$steps), c("done", "done", "next"))
  # Drawn: the page says so beside the button and lists the charts.
  expect_identical(unlist(lSeen$applied$steps), c("done", "done", "done"))
  expect_match(lSeen$applied$saidSays, "^The charts are drawn on lb.csv, dm.csv, loaded in this session\\. 5 of the 6 charts are ready\\.")
  expect_gte(lSeen$applied$said$left, lSeen$applied$button$right)
  lReady <- lPage$Evaluate("Array.from(document.querySelectorAll('#gsm_bio_data_said li')).map((item) => ({ chart: item.querySelector('a').dataset.gsmBioOpen, says: item.textContent.replace(/\\s+/g, ' ').trim(), lacks: item.classList.contains('gsm-bio-app-ready-lacks') }))")
  expect_identical(vapply(lReady, function(lOne) lOne$chart, character(1)), names(chrAppCharts))
  expect_identical(vapply(lReady, function(lOne) lOne$lacks, logical(1)), c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(lReady[[2]]$says, paste(chrAppCharts[["AssociationScatter"]], chrAppWhat[["AssociationScatter"]]))
  expect_match(lReady[[6]]$says, "reads an outcomes table", fixed = TRUE)

  # A chart of the list opens from it, and so does the rail's. A reader goes
  # from page to page no faster than the page tells the session which of its
  # parts are in view, so each step waits for that: Shiny 1.14 loses track of
  # a part shown, hidden and shown again inside one frame of the browser.
  Open <- function(strClick, strPage) {
    lPage$Evaluate(sprintf("document.querySelector('%s').click()", strClick))
    expect_true(bWaitFor(lPage, sprintf(
      "document.querySelector('.gsm-bio-app-main > .tab-content > .tab-pane.active').dataset.value === '%s' && Shiny.shinyapp.$inputValues.gsm_bio_chart === '%s' && Shiny.shinyapp.$inputValues['.clientdata_output_gsm_bio_rail_hidden'] === %s",
      strPage, strPage, if (identical(strPage, "Data")) "false" else "true"
    )), label = paste(strClick, "opens", strPage))
  }
  Open("#gsm_bio_data_said a[data-gsm-bio-open=\"AssociationScatter\"]", "AssociationScatter")
  expect_identical(lPage$Evaluate("window.scrollY"), 0L)
  Open("#gsm_bio_chart a[data-value=\"Data\"]", "Data")
  Open("#gsm_bio_rail a[data-gsm-bio-open]", "GroupComparison")
  Open("#gsm_bio_chart a[data-value=\"Data\"]", "Data")

  # The inputs the page has sent the session, beside the one the charts ask R
  # through (#71): the ones the page is written with, and the ones the
  # session writes for a chosen file, which are a select for each column and
  # the control that takes the file away. No chart control is among them.
  expect_setequal(
    setdiff(sub(":.*$", "", unlist(lPage$Evaluate("Object.keys(Shiny.shinyapp.$inputValues).filter((name) => !name.startsWith('.clientdata'))"))), strServeInput),
    c(
      "gsm_bio_chart", "gsm_bio_view_table", "gsm_bio_view_previous", "gsm_bio_view_next",
      "gsm_bio_file_results", "gsm_bio_file_participants", "gsm_bio_file_outcomes", "gsm_bio_apply",
      paste0("gsm_bio_column_results_", names(App_Tables()$results$columns)), "gsm_bio_column_participants_USUBJID",
      "gsm_bio_remove_results", "gsm_bio_remove_participants", "gsm_bio_remove_outcomes"
    )
  )

  # The same file chosen again after it was taken away is read again, and is
  # taken away again.
  lPage$Upload("#gsm_bio_file_outcomes", lFiles$outcomes)
  expect_true(bWaitFor(lPage, "document.querySelector('#gsm_bio_card_outcomes .gsm-bio-app-problem')"), label = "the file taken away can be chosen again")
  lPage$Evaluate("document.querySelector('#gsm_bio_remove_outcomes').click()")
  expect_true(bWaitFor(lPage, "!document.querySelector('#gsm_bio_card_outcomes .gsm-bio-app-problem')"), label = "and taken away again")
  expect_identical(lPage$Errors(), character(0))
})

test_that("on a 390-pixel phone the Data page is one column, the rail first, and does not scroll sideways as it opens, with a file chosen and two columns unsaid, with the button pressed, with an unreadable file, or once the charts are drawn (#85)", {
  NeedApp()
  lFiles <- lLayoutFiles()
  lApp <- lRunApp("RunApp()")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, nWidth = 390L, nHeight = 844L, strAddress = lApp$address, bPhone = TRUE, chrBlocked = c("*fonts.googleapis.com*", "*fonts.gstatic.com*"))
  on.exit(lPage$Close(), add = TRUE)
  lSeen <- lWalkDataPage(lPage, lFiles)
  expect_named(lSeen, c("opens", "chosen", "pressed", "removed", "said", "applied"))
  for (strState in names(lSeen)) {
    lState <- lSeen[[strState]]
    expect_identical(lState$window, 390L, label = strState)
    expect_lte(lState$room, 390, label = strState)
    expect_lte(lState$wide, lState$room, label = paste(strState, "does not scroll sideways"))
    # One column: the rail over the cards, each inside the window.
    expect_lte(lState$rail$bottom, lState$cards$top, label = paste(strState, "has the rail over the cards"))
    expect_gte(lState$rail$left, 0, label = strState)
    expect_lte(lState$rail$right, lState$room, label = strState)
    expect_lte(lState$cards$right, lState$room, label = strState)
    expect_lte(lState$button$right, lState$room, label = strState)
  }
  # The states are the ones meant.
  expect_identical(vapply(lSeen$chosen$asks, function(lAsk) lAsk$unsaid, logical(1)), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(lSeen$pressed$saidSays, strUnsaid)
  expect_lte(lSeen$pressed$said$right, lSeen$pressed$room)
  expect_identical(lSeen$chosen$outcomes$says, strNotRead)
  expect_lte(lSeen$chosen$outcomes$problem$right, lSeen$chosen$room)
  expect_match(lSeen$applied$saidSays, "5 of the 6 charts are ready", fixed = TRUE)
  expect_lte(lSeen$applied$said$right, lSeen$applied$room)
  expect_identical(lPage$Errors(), character(0))
})

# ---- A file input the page set itself (#97) ----------------------------------

# The one sentence a file is refused with when it is not an upload.
strNotUploaded <- "Nothing was read: a card takes one file, uploaded with its own control, and the page sent something else."

# A file of the server's own, holding a line that is nowhere else, in a folder
# of the temporary folder that is no upload's.
strKnownLine <- "KNOWN_LINE_97"
strServerFile <- function() {
  strDir <- tempfile("gsm-bio-server")
  dir.create(strDir)
  strFile <- file.path(strDir, "server-only.csv")
  writeLines(c(paste0(strKnownLine, ",only_on_the_server"), "row two,of a file", "row three,never uploaded"), strFile)
  strFile
}

# A file that is not in the temporary folder at all: one the package ships.
strShippedFile <- function() {
  system.file("extdata", "synthetic_participants.csv", package = "gsm.bio", mustWork = TRUE)
}

# What App_Uploaded() says of a file input's value: the sentence it stops
# with, or NA when it takes the value as an upload.
strUploadRefused <- function(xChosen) {
  tryCatch(
    {
      App_Uploaded(xChosen)
      NA_character_
    },
    error = function(cndError) conditionMessage(cndError)
  )
}

test_that("only a file directly inside a folder Shiny made for an upload, under this R process's temporary folder, is taken as an upload: a path beside it, above it, under it, through it, linked from it or on the web is refused with one sentence (#97)", {
  dfReal <- dfUploaded(strShippedFile(), "participants.csv")
  # An upload is taken, as the data frame Shiny sets and as a list of the same.
  strTaken <- normalizePath(dfReal$datapath, winslash = "/")
  expect_identical(App_Uploaded(dfReal), strTaken)
  expect_identical(App_Uploaded(as.list(dfReal)), strTaken)
  expect_match(basename(dirname(strTaken)), "^[0-9a-f]{24}$")
  expect_identical(dirname(dirname(strTaken)), normalizePath(tempdir(), winslash = "/"))
  # However the same path is written, it is the same file.
  expect_identical(App_Uploaded(list(datapath = file.path(dirname(dfReal$datapath), ".", basename(dfReal$datapath)))), strTaken)

  strOther <- strServerFile()
  strLoose <- tempfile(fileext = ".csv")
  writeLines("A,B", strLoose)
  strDeep <- file.path(strUploadFolder(), "deeper", "0.csv")
  dir.create(dirname(strDeep))
  writeLines("A,B", strDeep)
  Named <- function(strFolder) {
    strFile <- file.path(tempdir(), strFolder, "0.csv")
    unlink(dirname(strFile), recursive = TRUE)
    dir.create(dirname(strFile))
    writeLines("A,B", strFile)
    strFile
  }
  strHex <- paste(rep("0123456789abcdef", 2), collapse = "")
  lRefused <- list(
    `a file outside the temporary folder` = strShippedFile(),
    `a web address` = "http://127.0.0.1:9/server-only.csv",
    `a secure web address` = "https://example.org/server-only.csv",
    `a file's address` = paste0("file://", strOther),
    `a file that is not there, in an upload's folder` = file.path(strUploadFolder(), "0.csv"),
    `a file that is not there` = tempfile(fileext = ".csv"),
    `a file in another folder of the temporary folder` = strOther,
    `a file loose in the temporary folder` = strLoose,
    `a file a folder deeper than an upload` = strDeep,
    `an upload's folder` = dirname(dfReal$datapath),
    `the temporary folder` = tempdir(),
    `a path through an upload's folder and out again` = file.path(dirname(dfReal$datapath), "..", basename(dirname(strOther)), basename(strOther)),
    `a folder of 23 digits` = Named(substr(strHex, 1, 23)),
    `a folder of 25 digits` = Named(substr(strHex, 1, 25)),
    `a folder of 24 letters that are not digits` = Named(strrep("g", 24)),
    `a folder of 24 digits with a word before them` = Named(paste0("file", substr(strHex, 1, 24)))
  )
  # A link in an upload's folder to a file elsewhere, and a link named as an
  # upload's folder is to a folder elsewhere: where links can be made, which
  # is everywhere but Windows.
  strLink <- file.path(strUploadFolder(), "0.csv")
  strLinkedFolder <- file.path(tempdir(), strrep("ab12", 6))
  unlink(strLinkedFolder)
  bLinked <- isTRUE(suppressWarnings(file.symlink(strOther, strLink))) && isTRUE(suppressWarnings(file.symlink(dirname(strOther), strLinkedFolder)))
  if (!identical(.Platform$OS.type, "windows")) {
    expect_true(bLinked, label = "a link can be made here")
  }
  if (bLinked) {
    expect_true(file.exists(strLink))
    expect_true(file.exists(file.path(strLinkedFolder, basename(strOther))))
    lRefused <- c(lRefused, list(
      `a link in an upload's folder to a file elsewhere` = strLink,
      `a file in a link named as an upload's folder is` = file.path(strLinkedFolder, basename(strOther))
    ))
  }
  for (strWhat in names(lRefused)) {
    expect_identical(strUploadRefused(list(name = "mine.csv", datapath = lRefused[[strWhat]])), strNotUploaded, label = strWhat)
  }
  # A value that is no file at all, or more than one.
  dfSecond <- dfUploaded(strShippedFile(), "second.csv")
  lShapes <- list(
    nothing = NULL, `a number` = 7, `a text` = dfReal$datapath, `a missing value` = NA, `an empty list` = list(),
    `a name and no path` = list(name = "mine.csv"), `a missing path` = list(datapath = NA_character_),
    `an empty path` = list(datapath = ""), `a number for a path` = list(datapath = 3),
    `a list for a path` = list(datapath = list(dfReal$datapath)), `no path at all` = list(datapath = character(0)),
    `two paths` = list(name = c("mine.csv", "second.csv"), datapath = c(dfReal$datapath, dfSecond$datapath)),
    `two uploads` = rbind(dfReal, dfSecond), `an upload of no rows` = dfReal[0, ]
  )
  for (strWhat in names(lShapes)) {
    expect_identical(strUploadRefused(lShapes[[strWhat]]), strNotUploaded, label = strWhat)
  }
  # The sentence is one, the same for all, and has nothing of a path in it.
  expect_identical(strAppNotUploaded, strNotUploaded)
  expect_false(grepl("[/\\\\]", strNotUploaded))

  # A file's name is whatever text the page sent, or a few words in its place.
  expect_identical(App_ChosenName(dfReal), "participants.csv")
  expect_identical(App_ChosenName(lShapes$`two uploads`), "participants.csv, second.csv")
  for (xValue in list(NULL, 7, "mine.csv", list(), list(name = 5), list(name = NA_character_), list(name = ""), list(name = list("a.csv")), list(name = character(0)))) {
    expect_identical(App_ChosenName(xValue), "A file with no name")
  }
})

test_that("at the server a file input naming a file outside the upload folder, a web address, a path that does not exist or a file in another folder of the temporary folder is refused with the same sentence and nothing is read, and a real upload is still read (#97)", {
  skip_if_not_installed("shiny")
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  strOther <- strServerFile()
  lForged <- list(
    `a file outside the upload folder` = strShippedFile(),
    `a web address` = "http://127.0.0.1:9/server-only.csv",
    `a file's address` = paste0("file://", strOther),
    `a path that does not exist` = file.path(strUploadFolder(), "0.csv"),
    `a file in another folder of the temporary folder` = strOther
  )
  # Every path the app's reader is handed, as it is handed it.
  chrRead <- character(0)
  ReadFile <- App_ReadFile
  local_mocked_bindings(App_ReadFile = function(strPath, strName) {
    chrRead <<- c(chrRead, strPath)
    ReadFile(strPath, strName)
  })
  Html <- function(xOutput) paste(as.character(xOutput$html), collapse = "")
  shiny::testServer(RunApp(), {
    nPressed <- 0L
    for (strWhat in names(lForged)) {
      session$setInputs(gsm_bio_file_results = list(name = "mine.csv", size = 1, type = "text/csv", datapath = lForged[[strWhat]]))
      # The card: the name the page gave, the sentence, and what to do.
      strCard <- Html(output$gsm_bio_columns_results)
      expect_match(strCard, sprintf("<p class=\"gsm-bio-app-problem\">%s</p>", strNotUploaded), fixed = TRUE, label = strWhat)
      expect_identical(
        strPageText(strCard), paste("mine.csv Remove", strNotUploaded, "Choose another results file, or remove this one."),
        label = strWhat
      )
      # The button says the same and draws nothing.
      nPressed <- nPressed + 1L
      session$setInputs(gsm_bio_apply = nPressed)
      expect_identical(Html(output$gsm_bio_data_said), sprintf("<p class=\"gsm-bio-app-problem\">%s</p>", strNotUploaded), label = strWhat)
      expect_identical(output$gsm_bio_source, "Drawn on the synthetic study that ships with gsm.bio.", label = strWhat)
      # Nothing of the file is anywhere the session writes.
      for (strOutput in c("gsm_bio_columns_results", "gsm_bio_rail", "gsm_bio_data_files", "gsm_bio_data_said", "gsm_bio_view")) {
        expect_false(grepl(strKnownLine, Html(output[[strOutput]]), fixed = TRUE), label = paste(strWhat, "in", strOutput))
        expect_false(grepl(lForged[[strWhat]], Html(output[[strOutput]]), fixed = TRUE), label = paste(strWhat, "is named in", strOutput))
      }
    }
    expect_identical(chrRead, character(0))

    # A file where Shiny keeps an upload is read, as it was.
    dfReal <- dfUploaded(strShippedFile(), "participants.csv")
    session$setInputs(gsm_bio_file_participants = dfReal)
    expect_match(Html(output$gsm_bio_columns_participants), "participants.csv: 200 rows, 6 columns. Which column is which?", fixed = TRUE)
    expect_identical(chrRead, normalizePath(dfReal$datapath, winslash = "/"))
  })
})

test_that("at the server a file input that is no file, a number, a text, a name with no path, two files at once, is refused with the same sentence and does not end the session, and a name the page sent is only ever text (#97)", {
  skip_if_not_installed("shiny")
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  dfReal <- dfUploaded(strShippedFile(), "participants.csv")
  dfSecond <- dfUploaded(strShippedFile(), "second.csv")
  strMarkup <- "<img src=x onerror=\"window.gsmBioName=1\">.csv"
  # What the page sent, and the name its card shows for it.
  lSent <- list(
    list(sent = 7, name = "A file with no name"),
    list(sent = "mine.csv", name = "A file with no name"),
    list(sent = list(), name = "A file with no name"),
    list(sent = list(name = "mine.csv"), name = "mine.csv"),
    list(sent = list(name = 5, datapath = list(a = dfReal$datapath)), name = "A file with no name"),
    list(sent = list(name = NA, datapath = NA_character_), name = "A file with no name"),
    list(sent = list(name = "mine.csv", datapath = 3), name = "mine.csv"),
    list(sent = list(name = c("mine.csv", "second.csv"), datapath = c(dfReal$datapath, dfSecond$datapath)), name = "mine.csv, second.csv"),
    list(sent = rbind(dfReal, dfSecond), name = "participants.csv, second.csv"),
    list(sent = dfReal[0, ], name = "A file with no name"),
    list(sent = list(name = strMarkup, datapath = strShippedFile()), name = strMarkup)
  )
  chrRead <- character(0)
  ReadFile <- App_ReadFile
  local_mocked_bindings(App_ReadFile = function(strPath, strName) {
    chrRead <<- c(chrRead, strPath)
    ReadFile(strPath, strName)
  })
  Html <- function(xOutput) paste(as.character(xOutput$html), collapse = "")
  Text <- function(strName) gsub("\"", "&quot;", gsub(">", "&gt;", gsub("<", "&lt;", strName, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  shiny::testServer(RunApp(), {
    for (iSent in seq_along(lSent)) {
      lOne <- lSent[[iSent]]
      session$setInputs(gsm_bio_file_results = lOne$sent)
      strCard <- Html(output$gsm_bio_columns_results)
      expect_match(strCard, sprintf("<p class=\"gsm-bio-app-problem\">%s</p>", strNotUploaded), fixed = TRUE, label = paste("value", iSent))
      # The name is the card's text and the Remove control's, never its markup.
      expect_match(strCard, sprintf("<span class=\"gsm-bio-app-chosen-name\">%s</span>", gsub("&quot;", "\"", Text(lOne$name), fixed = TRUE)), fixed = TRUE, label = paste("value", iSent))
      expect_match(strCard, sprintf("aria-label=\"Remove %s\"", Text(lOne$name)), fixed = TRUE, label = paste("value", iSent))
      expect_false(grepl("<img", strCard, fixed = TRUE), label = paste("value", iSent))
      # The rail and the button's line name it too, and the session goes on.
      expect_match(strPageText(Html(output$gsm_bio_rail)), "1 not read", fixed = TRUE, label = paste("value", iSent))
      expect_false(grepl("<img", Html(output$gsm_bio_rail), fixed = TRUE))
      expect_false(grepl("<img", Html(output$gsm_bio_data_files), fixed = TRUE))
      session$setInputs(gsm_bio_apply = iSent)
      expect_identical(Html(output$gsm_bio_data_said), sprintf("<p class=\"gsm-bio-app-problem\">%s</p>", strNotUploaded), label = paste("value", iSent))
    }
    expect_identical(chrRead, character(0))
    # The session is still there: a real upload after them all is read.
    session$setInputs(gsm_bio_file_results = dfUploaded(strWritten(Synthetic_Results, ".csv")))
    expect_match(Html(output$gsm_bio_columns_results), "results.csv: 11,472 rows, 6 columns. Which column is which?", fixed = TRUE)
    expect_length(chrRead, 1L)
  })
})

test_that("in a browser a page that sets a file input itself, to a file on the server that holds a known line, is refused: the line is nowhere on the page, the tables and the charts are as they were, a number in a file's place does not end the session, and a file chosen with the control is still read (#97)", {
  NeedApp()
  strSecret <- normalizePath(strServerFile())
  lFiles <- lLayoutFiles()
  # The app serves the same file at an address of its own, so the page can
  # name it as a web address too.
  lApp <- lRunApp(sprintf("{ shiny::addResourcePath('known', %s); RunApp() }", deparse(dirname(strSecret))))
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, nWidth = 1280L, nHeight = 900L, strAddress = lApp$address, chrBlocked = c("*fonts.googleapis.com*", "*fonts.gstatic.com*"))
  on.exit(lPage$Close(), add = TRUE)
  Quote <- function(xValue) as.character(jsonlite::toJSON(xValue, auto_unbox = TRUE))
  Wait <- function(strCondition, strLabel) expect_true(bWaitFor(lPage, strCondition), label = strLabel)
  Words <- function(strSelector) lPage$Evaluate(sprintf("(document.querySelector(%s) || { textContent: '' }).textContent.replace(/\\s+/g, ' ').trim()", Quote(strSelector)))
  # The page sets a file input, as any script in it can.
  Send <- function(strTable, strValue) {
    lPage$Evaluate(sprintf("(() => { Shiny.setInputValue('gsm_bio_file_%s', %s, { priority: 'event' }); return true; })()", strTable, strValue))
  }
  Known <- function() lPage$Evaluate(sprintf("document.documentElement.outerHTML.includes(%s) || document.body.innerText.includes(%s)", Quote(strKnownLine), Quote(strKnownLine)))
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"Data\"]').click()")
  Wait("document.querySelector('#gsm_bio_view table') && document.querySelector('#gsm_bio_rail .gsm-bio-app-step')", "the Data page is drawn")
  # The file is served: the address the page will name has the known line.
  strAddress <- paste0(lApp$address, "known/server-only.csv")
  expect_match(lPage$Evaluate(sprintf("fetch(%s).then((answer) => answer.text())", Quote(strAddress))), strKnownLine, fixed = TRUE)
  lWas <- list(viewer = Words("#gsm_bio_view"), tabs = Words("#gsm_bio_view_tabs"), source = Words("#gsm_bio_source"))
  expect_match(lWas$viewer, "Results, from the synthetic study that ships with gsm.bio: 11,472 rows", fixed = TRUE)

  # The results card is sent the server's file by its path, the participants
  # card the same file by a web address, and the outcomes card the path again
  # under a SAS name.
  Send("results", Quote(list(name = "mine.csv", size = 1, type = "text/csv", datapath = strSecret)))
  Send("participants", Quote(list(name = "mine.csv", datapath = strAddress)))
  Send("outcomes", Quote(list(name = "mine.xpt", datapath = strSecret)))
  for (strTable in c("results", "participants", "outcomes")) {
    Wait(sprintf("document.querySelector('#gsm_bio_columns_%s').textContent.includes('Remove')", strTable), paste("the", strTable, "card answers"))
  }
  strOptional <- "Choose another file, or remove this one: the charts can be drawn without %s table."
  expect_identical(Words("#gsm_bio_columns_results"), paste("mine.csv Remove", strNotUploaded, "Choose another results file, or remove this one."))
  expect_identical(Words("#gsm_bio_columns_participants"), paste("mine.csv Remove", strNotUploaded, sprintf(strOptional, "a participants")))
  expect_identical(Words("#gsm_bio_columns_outcomes"), paste("mine.xpt Remove", strNotUploaded, sprintf(strOptional, "an outcomes")))
  expect_false(Known(), label = "the server's line is on the page")
  # The button draws nothing and says the same.
  lPage$Evaluate("document.querySelector('#gsm_bio_apply').click()")
  Wait("document.querySelector('#gsm_bio_data_said .gsm-bio-app-problem')", "the button answers")
  expect_identical(Words("#gsm_bio_data_said"), strNotUploaded)
  expect_false(Known(), label = "the server's line is on the page after the button")

  # A number where a file would be: refused, and the session is still there.
  Send("results", "7")
  Wait("document.querySelector('#gsm_bio_columns_results').textContent.includes('A file with no name')", "a number is answered")
  expect_identical(Words("#gsm_bio_columns_results"), paste("A file with no name Remove", strNotUploaded, "Choose another results file, or remove this one."))
  expect_true(lPage$Evaluate("Shiny.shinyapp.isConnected()"), label = "the session goes on")

  # The tables are as they were, and so is a chart: it is drawn, on the
  # synthetic study, with nothing of the server's file.
  expect_identical(list(viewer = Words("#gsm_bio_view"), tabs = Words("#gsm_bio_view_tabs"), source = Words("#gsm_bio_source")), lWas)
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"GroupComparison\"]').click()")
  Wait("HTMLWidgets.find('#GroupComparison') && HTMLWidgets.find('#GroupComparison').chart() && document.querySelector('#GroupComparison .gsm-bio-chart').childElementCount > 0", "the group comparison is drawn")
  expect_false(Known(), label = "the server's line is on a chart's page")
  expect_identical(Words("#gsm_bio_source"), "Drawn on the synthetic study that ships with gsm.bio.")

  # A file chosen with the control, which Shiny uploads, is read as it was.
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"Data\"]').click()")
  lPage$Upload("#gsm_bio_file_results", lFiles$results)
  Wait("document.querySelector('#gsm_bio_columns_results').textContent.includes('lb.csv: 11,472 rows, 6 columns')", "a real upload is read")
  expect_true(lPage$Evaluate("Shiny.shinyapp.isConnected()"))
  expect_identical(lPage$Errors(), character(0))
})

# ---- A quote never closed on a file's first lines (#97) ----------------------

# The third review's file, made here byte for byte: a results table of 1,000
# rows under gsm.bio's own column names. `nOpen` is the row whose visit opens a
# quote that is never closed, 0 for the header's; `bEnds` is whether the file
# ends with a line ending.
strOpenQuote <- function(nOpen = NULL, bEnds = TRUE, chrRows = sprintf("s%d,IL-6,%d,Baseline,0", 1:1000, 1:1000)) {
  strHeader <- "USUBJID,TEST,STRESN,VISIT,VISITNUM"
  if (isTRUE(nOpen == 0)) {
    strHeader <- "USUBJID,TEST,STRESN,\"VISIT,VISITNUM"
  } else if (!is.null(nOpen)) {
    chrRows[nOpen] <- sprintf("s%d,IL-6,%d,\"Baseline,0", nOpen, nOpen)
  }
  strDir <- tempfile("gsm-bio-quote")
  dir.create(strDir)
  strFile <- file.path(strDir, "results.csv")
  writeBin(charToRaw(paste0(paste(c(strHeader, chrRows), collapse = "\n"), if (bEnds) "\n")), strFile)
  strFile
}

# The sentence a .csv file with a quote never closed is refused with.
strQuoteOpen <- function(strName) {
  paste0(
    strName, " was not loaded: a quote in it is never closed, so R would read it short. ",
    "The file has an odd number of double-quote characters, and in a .csv file a quote that opens a value closes it, and a quote inside a value is written twice."
  )
}

test_that("a 1,000-row .csv file with a quote left open on its first, second, third or fourth row, or on its header, is refused and not read short, whether or not it ends with a line ending; R alone reads each short with no warning but that its last line is incomplete (#97)", {
  for (bEnds in c(TRUE, FALSE)) {
    for (nOpen in 0:4) {
      strLabel <- paste(if (nOpen == 0) "the header" else paste("row", nOpen), if (bEnds) "with" else "without", "a final line ending")
      strFile <- strOpenQuote(nOpen, bEnds)
      # The file is what the review found: R reads it short, and its one
      # warning is the one a short file with no last line ending gets too.
      lPlain <- lReadPlainly(strFile)
      expect_lt(lPlain$rows, 1000L, label = paste("the rows R reads,", strLabel))
      expect_gt(lPlain$rows, 990L, label = paste("the rows R reads,", strLabel))
      expect_length(lPlain$warned, 1L)
      expect_match(lPlain$warned, "^incomplete final line found by readTableHeader", label = strLabel)
      # The app reads no table from it, and says why.
      strSaid <- strRefused(strFile, "results.csv")
      expect_identical(strSaid, strQuoteOpen("results.csv"), label = strLabel)
      ExpectNoPath(strSaid, strFile, strLabel)
    }
    # A quote left open further down, where R itself warns of it, is refused
    # as it was, in R's words.
    for (nOpen in c(5, 50, 1000)) {
      expect_match(
        strRefused(strOpenQuote(nOpen, bEnds), "results.csv"),
        "^results.csv was not loaded: R warned as it read the file, .* R said: EOF within quoted string\\.",
        label = paste("row", nOpen)
      )
    }
  }
})

test_that("a whole .csv file is still read in full: one with no final line ending, one with Windows line endings, and one whose quoted values hold a quote written twice, a comma and a line break (#97)", {
  # 1,000 rows and no line ending after the last.
  for (bEnds in c(TRUE, FALSE)) {
    dfWhole <- App_ReadFile(strOpenQuote(NULL, bEnds), "results.csv")
    expect_identical(dim(dfWhole), c(1000L, 5L), label = paste("a final line ending:", bEnds))
    expect_identical(dfWhole$USUBJID[c(1, 1000)], c("s1", "s1000"))
    expect_identical(dfWhole$STRESN, 1:1000)
  }
  # Quotes that close: around a value with a comma, around one with a quote
  # inside it written twice, and around one with a line break.
  chrRows <- sprintf("s%d,IL-6,%d,Baseline,0", 1:1000, 1:1000)
  chrRows[1] <- "s1,\"IL-6, the \"\"six\"\"\",1,\"Base\nline\",0"
  chrRows[4] <- "\"s4\",\"IL-6\",4,\"Baseline\",0"
  chrRows[1000] <- "s1000,\"\"\"IL-6\"\"\",1000,Baseline,0"
  for (bEnds in c(TRUE, FALSE)) {
    strFile <- strOpenQuote(NULL, bEnds, chrRows)
    lBytes <- App_CsvBytes(strFile)
    expect_false(lBytes$odd)
    expect_identical(lBytes$ends, bEnds)
    dfQuoted <- App_ReadFile(strFile, "results.csv")
    expect_identical(dim(dfQuoted), c(1000L, 5L), label = paste("a final line ending:", bEnds))
    expect_identical(dfQuoted$TEST[c(1, 4, 1000)], c("IL-6, the \"six\"", "IL-6", "\"IL-6\""))
    expect_identical(dfQuoted$VISIT[1:2], c("Base\nline", "Baseline"))
    expect_identical(dfQuoted$USUBJID[c(1, 4, 1000)], c("s1", "s4", "s1000"))
  }
  # Windows line endings, with and without the last.
  for (bEnds in c(TRUE, FALSE)) {
    strWindows <- tempfile(fileext = ".csv")
    writeBin(charToRaw(paste0(paste(c("USUBJID,STRESN", sprintf("s%d,%d", 1:1000, 1:1000)), collapse = "\r\n"), if (bEnds) "\r\n")), strWindows)
    expect_identical(App_CsvBytes(strWindows), list(odd = FALSE, ends = bEnds))
    expect_identical(nrow(App_ReadFile(strWindows, "windows.csv")), 1000L)
  }
})

test_that("a file's quotes are counted and its last byte read a piece of the file at a time, to the same answer whatever the size of the piece (#97)", {
  strOdd <- strOpenQuote(3)
  strEven <- strOpenQuote(NULL, FALSE, c("a,\"b\"", "\"c\"\"d\",e"))
  strNone <- tempfile(fileext = ".csv")
  file.create(strNone)
  for (nPiece in c(1L, 2L, 7L, 4096L, 1048576L)) {
    expect_identical(App_CsvBytes(strOdd, nPiece), list(odd = TRUE, ends = TRUE), label = paste("pieces of", nPiece))
    expect_identical(App_CsvBytes(strEven, nPiece), list(odd = FALSE, ends = FALSE), label = paste("pieces of", nPiece))
    expect_identical(App_CsvBytes(strNone, nPiece), list(odd = FALSE, ends = FALSE), label = paste("pieces of", nPiece))
  }
  # The count is of the file's bytes: the same as counting its characters.
  for (strFile in c(strOdd, strEven)) {
    nQuotes <- sum(readBin(strFile, "raw", file.size(strFile)) == charToRaw("\""))
    expect_identical(App_CsvBytes(strFile)$odd, nQuotes %% 2 == 1)
  }
  # A file that ends with the line ending of an old Mac ends with one.
  strMac <- tempfile(fileext = ".csv")
  writeBin(charToRaw("A,B\r1,2\r"), strMac)
  expect_true(App_CsvBytes(strMac)$ends)
})

test_that("R's warning that a file's last line is incomplete is read through only when the file does end without a line ending: of a file that ends with one, it is a warning like any other and the file is refused (#97)", {
  # A reader that warns as R does of a short file with no last line ending,
  # whatever the file.
  local_mocked_bindings(App_ReadCsv = function(strPath) {
    warning("incomplete final line found by readTableHeader on '", strPath, "'", call. = FALSE)
    data.frame(USUBJID = c("BIO-001", "BIO-002"), STRESN = c(1.5, 2.5))
  })
  strEnds <- tempfile(fileext = ".csv")
  cat("USUBJID,STRESN\nBIO-001,1.5\nBIO-002,2.5\n", file = strEnds)
  strSaid <- strRefused(strEnds, "ends.csv")
  expect_identical(
    strSaid,
    paste0(
      "ends.csv was not loaded: R warned as it read the file, and a file R warns of may have been read short. ",
      "R said: incomplete final line found by readTableHeader on 'ends.csv'. The app reads a .csv file as comma-separated text in UTF-8."
    )
  )
  ExpectNoPath(strSaid, strEnds, "the sentence")
  # The same warning of a file that does end without one is no reason to refuse.
  strOpen <- tempfile(fileext = ".csv")
  cat("USUBJID,STRESN\nBIO-001,1.5\nBIO-002,2.5", file = strOpen)
  expect_identical(App_ReadFile(strOpen, "open.csv"), data.frame(USUBJID = c("BIO-001", "BIO-002"), STRESN = c(1.5, 2.5)))
})

test_that("in a session a .csv file with a quote left open on its first row is said in the file's place and again on the button, no column of it is asked for, and the charts are left as they were (#97)", {
  skip_if_not_installed("shiny")
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  Payload <- function(strJson) jsonlite::fromJSON(strJson, simplifyVector = FALSE)$x
  Html <- function(xOutput) paste(as.character(xOutput$html), collapse = "")
  dfOpen <- dfUploaded(strOpenQuote(1))
  shiny::testServer(RunApp(), {
    session$setInputs(gsm_bio_file_results = dfOpen)
    strPlace <- Html(output$gsm_bio_columns_results)
    expect_match(strPlace, sprintf("<p class=\"gsm-bio-app-problem\">%s</p>", strQuoteOpen("results.csv")), fixed = TRUE)
    # Nothing of the file is offered: no rows counted, no column to say.
    expect_false(grepl("998 rows", strPlace, fixed = TRUE))
    expect_false(grepl("gsm_bio_column_results_", strPlace, fixed = TRUE))
    ExpectNoPath(strPlace, dfOpen$datapath, "the file's place")
    session$setInputs(
      gsm_bio_column_results_USUBJID = "USUBJID", gsm_bio_column_results_TEST = "TEST", gsm_bio_column_results_STRESN = "STRESN",
      gsm_bio_column_results_VISIT = "VISIT", gsm_bio_column_results_VISITNUM = "VISITNUM", gsm_bio_apply = 1
    )
    expect_identical(Html(output$gsm_bio_data_said), sprintf("<p class=\"gsm-bio-app-problem\">%s</p>", strQuoteOpen("results.csv")))
    expect_identical(output$gsm_bio_source, "Drawn on the synthetic study that ships with gsm.bio.")
    expect_length(Payload(output$GroupComparison)$dfResults$USUBJID, nrow(Synthetic_Results))
  })
})
