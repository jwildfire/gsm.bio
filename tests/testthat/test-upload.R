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

# What a session is told when a reader chooses a file.
dfChosen <- function(strFile) {
  data.frame(name = basename(strFile), size = file.size(strFile), type = "", datapath = strFile, stringsAsFactors = FALSE)
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
      session$setInputs(gsm_bio_file_results = dfChosen(lFiles[[strWhich]]))
      strPlace <- Html(output$gsm_bio_columns_results)
      expect_match(strPlace, "gsm-bio-app-problem", fixed = TRUE, label = strWhich)
      expect_match(strPlace, sprintf("%s.csv was not loaded: R warned as it read the file", strWhich), fixed = TRUE, label = strWhich)
      expect_match(strPlace, if (strWhich == "latin1") "invalid input found on input connection" else "EOF within quoted string", fixed = TRUE)
      # Nothing of the file is offered: no rows counted, no column to say.
      expect_false(grepl("50 rows", strPlace, fixed = TRUE), label = paste(strWhich, "counts the rows read"))
      expect_false(grepl("gsm_bio_column_results_", strPlace, fixed = TRUE), label = paste(strWhich, "asks for columns"))
      ExpectNoPath(strPlace, lFiles[[strWhich]], strWhich)
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
    session$setInputs(gsm_bio_file_results = transform(dfChosen(strBroken), name = "results.xpt"))
    strPlace <- Html(output$gsm_bio_columns_results)
    expect_match(strPlace, "results.xpt could not be read as a .xpt file: Failed to parse results.xpt", fixed = TRUE)
    ExpectNoPath(strPlace, strBroken, "the file's place")
    session$setInputs(gsm_bio_apply = nApplied + 1L)
    ExpectNoPath(Html(output$gsm_bio_data_said), strBroken, "the button")
    expect_length(Payload(output$GroupComparison)$dfResults$USUBJID, nSynthetic)
  })
})
