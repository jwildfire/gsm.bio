# The batch runner (#39): bio.viz's chart specifications (bio.viz#68,
# docs/output.md "Specifications") read as data and run against the study's
# tables, each to a figure and an RTF table in a folder, with a manifest; one
# specification can run across every biomarker. The reader is held to bio.viz's
# own reader, run on the same specifications in node from the copied bundle.

# Every call in an environment's functions that evaluates text or calls what
# it is handed: eval(), parse() and the like by name, and do.call() on anything
# but the functions the package calls it on. By function, the calls deparsed.
lEvaluatingCalls <- function(envFunctions) {
  chrForbidden <- c(
    "eval", "evalq", "eval.parent", "parse", "str2lang", "str2expression", "match.fun", "get", "get0", "mget",
    "getFunction", "source", "sys.source", "system", "system2", "shell", "Recall", "body<-", "environment<-", "assign", "makeActiveBinding"
  )
  chrDoCall <- c("rbind", "ggplot2::aes", "lFunctions[[lRequest$name]]")
  Walk <- function(xCode) {
    if (is.function(xCode)) return(c(Walk(formals(xCode)), Walk(body(xCode))))
    if (is.pairlist(xCode) || is.list(xCode)) return(unlist(lapply(as.list(xCode), function(x) if (missing(x)) NULL else Walk(x))))
    if (!is.call(xCode)) return(character(0))
    xHead <- xCode[[1]]
    strHead <- if (is.name(xHead)) as.character(xHead) else if (is.call(xHead) && as.character(xHead[[1]]) %in% c("::", ":::")) as.character(xHead[[3]]) else ""
    bBad <- strHead %in% chrForbidden || (identical(strHead, "do.call") && !paste(deparse(xCode[[2]]), collapse = "") %in% chrDoCall)
    c(if (bBad) paste(deparse(xCode), collapse = " "), unlist(lapply(as.list(xCode), function(x) if (missing(x)) NULL else Walk(x))))
  }
  lFound <- list()
  for (strName in sort(ls(envFunctions, all.names = TRUE))) {
    xValue <- get(strName, envir = envFunctions)
    if (is.function(xValue) && !is.primitive(xValue)) {
      chrCalls <- Walk(xValue)
      if (length(chrCalls) > 0L) lFound[[strName]] <- chrCalls
    }
  }
  lFound
}

test_that("gsm.bio reads every specification bio.viz's charts wrote, with its filter laid onto the chart's own (#39)", {
  lSpecs <- lSavedSpecs()
  expect_identical(vapply(lSpecs, `[[`, character(1), "chart"), c(
    "group-comparison", "group-comparison", "association-scatter", "correlation-matrix", "biomarker-screen",
    "cross-tab", "stratified-survival"
  ))
  for (lSpec in lSpecs) {
    lRead <- Spec_Read(strSpecText(lSpec))
    expect_identical(lRead$chart, lSpec$chart)
    expect_identical(lRead$version, "0.1.0")
  }
  lFiltered <- Spec_Read(strSpecText(lSpecs[[2]]))
  lSex <- Filter(function(lFilter) identical(lFilter$value_col, "SEX"), lFiltered$settings$filters)
  expect_length(lSex, 1L)
  expect_identical(lSex[[1]]$start, "F")
  expect_identical(lSex[[1]]$label, "Sex")
  # The schema the copied bundle's settings are read by is bio.viz's own.
  expect_true("start_value" %in% Spec_SettingNames()[["group-comparison"]])
  expect_setequal(names(Spec_SettingNames()), c(
    "group-comparison", "association-scatter", "correlation-matrix", "biomarker-screen", "cross-tab", "stratified-survival"
  ))
})

test_that("gsm.bio accepts and refuses each specification as bio.viz's own reader does, and reads the same settings (#39)", {
  lCases <- lSpecCases()
  lTheirs <- lBioVizReads(lCases)
  expect_setequal(names(lTheirs), names(lCases))
  for (strCase in names(lCases)) {
    lMine <- tryCatch(list(accepted = TRUE, read = Spec_ReadChecked(lCases[[strCase]])), error = function(cndError) {
      list(accepted = FALSE, refusal = conditionMessage(cndError))
    })
    expect_identical(lMine$accepted, lTheirs[[strCase]]$accepted, label = paste(strCase, "accepted"))
    if (isTRUE(lMine$accepted) && isTRUE(lTheirs[[strCase]]$accepted)) {
      expect_identical(lMine$read$chart, lTheirs[[strCase]]$chart, label = paste(strCase, "chart"))
      expect_identical(lAsJson(lMine$read$settings), lTheirs[[strCase]]$settings, label = paste(strCase, "settings"))
    }
  }
  # Where the format refuses, gsm.bio says it in bio.viz's words.
  chrSame <- c(
    "another format", "another format version", "a member it does not have", "a version that is not text", "no version",
    "a chart bio.viz does not have", "a setting the chart does not have", "the page's connection", "settings that are not an object",
    "a filters setting that is not a list", "a setting that names __proto__", "a setting named __proto__",
    "filters that are not a list", "a filter that is not an object", "a filter with more than it has", "a filter with no column",
    "a filter on constructor", "a filter on prototype", "a column filtered twice", "an operator it does not have",
    "values that are not a list", "a value that is not text or a number", "a value twice"
  )
  for (strCase in chrSame) {
    strMine <- tryCatch({
      Spec_Read(lCases[[strCase]])
      "accepted"
    }, error = conditionMessage)
    expect_identical(strAscii(paste0("bio.viz: ", strMine)), strAscii(lTheirs[[strCase]]$refusal), label = strCase)
  }
})

test_that("the reads recorded for a session with no node are bio.viz's reader's reads now, run in node (#39)", {
  skip_if(!nzchar(Sys.which("node")), "node is not installed, so bio.viz's reader is not run live: the recorded reads stand in for it")
  expect_identical(lBioVizRecorded(), lBioVizAllLive(), label = "fixtures/specifications/bioviz-reader.json")
  lCases <- lFuzzCases()
  expect_identical(lFuzzTheirs(lCases, bNode = FALSE), lFuzzTheirs(lCases, bNode = TRUE), label = "fixtures/specifications/bioviz-fuzz.json")
})

test_that("over every setting of every chart, given each of 21 values, gsm.bio accepts and refuses as bio.viz's reader does, but for the differences listed (#39)", {
  lCases <- lFuzzCases()
  expect_identical(length(lCases), 4746L)
  lMine <- lFuzzMine(lCases)
  lTheirs <- lFuzzTheirs(lCases)
  bDiffers <- vapply(names(lCases), function(strCase) {
    !identical(lMine[[strCase]]$accepted, lTheirs[[strCase]]$accepted) || !identical(lMine[[strCase]]$same, lTheirs[[strCase]]$same)
  }, logical(1))
  # The differences there are, each on purpose. A chart's `statistic` (and the
  # scatter's `fit_statistic`, and the group comparison's `statistic_by_visit`)
  # names the R function the page asks; bio.viz takes any name, and gsm.bio
  # computes only the one Analyze_*() function the chart's statistics are, so
  # it refuses any other with a sentence.
  chrIntended <- names(lCases)[grepl("^[a-z-]+ [|] (statistic|fit_statistic|statistic_by_visit) [|] \"[^\"]+\"$", names(lCases))]
  expect_length(chrIntended, 32L)
  expect_identical(names(lCases)[bDiffers], chrIntended)
  for (strCase in chrIntended) {
    expect_match(lMine[[strCase]]$refusal, "gsm.bio computes", fixed = TRUE, label = strCase)
  }
  # Where bio.viz refuses a value's shape, gsm.bio says it in bio.viz's words.
  strShape <- "must be a name, or a list of names|is not a column name or|`footnotes` must be text|must be an object of settings|`cuts` must be a list|`at_risk_times` must be a list"
  chrShape <- names(lCases)[vapply(lTheirs, function(lOne) !lOne$accepted && grepl(strShape, lOne$refusal), logical(1))]
  expect_gte(length(chrShape), 150L)
  for (strCase in chrShape) {
    expect_identical(lMine[[strCase]]$refusal, lTheirs[[strCase]]$refusal, label = strCase)
  }
})

test_that("a specification nested deeper than 64 is refused as bio.viz's reader refuses one handed it as an object, given as text or as a list (#39)", {
  lCases <- lSpecDepthCases()
  lTheirs <- lBioVizReads(lCases, bObject = TRUE)
  chrAccepted <- character(0)
  for (strCase in names(lCases)) {
    for (xGiven in list(lCases[[strCase]], jsonlite::parse_json(lCases[[strCase]], simplifyVector = FALSE))) {
      strMine <- tryCatch({
        Spec_ReadChecked(xGiven)
        "accepted"
      }, error = function(cndError) paste0("bio.viz: ", conditionMessage(cndError)))
      strTheirs <- if (lTheirs[[strCase]]$accepted) "accepted" else lTheirs[[strCase]]$refusal
      expect_identical(strAscii(strMine), strAscii(strTheirs), label = strCase)
    }
    if (lTheirs[[strCase]]$accepted) chrAccepted <- c(chrAccepted, strCase)
  }
  # The cases reach the edge: some are accepted, and the rest are refused for
  # their depth alone.
  expect_identical(chrAccepted, paste("as an object: nested", 55:61, "deep in a filter"))
  expect_match(lTheirs[[length(lTheirs)]]$refusal, "nested more than 64 deep", fixed = TRUE)
})

test_that("a specification is data: code-like text is read and drawn as text, and nothing in it is evaluated (#39)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("r2rtf")
  strTitle <- "${1 + 1} <script>alert(1)</script> {{x}} system('touch PWNED') {rows}"
  lSpec <- lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE", title = strTitle))
  expect_identical(Spec_Read(strSpecText(lSpec))$settings$title, strTitle)
  strDir <- tempfile("batch-text")
  dir.create(strDir)
  strWas <- setwd(strDir)
  on.exit(setwd(strWas), add = TRUE)
  dfManifest <- Run_Specifications(strSpecText(list(lSpec)), Synthetic_Results, Synthetic_Participants, strFolder = file.path(strDir, "out"))
  expect_identical(dfManifest$status, "written")
  expect_identical(dfManifest$title, "${1 + 1} <script>alert(1)</script> {{x}} system('touch PWNED') ARM")
  strRtf <- paste(readLines(file.path(strDir, "out", dfManifest$table), warn = FALSE), collapse = "\n")
  expect_true(grepl("system('touch PWNED')", strRtf, fixed = TRUE))
  expect_true(grepl("$\\{1 + 1\\} <script>alert(1)</script> \\{\\{x\\}\\}", strRtf, fixed = TRUE))
  expect_false(file.exists(file.path(strDir, "PWNED")))
  expect_false(file.exists("PWNED"))
  # Nothing in the package evaluates text: every function in the namespace is
  # walked, call by call. do.call() is called only on rbind, on ggplot2::aes
  # with names, and in Chart_Answer() on a statistics function from the fixed
  # list its caller hands it.
  expect_identical(lEvaluatingCalls(asNamespace("gsm.bio")), list())
  envMutant <- new.env()
  envMutant$Batch_Slug <- function(strText) eval(parse(text = strText))
  envMutant$Other <- function(x) do.call(x, list())
  expect_identical(names(lEvaluatingCalls(envMutant)), c("Batch_Slug", "Other"))
})

test_that("a batch run writes a figure and a table for each specification bio.viz wrote, and a manifest, returned and saved (#39)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("r2rtf")
  strFolder <- tempfile("batch")
  dfManifest <- Run_Specifications(
    strSpecFixture("charts.json"), Synthetic_Results, Synthetic_Participants, dfOutcomes = Synthetic_Outcomes,
    strFolder = strFolder, chrFormats = c("png", "pdf")
  )
  expect_s3_class(dfManifest, "data.frame")
  expect_identical(nrow(dfManifest), 7L)
  expect_identical(dfManifest$specification, 1:7)
  expect_identical(dfManifest$status, rep("written", 7L))
  expect_true(all(is.na(dfManifest$biomarker)))
  for (iRow in seq_len(nrow(dfManifest))) {
    chrFigures <- strsplit(dfManifest$figure[iRow], ";", fixed = TRUE)[[1]]
    expect_identical(tools::file_ext(chrFigures), c("png", "pdf"), label = paste(iRow, "figures"))
    expect_true(all(file.exists(file.path(strFolder, chrFigures))), label = paste(iRow, "figures written"))
    expect_true(file.exists(file.path(strFolder, dfManifest$table[iRow])), label = paste(iRow, "table written"))
    expect_identical(readBin(file.path(strFolder, chrFigures[1]), "raw", 8L), as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
    expect_identical(rawToChar(readBin(file.path(strFolder, chrFigures[2]), "raw", 4L)), "%PDF")
    expect_match(readLines(file.path(strFolder, dfManifest$table[iRow]), n = 1L, warn = FALSE), "^\\{\\\\rtf1")
  }
  # Each with its title, and the filter in force applied.
  expect_identical(dfManifest$title[1], "IL-6: Change from baseline by Arm")
  expect_identical(dfManifest$subtitle[1], "200 participants, at Week 4, Week 8")
  expect_identical(dfManifest$subtitle[2], paste(sum(Synthetic_Participants$SEX == "F"), "participants, at Week 4, Week 8"))
  # The manifest saved beside them is the one returned.
  lSaved <- jsonlite::fromJSON(file.path(strFolder, "manifest.json"), simplifyVector = TRUE)
  expect_identical(lSaved$status, dfManifest$status)
  expect_identical(lSaved$figure, dfManifest$figure)
  expect_setequal(list.files(strFolder), c("manifest.json", unlist(strsplit(dfManifest$figure, ";", fixed = TRUE)), dfManifest$table))
})

test_that("one specification across every biomarker writes one figure and one table per biomarker, each with its own statistic (#39)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("r2rtf")
  strFolder <- tempfile("batch-each")
  lSpec <- lSavedSpecs()[[1]]
  dfManifest <- Run_Specifications(strSpecText(list(lSpec)), Synthetic_Results, Synthetic_Participants, strFolder = strFolder, bAcrossBiomarkers = TRUE)
  chrBiomarkers <- c("CRP", "D-dimer", "Ferritin", "IFN-gamma", "IL-1beta", "IL-2", "IL-6", "IL-8", "IL-10", "LDH", "TNF-alpha", "VEGF")
  expect_identical(dfManifest$biomarker, chrBiomarkers)
  expect_identical(dfManifest$status, rep("written", 12L))
  expect_identical(dfManifest$title, paste0(chrBiomarkers, ": Change from baseline by Arm"))
  expect_identical(anyDuplicated(dfManifest$figure), 0L)
  expect_true(all(file.exists(file.path(strFolder, dfManifest$figure))))
  expect_true(all(file.exists(file.path(strFolder, dfManifest$table))))
  # Every statistic is still the Analyze_*() answer: TNF-alpha's Week 4 test.
  dfRows <- data.frame(y = nResultAt("TNF-alpha", "Week 4") - nResultAt("TNF-alpha", "Baseline"), x = Synthetic_Participants$ARM)
  dfRows <- dfRows[!is.na(dfRows$y), ]
  strTheirs <- Output_StatisticText(Analyze_GroupDifference(dfRows, "y", "x", strMethod = "t"))
  expect_match(dfManifest$statistics[dfManifest$biomarker == "TNF-alpha"], strTheirs, fixed = TRUE)
  # Each chart's biomarker setting takes each biomarker in turn.
  lCut <- Spec_Expand(Spec_Read(strSpecText(lSavedSpecs()[[7]])), Synthetic_Results, Synthetic_Participants)
  expect_identical(names(lCut), chrBiomarkers)
  expect_identical(lCut[["IL-6"]]$settings$group_by$measure, "IL-6")
  expect_identical(lCut[["IL-6"]]$settings$group_by$cut, "median")
  lScatter <- Spec_Expand(Spec_Read(strSpecText(lSavedSpecs()[[3]])), Synthetic_Results, Synthetic_Participants)
  expect_identical(names(lScatter), setdiff(chrBiomarkers, "IL-10"))
  expect_identical(lScatter[["CRP"]]$settings$x$measure, "CRP")
  expect_identical(lScatter[["CRP"]]$settings$y$measure, "IL-10")
  # A chart that is of every biomarker already is one output.
  lScreen <- Spec_Expand(Spec_Read(strSpecText(lSavedSpecs()[[5]])), Synthetic_Results, Synthetic_Participants)
  expect_identical(names(lScreen), NA_character_)
})

test_that("a specification gsm.bio cannot read is refused with a sentence, and the rest still run (#39)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("r2rtf")
  strFolder <- tempfile("batch-refused")
  strList <- paste0("[", strSpecText(lSpecOf(lSettings = list(percent = "rows"))), ",", strSpecText(lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE"))), ",", strSpecText(lSpecOf("pie-chart")), "]")
  dfManifest <- Run_Specifications(strList, Synthetic_Results, Synthetic_Participants, strFolder = strFolder)
  expect_identical(dfManifest$status, c("refused", "written", "refused"))
  expect_match(dfManifest$reason[1], "percent")
  expect_match(dfManifest$reason[3], "pie-chart")
  expect_true(is.na(dfManifest$figure[1]))
  expect_true(file.exists(file.path(strFolder, dfManifest$figure[2])))
  # A list that is not one, or a folder that cannot be, is refused before anything runs.
  expect_error(Run_Specifications("{\"format\": ", Synthetic_Results, strFolder = strFolder), "must be JSON")
  expect_error(Run_Specifications("[]", Synthetic_Results, strFolder = strFolder), "no specification")
  expect_error(Run_Specifications(strList, Synthetic_Results, strFolder = strFolder, chrFormats = "gif"), "chrFormats")
})

test_that("a filter of no value lets nobody through, as bio.viz's chart opens it: each row counts no participant and says none passes the filters (#39)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("r2rtf")
  strFolder <- tempfile("batch-nobody")
  lSpecs <- lapply(lSavedSpecs(), function(lSpec) {
    lSpec$filters <- list(list(column = "ARM", operator = "in", values = list()))
    lSpec
  })
  expect_identical(Spec_Read(strSpecText(lSpecs[[6]]))$settings$filters[[1]]$start, character(0))
  dfManifest <- Run_Specifications(strSpecText(lSpecs), Synthetic_Results, Synthetic_Participants, dfOutcomes = Synthetic_Outcomes, strFolder = strFolder)
  expect_identical(dfManifest$participants, rep(0L, 7L))
  expect_identical(dfManifest$status, rep("failed", 7L))
  for (strReason in dfManifest$reason) {
    expect_match(strReason, "No participant passes the filters.", fixed = TRUE)
  }
  expect_true(all(is.na(dfManifest$figure)))
  expect_setequal(list.files(strFolder), "manifest.json")
  # Each figure and table says so too, before any other reason it has nothing
  # to show, as the chart's footnote does.
  lFunctions <- list(
    "group-comparison" = "GroupComparison", "association-scatter" = "AssociationScatter", "correlation-matrix" = "CorrelationMatrix",
    "biomarker-screen" = "BiomarkerScreen", "cross-tab" = "CrossTab", "stratified-survival" = "StratifiedSurvival"
  )
  for (lSpec in lSpecs) {
    lSettings <- Spec_Read(strSpecText(lSpec))$settings
    for (strKind in c("Visualize_", "Table_")) {
      strFunction <- paste0(strKind, lFunctions[[lSpec$chart]])
      lArgs <- list(Synthetic_Results, Synthetic_Participants, lSettings)
      if ("dfOutcomes" %in% names(formals(get(strFunction)))) lArgs$dfOutcomes <- Synthetic_Outcomes
      expect_error(
        do.call(strFunction, lArgs),
        paste0(strFunction, "(): no participant passes the filters"),
        fixed = TRUE
      )
    }
  }
  # The filters the saved specifications were written with keep their own count.
  dfSaved <- Run_Specifications(strSpecFixture("charts.json"), Synthetic_Results, Synthetic_Participants, dfOutcomes = Synthetic_Outcomes, strFolder = tempfile("batch-counted"), bTables = FALSE)
  expect_identical(dfSaved$participants[1:2], c(200L, sum(Synthetic_Participants$SEX == "F")))
})

test_that("a view the tables cannot honour is written as the chart draws it, and its row says what is not drawn as asked, as bio.viz's notices say it (#39)", {
  skip_if_not_installed("ggplot2")
  lSpecs <- list(
    lSpecOf(lSettings = list(row_by = "NOPE", col_by = "RESPONSE")),
    lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE"), lFilters = list(list(column = "SEX", operator = "in", values = list("X")))),
    lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE"), lFilters = list(list(column = "NOPE", operator = "in", values = list("a")))),
    lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE"), lFilters = list(list(column = "USUBJID", operator = "in", values = list("S001")))),
    lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE"), lFilters = list(list(column = "SEX", operator = "in", values = list("F", "X")))),
    lSpecOf("group-comparison", lSettings = list(start_value = "IL-6", visits = list("Week 4", "Week 99"), group_by = "ARM")),
    lSpecOf("correlation-matrix", lSettings = list(mode = "biomarkers", visit = "Week 99"))
  )
  dfManifest <- Run_Specifications(strSpecText(lSpecs), Synthetic_Results, Synthetic_Participants, strFolder = tempfile("batch-notices"), bTables = FALSE)
  expect_identical(dfManifest$status, rep("written", 7L))
  strAsks <- "Not drawn as the specification asks: "
  lState <- CrossTab_State(Synthetic_Results, Synthetic_Participants, CrossTab_Settings(list(row_by = "NOPE", col_by = "RESPONSE")))
  expect_identical(dfManifest$reason[1], paste0(strAsks, "Rows: NOPE is not in the tables, so the chart draws ", lState$row_by, "."))
  expect_identical(dfManifest$reason[2], paste0(strAsks, "Filter SEX: X is not one of its values, so it is at All."))
  expect_identical(dfManifest$reason[3], paste0(strAsks, "Filter NOPE: the participant table has no such column, so it is not a filter."))
  expect_identical(dfManifest$reason[4], paste0(strAsks, "Filter USUBJID: the participant id is not a filter."))
  expect_identical(dfManifest$reason[5], paste0(strAsks, "Filter SEX: X is not one of its values, so it is at F."))
  expect_identical(dfManifest$reason[6], paste0(strAsks, "Visits: Week 4, Week 99 is not in the tables, so the chart draws Week 4."))
  expect_identical(dfManifest$reason[7], paste0(strAsks, "Visit: Week 99 is not in the tables, so the chart draws Baseline."))
  # Each is counted as the chart draws it: the filters it could not honour let everyone through.
  expect_identical(dfManifest$participants[2:4], rep(nrow(Synthetic_Participants), 3L))
  expect_identical(dfManifest$participants[5], sum(Synthetic_Participants$SEX == "F"))
  # A view drawn as asked has no notice: every specification bio.viz wrote,
  # and each biomarker of one.
  dfSaved <- Run_Specifications(strSpecFixture("charts.json"), Synthetic_Results, Synthetic_Participants, dfOutcomes = Synthetic_Outcomes, strFolder = tempfile("batch-asked"), bTables = FALSE)
  dfEach <- Run_Specifications(strSpecText(lSavedSpecs()[c(1, 3, 6, 7)]), Synthetic_Results, Synthetic_Participants, dfOutcomes = Synthetic_Outcomes,
    strFolder = tempfile("batch-asked-each"), bTables = FALSE, bAcrossBiomarkers = TRUE)
  expect_true(all(is.na(c(dfSaved$reason, dfEach$reason))))
})

test_that("each output's file name is its own: biomarkers whose names make one name are numbered, a name with no letter it can keep is its place, and the manifest names the file written (#39)", {
  skip_if_not_installed("ggplot2")
  strNoAscii <- intToUtf8(c(0x4E2D, 0x6587))
  dfResults <- Synthetic_Results
  dfResults$TEST[dfResults$TEST == "IL-8"] <- "IL_6"
  dfResults$TEST[dfResults$TEST == "VEGF"] <- strNoAscii
  strFolder <- tempfile("batch-names")
  dfManifest <- Run_Specifications(strSpecText(lSavedSpecs()[1]), dfResults, Synthetic_Participants, strFolder = strFolder, bTables = FALSE, bAcrossBiomarkers = TRUE)
  expect_identical(dfManifest$status, rep("written", 12L))
  expect_identical(anyDuplicated(dfManifest$figure), 0L)
  expect_true(all(file.exists(file.path(strFolder, dfManifest$figure))))
  expect_setequal(list.files(strFolder), c("manifest.json", dfManifest$figure))
  chrSame <- dfManifest$figure[dfManifest$biomarker %in% c("IL-6", "IL_6")]
  expect_identical(chrSame, c("01-group-comparison-il-6.png", "01-group-comparison-il-6-2.png"))
  iPlace <- which(dfManifest$biomarker == strNoAscii)
  expect_identical(dfManifest$figure[iPlace], sprintf("01-group-comparison-%d.png", iPlace))
  expect_identical(Batch_Slug(strNoAscii), "")
})

test_that("a view that cannot be drawn is failed with its reason, leaves no file of it behind, and the rest still run (#39)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("r2rtf")
  strFolder <- tempfile("batch-failed")
  # The survival chart is drawn of the outcomes, which are not given.
  dfManifest <- Run_Specifications(strSpecText(lSavedSpecs()[c(7, 1)]), Synthetic_Results, Synthetic_Participants, strFolder = strFolder)
  expect_identical(dfManifest$status, c("failed", "written"))
  expect_match(dfManifest$reason[1], "needs an outcomes table", fixed = TRUE)
  expect_true(is.na(dfManifest$figure[1]) && is.na(dfManifest$table[1]))
  # A format that fails part way leaves none of the view's files: the PNG
  # written before the PDF failed is taken away again.
  strPartial <- tempfile("batch-partial")
  local_mocked_bindings(Batch_Save = function(strFile, gg, strFormat, nWidth, nHeight) {
    if (strFormat == "pdf") stop("the PDF device failed")
    writeLines("drawn", strFile)
    FALSE
  })
  dfPartial <- Run_Specifications(strSpecText(lSavedSpecs()[1]), Synthetic_Results, Synthetic_Participants, strFolder = strPartial, chrFormats = c("png", "pdf"))
  expect_identical(dfPartial$status, "failed")
  expect_match(dfPartial$reason, "the PDF device failed", fixed = TRUE)
  expect_identical(list.files(strPartial), "manifest.json")
})

test_that("a figure with no table to go with it is written, and its row says why there is no table (#39)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("r2rtf")
  strFolder <- tempfile("batch-no-table")
  lSpec <- lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE", statistic = NULL))
  dfManifest <- Run_Specifications(strSpecText(list(lSpec)), Synthetic_Results, Synthetic_Participants, strFolder = strFolder)
  expect_identical(dfManifest$status, "written")
  expect_identical(dfManifest$reason, "No table: Table_CrossTab() has no statistic to show: the setting 'statistic' is NULL, which asks R for no test")
  expect_true(is.na(dfManifest$table))
  expect_true(file.exists(file.path(strFolder, dfManifest$figure)))
})

test_that("across every biomarker, the cross-tabulation's cut row or column and the matrix across visits take each biomarker; an axis at another visit keeps its own; explicit cut points are named in each row (#39)", {
  chrBiomarkers <- sort(unique(Synthetic_Results$TEST))
  lCross <- Spec_Expand(Spec_Read(strSpecText(lSavedSpecs()[[6]])), Synthetic_Results, Synthetic_Participants)
  expect_setequal(names(lCross), chrBiomarkers)
  expect_identical(unname(vapply(lCross, function(lView) lView$settings$col_by$measure, character(1))), names(lCross))
  expect_identical(unname(vapply(lCross, function(lView) lView$settings$row_by, character(1))), rep("RESPONSE", length(lCross)))
  lRows <- Spec_Expand(Spec_Read(strSpecText(lSpecOf(lSettings = list(row_by = list(measure = "CRP", visit = "Baseline", value = "raw", cut = "median"), col_by = "ARM")))), Synthetic_Results)
  expect_identical(unname(vapply(lRows, function(lView) lView$settings$row_by$measure, character(1))), names(lRows))
  expect_length(lRows, length(chrBiomarkers))
  lMatrix <- Spec_Expand(Spec_Read(strSpecText(lSpecOf("correlation-matrix", lSettings = list(mode = "visits", measure = "CRP")))), Synthetic_Results)
  expect_length(lMatrix, length(chrBiomarkers))
  expect_identical(unname(vapply(lMatrix, function(lView) lView$settings$measure, character(1))), names(lMatrix))
  # The scatter's y at another visit is a pair of its own for every x, its own biomarker too.
  lSpec <- lSavedSpecs()[[3]]
  lSpec$settings$y$visit <- "Week 4"
  lScatter <- Spec_Expand(Spec_Read(strSpecText(lSpec)), Synthetic_Results, Synthetic_Participants)
  expect_setequal(names(lScatter), chrBiomarkers)
  expect_null(lScatter[["IL-10"]]$note)
  # Explicit cut points stay as they are for every biomarker, and each row says so.
  lCut <- lSpecOf("stratified-survival", lSettings = list(group_by = list(measure = "CRP", visit = "Baseline", value = "raw", cut = list(2.5, 4))))
  lCuts <- Spec_Expand(Spec_Read(strSpecText(lCut)), Synthetic_Results)
  expect_identical(unname(vapply(lCuts, function(lView) lView$settings$group_by$cut[[2]], numeric(1))), rep(4, length(lCuts)))
  expect_identical(lCuts[["IL-6"]]$note, "The explicit cut 2.5, 4 is applied to IL-6 as it is.")
  expect_null(Spec_Expand(Spec_Read(strSpecText(lSavedSpecs()[[7]])), Synthetic_Results)[["IL-6"]]$note)
  skip_if_not_installed("ggplot2")
  dfManifest <- Run_Specifications(strSpecText(list(lCut)), Synthetic_Results, Synthetic_Participants, dfOutcomes = Synthetic_Outcomes,
    strFolder = tempfile("batch-cut"), bTables = FALSE, bAcrossBiomarkers = TRUE)
  expect_identical(dfManifest$reason[dfManifest$biomarker == "LDH"], "The explicit cut 2.5, 4 is applied to LDH as it is.")
})

test_that("a PDF drawn without cairo says so, naming only the outputs asked for that have every character (#39)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("r2rtf")
  local_mocked_bindings(Batch_HasCairo = function() FALSE)
  # The pdf device draws such a character as a dot (macOS) or as a stand-in
  # such as <= (Linux), and warns either way; the warning is the row's note.
  strDots <- "The PDF was drawn without cairo, which this R cannot load, so a character beyond Latin-1 is not drawn as itself in it (it is a dot, or a stand-in such as <= for the sign of a cut)"
  # The survival chart's cut is labelled with its sign, beyond Latin-1.
  lSpec <- lSavedSpecs()[7]
  dfAlone <- expect_no_warning(Run_Specifications(strSpecText(lSpec), Synthetic_Results, Synthetic_Participants, dfOutcomes = Synthetic_Outcomes,
    strFolder = tempfile("batch-pdf"), chrFormats = "pdf", bTables = FALSE))
  expect_identical(dfAlone$reason, paste0(strDots, "."))
  dfBoth <- Run_Specifications(strSpecText(lSpec), Synthetic_Results, Synthetic_Participants, dfOutcomes = Synthetic_Outcomes,
    strFolder = tempfile("batch-pdf-png"), chrFormats = c("pdf", "png"))
  expect_identical(dfBoth$reason, paste0(strDots, "; the PNG and the RTF table have every character."))
  # Each platform's warning is taken as the note, and no other warning is.
  for (strWarning in c(
    "conversion failure on '<U+2264> 2.783' in 'mbcsToSbcs': dot substituted for <e2>",
    "for '<U+2264> 2.783 (n = 100)' in 'mbcsToSbcs': <= substituted for <U+2264> (U+2264)"
  )) {
    local_mocked_bindings(ggsave = function(...) warning(strWarning), .package = "ggplot2")
    expect_true(expect_no_warning(Batch_Save(tempfile(fileext = ".pdf"), NULL, "pdf", 1, 1)), label = strWarning)
  }
  local_mocked_bindings(ggsave = function(...) warning("something else"), .package = "ggplot2")
  expect_warning(bDots <- Batch_Save(tempfile(fileext = ".pdf"), NULL, "pdf", 1, 1), "something else")
  expect_false(bDots)
})

test_that("specifications jsonlite simplified are refused with a sentence that says how to read them (#39)", {
  strFile <- strSpecFixture("charts.json")
  strHow <- "read_json() or fromJSON(simplifyVector = FALSE)"
  expect_error(Run_Specifications(jsonlite::fromJSON(strFile), Synthetic_Results, strFolder = tempfile("batch-df")), strHow, fixed = TRUE)
  expect_error(Run_Specifications(jsonlite::fromJSON(strFile), Synthetic_Results, strFolder = tempfile("batch-df")), "a data frame", fixed = TRUE)
  lSimplified <- jsonlite::fromJSON(strSpecText(lSavedSpecs()[[1]]))
  expect_error(Spec_Read(lSimplified), strHow, fixed = TRUE)
  # Read as data, the same specifications run.
  expect_identical(Spec_Read(jsonlite::read_json(strFile)[[1]])$chart, "group-comparison")
  expect_length(Spec_Parse(jsonlite::fromJSON(strFile, simplifyVector = FALSE)), 7L)
})

test_that("a view whose cut names what the tables lack fails on its own row with R's sentence, and the rest still run, across biomarkers too (#48)", {
  skip_if_not_installed("ggplot2")
  lGood <- lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE"))
  lSpecs <- list(
    lSpecOf(lSettings = list(row_by = "ARM", col_by = list(col = "NOPE", type = "number", cut = "median"))),
    lSpecOf(lSettings = list(row_by = "ARM", col_by = list(measure = "NOPE", visit = "Baseline", cut = "median"))),
    lSpecOf("stratified-survival", lSettings = list(group_by = list(col = "NOPE", type = "number", cut = "median"))),
    lSpecOf(lSettings = list(row_by = "ARM", col_by = list(col = "__proto__", type = "number", cut = "median"))),
    lGood
  )
  for (bAcross in c(FALSE, TRUE)) {
    dfManifest <- Run_Specifications(strSpecText(lSpecs), Synthetic_Results, Synthetic_Participants, dfOutcomes = Synthetic_Outcomes,
      strFolder = tempfile("batch-bad-cut"), bTables = FALSE, bAcrossBiomarkers = bAcross)
    # Across biomarkers, the cut of a biomarker the tables lack takes each
    # biomarker they have instead, and is drawn; on its own it fails.
    chrBad <- if (bAcross) c(1L, 3L, 4L) else 1:4
    dfBad <- dfManifest[dfManifest$specification %in% chrBad, ]
    expect_identical(unique(dfBad$status), "failed", label = paste("across", bAcross))
    expect_true(all(!is.na(dfBad$reason)), label = paste("across", bAcross, "reasons"))
    expect_match(dfBad$reason[dfBad$specification == 1L], "cuts the column 'NOPE', which neither table has", fixed = TRUE, all = TRUE)
    if (!bAcross) expect_match(dfBad$reason[dfBad$specification == 2L], "NOPE", fixed = TRUE)
    if (bAcross) expect_identical(unique(dfManifest$status[dfManifest$specification == 2L]), "written")
    expect_identical(dfManifest$status[dfManifest$specification == 5L], "written", label = paste("across", bAcross))
  }
})

test_that("a specification nested far past the limit is refused with gsm.bio's depth sentence, not R's stack overflow (#48)", {
  for (nDeep in c(1000L, 100000L)) {
    strDeep <- paste0('{"format":"bio.viz specification","format_version":1,"bio_viz_version":"0.3.0","chart":"cross-tab","settings":{"title":',
      strrep("[", nDeep), '"x"', strrep("]", nDeep), "}}")
    expect_error(Spec_Read(strDeep), "nested more than 64 deep", fixed = TRUE, label = paste(nDeep, "deep"))
  }
  # Brackets inside text are text, and do not count.
  strText <- strSpecText(lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE", title = strrep("[", 200))))
  expect_identical(Spec_Read(strText)$settings$title, strrep("[", 200))
})

test_that("when two biomarkers make one file name, the one whose name is the name keeps it: IL-6 is il-6 before IL 6 (#48)", {
  skip_if_not_installed("ggplot2")
  dfResults <- Synthetic_Results
  dfResults$TEST[dfResults$TEST == "IL-8"] <- "IL 6"
  dfManifest <- Run_Specifications(strSpecText(lSavedSpecs()[1]), dfResults, Synthetic_Participants,
    strFolder = tempfile("batch-exact"), bTables = FALSE, bAcrossBiomarkers = TRUE)
  expect_identical(dfManifest$figure[dfManifest$biomarker == "IL-6"], "01-group-comparison-il-6.png")
  expect_identical(dfManifest$figure[dfManifest$biomarker == "IL 6"], "01-group-comparison-il-6-2.png")
  expect_identical(anyDuplicated(dfManifest$figure), 0L)
})
