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
  expect_identical(length(lCases), 4578L)
  lMine <- lFuzzMine(lCases)
  lTheirs <- lFuzzTheirs(lCases)
  bDiffers <- vapply(names(lCases), function(strCase) {
    !identical(lMine[[strCase]]$accepted, lTheirs[[strCase]]$accepted) || !identical(lMine[[strCase]]$same, lTheirs[[strCase]]$same)
  }, logical(1))
  # The differences there are, each on purpose. A chart's `statistic` (and the
  # scatter's `fit_statistic`) names the R function the page asks; bio.viz
  # takes any name, and gsm.bio computes only the one Analyze_*() function the
  # chart's statistics are, so it refuses any other with a sentence.
  chrIntended <- names(lCases)[grepl("^[a-z-]+ [|] (statistic|fit_statistic) [|] \"[^\"]+\"$", names(lCases))]
  expect_length(chrIntended, 28L)
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
