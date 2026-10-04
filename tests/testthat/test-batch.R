# The batch runner (#39): bio.viz's chart specifications (bio.viz#68,
# docs/output.md "Specifications") read as data and run against the study's
# tables, each to a figure and an RTF table in a folder, with a manifest; one
# specification can run across every biomarker. The reader is held to bio.viz's
# own reader, run on the same specifications in node from the copied bundle.

strSpecFixture <- function(...) testthat::test_path("fixtures", "specifications", ...)

# The specifications bio.viz's charts wrote (fixtures/specifications/charts.json).
lSavedSpecs <- function() {
  lReadJson(strSpecFixture(), "charts.json")
}

# A specification as JSON text, from an R list.
strSpecText <- function(lSpec) {
  as.character(jsonlite::toJSON(lSpec, auto_unbox = TRUE, null = "null", digits = NA))
}

# The smallest specification of a chart, with settings and filters to add.
lSpecOf <- function(strChart = "cross-tab", lSettings = NULL, lFilters = NULL, ...) {
  lSpec <- list(format = "bio.viz specification", format_version = 1L, bio_viz_version = "0.1.0", chart = strChart)
  if (!is.null(lSettings)) lSpec$settings <- lSettings
  if (!is.null(lFilters)) lSpec$filters <- lFilters
  utils::modifyList(lSpec, list(...))
}

# Specifications bio.viz's reader accepts or refuses, each as JSON text. Every
# rule of the format is here, and a value each chart checks.
lSpecCases <- function() {
  strDeep <- paste0('{"format":"bio.viz specification","format_version":1,"bio_viz_version":"0.1.0","chart":"cross-tab","settings":{"title":',
    strrep("[", 70), '"x"', strrep("]", 70), "}}")
  lCases <- list(
    "the six charts' own" = NULL,
    "a filter in force" = strSpecText(lSpecOf(lSettings = list(row_by = "ARM", col_by = "RESPONSE"), lFilters = list(list(column = "SEX", operator = "in", values = list("F"))))),
    "a filter of two values" = strSpecText(lSpecOf(lFilters = list(list(column = "ARM", operator = "in", values = list("Placebo", "Treatment"))))),
    "a filter of none" = strSpecText(lSpecOf(lFilters = list(list(column = "ARM", operator = "in", values = list())))),
    "a filter onto the chart's own" = strSpecText(lSpecOf(lSettings = list(filters = list(list(value_col = "SEX", label = "Sex"))), lFilters = list(list(column = "SEX", operator = "in", values = list("M"))))),
    "no settings" = strSpecText(lSpecOf("stratified-survival")),
    "code-like text" = strSpecText(lSpecOf(lSettings = list(title = "${1 + 1} <script>alert(1)</script> {{x}} system('touch PWNED')"))),
    "a number as a filter value" = strSpecText(lSpecOf(lFilters = list(list(column = "AGE", operator = "in", values = list(35, "36"))))),
    "a member twice, the last kept" = '{"format":"vega-lite","format":"bio.viz specification","format_version":1,"bio_viz_version":"0.1.0","chart":"cross-tab","settings":{"row_by":"SEX","row_by":"ARM"}}',
    "not an object" = "[1, 2]",
    "not JSON" = "{format: bio.viz}",
    "another format" = strSpecText(lSpecOf(format = "vega-lite")),
    "another format version" = strSpecText(lSpecOf(format_version = 2L)),
    "a member it does not have" = strSpecText(lSpecOf(data = list(1))),
    "a version that is not text" = strSpecText(lSpecOf(bio_viz_version = 1L)),
    "no version" = '{"format":"bio.viz specification","format_version":1,"chart":"cross-tab"}',
    "a chart bio.viz does not have" = strSpecText(lSpecOf("pie-chart")),
    "a setting the chart does not have" = strSpecText(lSpecOf(lSettings = list(row_by = "ARM", colour = "red"))),
    "the page's connection" = strSpecText(lSpecOf(lSettings = list(connection = list(results = list())))),
    "settings that are not an object" = strSpecText(lSpecOf(lSettings = list("ARM"))),
    "a filters setting that is not a list" = strSpecText(lSpecOf(lSettings = list(filters = "SEX"))),
    "a setting that names __proto__" = strSpecText(lSpecOf(lSettings = list(row_by = "__proto__"))),
    "a setting named __proto__" = '{"format":"bio.viz specification","format_version":1,"bio_viz_version":"0.1.0","chart":"cross-tab","settings":{"__proto__":{"row_by":"ARM"}}}',
    "filters that are not a list" = strSpecText(lSpecOf(lFilters = list(column = "SEX"))),
    "a filter that is not an object" = strSpecText(lSpecOf(lFilters = list("SEX"))),
    "a filter with more than it has" = strSpecText(lSpecOf(lFilters = list(list(column = "SEX", operator = "in", values = list("F"), label = "Sex")))),
    "a filter with no column" = strSpecText(lSpecOf(lFilters = list(list(column = " ", operator = "in", values = list("F"))))),
    "a filter on constructor" = strSpecText(lSpecOf(lFilters = list(list(column = "constructor", operator = "in", values = list("F"))))),
    "a filter on prototype" = strSpecText(lSpecOf(lFilters = list(list(column = "prototype", operator = "in", values = list("F"))))),
    "a column filtered twice" = strSpecText(lSpecOf(lFilters = list(list(column = "SEX", operator = "in", values = list("F")), list(column = "SEX", operator = "in", values = list("M"))))),
    "an operator it does not have" = strSpecText(lSpecOf(lFilters = list(list(column = "SEX", operator = "eq", values = list("F"))))),
    "values that are not a list" = strSpecText(lSpecOf(lFilters = list(list(column = "SEX", operator = "in", values = "F")))),
    "a value that is not text or a number" = strSpecText(lSpecOf(lFilters = list(list(column = "SEX", operator = "in", values = list(list(x = 1)))))),
    "a value twice" = strSpecText(lSpecOf(lFilters = list(list(column = "AGE", operator = "in", values = list(35, "35"))))),
    "nested more than 64 deep" = strDeep,
    "a choice the chart does not have" = strSpecText(lSpecOf(lSettings = list(percent = "rows"))),
    "a test the chart does not have" = strSpecText(lSpecOf("group-comparison", lSettings = list(test = "anova2"))),
    "a title that is not text" = strSpecText(lSpecOf(lSettings = list(title = 42L))),
    "footnotes that are not texts" = strSpecText(lSpecOf(lSettings = list(footnotes = list("One.", list(text = "Two."))))),
    "a mark the chart does not have" = strSpecText(lSpecOf("group-comparison", lSettings = list(mark = "pie"))),
    "a page below nought" = strSpecText(lSpecOf("biomarker-screen", lSettings = list(page = -1L))),
    "a limit of nought" = strSpecText(lSpecOf("biomarker-screen", lSettings = list(limit = 0L))),
    "a sort it does not have" = strSpecText(lSpecOf("biomarker-screen", lSettings = list(sort = "random"))),
    "a view it does not have" = strSpecText(lSpecOf("correlation-matrix", lSettings = list(view = "table"))),
    "a png_scale beyond 4" = strSpecText(lSpecOf(lSettings = list(png_scale = 9L))),
    "downloads that is not true or false" = strSpecText(lSpecOf(lSettings = list(downloads = "yes"))),
    "a profile that is not true or false" = strSpecText(lSpecOf("stratified-survival", lSettings = list(profile = "no"))),
    "a waiting note of white space" = strSpecText(lSpecOf(lSettings = list(waiting_note = " "))),
    "a page size that is not whole" = strSpecText(lSpecOf(lSettings = list(page_size = 2.5))),
    "details that are not columns" = strSpecText(lSpecOf(lSettings = list(details = list(list(label = "Arm"))))),
    "a study day column that is not a name" = strSpecText(lSpecOf(lSettings = list(studyday_col = 3L)))
  )
  lCases[["the six charts' own"]] <- NULL
  chrSaved <- vapply(lSavedSpecs(), strSpecText, character(1))
  names(chrSaved) <- paste("saved by bio.viz:", seq_along(chrSaved), vapply(lSavedSpecs(), `[[`, character(1), "chart"))
  c(as.list(chrSaved), lCases)
}

# What bio.viz's own reader makes of each case: run in node from the copied
# bundle where node is installed (CI has it), and otherwise as recorded in
# fixtures/specifications/bioviz-reader.json by the same script.
lBioVizReads <- function(lCases, bNode = nzchar(Sys.which("node"))) {
  strNode <- Sys.which("node")
  strBundle <- system.file("htmlwidgets", "lib", "bio.viz-0.1.0", "bio.viz.js", package = "gsm.bio")
  strCases <- tempfile(fileext = ".json")
  writeLines(as.character(jsonlite::toJSON(
    unname(Map(function(strCase, strText) list(case = strCase, text = strText), names(lCases), unlist(lCases))),
    auto_unbox = TRUE
  )), strCases, useBytes = TRUE)
  if (bNode) {
    strOut <- tempfile(fileext = ".json")
    iStatus <- system2(strNode, c(shQuote(strSpecFixture("bioviz-reader.mjs")), shQuote(strBundle), shQuote(strCases)), stdout = strOut)
    expect_identical(iStatus, 0L, label = "bio.viz's reader ran in node")
    lRead <- lReadJson(strOut)
  } else {
    lRead <- lReadJson(strSpecFixture(), "bioviz-reader.json")
  }
  stats::setNames(lRead, vapply(lRead, `[[`, character(1), "case"))
}

# A sentence as ASCII, a character beyond it as its code point: the same text
# whichever locale R or node wrote it in.
strAscii <- function(strText) {
  if (Encoding(strText) == "unknown" && validUTF8(strText)) Encoding(strText) <- "UTF-8"
  iconv(enc2utf8(strText), "UTF-8", "ASCII", sub = "Unicode")
}

# A value as JSON reads it back, so R's settings and bio.viz's compare alike.
lAsJson <- function(xValue) {
  jsonlite::fromJSON(jsonlite::toJSON(xValue, auto_unbox = TRUE, null = "null", digits = NA), simplifyVector = FALSE)
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
  # The reads recorded for a session with no node are bio.viz's reads now.
  if (nzchar(Sys.which("node"))) {
    expect_identical(lBioVizReads(lCases, bNode = FALSE), lTheirs, label = "fixtures/specifications/bioviz-reader.json is current")
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
  # The reader and the runner call nothing that evaluates text.
  chrCode <- unlist(lapply(c("Spec_Read", "Spec_Check", "Spec_ReadChecked", "Spec_Expand", "Run_Specifications", "Spec_Parse", "Spec_FromText", "Spec_Tidy", "Batch_Draw"), function(strName) {
    deparse(get(strName, envir = asNamespace("gsm.bio")))
  }))
  for (strCall in c("eval(", "parse(", "str2lang(", "str2expression(", "do.call(", "match.fun(", "source(", "system(")) {
    expect_false(any(grepl(strCall, chrCode, fixed = TRUE)), label = paste("the reader calls", strCall))
  }
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
