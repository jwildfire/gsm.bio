# The specifications the reader and the batch runner are tested on, and
# bio.viz's own reader run on them (#39): in node from the copied bundle, or as
# data-raw/specifications/record-readers.R recorded it.

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

# What bio.viz's own reader makes of each case, run in node from the copied
# bundle (fixtures/specifications/bioviz-reader.mjs): `{ case, accepted, chart,
# settings }` or `{ case, accepted: false, refusal }`, named by case. The
# reason V8 gives for text that is not JSON is its own, and changes with
# node's version, so only bio.viz's sentence before it is kept.
lBioVizLive <- function(lCases, bObject = FALSE) {
  strBundle <- system.file("htmlwidgets", "lib", "bio.viz-0.1.0", "bio.viz.js", package = "gsm.bio")
  strCases <- tempfile(fileext = ".json")
  writeLines(as.character(jsonlite::toJSON(
    unname(Map(function(strCase, strText) list(case = strCase, text = strText), names(lCases), unlist(lCases))),
    auto_unbox = TRUE
  )), strCases, useBytes = TRUE)
  strOut <- tempfile(fileext = ".json")
  iStatus <- system2(Sys.which("node"), c(shQuote(strSpecFixture("bioviz-reader.mjs")), shQuote(strBundle), shQuote(strCases), if (bObject) "--object"), stdout = strOut)
  if (!identical(iStatus, 0L)) stop("bio.viz's reader did not run in node")
  lRead <- lapply(lReadJson(strOut), function(lOne) {
    if (!is.null(lOne$refusal)) lOne$refusal <- sub("^(bio[.]viz: a specification given as text must be JSON:).*$", "\\1 ...", lOne$refusal)
    lOne
  })
  stats::setNames(lRead, vapply(lRead, `[[`, character(1), "case"))
}

# Specifications nested near the depth bio.viz's reader allows: a filter
# spec carrying a member nested `n` deep, for each `n` from 55 to 66, and
# nothing else the reader would refuse.
lSpecDepthCases <- function() {
  lCases <- lapply(55:66, function(nDeep) {
    paste0(
      '{"format":"bio.viz specification","format_version":1,"bio_viz_version":"0.1.0","chart":"cross-tab",',
      '"settings":{"filters":[{"value_col":"SEX","label":"Sex","x":', strrep("[", nDeep), '"x"', strrep("]", nDeep), "}]}}"
    )
  })
  stats::setNames(lCases, paste("as an object: nested", 55:66, "deep in a filter"))
}

# bio.viz's reads of the format's cases as text, and of the depth cases as
# objects, as the recording holds them.
lBioVizAllLive <- function() {
  c(lBioVizLive(lSpecCases()), lBioVizLive(lSpecDepthCases(), bObject = TRUE))
}

# The same, as recorded in fixtures/specifications/bioviz-reader.json by
# data-raw/specifications/record-readers.R, for a session with no node.
lBioVizRecorded <- function() {
  lRead <- lReadJson(strSpecFixture(), "bioviz-reader.json")
  stats::setNames(lRead, vapply(lRead, `[[`, character(1), "case"))
}

lBioVizReads <- function(lCases, bNode = nzchar(Sys.which("node")), bObject = FALSE) {
  if (bNode) lBioVizLive(lCases, bObject) else lBioVizRecorded()[names(lCases)]
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


# Every setting of every chart, given each of a fixed list of values: the fuzz
# the reader is held to bio.viz's by, deterministic, one specification per
# chart, setting and value, named "chart | setting | value". The page's own
# settings, which a specification never holds, are left out.
chrFuzzValues <- c(
  "null", "[]", "{}", '""', '"x"', '"ARM"', '"Baseline"', '"CRP"', "0", "-1", "1.5", "3", "true", '["x"]', '["Baseline"]', "[1]",
  '{"col":"AGE"}', '{"measure":"CRP","visit":"Baseline","value":"raw"}', '{"measure":"CRP","visit":"Baseline","value":"raw","cut":"median"}',
  '[{"value_col":"ARM","label":"Arm"}]', '["ARM"]'
)

lFuzzCases <- function() {
  lNames <- Spec_SettingNames()
  lCases <- list()
  for (strChart in names(lNames)) {
    for (strSetting in setdiff(lNames[[strChart]], c("connection", "back"))) {
      for (strValue in chrFuzzValues) {
        lCases[[paste(strChart, strSetting, strValue, sep = " | ")]] <- paste0(
          '{"format":"bio.viz specification","format_version":1,"bio_viz_version":"0.1.0","chart":"', strChart,
          '","settings":{"', strSetting, '":', strValue, "}}"
        )
      }
    }
  }
  lCases
}

# The fuzz cases' fingerprint, which the recording names.
strFuzzPrint <- function(lCases) {
  digest::digest(paste(names(lCases), unlist(lCases), sep = "\t", collapse = "\n"), algo = "sha256", serialize = FALSE)
}

# What a reader made of each fuzz case, in brief: whether it accepted it, its
# refusal, and whether the settings it read are the ones given.
lFuzzBrief <- function(lCases, fnRead) {
  stats::setNames(lapply(names(lCases), function(strCase) {
    lGiven <- jsonlite::parse_json(lCases[[strCase]], simplifyVector = FALSE)$settings
    lRead <- fnRead(strCase)
    list(
      accepted = isTRUE(lRead$accepted),
      refusal = if (isTRUE(lRead$accepted)) NA_character_ else strAscii(lRead$refusal),
      same = isTRUE(lRead$accepted) && identical(lRead$settings, lGiven)
    )
  }), names(lCases))
}

# gsm.bio's reader on the fuzz, in brief, its refusals in bio.viz's form.
lFuzzMine <- function(lCases) {
  lFuzzBrief(lCases, function(strCase) {
    tryCatch(
      {
        lRead <- Spec_ReadChecked(lCases[[strCase]])
        list(accepted = TRUE, settings = lAsJson(lRead$settings))
      },
      error = function(cndError) list(accepted = FALSE, refusal = paste0("bio.viz: ", conditionMessage(cndError)))
    )
  })
}

# bio.viz's reader on the fuzz, in brief: run in node, or as recorded in
# fixtures/specifications/bioviz-fuzz.json, which holds each case's brief by
# its place, with each refusal once.
lFuzzTheirs <- function(lCases, bNode = nzchar(Sys.which("node"))) {
  if (bNode) {
    lLive <- lBioVizLive(lCases)
    return(lFuzzBrief(lCases, function(strCase) lLive[[strCase]]))
  }
  lRecord <- lReadJson(strSpecFixture(), "bioviz-fuzz.json")
  if (!identical(lRecord$cases, strFuzzPrint(lCases))) stop("fixtures/specifications/bioviz-fuzz.json records other cases")
  chrSentences <- unlist(lRecord$sentences)
  stats::setNames(lapply(lRecord$reads, function(lOne) {
    list(accepted = lOne[[1]] == 1L, refusal = if (lOne[[2]] < 0L) NA_character_ else chrSentences[lOne[[2]] + 1L], same = lOne[[3]] == 1L)
  }), names(lCases))
}

# The brief as the recording holds it.
lFuzzRecord <- function(lCases, lBrief) {
  chrSentences <- sort(unique(stats::na.omit(vapply(lBrief, `[[`, character(1), "refusal"))), method = "radix")
  list(
    what = paste(
      "bio.viz's reader on every setting of every chart given each of a fixed list of values (helper-specifications.R, lFuzzCases):",
      "for each case by its place, whether it accepted it, its refusal by its place in sentences (from 0; -1 for none),",
      "and whether the settings it read are the ones given."
    ),
    written_by = "data-raw/specifications/record-readers.R",
    cases = strFuzzPrint(lCases),
    sentences = chrSentences,
    reads = unname(lapply(lBrief, function(lOne) {
      c(as.integer(lOne$accepted), if (is.na(lOne$refusal)) -1L else match(lOne$refusal, chrSentences) - 1L, as.integer(lOne$same))
    }))
  )
}
