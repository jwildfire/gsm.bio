# The correlation matrix widget (#13): bio.viz's grid from R, with R's
# coefficients computed when the widget is made and stored in the page, and
# with them what the scatter of every cell asks, so a saved page shows the grid
# and every cell's scatter with no R and no network.

lMatrixSettings <- function() {
  list(visit = "Baseline", baseline_visits = "Baseline")
}

lMatrixWidget <- function(lSettings = lMatrixSettings(), ...) {
  Widget_CorrelationMatrix(Synthetic_Results, Synthetic_Participants, lSettings = lSettings, ...)
}

# The grid's biomarkers in the Biomarkers control's order: by name, with
# numbers inside a name as numbers.
chrGridBiomarkers <- function() {
  c("CRP", "D-dimer", "Ferritin", "IFN-gamma", "IL-1beta", "IL-2", "IL-6", "IL-8", "IL-10", "LDH", "TNF-alpha", "VEGF")
}

# The grid's frame at Baseline, worked out here from the study's tables and
# nothing of the widget's: one row per participant who has any of the values,
# one column per biomarker, NA where the participant has none.
dfGridFrame <- function() {
  dfFrame <- data.frame(USUBJID = Synthetic_Participants$USUBJID, stringsAsFactors = FALSE)
  for (iColumn in seq_along(chrGridBiomarkers())) {
    dfFrame[[paste0("v", iColumn)]] <- nResultAt(chrGridBiomarkers()[iColumn], "Baseline")
  }
  dfFrame <- dfFrame[rowSums(!is.na(dfFrame[-1])) > 0, ]
  rownames(dfFrame) <- NULL
  dfFrame
}

# The rows a cell's scatter hands R: the participants with both values.
dfCellRows <- function(strX, strY) {
  dfRows <- data.frame(
    USUBJID = Synthetic_Participants$USUBJID, x = nResultAt(strX, "Baseline"), y = nResultAt(strY, "Baseline"),
    stringsAsFactors = FALSE
  )
  dfRows[!is.na(dfRows$x) & !is.na(dfRows$y), ]
}

lSavedMatrix <- local({
  lSaved <- NULL
  function() {
    if (is.null(lSaved)) {
      lWidget <- lMatrixWidget()
      strPage <- strSavedPage(lWidget)
      lSaved <<- list(widget = lWidget, page = strPage, payload = lPagePayload(strPage))
    }
    lSaved
  }
})

test_that("Widget_CorrelationMatrix returns an htmlwidget carrying the tables, the settings and the stored results (#13)", {
  lWidget <- lMatrixWidget()
  expect_s3_class(lWidget, c("Widget_CorrelationMatrix", "htmlwidget"))
  expect_named(lWidget$x, c("dfResults", "dfParticipants", "lSettings", "bDebug", "bAutoWidth", "bAutoHeight", "lStatistics"))
  expect_identical(lWidget$x$dfResults, Synthetic_Results)
  expect_identical(lWidget$x$dfParticipants, Synthetic_Participants)
  expect_identical(lWidget$x$lSettings, lMatrixSettings())
  expect_named(lWidget$x$lStatistics, c("computed_by", "results"))
  lSized <- lMatrixWidget(width = "100%", height = "700px", elementId = "matrix-widget", bDebug = TRUE)
  expect_identical(lSized[c("width", "height", "elementId")], list(width = "100%", height = "700px", elementId = "matrix-widget"))
  expect_true(lSized$x$bDebug)
  # No settings for the scatter is none, written as none.
  expect_null(lMatrixWidget(c(lMatrixSettings(), list(scatter = list())))$x$lSettings$scatter)
  expect_true("scatter" %in% names(lMatrixWidget(c(lMatrixSettings(), list(scatter = list())))$x$lSettings))
})

test_that("Widget_CorrelationMatrix rejects invalid inputs before a page is made (#13)", {
  expect_error(Widget_CorrelationMatrix("not a data.frame"), "dfResults is not a data.frame")
  expect_error(Widget_CorrelationMatrix(Synthetic_Results, lSettings = list(value_col = "NOT_A_COLUMN")), "NOT_A_COLUMN.*value_col")
  expect_error(lMatrixWidget(list(mode = "pairs")), "mode.*must be one of")
  expect_error(lMatrixWidget(list(method = "kendall")), "method.*must be one of")
  expect_error(lMatrixWidget(list(limit = 1)), "limit.*two or more")
  expect_error(lMatrixWidget(list(min_pairs = 0)), "min_pairs.*above zero")
  expect_error(lMatrixWidget(list(statistic = "my_matrix")), "statistic.*Analyze_CorrelationMatrix")
  expect_error(lMatrixWidget(list(connection = list())), "connection.*cannot be given")
  expect_error(lMatrixWidget(list(scatter = "linear")), "scatter.*named list")
  # What the grid hands the scatter is the grid's to set.
  expect_error(lMatrixWidget(list(scatter = list(x = list(col = "AGE")))), "scatter.*cannot name 'x'")
  expect_error(lMatrixWidget(list(scatter = list(method = "spearman", back = list()))), "cannot name 'method', 'back'")
  # And what the page set for the scatter is checked as the scatter checks it.
  expect_error(lMatrixWidget(list(scatter = list(fit = "quadratic"))), "fit.*must be one of")
})

test_that("the widget stores the grid and, for every cell, what its scatter asks when the cell opens it (#13)", {
  lResults <- lMatrixWidget()$x$lStatistics$results
  chrBiomarkers <- chrGridBiomarkers()
  nPairs <- length(chrBiomarkers) * (length(chrBiomarkers) - 1L)

  # One grid, and one scatter for each cell on either side of the diagonal:
  # the column's biomarker on x, the row's on y.
  expect_identical(length(lResults), 1L + nPairs)
  expect_identical(nPairs, 132L)
  lGrid <- lResults[[1]]
  expect_identical(lGrid$name, "Analyze_CorrelationMatrix")
  expect_identical(lGrid$args, list(chrCols = as.list(paste0("v", 1:12)), strMethod = "pearson"))
  expect_identical(
    lGrid$dataId,
    list(
      chart = "correlation-matrix",
      variables = lapply(chrBiomarkers, function(strMeasure) list(measure = strMeasure, value = "raw", visit = "Baseline")),
      baseline_visits = list("Baseline"), baseline_stat = "mean"
    )
  )
  expect_identical(lGrid$rows, nrow(dfGridFrame()))

  lScatters <- lResults[-1]
  dfPairs <- data.frame(
    x = vapply(lScatters, function(lResult) lResult$dataId$x$measure, character(1)),
    y = vapply(lScatters, function(lResult) lResult$dataId$y$measure, character(1))
  )
  expect_identical(anyDuplicated(dfPairs), 0L)
  expect_false(any(dfPairs$x == dfPairs$y))
  expect_setequal(paste(dfPairs$x, dfPairs$y), as.vector(outer(chrBiomarkers, chrBiomarkers, paste))[!diag(12)])
  for (lResult in lScatters) {
    expect_identical(lResult$name, "Analyze_Correlation")
    # The grid's method, no colour, no line: what the scatter asks as it opens.
    expect_identical(lResult$args, list(strXCol = "x", strYCol = "y", strMethod = "pearson"))
    expect_identical(names(lResult$dataId), c("chart", "x", "y", "baseline_visits", "baseline_stat"))
    expect_identical(lResult$dataId$chart, "association-scatter")
  }

  # The minimum is sent only when it is set, and then to the grid alone.
  lMinimum <- lMatrixWidget(c(lMatrixSettings(), list(min_pairs = 150)))$x$lStatistics$results
  expect_identical(lMinimum[[1]]$args$nMinPairs, 150L)
  expect_false(any(vapply(lMinimum[-1], function(lResult) "nMinPairs" %in% names(lResult$args), logical(1))))
  expect_false("nMinPairs" %in% names(lGrid$args))
  # The grid's method, filters and limit are the scatters' too; a line the
  # page asks the scatter for is stored with its coefficient.
  lOther <- lMatrixWidget(c(lMatrixSettings(), list(
    method = "spearman", limit = 3, filters = list(list(value_col = "SEX", start = "F")),
    scatter = list(fit = "linear", color_by = "ARM")
  )))$x$lStatistics$results
  expect_identical(length(lOther), 1L + 2L * 6L)
  expect_identical(lOther[[1]]$dataId$filters, list(SEX = list("F")))
  expect_identical(
    table(vapply(lOther[-1], function(lResult) paste(lResult$name, lResult$args$strMethod), character(1))),
    table(rep(c("Analyze_Correlation spearman", "Analyze_Fit linear"), each = 6))
  )
  expect_true(all(vapply(lOther[-1], function(lResult) identical(lResult$dataId$filters, list(SEX = list("F"))), logical(1))))
  # Across visits: the visits of one biomarker, the baseline of a change left out.
  lVisits <- lMatrixWidget(list(mode = "visits", measure = "IL-6", value_type = "change", baseline_visits = "Baseline"))$x$lStatistics$results
  expect_identical(length(lVisits), 1L + 4L * 3L)
  # No grid, no scatters: no function named, or a change at the baseline visit.
  expect_identical(lMatrixWidget(c(lMatrixSettings(), list(statistic = NULL)))$x$lStatistics$results, list())
  expect_identical(lMatrixWidget(list(value_type = "change", baseline_visits = "Baseline"))$x$lStatistics$results, list())
})

test_that("the saved matrix page holds the stored results, equal to the R functions' answers member by member (#13)", {
  lResults <- lSavedMatrix()$payload$lStatistics$results
  expect_identical(length(lResults), 133L)

  # The grid, on its frame worked out here.
  dfFrame <- dfGridFrame()
  lGrid <- lResults[[1]]
  expect_identical(lGrid$args$chrCols, as.list(paste0("v", 1:12)))
  ExpectInPage(lGrid$value, do.call(Analyze_CorrelationMatrix, c(list(dfFrame), lGrid$args)), "the grid")

  # Every cell's scatter, on its pair's rows worked out here.
  for (lResult in lResults[-1]) {
    strLabel <- paste(lResult$dataId$x$measure, "against", lResult$dataId$y$measure)
    dfRows <- dfCellRows(lResult$dataId$x$measure, lResult$dataId$y$measure)
    expect_identical(lResult$rows, nrow(dfRows), label = paste(strLabel, "rows"))
    ExpectInPage(lResult$value, do.call(Analyze_Correlation, c(list(dfRows), lResult$args)), strLabel)
  }

  # The planted cell: TNF-alpha with IL-10 at Baseline, the same coefficient in
  # the grid and in its scatter, either way round.
  lTruth <- Synthetic_Truth$Correlation
  lCell <- Filter(function(lRow) {
    setequal(c(lRow$x, lRow$y), paste0("v", match(lTruth$Biomarkers, chrGridBiomarkers())))
  }, lGrid$value$rows)[[1]]
  expect_identical(lCell$counts, 200L)
  expect_true(lCell$lower < lTruth$Value && lTruth$Value < lCell$upper)
  for (chrWay in list(lTruth$Biomarkers, rev(lTruth$Biomarkers))) {
    lScatter <- Filter(function(lResult) identical(c(lResult$dataId$x$measure, lResult$dataId$y$measure), chrWay), lResults[-1])[[1]]$value
    expect_equal(lScatter$estimates[[1]]$estimate, lCell$estimate, tolerance = 1e-12)
    expect_equal(c(lScatter$estimates[[1]]$lower, lScatter$estimates[[1]]$upper), c(lCell$lower, lCell$upper), tolerance = 1e-12)
  }

  # The grid holds no p-value, and so neither does the page: none at the top and none in a cell.
  expect_null(lGrid$value$p_value)
  expect_false(any(grepl("^p_", unique(unlist(lapply(lGrid$value$rows, names))))))
  expect_match(lGrid$value$notes[[1]], "No p-values", fixed = TRUE)
  # The comparison can fail: one cell's scatter is not another's answer.
  expect_gt(length(chrPageDifferences(lResults[[2]]$value, Analyze_Correlation(dfCellRows("IL-6", "CRP"), "x", "y"), "another pair")), 0)
})

test_that("the saved matrix page holds no result under a key it was not computed for (#13)", {
  lResults <- lSavedMatrix()$payload$lStatistics$results
  chrKeys <- vapply(lResults, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  expect_identical(anyDuplicated(chrKeys), 0L)

  Key <- function(iResult, lArgs = list(), lDataId = list()) {
    lKey <- lResults[[iResult]][c("name", "args", "dataId")]
    lKey$args[names(lArgs)] <- lArgs
    lKey$dataId[names(lDataId)] <- lDataId
    Chart_KeyText(lKey)
  }
  # The grid by another method, with a minimum, at another visit, under a filter.
  expect_true(Key(1) %in% chrKeys)
  expect_false(Key(1, list(strMethod = "spearman")) %in% chrKeys)
  expect_false(Key(1, list(nMinPairs = 5L)) %in% chrKeys)
  expect_false(Key(1, lDataId = list(variables = lapply(chrGridBiomarkers(), function(strMeasure) list(measure = strMeasure, value = "raw", visit = "Week 4")))) %in% chrKeys)
  expect_false(Key(1, lDataId = list(filters = list(SEX = list("F")))) %in% chrKeys)
  # A cell's scatter by another method, with a colour, on a logarithmic axis,
  # or of a variable with itself.
  expect_true(Key(2) %in% chrKeys)
  expect_false(Key(2, list(strMethod = "spearman")) %in% chrKeys)
  expect_false(Key(2, list(strGroupCol = "color"), list(color_by = "ARM", groups = list("Placebo", "Treatment"))) %in% chrKeys)
  expect_false(Key(2, lDataId = list(x_scale = "log")) %in% chrKeys)
  expect_false(Key(2, lDataId = list(y = lResults[[2]]$dataId$x)) %in% chrKeys)
  # A fitted line is no result of this page: the scatter opens with none.
  expect_false(any(vapply(lResults, function(lResult) lResult$name == "Analyze_Fit", logical(1))))
})

test_that("the matrix page records which R computed the results, and is made by the script every widget shares (#13)", {
  lSaved <- lSavedMatrix()
  expect_identical(lSaved$payload$lStatistics$computed_by, lSaved$widget$x$lStatistics$computed_by)
  strScripts <- strWidgetScripts("Widget_CorrelationMatrix")
  expect_match(strScripts, "BioViz.correlationMatrix(chart, settings)", fixed = TRUE)
  expect_match(strScripts, "BioViz.r.createConnection({ results: statistics.results, computedBy: statistics.computed_by })", fixed = TRUE)
  for (strNever in c("browser", "webr", "sourceUrl", "http", "fetch(", "import(")) {
    expect_false(grepl(strNever, strScripts, fixed = TRUE), label = paste("the scripts name", strNever))
  }
})

test_that("the matrix widget saves as one self-contained file that holds both bundles and loads nothing (#13)", {
  if (!bPandoc()) {
    if (bSourceTree()) {
      fail("pandoc was not found: htmlwidgets::saveWidget(selfcontained = TRUE) needs it, and so does this test")
    }
    skip("pandoc is not available to save a self-contained page")
  }
  strDir <- tempfile("Widget_CorrelationMatrix")
  dir.create(strDir)
  strFile <- file.path(strDir, "correlation-matrix.html")
  htmlwidgets::saveWidget(lMatrixWidget(), file = strFile, selfcontained = TRUE)
  strPage <- paste(readLines(strFile, warn = FALSE), collapse = "\n")
  expect_match(strPage, "var BioViz = ", fixed = TRUE)
  expect_match(strPage, "var SafetyViz = ", fixed = TRUE)
  expect_match(strPage, "name: 'Widget_CorrelationMatrix'", fixed = TRUE)
  expect_match(strPage, "\"name\":\"Analyze_CorrelationMatrix\"", fixed = TRUE)
  expect_false(grepl("<(script|img|iframe|link)[^>]*\\s(src|href)\\s*=", strPage, perl = TRUE))
  expect_false(grepl("correlation-matrix_files", strPage, fixed = TRUE))
  expect_identical(length(lPagePayload(strPage)$lStatistics$results), 133L)
})
