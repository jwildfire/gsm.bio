# The biomarker screen widget (#16): bio.viz's screen from R, with R's rows
# computed when the widget is made and stored in the page, and with them what
# the chart each row opens asks, so a saved page shows the screen and every
# row's chart with no R and no network.

lScreenColumns <- function() {
  list(
    list(value_col = "ARM", label = "Arm"),
    list(value_col = "SEX", label = "Sex"),
    list(value_col = "RESPONSE", label = "Response")
  )
}

lScreenSettings <- function() {
  list(
    visit = "Week 4", value_type = "change", group_by = "ARM", baseline_visits = "Baseline",
    groups = lScreenColumns(), filters = lScreenColumns(), group_comparison = list(groups = lScreenColumns())
  )
}

lScreenWidget <- function(lSettings = lScreenSettings(), ...) {
  Widget_BiomarkerScreen(Synthetic_Results, Synthetic_Participants, lSettings = lSettings, ...)
}

# The screen's biomarkers in the Biomarker control's order: by name, with
# numbers inside a name as numbers.
chrScreenBiomarkers <- function() {
  c("CRP", "D-dimer", "Ferritin", "IFN-gamma", "IL-1beta", "IL-2", "IL-6", "IL-8", "IL-10", "LDH", "TNF-alpha", "VEGF")
}

nChangeAt <- function(strBiomarker, strVisit) {
  nResultAt(strBiomarker, strVisit) - nResultAt(strBiomarker, "Baseline")
}

# The screen's frame for the change to Week 4 by arm, worked out here from the
# study's tables and nothing of the widget's: one row per participant who has
# any of the changes, one column per biomarker, NA where the participant has
# none, and the arm.
dfScreenFrame <- function() {
  dfFrame <- data.frame(USUBJID = Synthetic_Participants$USUBJID, stringsAsFactors = FALSE)
  for (strBiomarker in chrScreenBiomarkers()) {
    dfFrame[[strBiomarker]] <- nChangeAt(strBiomarker, "Week 4")
  }
  dfFrame$ARM <- Synthetic_Participants$ARM
  dfFrame <- dfFrame[rowSums(!is.na(dfFrame[chrScreenBiomarkers()])) > 0, ]
  rownames(dfFrame) <- NULL
  dfFrame
}

# The rows a row's group comparison hands R: the participants with the change
# and an arm, as the value and the group.
dfRowRows <- function(strBiomarker) {
  dfRows <- data.frame(
    USUBJID = Synthetic_Participants$USUBJID, y = nChangeAt(strBiomarker, "Week 4"), x = Synthetic_Participants$ARM,
    stringsAsFactors = FALSE
  )
  dfRows[!is.na(dfRows$y), ]
}

lSavedScreen <- local({
  lSaved <- NULL
  function() {
    if (is.null(lSaved)) {
      lWidget <- lScreenWidget()
      strPage <- strSavedPage(lWidget)
      lSaved <<- list(widget = lWidget, page = strPage, payload = lPagePayload(strPage))
    }
    lSaved
  }
})

test_that("Widget_BiomarkerScreen returns an htmlwidget carrying the tables, the settings and the stored results (#16)", {
  lWidget <- lScreenWidget()
  expect_s3_class(lWidget, c("Widget_BiomarkerScreen", "htmlwidget"))
  expect_named(lWidget$x, c("dfResults", "dfParticipants", "lSettings", "bDebug", "bAutoWidth", "bAutoHeight", "lStatistics"))
  expect_identical(lWidget$x$dfResults, Synthetic_Results)
  expect_identical(lWidget$x$dfParticipants, Synthetic_Participants)
  expect_identical(lWidget$x$lSettings, lScreenSettings())
  expect_named(lWidget$x$lStatistics, c("computed_by", "results"))
  lSized <- lScreenWidget(width = "100%", height = "700px", elementId = "screen-widget", bDebug = TRUE)
  expect_identical(lSized[c("width", "height", "elementId")], list(width = "100%", height = "700px", elementId = "screen-widget"))
  expect_true(lSized$x$bDebug)
  # No settings for the chart a row opens is none, written as none.
  lEmpty <- lScreenWidget(c(lScreenSettings()[c("visit", "value_type")], list(group_comparison = list(), association_scatter = list())))
  expect_true(all(c("group_comparison", "association_scatter") %in% names(lEmpty$x$lSettings)))
  expect_null(lEmpty$x$lSettings$group_comparison)
  expect_null(lEmpty$x$lSettings$association_scatter)
  # The baseline is named to the chart outright.
  expect_identical(lScreenWidget(list(visit = "Week 4"))$x$lSettings$baseline_visits, "Baseline")
})

test_that("Widget_BiomarkerScreen rejects invalid inputs before a page is made (#16)", {
  expect_error(Widget_BiomarkerScreen("not a data.frame"), "dfResults is not a data.frame")
  expect_error(Widget_BiomarkerScreen(Synthetic_Results, lSettings = list(value_col = "NOT_A_COLUMN")), "NOT_A_COLUMN.*value_col")
  expect_error(lScreenWidget(list(comparison = "ratio")), "comparison.*must be one of")
  expect_error(lScreenWidget(list(adjustment = "bonferroni")), "adjustment.*must be one of")
  expect_error(lScreenWidget(list(levels = "Placebo")), "levels.*two groups")
  expect_error(lScreenWidget(list(statistic = "my_screen")), "statistic.*Analyze_Screen")
  expect_error(lScreenWidget(list(connection = list())), "connection.*cannot be given")
  expect_error(lScreenWidget(list(group_comparison = "t")), "group_comparison.*named list")
  # What the screen hands the chart a row opens is the screen's to set.
  expect_error(lScreenWidget(list(group_comparison = list(test = "wilcoxon"))), "group_comparison.*cannot name 'test'")
  expect_error(lScreenWidget(list(association_scatter = list(y = list(col = "AGE"), back = list()))), "cannot name 'y', 'back'")
  # And what the page set for that chart is checked as that chart checks it.
  expect_error(lScreenWidget(list(group_comparison = list(y_scale = "square"))), "y_scale.*must be one of")
  expect_error(
    lScreenWidget(list(comparison = "correlation", association_scatter = list(fit = "quadratic"))),
    "fit.*must be one of"
  )
})

test_that("the widget stores the screen and, for every row, what the chart it opens asks (#16)", {
  lResults <- lScreenWidget()$x$lStatistics$results
  chrBiomarkers <- chrScreenBiomarkers()

  # One screen, and one group comparison for each row.
  expect_identical(length(lResults), 1L + length(chrBiomarkers))
  lScreen <- lResults[[1]]
  expect_identical(lScreen$name, "Analyze_Screen")
  expect_identical(lScreen$args, list(
    chrCols = as.list(chrBiomarkers), strComparison = "difference", strGroupCol = "ARM",
    chrGroups = list("Placebo", "Treatment"), strPAdjust = "BH"
  ))
  expect_identical(lScreen$dataId, list(
    chart = "biomarker-screen", value_type = "change", visit = "Week 4", baseline_visits = list("Baseline"), baseline_stat = "mean"
  ))
  expect_identical(lScreen$rows, nrow(dfScreenFrame()))

  for (iRow in seq_along(chrBiomarkers)) {
    lResult <- lResults[[iRow + 1L]]
    expect_identical(lResult$name, "Analyze_GroupDifference")
    # Welch's test, on one visit, of the two groups: what the chart asks as it opens.
    expect_identical(lResult$args, list(strValueCol = "y", strGroupCol = "x", strMethod = "t", bPairwise = FALSE))
    expect_identical(lResult$dataId, list(
      chart = "group-comparison", measure = chrBiomarkers[iRow], value_type = "change", visit = "Week 4",
      baseline_visits = list("Baseline"), baseline_stat = "mean", group_by = "ARM", groups = list("Placebo", "Treatment")
    ))
  }

  # The screen's filters are every row's chart's too.
  lFiltered <- lScreenWidget(c(
    lScreenSettings()[setdiff(names(lScreenSettings()), "filters")],
    list(filters = list(list(value_col = "SEX", start = "F")))
  ))$x$lStatistics$results
  expect_true(all(vapply(lFiltered, function(lResult) identical(lResult$dataId$filters, list(SEX = list("F"))), logical(1))))
  # A correlation: one screen, and a scatter for every biomarker but the variable.
  lCorrelation <- lScreenWidget(list(
    comparison = "correlation", visit = "Baseline", baseline_visits = "Baseline",
    with = list(measure = "IL-10", value = "raw", visit = "Baseline"), method = "spearman",
    association_scatter = list(fit = "linear")
  ))$x$lStatistics$results
  expect_identical(
    table(vapply(lCorrelation, function(lResult) lResult$name, character(1))),
    table(c("Analyze_Screen", rep(c("Analyze_Correlation", "Analyze_Fit"), each = 11)))
  )
  for (lResult in Filter(function(lResult) lResult$name == "Analyze_Correlation", lCorrelation)) {
    expect_identical(lResult$args$strMethod, "spearman")
    expect_identical(lResult$dataId$y, list(measure = "IL-10", value = "raw", visit = "Baseline"))
    expect_false(identical(lResult$dataId$x$measure, "IL-10"))
  }
  # No screen, nothing stored: no function named, or a change at the baseline visit.
  expect_identical(lScreenWidget(c(lScreenSettings(), list(statistic = NULL)))$x$lStatistics$results, list())
  expect_identical(lScreenWidget(list(value_type = "change", visit = "Baseline", baseline_visits = "Baseline"))$x$lStatistics$results, list())
})

test_that("the saved screen page holds the stored rows, equal to Analyze_Screen's answer member by member (#16)", {
  lResults <- lSavedScreen()$payload$lStatistics$results
  expect_identical(length(lResults), 13L)

  # The screen, on its frame worked out here.
  dfFrame <- dfScreenFrame()
  lScreen <- lResults[[1]]
  lAnswer <- do.call(Analyze_Screen, c(list(dfFrame), lScreen$args))
  ExpectInPage(lScreen$value, lAnswer, "the screen")
  # Every row with its estimate, interval, and raw and adjusted p-values.
  expect_identical(vapply(lScreen$value$rows, function(lRow) lRow$biomarker, character(1)), chrScreenBiomarkers())
  for (lRow in lScreen$value$rows) {
    expect_true(all(c("estimate", "lower", "upper", "p_unadjusted", "p_value") %in% names(lRow)), label = lRow$biomarker)
  }

  # Every row's chart, on that biomarker's rows worked out here.
  for (lResult in lResults[-1]) {
    strBiomarker <- lResult$dataId$measure
    dfRows <- dfRowRows(strBiomarker)
    expect_identical(lResult$rows, nrow(dfRows), label = paste(strBiomarker, "rows"))
    ExpectInPage(lResult$value, do.call(Analyze_GroupDifference, c(list(dfRows), lResult$args)), strBiomarker)
  }

  # The planted difference: its row is the one whose adjusted p-value is
  # smallest, and its chart's Welch p-value is its raw one.
  lTruth <- Synthetic_Truth$GroupDifference
  dfRows <- lAnswer$rows
  expect_identical(dfRows$biomarker[which.min(dfRows$p_value)], lTruth$Biomarker)
  lPlanted <- Filter(function(lResult) identical(lResult$dataId$measure, lTruth$Biomarker), lResults[-1])[[1]]$value
  expect_equal(lPlanted$p_value, dfRows$p_unadjusted[dfRows$biomarker == lTruth$Biomarker], tolerance = 1e-12)
  # The comparison can fail: the screen of another frame is not this answer.
  dfOther <- dfFrame
  dfOther$ARM <- rev(dfOther$ARM)
  expect_gt(length(chrPageDifferences(lScreen$value, do.call(Analyze_Screen, c(list(dfOther), lScreen$args)), "another frame")), 0)
})

test_that("the saved screen page holds no result under a key it was not computed for (#16)", {
  lResults <- lSavedScreen()$payload$lStatistics$results
  chrKeys <- vapply(lResults, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  expect_identical(anyDuplicated(chrKeys), 0L)

  Key <- function(iResult, lArgs = list(), lDataId = list()) {
    lKey <- lResults[[iResult]][c("name", "args", "dataId")]
    lKey$args[names(lArgs)] <- lArgs
    lKey$dataId[names(lDataId)] <- lDataId
    Chart_KeyText(lKey)
  }
  # The screen by another adjustment, of the groups the other way round, of
  # another column, at another visit, as a result, or under a filter.
  expect_true(Key(1) %in% chrKeys)
  expect_false(Key(1, list(strPAdjust = "holm")) %in% chrKeys)
  expect_false(Key(1, list(chrGroups = list("Treatment", "Placebo"))) %in% chrKeys)
  expect_false(Key(1, list(strGroupCol = "SEX", chrGroups = list("F", "M"))) %in% chrKeys)
  expect_false(Key(1, lDataId = list(visit = "Week 8")) %in% chrKeys)
  expect_false(Key(1, lDataId = list(value_type = "raw")) %in% chrKeys)
  expect_false(Key(1, lDataId = list(filters = list(SEX = list("F")))) %in% chrKeys)
  # A correlation is no result of this page.
  expect_false(Key(1, list(strComparison = "correlation")) %in% chrKeys)
  # A row's chart with another test, at another visit, of every group, or
  # under a filter.
  expect_true(Key(2) %in% chrKeys)
  expect_false(Key(2, list(strMethod = "wilcoxon")) %in% chrKeys)
  expect_false(Key(2, lDataId = list(visit = "Week 8")) %in% chrKeys)
  expect_false(Key(2, lDataId = list(groups = NULL)) %in% chrKeys)
  expect_false(Key(2, lDataId = list(filters = list(SEX = list("F")))) %in% chrKeys)
  expect_false(any(vapply(lResults, function(lResult) lResult$name %in% c("Analyze_Correlation", "Analyze_Fit"), logical(1))))
})

test_that("the screen page records which R computed the results, and is made by the script every widget shares (#16)", {
  lSaved <- lSavedScreen()
  expect_identical(lSaved$payload$lStatistics$computed_by, lSaved$widget$x$lStatistics$computed_by)
  strScripts <- strWidgetScripts("Widget_BiomarkerScreen")
  expect_match(strScripts, "BioViz.biomarkerScreen(chart, settings)", fixed = TRUE)
  expect_match(strScripts, "BioViz.r.createConnection({ results: statistics.results })", fixed = TRUE)
  for (strNever in c("browser", "webr", "sourceUrl", "http", "fetch(", "import(")) {
    expect_false(grepl(strNever, strScripts, fixed = TRUE), label = paste("the scripts name", strNever))
  }
})

test_that("the screen widget saves as one self-contained file that holds both bundles and loads nothing (#16)", {
  if (!bPandoc()) {
    if (bSourceTree()) {
      fail("pandoc was not found: htmlwidgets::saveWidget(selfcontained = TRUE) needs it, and so does this test")
    }
    skip("pandoc is not available to save a self-contained page")
  }
  strDir <- tempfile("Widget_BiomarkerScreen")
  dir.create(strDir)
  strFile <- file.path(strDir, "biomarker-screen.html")
  htmlwidgets::saveWidget(lScreenWidget(), file = strFile, selfcontained = TRUE)
  strPage <- paste(readLines(strFile, warn = FALSE), collapse = "\n")
  expect_match(strPage, "var BioViz = ", fixed = TRUE)
  expect_match(strPage, "var SafetyViz = ", fixed = TRUE)
  expect_match(strPage, "name: 'Widget_BiomarkerScreen'", fixed = TRUE)
  expect_match(strPage, "\"name\":\"Analyze_Screen\"", fixed = TRUE)
  expect_false(grepl("<(script|img|iframe|link)[^>]*\\s(src|href)\\s*=", strPage, perl = TRUE))
  expect_false(grepl("biomarker-screen_files", strPage, fixed = TRUE))
  expect_identical(length(lPagePayload(strPage)$lStatistics$results), 13L)
})
