# The association scatter widget (#13): bio.viz's chart from R, with R's
# coefficient and fitted line computed when the widget is made and stored in the
# page, so a saved page shows them with no R and no network.

# The view these tests are of: the planted pair, TNF-alpha against IL-10 at
# Baseline, coloured by arm, with a linear fit.
lScatterSettings <- function() {
  list(
    x = list(measure = "TNF-alpha", visit = "Baseline"),
    y = list(measure = "IL-10", visit = "Baseline"),
    baseline_visits = "Baseline",
    color_by = "ARM",
    fit = "linear"
  )
}

lScatterWidget <- function(lSettings = lScatterSettings(), ...) {
  Widget_AssociationScatter(Synthetic_Results, Synthetic_Participants, lSettings = lSettings, ...)
}

# One row per participant for two biomarkers at a visit, with the arm as the
# colour, worked out here from the study's tables and nothing of the widget's:
# the participants who have both results, in the participant table's order.
dfPairByArm <- function(strX, strY, strVisit) {
  dfRows <- data.frame(
    USUBJID = Synthetic_Participants$USUBJID,
    x = nResultAt(strX, strVisit),
    y = nResultAt(strY, strVisit),
    color = Synthetic_Participants$ARM,
    stringsAsFactors = FALSE
  )
  dfRows[!is.na(dfRows$x) & !is.na(dfRows$y), ]
}

# The widget at the settings these tests open on, saved once, with the page's
# text and the payload read back out of it.
lSavedScatter <- local({
  lSaved <- NULL
  function() {
    if (is.null(lSaved)) {
      lWidget <- lScatterWidget()
      strPage <- strSavedPage(lWidget)
      lSaved <<- list(widget = lWidget, page = strPage, payload = lPagePayload(strPage))
    }
    lSaved
  }
})

# What a stored result is of, in a few words: the function and its method.
chrStoredAs <- function(lResults) {
  vapply(lResults, function(lResult) paste(lResult$name, lResult$args$strMethod), character(1))
}

test_that("Widget_AssociationScatter returns an htmlwidget carrying the tables, the settings and the stored results (#13)", {
  lSettings <- lScatterSettings()
  lWidget <- lScatterWidget()

  expect_s3_class(lWidget, c("Widget_AssociationScatter", "htmlwidget"))
  expect_named(lWidget$x, c("dfResults", "dfParticipants", "lSettings", "bDebug", "bAutoWidth", "bAutoHeight", "lStatistics"))
  expect_identical(lWidget$x$dfResults, Synthetic_Results)
  expect_identical(lWidget$x$dfParticipants, Synthetic_Participants)
  expect_identical(lWidget$x$lSettings, lSettings)
  expect_false(lWidget$x$bDebug)
  expect_named(lWidget$x$lStatistics, c("computed_by", "results"))

  # The participant table is optional: a colour then comes from the results rows.
  dfAlone <- Synthetic_Results
  dfAlone$ARM <- Synthetic_Participants$ARM[match(dfAlone$USUBJID, Synthetic_Participants$USUBJID)]
  lAlone <- Widget_AssociationScatter(dfAlone, lSettings = lSettings)
  expect_null(lAlone$x$dfParticipants)
  # The same participants, so the same answers.
  expect_identical(lAlone$x$lStatistics$results, lWidget$x$lStatistics$results)

  # Width, height and the element's id pass through, as for every widget.
  lSized <- lScatterWidget(width = "100%", height = "600px", elementId = "scatter-widget", bDebug = TRUE)
  expect_identical(lSized[c("width", "height", "elementId")], list(width = "100%", height = "600px", elementId = "scatter-widget"))
  expect_true(lSized$x$bDebug)
  expect_false(lSized$x$bAutoWidth || lSized$x$bAutoHeight)
  expect_true(lWidget$x$bAutoWidth && lWidget$x$bAutoHeight)
  expect_identical(lWidget$sizingPolicy$defaultWidth, "100%")
})

test_that("Widget_AssociationScatter rejects invalid inputs before a page is made (#13)", {
  expect_error(Widget_AssociationScatter("not a data.frame"), "dfResults is not a data.frame")
  expect_error(Widget_AssociationScatter(Synthetic_Results, "nope"), "dfParticipants is not a data.frame")
  expect_error(Widget_AssociationScatter(Synthetic_Results, lSettings = list("ARM")), "name every setting")
  expect_error(Widget_AssociationScatter(Synthetic_Results, bDebug = "yes"), "bDebug is not a logical")
  expect_error(
    Widget_AssociationScatter(Synthetic_Results, lSettings = list(value_col = "NOT_A_COLUMN")),
    "NOT_A_COLUMN.*value_col"
  )
  expect_error(lScatterWidget(list(fit = "quadratic")), "fit.*must be one of")
  expect_error(lScatterWidget(list(method = "kendall")), "method.*must be one of")
  expect_error(lScatterWidget(list(x_scale = "sqrt")), "x_scale.*must be one of")
  # An axis is a variable, written as bio.viz writes one.
  expect_error(lScatterWidget(list(x = "IL-6")), "'x' must be a variable")
  expect_error(lScatterWidget(list(x = list(measure = "IL-6"))), "'x'.*must name its visit")
  expect_error(lScatterWidget(list(y = list(measure = "IL-6", visit = "Week 4", value = "delta"))), "'y'.*`value` must be one of")
  # The connection is the widget's to make, a widget is opened by no other
  # chart, and only gsm.bio's functions answer.
  expect_error(lScatterWidget(list(connection = list())), "connection.*cannot be given")
  expect_error(lScatterWidget(list(back = list(label = "Back"))), "back.*cannot be given")
  expect_error(lScatterWidget(list(statistic = "my_cor")), "statistic.*Analyze_Correlation")
  expect_error(lScatterWidget(list(fit_statistic = "my_fit")), "fit_statistic.*Analyze_Fit")
  # A column to colour by that no table has is refused by the frame.
  expect_error(lScatterWidget(list(groups = "NOPE", color_by = "NOPE")), "no table has the column `NOPE`")
})

test_that("the widget stores both coefficients and both lines for each panel of the view the settings open on (#13)", {
  lResults <- lScatterWidget()$x$lStatistics$results

  # One panel: four results, the coefficient and the line the settings open on
  # first, all of the same rows.
  expect_identical(
    chrStoredAs(lResults),
    c("Analyze_Correlation pearson", "Analyze_Fit linear", "Analyze_Fit smooth", "Analyze_Correlation spearman")
  )
  lDataId <- list(
    chart = "association-scatter",
    x = list(measure = "TNF-alpha", value = "raw", visit = "Baseline"),
    y = list(measure = "IL-10", value = "raw", visit = "Baseline"),
    baseline_visits = list("Baseline"), baseline_stat = "mean",
    color_by = "ARM", groups = list("Placebo", "Treatment")
  )
  for (lResult in lResults) {
    expect_named(lResult, c("name", "args", "dataId", "rows", "value"))
    expect_identical(lResult$dataId, lDataId)
    expect_identical(lResult$rows, 200L)
    expect_identical(lResult$args[c("strXCol", "strYCol", "strGroupCol")], list(strXCol = "x", strYCol = "y", strGroupCol = "color"))
    expect_identical(lResult$value$status, "ok")
  }

  # Whichever coefficient and line the settings open on, the same four are stored.
  lOther <- lScatterWidget(c(lScatterSettings()[1:4], list(fit = "none", method = "spearman")))$x$lStatistics$results
  expect_setequal(chrStoredAs(lOther), chrStoredAs(lResults))
  expect_identical(chrStoredAs(lOther)[1], "Analyze_Correlation spearman")

  # A panel column: four for each panel, each of its own rows.
  lPanels <- lScatterWidget(c(lScatterSettings(), list(panel_by = "SEX")))$x$lStatistics$results
  expect_identical(length(lPanels), 8L)
  expect_identical(as.vector(table(vapply(lPanels, function(lResult) lResult$dataId$panel, character(1)))), c(4L, 4L))
  expect_identical(sum(unique(vapply(lPanels, function(lResult) lResult$rows, integer(1)))), 200L)

  # No colour: no group in the arguments or the identity. A filter the
  # settings open on and a logarithmic axis are part of the identity.
  lPlain <- lScatterWidget(list(
    x = list(measure = "CRP", visit = "Baseline"), y = list(col = "AGE"), x_scale = "log",
    filters = list(list(value_col = "SEX", start = "F"))
  ))$x$lStatistics$results
  expect_identical(length(lPlain), 4L)
  expect_identical(lPlain[[1]]$args, list(strXCol = "x", strYCol = "y", strMethod = "pearson"))
  expect_identical(
    lPlain[[1]]$dataId,
    list(
      chart = "association-scatter", x = list(measure = "CRP", value = "raw", visit = "Baseline"), y = list(col = "AGE"),
      baseline_visits = list("Baseline"), baseline_stat = "mean", filters = list(SEX = list("F")), x_scale = "log"
    )
  )
  expect_identical(lPlain[[1]]$rows, sum(Synthetic_Participants$SEX == "F"))

  # With no function named for the coefficient or for the line, that one is not
  # stored; with neither, nothing is.
  expect_identical(chrStoredAs(lScatterWidget(c(lScatterSettings(), list(statistic = NULL)))$x$lStatistics$results), c("Analyze_Fit linear", "Analyze_Fit smooth"))
  expect_identical(
    chrStoredAs(lScatterWidget(c(lScatterSettings(), list(fit_statistic = NULL)))$x$lStatistics$results),
    c("Analyze_Correlation pearson", "Analyze_Correlation spearman")
  )
  expect_identical(lScatterWidget(c(lScatterSettings(), list(statistic = NULL, fit_statistic = NULL)))$x$lStatistics$results, list())
  # A change read at the baseline visit draws nothing, so nothing is stored.
  expect_identical(
    lScatterWidget(list(x = list(measure = "IL-6", visit = "Baseline", value = "change"), baseline_visits = "Baseline"))$x$lStatistics$results,
    list()
  )
  # With nothing named the chart opens on the first two biomarkers, and the
  # baseline visit is named to it.
  lDefault <- Widget_AssociationScatter(Synthetic_Results, Synthetic_Participants)
  expect_identical(lDefault$x$lSettings, list(baseline_visits = "Baseline"))
  expect_identical(lDefault$x$lStatistics$results[[1]]$dataId$x, list(measure = "CRP", value = "raw", visit = "Baseline"))
  expect_identical(lDefault$x$lStatistics$results[[1]]$dataId$y, list(measure = "D-dimer", value = "raw", visit = "Baseline"))
})

test_that("the saved scatter page holds the stored results, equal to the R functions' answers member by member (#13)", {
  lResults <- lSavedScatter()$payload$lStatistics$results
  expect_identical(length(lResults), 4L)

  # The rows of the view, worked out here from the study's tables, and R's
  # answers on them with the arguments each key carries.
  dfRows <- dfPairByArm("TNF-alpha", "IL-10", "Baseline")
  lFunctions <- list(Analyze_Correlation = Analyze_Correlation, Analyze_Fit = Analyze_Fit)
  for (lResult in lResults) {
    strLabel <- paste(lResult$name, lResult$args$strMethod)
    # The key reads back as it was written: single values single, lists lists.
    expect_identical(lResult$args[c("strXCol", "strYCol", "strGroupCol")], list(strXCol = "x", strYCol = "y", strGroupCol = "color"), label = strLabel)
    expect_identical(lResult$dataId$x, list(measure = "TNF-alpha", value = "raw", visit = "Baseline"), label = strLabel)
    expect_identical(lResult$dataId$groups, list("Placebo", "Treatment"), label = strLabel)
    expect_identical(lResult$rows, nrow(dfRows), label = paste(strLabel, "rows"))
    lAnswer <- do.call(lFunctions[[lResult$name]], c(list(dfRows), lResult$args))
    ExpectInPage(lResult$value, lAnswer, strLabel)
  }
  expect_setequal(
    chrStoredAs(lResults),
    c("Analyze_Correlation pearson", "Analyze_Correlation spearman", "Analyze_Fit linear", "Analyze_Fit smooth")
  )

  # What the page prints and draws is there: the coefficient of everyone and of
  # each arm, the slope and intercept of each line, and the line's points.
  lPearson <- lResults[[which(chrStoredAs(lResults) == "Analyze_Correlation pearson")]]$value
  lLinear <- lResults[[which(chrStoredAs(lResults) == "Analyze_Fit linear")]]$value
  expect_identical(lPearson$estimates[[1]]$name, "cor")
  expect_identical(vapply(lPearson$rows, function(lRow) lRow$group, character(1)), c("Placebo", "Treatment"))
  expect_identical(
    vapply(lLinear$estimates, function(lRow) paste(lRow$name, if (is.null(lRow$group)) "everyone" else lRow$group), character(1)),
    c("Intercept everyone", "Slope everyone", "Intercept Placebo", "Slope Placebo", "Intercept Treatment", "Slope Treatment")
  )
  # Fifty points a line, the line of everyone first, with no group.
  expect_identical(length(lLinear$rows), 150L)
  expect_null(lLinear$rows[[1]]$group)
  expect_true(all(c("x", "fit", "lower", "upper") %in% names(lLinear$rows[[1]])))
  # The planted correlation is inside the interval the page prints.
  lTruth <- Synthetic_Truth$Correlation
  expect_true(lPearson$estimates[[1]]$lower < lTruth$Value && lTruth$Value < lPearson$estimates[[1]]$upper)

  # The comparison can fail: one result is not another's answer.
  expect_gt(length(chrPageDifferences(lPearson, Analyze_Correlation(dfRows, "x", "y", strMethod = "spearman", strGroupCol = "color"), "the other coefficient")), 0)
  expect_gt(length(chrPageDifferences(lLinear, Analyze_Fit(dfRows, "x", "y", strMethod = "linear"), "the line with no colour")), 0)
})

test_that("the saved scatter page holds no result under a key it was not computed for (#13)", {
  lResults <- lSavedScatter()$payload$lStatistics$results
  chrKeys <- vapply(lResults, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))

  # One result per key, and all four are of the one view: the same identity.
  expect_identical(anyDuplicated(chrKeys), 0L)
  expect_identical(length(unique(lapply(lResults, function(lResult) lResult$dataId))), 1L)
  expect_false(any(c("filters", "panel_by", "panel", "x_scale", "y_scale") %in% names(lResults[[1]]$dataId)))

  # A view the reader can move to that was not computed has no key in the page:
  # another variable, value type or visit, another colour or none, a panel
  # column, a filter, a logarithmic axis, another baseline.
  Key <- function(lArgs = list(), lDataId = list(), chrDrop = character(0)) {
    lKey <- lResults[[1]][c("name", "args", "dataId")]
    lKey$args[names(lArgs)] <- lArgs
    lKey$dataId[names(lDataId)] <- lDataId
    lKey$dataId[chrDrop] <- NULL
    Chart_KeyText(lKey)
  }
  expect_true(Key() %in% chrKeys)
  # The other coefficient is a view that was computed.
  expect_true(Key(list(strMethod = "spearman")) %in% chrKeys)
  expect_false(Key(list(strMethod = "kendall")) %in% chrKeys)
  expect_false(Key(lDataId = list(x = list(measure = "IL-6", value = "raw", visit = "Baseline"))) %in% chrKeys)
  expect_false(Key(lDataId = list(x = list(measure = "TNF-alpha", value = "raw", visit = "Week 4"))) %in% chrKeys)
  expect_false(Key(lDataId = list(y = list(measure = "IL-10", value = "change", visit = "Week 4"))) %in% chrKeys)
  expect_false(Key(lDataId = list(color_by = "SEX", groups = list("F", "M"))) %in% chrKeys)
  expect_false(Key(list(strGroupCol = NULL), chrDrop = c("color_by", "groups")) %in% chrKeys)
  expect_false(Key(lDataId = list(panel_by = "SEX", panel = "F")) %in% chrKeys)
  expect_false(Key(lDataId = list(filters = list(SEX = list("F")))) %in% chrKeys)
  expect_false(Key(lDataId = list(x_scale = "log")) %in% chrKeys)
  expect_false(Key(lDataId = list(y_scale = "log")) %in% chrKeys)
  expect_false(Key(lDataId = list(baseline_visits = list("Week 2"))) %in% chrKeys)
})

test_that("the scatter page records which R computed the results, and is made by the script every widget shares (#13)", {
  lSaved <- lSavedScatter()
  lBy <- lSaved$payload$lStatistics$computed_by
  expect_named(lBy, c("r_version", "gsm_bio_version", "platform", "computed_at"))
  expect_identical(lBy$r_version, paste(R.version$major, R.version$minor, sep = "."))
  expect_identical(lBy$gsm_bio_version, as.character(utils::packageVersion("gsm.bio")))
  expect_identical(lBy, lSaved$widget$x$lStatistics$computed_by)

  strScripts <- strWidgetScripts("Widget_AssociationScatter")
  expect_match(strScripts, "name: 'Widget_AssociationScatter'", fixed = TRUE)
  expect_match(strScripts, "BioViz.associationScatter(chart, settings)", fixed = TRUE)
  expect_match(strScripts, "BioViz.r.createConnection({ results: statistics.results })", fixed = TRUE)
  expect_match(strScripts, "gsm-bio-provenance", fixed = TRUE)
  expect_match(strScripts, "by.r_version", fixed = TRUE)
  # Nothing that starts R in the page or fetches anything.
  for (strNever in c("browser", "webr", "sourceUrl", "http", "fetch(", "import(")) {
    expect_false(grepl(strNever, strScripts, fixed = TRUE), label = paste("the scripts name", strNever))
  }
  # The binding itself names its chart and nothing more: the rest is shared.
  chrBinding <- readLines(system.file("htmlwidgets", "Widget_AssociationScatter.js", package = "gsm.bio"), warn = FALSE)
  expect_lt(length(chrBinding), 12)

  # In the page the tables are written by column, and the settings as given.
  expect_identical(names(lSaved$payload$dfResults), names(Synthetic_Results))
  expect_identical(lSaved$payload$lSettings$x, list(measure = "TNF-alpha", visit = "Baseline"))
  expect_identical(lSaved$payload$lSettings[c("color_by", "fit")], list(color_by = "ARM", fit = "linear"))
})

test_that("the scatter widget saves as one self-contained file that holds both bundles and loads nothing (#13)", {
  if (!bPandoc()) {
    # In the source tree a missing pandoc is a failure, never a skip: this is
    # the test that proves the saved page, and it must not read as a pass.
    if (bSourceTree()) {
      fail("pandoc was not found: htmlwidgets::saveWidget(selfcontained = TRUE) needs it, and so does this test")
    }
    skip("pandoc is not available to save a self-contained page")
  }
  strDir <- tempfile("Widget_AssociationScatter")
  dir.create(strDir)
  strFile <- file.path(strDir, "association-scatter.html")
  htmlwidgets::saveWidget(lScatterWidget(), file = strFile, selfcontained = TRUE)

  strPage <- paste(readLines(strFile, warn = FALSE), collapse = "\n")
  expect_match(strPage, "var BioViz = ", fixed = TRUE)
  expect_match(strPage, "var SafetyViz = ", fixed = TRUE)
  expect_match(strPage, "window.GsmBioWidget = ", fixed = TRUE)
  expect_match(strPage, "name: 'Widget_AssociationScatter'", fixed = TRUE)
  expect_match(strPage, "\"lStatistics\":{\"computed_by\":{\"r_version\":", fixed = TRUE)
  expect_match(strPage, "\"method\":\"Pearson's product-moment correlation\"", fixed = TRUE)
  expect_match(strPage, "\"method\":\"Linear regression\"", fixed = TRUE)
  # One file: no script, stylesheet, image or frame is loaded from anywhere,
  # the folder htmlwidgets wrote the scripts to on the way included.
  expect_false(grepl("<(script|img|iframe|link)[^>]*\\s(src|href)\\s*=", strPage, perl = TRUE))
  expect_false(grepl("association-scatter_files", strPage, fixed = TRUE))
  expect_identical(length(lPagePayload(strPage)$lStatistics$results), 4L)
})
