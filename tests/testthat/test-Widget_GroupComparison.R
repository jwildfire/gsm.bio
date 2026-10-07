# The group comparison widget (#9, #53): bio.viz's chart from R, with R's
# answers computed when the widget is made and stored in the page, so a saved
# page shows them with no R and no network. The chart draws at three levels:
# a trend tile for every biomarker, which asks R for nothing; one biomarker
# over time, with one request for the test under every visit; and one visit
# alone, with a request per panel. test-GroupComparison-page.R opens the saved
# page in a browser and holds what is stored to what the chart itself asks.

# One row per participant for a biomarker's change from Baseline to a visit, by
# arm, worked out here from the study's tables and nothing of the widget's: the
# participants who have both results, in the participant table's order.
dfChangeByArm <- function(strBiomarker, strVisit) {
  dfRows <- data.frame(
    USUBJID = Synthetic_Participants$USUBJID,
    y = nResultAt(strBiomarker, strVisit) - nResultAt(strBiomarker, Synthetic_Truth$GroupDifference$BaselineVisit),
    x = Synthetic_Participants$ARM,
    stringsAsFactors = FALSE
  )
  dfRows[!is.na(dfRows$y), ]
}

# The same rows for several visits, long: one per participant and visit, with
# the visit named, as the chart hands R the rows of one biomarker over time.
dfChangeOverTime <- function(strBiomarker, chrVisits) {
  dfLong <- do.call(rbind, lapply(chrVisits, function(strVisit) {
    dfRows <- dfChangeByArm(strBiomarker, strVisit)
    dfRows$visit <- rep(strVisit, nrow(dfRows))
    dfRows
  }))
  rownames(dfLong) <- NULL
  dfLong
}

# The view the widget tests are of: change from Baseline, by arm, with no visit
# named, so every visit is chosen. Opened on a biomarker, the chart draws it
# over time and asks for the test under each visit after Baseline in one
# request (a change at the baseline visit is the same for everyone, and is not
# tested); a click on one of those visits draws it alone and asks for its test.
chrVisitsDrawn <- function() {
  setdiff(unique(Synthetic_Results$VISIT), Synthetic_Truth$GroupDifference$BaselineVisit)
}

# For each biomarker: a panel's result for each visit after Baseline, and one
# result for the tests under its visits over time.
nPanelsForChange <- function() {
  length(chrSyntheticBiomarkers()) * length(chrVisitsDrawn())
}

nStoredForChange <- function() {
  nPanelsForChange() + length(chrSyntheticBiomarkers())
}

# The stored results of one kind: the panels' or the pictures' over time.
lOfPanels <- function(lResults) {
  Filter(function(lResult) identical(lResult$name, "Analyze_GroupDifference"), lResults)
}

lOverTime <- function(lResults) {
  Filter(function(lResult) identical(lResult$name, "Analyze_GroupDifferenceBy"), lResults)
}

chrStoredKeys <- function(lResults) {
  vapply(lResults, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
}

# The settings the page is given: the ones asked for, with the unscheduled
# visits R found named to the chart outright. The study has none.
lAsGiven <- function(lSettings) {
  c(lSettings, list(unscheduled_visit_values = list()))
}

lSyntheticWidget <- function(lSettings = lWidgetSettings(), ...) {
  Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lSettings, ...)
}

# The widget at the settings these tests open on, saved once, with the page's
# text and the payload read back out of it.
lSavedWidget <- local({
  lSaved <- NULL
  function() {
    if (is.null(lSaved)) {
      lWidget <- lSyntheticWidget()
      strPage <- strSavedPage(lWidget)
      lSaved <<- list(widget = lWidget, page = strPage, payload = lPagePayload(strPage))
    }
    lSaved
  }
})

test_that("Widget_GroupComparison returns an htmlwidget carrying the tables, the settings and the stored results (#9)", {
  lSettings <- lWidgetSettings()
  lWidget <- lSyntheticWidget()

  expect_s3_class(lWidget, c("Widget_GroupComparison", "htmlwidget"))
  expect_named(lWidget$x, c("dfResults", "dfParticipants", "lSettings", "bDebug", "bAutoWidth", "bAutoHeight", "lStatistics"))
  expect_identical(lWidget$x$dfResults, Synthetic_Results)
  expect_identical(lWidget$x$dfParticipants, Synthetic_Participants)
  expect_identical(lWidget$x$lSettings, lAsGiven(lSettings))
  expect_false(lWidget$x$bDebug)
  expect_named(lWidget$x$lStatistics, c("computed_by", "results"))

  # The participant table is optional: a group then comes from the results rows.
  dfAlone <- Synthetic_Results
  dfAlone$ARM <- Synthetic_Participants$ARM[match(dfAlone$USUBJID, Synthetic_Participants$USUBJID)]
  lAlone <- Widget_GroupComparison(dfAlone, lSettings = lSettings)
  expect_null(lAlone$x$dfParticipants)
  expect_identical(length(lAlone$x$lStatistics$results), nStoredForChange())
  expect_identical(nStoredForChange(), 60L)
  # The same participants, so the same answers.
  expect_identical(lAlone$x$lStatistics$results, lWidget$x$lStatistics$results)
})

test_that("Widget_GroupComparison passes width, height, and elementId through (#9)", {
  lWidget <- lSyntheticWidget(width = "100%", height = "600px", elementId = "group-comparison-widget", bDebug = TRUE)

  expect_identical(lWidget$width, "100%")
  expect_identical(lWidget$height, "600px")
  expect_identical(lWidget$elementId, "group-comparison-widget")
  expect_true(lWidget$x$bDebug)
  # With no size asked for the widget is as wide as its container and as tall
  # as its chart, whatever size the page it is shown in gives a widget.
  expect_false(lWidget$x$bAutoWidth || lWidget$x$bAutoHeight)
  expect_true(lSyntheticWidget()$x$bAutoWidth && lSyntheticWidget()$x$bAutoHeight)
  expect_identical(lWidget$sizingPolicy$defaultWidth, "100%")
  expect_identical(lWidget$sizingPolicy$browser$defaultWidth, "100%")
})

test_that("Widget_GroupComparison rejects invalid inputs before a page is made (#9)", {
  expect_error(Widget_GroupComparison("not a data.frame"), "dfResults is not a data.frame")
  expect_error(Widget_GroupComparison(Synthetic_Results, "nope"), "dfParticipants is not a data.frame")
  expect_error(Widget_GroupComparison(Synthetic_Results, lSettings = Synthetic_Participants), "lSettings must be a list")
  expect_error(Widget_GroupComparison(Synthetic_Results, lSettings = list("ARM")), "name every setting")
  expect_error(Widget_GroupComparison(Synthetic_Results, bDebug = "yes"), "bDebug is not a logical")
  expect_error(
    Widget_GroupComparison(Synthetic_Results, lSettings = list(measure_col = "NOT_A_COLUMN")),
    "NOT_A_COLUMN.*measure_col"
  )
  expect_error(
    Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = list(participant_id_col = "NOT_A_COLUMN")),
    "NOT_A_COLUMN.*participant_id_col"
  )
  expect_error(lSyntheticWidget(list(value_type = "delta")), "value_type.*must be one of")
  expect_error(lSyntheticWidget(list(test = "anova2")), "test.*must be one of")
  expect_error(lSyntheticWidget(list(pairwise = "yes")), "pairwise.*TRUE or FALSE")
  # The connection is the widget's to make, and only gsm.bio's function answers.
  expect_error(lSyntheticWidget(list(connection = list())), "connection.*cannot be given")
  expect_error(lSyntheticWidget(list(statistic = "my_test")), "statistic.*Analyze_GroupDifference")
  expect_error(lSyntheticWidget(list(statistic_by_visit = "my_test")), "statistic_by_visit.*Analyze_GroupDifferenceBy")
  # A column to group by that no table has is refused by the frame.
  expect_error(lSyntheticWidget(list(groups = "NOPE", group_by = "NOPE")), "no table has the column `NOPE`")
})

test_that("the widget stores, for each biomarker, a result for each visit's panel and one for its visits over time, keyed to the settings (#9, #53)", {
  lResults <- lSyntheticWidget()$x$lStatistics$results
  lPanels <- lOfPanels(lResults)
  lTimes <- lOverTime(lResults)
  chrBiomarkers <- chrSyntheticBiomarkers()
  chrVisits <- unique(Synthetic_Results$VISIT)

  # Change from Baseline: a panel, and so a result, for each visit after
  # Baseline, and none for Baseline itself; and one result for the row of tests
  # under the biomarker's visits over time. Nothing is stored for the tiles.
  expect_identical(chrVisitsDrawn(), c("Week 2", "Week 4", "Week 8", "Week 12"))
  expect_identical(length(lResults), length(lPanels) + length(lTimes))
  expect_identical(length(lPanels), length(chrBiomarkers) * length(chrVisitsDrawn()))
  expect_identical(length(lTimes), length(chrBiomarkers))
  dfKeys <- data.frame(
    measure = vapply(lPanels, function(lResult) lResult$dataId$measure, character(1)),
    visit = vapply(lPanels, function(lResult) lResult$dataId$visit, character(1))
  )
  expect_identical(anyDuplicated(dfKeys), 0L)
  expect_setequal(dfKeys$measure, chrBiomarkers)
  expect_setequal(dfKeys$visit, chrVisitsDrawn())
  expect_setequal(vapply(lTimes, function(lResult) lResult$dataId$measure, character(1)), chrBiomarkers)
  expect_identical(anyDuplicated(chrStoredKeys(lResults)), 0L)
  # Every biomarker is stored whichever the page opens on: the tiles, when
  # `start_value` names none, print no test, and a reader opens one from them.
  lFromTiles <- lSyntheticWidget(lWidgetSettings()[c("value_type", "baseline_visits", "group_by")])$x$lStatistics$results
  expect_identical(lFromTiles, lResults)
  # Visits named in the settings are the panels the page opens a biomarker on.
  # A reader reaches every other visit, and the picture over time, from there,
  # so the same views are stored.
  lWeek4 <- lSyntheticWidget(c(lWidgetSettings(), list(visits = c("Week 4", "Baseline"))))$x$lStatistics$results
  expect_setequal(chrStoredKeys(lWeek4), chrStoredKeys(lResults))
  # Against two baseline visits a change is drawn, and tested, at every visit.
  lTwo <- lSyntheticWidget(list(value_type = "change", baseline_visits = c("Baseline", "Week 2"), group_by = "ARM"))$x$lStatistics$results
  expect_identical(length(lOfPanels(lTwo)), length(chrBiomarkers) * length(chrVisits))
  expect_identical(lOverTime(lTwo)[[1]]$dataId$visits, as.list(chrVisits))
  for (lResult in lPanels) {
    expect_named(lResult, c("name", "args", "dataId", "rows", "value"))
    expect_identical(lResult$args, list(strValueCol = "y", strGroupCol = "x", strMethod = "t", bPairwise = FALSE))
    expect_identical(
      lResult$dataId[setdiff(names(lResult$dataId), c("measure", "visit"))],
      list(
        chart = "group-comparison", value_type = "change", baseline_visits = list("Baseline"),
        baseline_stat = "mean", group_by = "ARM", groups = list("Placebo", "Treatment")
      )
    )
    expect_identical(names(lResult$dataId)[1:4], c("chart", "measure", "value_type", "visit"))
  }
  # Over time: the test at each visit after Baseline, in visit order, and the
  # p-values as R gives them, which is what the settings open on.
  for (lResult in lTimes) {
    expect_named(lResult, c("name", "args", "dataId", "rows", "value"))
    expect_identical(lResult$args, list(
      strValueCol = "y", strGroupCol = "x", strByCol = "visit", strMethod = "t",
      chrBy = as.list(chrVisitsDrawn()), strPAdjust = "none"
    ))
    expect_identical(
      lResult$dataId[setdiff(names(lResult$dataId), "measure")],
      list(
        chart = "group-comparison", value_type = "change", visits = as.list(chrVisitsDrawn()),
        baseline_visits = list("Baseline"), baseline_stat = "mean", group_by = "ARM", groups = list("Placebo", "Treatment")
      )
    )
  }

  # What the settings open on is what is computed: another test, a filter, a
  # panel column and a logarithmic scale each give their own keys. The result
  # itself, not a change, is drawn at every visit, Baseline included: a panel
  # per visit and per sex. The picture over time takes no second grouping and
  # no panels, so its key has neither.
  lMore <- lSyntheticWidget(c(lWidgetSettings(), list(
    test = "wilcoxon", y_scale = "log", panel_by = "SEX", color_by = "RESPONSE", value_type = "raw",
    filters = list(list(value_col = "RESPONSE", start = "Responder"))
  ))[-2])$x$lStatistics$results
  lMorePanels <- lOfPanels(lMore)
  expect_identical(length(lMorePanels), length(chrBiomarkers) * length(chrVisits) * 2L)
  expect_identical(unique(vapply(lMore, function(lResult) lResult$args$strMethod, character(1))), "wilcoxon")
  expect_identical(lMorePanels[[1]]$dataId$filters, list(RESPONSE = list("Responder")))
  expect_true(lMorePanels[[1]]$dataId$positive_only)
  expect_identical(lMorePanels[[1]]$dataId$color_by, "RESPONSE")
  expect_setequal(vapply(lMorePanels, function(lResult) lResult$dataId$panel, character(1)), c("F", "M"))
  lMoreTimes <- lOverTime(lMore)
  expect_identical(length(lMoreTimes), length(chrBiomarkers))
  expect_identical(lMoreTimes[[1]]$dataId$visits, as.list(chrVisits))
  expect_identical(lMoreTimes[[1]]$dataId$filters, list(RESPONSE = list("Responder")))
  expect_true(lMoreTimes[[1]]$dataId$positive_only)
  expect_false(any(c("color_by", "panel_by", "panel", "visit") %in% names(lMoreTimes[[1]]$dataId)))

  # A baseline value has no visit: one result per biomarker, no `visit`, and
  # no picture over time.
  lBaseline <- lSyntheticWidget(list(value_type = "baseline", baseline_visits = "Baseline", group_by = "ARM"))$x$lStatistics$results
  expect_identical(length(lBaseline), length(chrBiomarkers))
  expect_false(any(vapply(lBaseline, function(lResult) "visit" %in% names(lResult$dataId), logical(1))))
  expect_identical(lOverTime(lBaseline), list())

  # No test asked for, or no statistics at all, and nothing is computed. With
  # no function named for the visits over time, the panels alone.
  expect_identical(lSyntheticWidget(c(lWidgetSettings(), list(test = "none")))$x$lStatistics$results, list())
  expect_identical(lSyntheticWidget(c(lWidgetSettings(), list(statistic = NULL)))$x$lStatistics$results, list())
  expect_identical(Widget_GroupComparison(Synthetic_Results)$x$lStatistics$results, list())
  lPanelsOnly <- lSyntheticWidget(c(lWidgetSettings(), list(statistic_by_visit = NULL)))$x$lStatistics$results
  expect_identical(chrStoredKeys(lPanelsOnly), chrStoredKeys(lPanels))
})

test_that("the widget stores the tests under a biomarker's visits unadjusted, and under the adjustment the settings name when they name one (#53)", {
  chrBiomarkers <- chrSyntheticBiomarkers()
  for (strAdjustment in c("holm", "BH")) {
    lResults <- lSyntheticWidget(c(lWidgetSettings(), list(visit_adjustment = strAdjustment)))$x$lStatistics$results
    lTimes <- lOverTime(lResults)
    # Two results per biomarker: the Adjust across visits control is answered
    # either way. The panels are the same as with no adjustment named.
    expect_identical(length(lTimes), 2L * length(chrBiomarkers), label = strAdjustment)
    expect_identical(length(lOfPanels(lResults)), nPanelsForChange(), label = strAdjustment)
    for (strBiomarker in chrBiomarkers) {
      lOf <- Filter(function(lResult) identical(lResult$dataId$measure, strBiomarker), lTimes)
      expect_identical(vapply(lOf, function(lResult) lResult$args$strPAdjust, character(1)), c("none", strAdjustment), label = strBiomarker)
      # The same rows and the same identity: the adjustment is an argument.
      expect_identical(lOf[[1]]$dataId, lOf[[2]]$dataId, label = strBiomarker)
      expect_identical(lOf[[1]]$rows, lOf[[2]]$rows, label = strBiomarker)
      # R's adjustment, across the visits that have a p-value.
      nUnadjusted <- vapply(lOf[[1]]$value$rows, function(lRow) lRow$p_value, numeric(1))
      nAdjusted <- vapply(lOf[[2]]$value$rows, function(lRow) lRow$p_value, numeric(1))
      expect_equal(nAdjusted, stats::p.adjust(nUnadjusted, method = strAdjustment), tolerance = 1e-12, label = strBiomarker)
      expect_identical(unique(vapply(lOf[[2]]$value$rows, function(lRow) lRow$adjustment, character(1))), strAdjustment)
    }
    # The adjustment the settings did not name is not in the page.
    expect_false(setdiff(c("holm", "BH"), strAdjustment) %in% vapply(lTimes, function(lResult) lResult$args$strPAdjust, character(1)))
  }
})

test_that("the widget names the baseline visits to the chart when the settings do not (#9)", {
  # R and the chart then cannot resolve the baseline differently, and every
  # stored result says which baseline it was computed from.
  lWidget <- lSyntheticWidget(list(value_type = "change", group_by = "ARM"))
  expect_identical(lWidget$x$lSettings$baseline_visits, "Baseline")
  expect_identical(lWidget$x$lSettings[c("value_type", "group_by")], list(value_type = "change", group_by = "ARM"))
  for (lResult in lWidget$x$lStatistics$results) {
    expect_identical(lResult$dataId$baseline_visits, list("Baseline"))
  }
  # Named settings are passed as they were given.
  lGiven <- list(value_type = "change", baseline_visits = c("Baseline", "Week 2"), group_by = "ARM")
  expect_identical(lSyntheticWidget(lGiven)$x$lSettings, lAsGiven(lGiven))
})

test_that("a result is written in the shape the chart's connection reads it in (#9)", {
  dfRows <- dfChangeByArm("IL-6", "Week 4")
  dfRows$x <- paste(dfRows$x, Synthetic_Participants$SEX[match(dfRows$USUBJID, Synthetic_Participants$USUBJID)])
  lResult <- Analyze_GroupDifference(dfRows, "y", "x", strMethod = "kruskal", bPairwise = TRUE)
  lStored <- StoredValue(lResult)

  # The same members, in the same order.
  expect_identical(names(lStored), chrResultMembers)
  # A table is an unnamed list of rows, each a named list of single values.
  expect_null(names(lStored$rows))
  expect_identical(length(lStored$rows), 6L)
  expect_identical(names(lStored$rows[[1]]), names(lResult$rows))
  expect_identical(lStored$rows[[1]]$group_1, lResult$rows$group_1[1])
  expect_identical(lStored$rows[[6]]$p_value, lResult$rows$p_value[6])
  # A table of no rows is an empty list, and NA is NULL.
  expect_identical(lStored$dropped, list())
  expect_null(lStored$reason)
  expect_null(lStored$rows[[1]]$reason)
  expect_true("reason" %in% names(lStored$rows[[1]]))
  # Counts by group stay a named list; a single value stays a single value.
  expect_identical(lStored$counts, lResult$counts)
  expect_identical(lStored$p_value, lResult$p_value)
  expect_identical(lStored$notes, lResult$notes)

  # As htmlwidgets writes it: rows, single values unboxed, NA as null.
  strJson <- as.character(jsonlite::toJSON(lStored, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
  expect_match(strJson, '"status":"ok","reason":null,"test":"kruskal"', fixed = TRUE)
  expect_match(strJson, '"statistic":[{"name":"Kruskal-Wallis chi-squared","value":', fixed = TRUE)
  expect_match(strJson, '"counts":{"Placebo F":', fixed = TRUE)
  # A table of no rows and a list are arrays. Which warnings R raises here is
  # the R version's own (with tied values R 4.3 warns and R 4.6 does not), so
  # the list is held to what this R returned and not to a sentence.
  expect_match(strJson, '"dropped":[],"warnings":[', fixed = TRUE)
  expect_match(strJson, '],"notes":["Pairwise: each pair is compared with wilcox.test()', fixed = TRUE)
  expect_identical(lStored$warnings, lResult$warnings)
  expect_identical(
    jsonlite::fromJSON(strJson, simplifyVector = FALSE)$warnings, lResult$warnings
  )
  expect_match(strJson, '"status":"ok","reason":null,"warning":', fixed = TRUE)

  # The rules, on values a result could hold.
  expect_identical(StoredValue(c(a = 1L, b = 2L)), list(a = 1L, b = 2L))
  expect_identical(StoredValue(c(1.5, NA)), list(1.5, NULL))
  expect_identical(StoredValue(list("one")), list("one"))
  expect_identical(StoredValue(factor("f")), "f")
  expect_null(StoredValue(NA_character_))
  expect_identical(StoredValue(data.frame(a = 1:2, b = c("x", NA))), list(list(a = 1L, b = "x"), list(a = 2L, b = NULL)))
})

test_that("the saved page holds the stored results, each equal to a direct call of its statistics function, member by member (#9, #53)", {
  lResults <- lSavedWidget()$payload$lStatistics$results
  expect_identical(length(lResults), nStoredForChange())
  lPanels <- lOfPanels(lResults)
  lTimes <- lOverTime(lResults)
  expect_identical(length(lPanels), nPanelsForChange())
  expect_identical(length(lTimes), length(chrSyntheticBiomarkers()))

  for (lResult in lPanels) {
    strLabel <- paste(lResult$dataId$measure, lResult$dataId$visit)
    # The key reads back as it was written: single values single, lists lists.
    expect_identical(lResult$args, list(strValueCol = "y", strGroupCol = "x", strMethod = "t", bPairwise = FALSE), label = strLabel)
    expect_identical(lResult$dataId$baseline_visits, list("Baseline"), label = strLabel)
    expect_identical(lResult$dataId$groups, list("Placebo", "Treatment"), label = strLabel)

    # The rows of this key's view, worked out here from the study's tables,
    # and R's answer on them with the arguments the key carries.
    dfRows <- dfChangeByArm(lResult$dataId$measure, lResult$dataId$visit)
    expect_identical(lResult$rows, nrow(dfRows), label = paste(strLabel, "rows"))
    lAnswer <- do.call(Analyze_GroupDifference, c(list(dfRows), lResult$args))
    ExpectInPage(lResult$value, lAnswer, strLabel)
  }

  # One biomarker over time: the long rows of every visit tested, worked out
  # here, and Analyze_GroupDifferenceBy()'s answer on them.
  for (lResult in lTimes) {
    strLabel <- paste(lResult$dataId$measure, "over time")
    expect_identical(lResult$dataId$visits, as.list(chrVisitsDrawn()), label = strLabel)
    expect_identical(lResult$args$chrBy, as.list(chrVisitsDrawn()), label = strLabel)
    dfRows <- dfChangeOverTime(lResult$dataId$measure, chrVisitsDrawn())
    expect_identical(lResult$rows, nrow(dfRows), label = paste(strLabel, "rows"))
    lAnswer <- do.call(Analyze_GroupDifferenceBy, c(list(dfRows), lResult$args))
    ExpectInPage(lResult$value, lAnswer, strLabel)
    # A row per visit, each the panel's own answer for that visit.
    expect_identical(vapply(lResult$value$rows, function(lRow) lRow$by, character(1)), chrVisitsDrawn(), label = strLabel)
    for (lRow in lResult$value$rows) {
      lPanel <- Filter(function(lOne) {
        identical(lOne$dataId$measure, lResult$dataId$measure) && identical(lOne$dataId$visit, lRow$by)
      }, lPanels)[[1]]
      expect_equal(lRow$p_unadjusted, lPanel$value$p_value, tolerance = 1e-12, label = paste(strLabel, lRow$by))
      expect_identical(lRow$counts, sum(unlist(lPanel$value$counts)), label = paste(strLabel, lRow$by, "counts"))
    }
  }

  # Every status the study gives rise to went through the page.
  chrStatus <- vapply(lResults, function(lResult) lResult$value$status, character(1))
  expect_true("ok" %in% chrStatus)
  # The comparison can fail: a result is not its neighbour's answer.
  lFirst <- lPanels[[1]]
  lSecond <- lPanels[[2]]
  dfSecond <- dfChangeByArm(lSecond$dataId$measure, lSecond$dataId$visit)
  expect_gt(length(chrPageDifferences(lFirst$value, do.call(Analyze_GroupDifference, c(list(dfSecond), lSecond$args)), "a neighbour")), 0)
  expect_gt(length(chrPageDifferences(lFirst$value$reason, "a reason", "a reason")), 0)
  expect_gt(length(chrPageDifferences(lFirst$value$counts, unlist(lFirst$value$counts), "counts as a bare vector")), 0)
  dfOther <- dfChangeOverTime(lTimes[[2]]$dataId$measure, chrVisitsDrawn())
  expect_gt(length(chrPageDifferences(lTimes[[1]]$value, do.call(Analyze_GroupDifferenceBy, c(list(dfOther), lTimes[[2]]$args)), "another biomarker")), 0)
})

test_that("the saved page prints the planted difference: a test, its method and its counts (#9)", {
  lTruth <- Synthetic_Truth$GroupDifference
  lPayload <- lSavedWidget()$payload
  lFound <- Filter(function(lResult) {
    identical(lResult$dataId$measure, lTruth$Biomarker) && identical(lResult$dataId$visit, lTruth$Visit)
  }, lPayload$lStatistics$results)
  expect_identical(length(lFound), 1L)
  lValue <- lFound[[1]]$value

  dfRows <- dfChangeByArm(lTruth$Biomarker, lTruth$Visit)
  lAnswer <- Analyze_GroupDifference(dfRows, "y", "x", strMethod = "t", bPairwise = FALSE)
  expect_identical(lValue$status, "ok")
  expect_identical(lValue$method, "Welch Two Sample t-test")
  expect_equal(lValue$p_value, lAnswer$p_value, tolerance = 1e-12)
  expect_identical(lValue$counts, lAnswer$counts)
  expect_identical(lValue$counts, lapply(split(dfRows$x, dfRows$x), length))
  # The difference the study plants is inside the interval R gives. The page's
  # difference is the first group minus the second, by name; the study states
  # its own the other way round.
  lDifference <- Filter(function(lRow) lRow$name == "Difference in means", lValue$estimates)[[1]]
  expect_identical(lDifference$group, paste(rev(lTruth$Groups), collapse = " - "))
  expect_true(lDifference$lower < -lTruth$Value && -lTruth$Value < lDifference$upper)
})

test_that("the saved page holds no result under a key it was not computed for (#9)", {
  lStored <- lSavedWidget()$payload$lStatistics$results
  chrKeys <- chrStoredKeys(lStored)
  lResults <- lOfPanels(lStored)

  # One result per key, and every key is a view the settings open on.
  expect_identical(anyDuplicated(chrKeys), 0L)
  lOpening <- GroupComparison_Settings(lWidgetSettings())
  for (lResult in lStored) {
    expect_identical(lResult$args$strMethod, lOpening$test)
    expect_identical(lResult$dataId$value_type, lOpening$value_type)
    expect_identical(lResult$dataId$group_by, lOpening$group_by)
    expect_false(any(c("filters", "color_by", "panel_by", "panel", "positive_only", "unscheduled_visits") %in% names(lResult$dataId)))
  }
  for (lResult in lResults) {
    expect_false(lResult$args$bPairwise)
    # The counts under a key are the counts of that key's own rows.
    dfRows <- dfChangeByArm(lResult$dataId$measure, lResult$dataId$visit)
    expect_identical(lResult$rows, nrow(dfRows))
    if (is.list(lResult$value$counts)) {
      expect_identical(sum(unlist(lResult$value$counts)), nrow(dfRows))
    }
  }

  # A view the reader can move to that was not computed has no key in the page:
  # another test, the pairwise switch, another group, a filter, another value.
  Key <- function(lArgs = list(), lDataId = list()) {
    lKey <- lResults[[1]][c("name", "args", "dataId")]
    lKey$args[names(lArgs)] <- lArgs
    lKey$dataId[names(lDataId)] <- lDataId
    Chart_KeyText(lKey)
  }
  expect_true(Key() %in% chrKeys)
  expect_false(Key(list(strMethod = "wilcoxon")) %in% chrKeys)
  expect_false(Key(list(bPairwise = TRUE)) %in% chrKeys)
  expect_false(Key(lDataId = list(group_by = "SEX", groups = list("F", "M"))) %in% chrKeys)
  expect_false(Key(lDataId = list(filters = list(SEX = list("F")))) %in% chrKeys)
  expect_false(Key(lDataId = list(value_type = "raw")) %in% chrKeys)
  expect_false(Key(lDataId = list(positive_only = TRUE)) %in% chrKeys)
  expect_false(Key(lDataId = list(baseline_visits = list("Week 2"))) %in% chrKeys)
  expect_false(Key(lDataId = list(unscheduled_visits = TRUE)) %in% chrKeys)
  # Over time, likewise: the adjustment the settings did not name, another
  # test, or fewer visits.
  TimeKey <- function(lArgs = list(), lDataId = list()) {
    lKey <- lOverTime(lStored)[[1]][c("name", "args", "dataId")]
    lKey$args[names(lArgs)] <- lArgs
    lKey$dataId[names(lDataId)] <- lDataId
    Chart_KeyText(lKey)
  }
  expect_true(TimeKey() %in% chrKeys)
  expect_false(TimeKey(list(strPAdjust = "holm")) %in% chrKeys)
  expect_false(TimeKey(list(strMethod = "wilcoxon")) %in% chrKeys)
  expect_false(TimeKey(lDataId = list(visits = list("Week 2", "Week 4"))) %in% chrKeys)
  expect_false(TimeKey(lDataId = list(unscheduled_visits = TRUE)) %in% chrKeys)
})

test_that("the page records which R computed the results, and the binding prints it (#9)", {
  tBefore <- Sys.time()
  lBy <- lSyntheticWidget()$x$lStatistics$computed_by

  expect_named(lBy, c("r_version", "gsm_bio_version", "platform", "computed_at"))
  expect_identical(lBy$r_version, paste(R.version$major, R.version$minor, sep = "."))
  expect_identical(lBy$gsm_bio_version, as.character(utils::packageVersion("gsm.bio")))
  expect_identical(lBy$platform, R.version$platform)
  expect_match(lBy$computed_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  tComputed <- as.POSIXct(lBy$computed_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expect_true(abs(as.numeric(difftime(tComputed, tBefore, units = "secs"))) < 120)

  # It reaches the page beside the results, as single values.
  lSaved <- lSavedWidget()
  expect_identical(lSaved$payload$lStatistics$computed_by, lSaved$widget$x$lStatistics$computed_by)
  expect_identical(lSaved$payload$lStatistics$computed_by[c("r_version", "gsm_bio_version")], lBy[c("r_version", "gsm_bio_version")])

  # The binding prints it from that record, in the page and not in bio.viz.
  strBinding <- strWidgetScripts("Widget_GroupComparison")
  expect_match(strBinding, "by.r_version", fixed = TRUE)
  expect_match(strBinding, "by.gsm_bio_version", fixed = TRUE)
  expect_match(strBinding, "by.computed_at", fixed = TRUE)
  expect_match(strBinding, "gsm-bio-provenance", fixed = TRUE)
})

test_that("the page's connection is made from the stored results alone: no R and no address in it (#9)", {
  strBinding <- strWidgetScripts("Widget_GroupComparison")
  expect_match(strBinding, "BioViz.r.createConnection({ results: statistics.results, computedBy: statistics.computed_by })", fixed = TRUE)
  expect_match(strBinding, "BioViz.groupComparison(chart, settings)", fixed = TRUE)
  # Nothing that starts R in the page or fetches anything.
  for (strNever in c("browser", "webr", "sourceUrl", "http", "fetch(", "import(")) {
    expect_false(grepl(strNever, strBinding, fixed = TRUE, ignore.case = FALSE), label = paste("the binding names", strNever))
  }
  # The tables go to the chart as arrays of row objects.
  expect_match(strBinding, "HTMLWidgets.dataframeToD3", fixed = TRUE)

  # In the page the tables are written by column, for the binding to turn into
  # rows, and a missing result is null.
  lPayload <- lSavedWidget()$payload
  expect_identical(names(lPayload$dfResults), names(Synthetic_Results))
  expect_identical(length(lPayload$dfResults$STRESN), nrow(Synthetic_Results))
  expect_identical(sum(vapply(lPayload$dfResults$STRESN, is.null, logical(1))), sum(is.na(Synthetic_Results$STRESN)))
  expect_identical(unlist(lPayload$dfParticipants$USUBJID), Synthetic_Participants$USUBJID)
  expect_identical(
    lPayload$lSettings,
    list(start_value = "IL-6", value_type = "change", baseline_visits = "Baseline", group_by = "ARM", unscheduled_visit_values = list())
  )
})

test_that("the widget saves as one self-contained file that holds both bundles and loads nothing (#9)", {
  if (!bPandoc()) {
    # In the source tree a missing pandoc is a failure, never a skip: this is
    # the test that proves the saved page, and it must not read as a pass.
    if (bSourceTree()) {
      fail("pandoc was not found: htmlwidgets::saveWidget(selfcontained = TRUE) needs it, and so does this test")
    }
    skip("pandoc is not available to save a self-contained page")
  }
  strDir <- tempfile("Widget_GroupComparison")
  dir.create(strDir)
  strFile <- file.path(strDir, "group-comparison.html")
  htmlwidgets::saveWidget(lSyntheticWidget(), file = strFile, selfcontained = TRUE)

  strPage <- paste(readLines(strFile, warn = FALSE), collapse = "\n")
  expect_match(strPage, "var BioViz = ", fixed = TRUE)
  expect_match(strPage, "var SafetyViz = ", fixed = TRUE)
  expect_match(strPage, "HTMLWidgets.widget", fixed = TRUE)
  expect_match(strPage, "name: 'Widget_GroupComparison'", fixed = TRUE)
  expect_match(strPage, "\"lStatistics\":{\"computed_by\":{\"r_version\":", fixed = TRUE)
  expect_match(strPage, "\"method\":\"Welch Two Sample t-test\"", fixed = TRUE)
  expect_match(strPage, "BIO-001", fixed = TRUE)
  # One file: no script, stylesheet, image or frame is loaded from anywhere,
  # the folder htmlwidgets wrote the scripts to on the way included.
  expect_false(grepl("<(script|img|iframe|link)[^>]*\\s(src|href)\\s*=", strPage, perl = TRUE))
  expect_false(grepl("group-comparison_files", strPage, fixed = TRUE))
  expect_identical(length(lPagePayload(strPage)$lStatistics$results), nStoredForChange())
})

test_that("the group comparison widget takes a cut category: the groups low to high, stored for every panel, equal to R's answer (#18)", {
  lCut <- list(measure = "CRP", visit = "Baseline", cut = "median")
  lWidget <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = list(
    start_value = "IL-6", visits = c("Week 4", "Week 12"), value_type = "change", baseline_visits = "Baseline", group_by = lCut
  ))
  # The page is given the cut as R reads it.
  expect_identical(lWidget$x$lSettings$group_by, list(measure = "CRP", visit = "Baseline", value = "raw", cut = "median"))
  lStored <- lPagePayload(strSavedPage(lWidget))$lStatistics$results
  # Every biomarker the chart can open, by the cut: each visit after Baseline
  # alone, and its visits over time.
  expect_length(lOfPanels(lStored), 48L)
  expect_length(lOverTime(lStored), 12L)
  lResults <- Filter(function(lResult) {
    identical(lResult$dataId$measure, "IL-6") && lResult$dataId$visit %in% c("Week 4", "Week 12")
  }, lOfPanels(lStored))
  expect_length(lResults, 2L)
  # The groups, worked out here: quantile() and cut() on the study's CRP.
  nCrp <- nResultAt("CRP", "Baseline")
  nMedian <- stats::median(nCrp, na.rm = TRUE)
  strBound <- format(signif(nMedian, 4), scientific = FALSE, trim = TRUE)
  chrGroups <- enc2utf8(c(paste0("≤ ", strBound), paste0("> ", strBound)))
  chrGroup <- ifelse(is.na(nCrp), NA, ifelse(nCrp <= nMedian, chrGroups[1], chrGroups[2]))
  for (lResult in lResults) {
    expect_identical(unlist(lResult$args$chrGroups), chrGroups)
    expect_identical(unlist(lResult$dataId$groups), sort(chrGroups, method = "radix"))
    nChange <- nResultAt("IL-6", lResult$dataId$visit) - nResultAt("IL-6", "Baseline")
    dfRows <- data.frame(USUBJID = Synthetic_Participants$USUBJID, y = nChange, x = chrGroup, stringsAsFactors = FALSE)
    dfRows <- dfRows[!is.na(dfRows$y) & !is.na(dfRows$x), ]
    expect_identical(lResult$rows, nrow(dfRows))
    ExpectInPage(
      lResult$value,
      Analyze_GroupDifference(dfRows, "y", "x", strMethod = "t", bPairwise = FALSE, chrGroups = chrGroups),
      paste("the panel at", lResult$dataId$visit)
    )
    # The difference is the lower group less the higher.
    expect_identical(lResult$value$estimates[[3]]$group, paste(chrGroups[1], "-", chrGroups[2]))
  }
  # Over time the groups are handed to R low to high as well, the same at
  # every visit, and each visit's row is R's answer on that visit's rows.
  lTime <- Filter(function(lResult) identical(lResult$dataId$measure, "IL-6"), lOverTime(lStored))[[1]]
  expect_identical(unlist(lTime$args$chrGroups), chrGroups)
  expect_identical(unlist(lTime$dataId$groups), sort(chrGroups, method = "radix"))
  chrTested <- unlist(lTime$dataId$visits)
  expect_identical(chrTested, chrVisitsDrawn())
  dfLong <- do.call(rbind, lapply(chrTested, function(strVisit) {
    nChange <- nResultAt("IL-6", strVisit) - nResultAt("IL-6", "Baseline")
    dfRows <- data.frame(USUBJID = Synthetic_Participants$USUBJID, y = nChange, x = chrGroup, visit = strVisit, stringsAsFactors = FALSE)
    dfRows[!is.na(dfRows$y) & !is.na(dfRows$x), ]
  }))
  expect_identical(lTime$rows, nrow(dfLong))
  ExpectInPage(
    lTime$value,
    Analyze_GroupDifferenceBy(dfLong, "y", "x", "visit", strMethod = "t", chrGroups = chrGroups, chrBy = chrTested, strPAdjust = "none"),
    "IL-6 over time, by the cut"
  )
})

test_that("a cut category is handed to the page as R reads it, and one with no value to cut stops nothing (#18)", {
  lWidget <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = list(
    start_value = "IL-6", visits = "Week 4", value_type = "change", baseline_visits = "Baseline",
    group_by = list(measure = "CRP", visit = "Baseline", cut = 3), panel_by = list(col = "AGE", type = "number", cut = 50)
  ))
  lPage <- lPagePayload(strSavedPage(lWidget))
  expect_identical(lPage$lSettings$group_by, list(measure = "CRP", visit = "Baseline", value = "raw", cut = list(3L)))
  expect_identical(lPage$lSettings$panel_by, list(col = "AGE", type = "number", cut = list(50L)))
  expect_gt(length(lPage$lStatistics$results), 0L)
  # A visit nobody has: no value to cut, no groups, nothing stored.
  lNone <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = list(
    start_value = "IL-6", visits = "Week 4", group_by = list(measure = "CRP", visit = "Week 99", cut = "median")
  ))
  expect_identical(lNone$x$lStatistics$results, list())
})

test_that("with unscheduled rows added, the widget stores results for the visits the chart draws and no others (#53)", {
  dfResults <- dfWithUnscheduled()
  chrUnscheduled <- c("Unscheduled 1", "EARLY TERMINATION")
  expect_true(all(chrUnscheduled %in% dfResults$VISIT))
  lSettings <- list(value_type = "change", group_by = "ARM", measures = c("IL-6", "CRP"))
  VisitsOf <- function(lResults) {
    unique(unlist(lapply(lResults, function(lResult) c(lResult$dataId$visit, unlist(lResult$dataId$visits), unlist(lResult$args$chrBy)))))
  }

  # Left out, which is the default: no stored result is of an unscheduled
  # visit, and the baseline a change is measured from is Baseline, the first
  # visit drawn, though an unscheduled visit sorts before it.
  lWidget <- Widget_GroupComparison(dfResults, Synthetic_Participants, lSettings = lSettings)
  lStored <- lWidget$x$lStatistics$results
  expect_identical(VisitsOf(lStored), chrVisitsDrawn())
  expect_identical(lWidget$x$lSettings$baseline_visits, "Baseline")
  expect_false(any(vapply(lStored, function(lResult) "unscheduled_visits" %in% names(lResult$dataId), logical(1))))
  # The rows at unscheduled visits were set aside before anything was read: the
  # stored results are the ones the study gives without them.
  lPlain <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lSettings)$x$lStatistics$results
  expect_identical(lStored, lPlain)
  expect_identical(length(lStored), 2L * (length(chrVisitsDrawn()) + 1L))
  # The page is handed the unscheduled visits R found, by name, so the page
  # and R cannot find different ones; the tables go to the page whole, for the
  # reader who switches them on.
  expect_identical(lWidget$x$lSettings$unscheduled_visit_values, as.list(chrUnscheduled))
  expect_identical(nrow(lWidget$x$dfResults), nrow(dfResults))

  # Switched on: they are visits like any other, in visit order, the identity
  # of every request says they are among the rows, and the baseline is the
  # first visit of them all.
  lOn <- Widget_GroupComparison(dfResults, Synthetic_Participants, lSettings = c(lSettings, list(unscheduled_visits = TRUE)))
  lOnStored <- lOn$x$lStatistics$results
  expect_identical(lOn$x$lSettings$baseline_visits, "Unscheduled 1")
  chrDrawn <- c("Baseline", "Week 2", "Week 4", "EARLY TERMINATION", "Week 8", "Week 12")
  expect_setequal(VisitsOf(lOnStored), chrDrawn)
  expect_identical(lOverTime(lOnStored)[[1]]$dataId$visits, as.list(chrDrawn))
  expect_true(all(vapply(lOnStored, function(lResult) isTRUE(lResult$dataId$unscheduled_visits), logical(1))))
  expect_identical(length(lOnStored), 2L * (length(chrDrawn) + 1L))
  # With none in the results the identity is as it was, switched on or not.
  lNone <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = c(lSettings, list(unscheduled_visits = TRUE)))
  expect_identical(lNone$x$lStatistics$results, lPlain)

  # Named in a list, the visits are those and no others: the pattern is not
  # read, and the list is passed as it was given.
  lListed <- Widget_GroupComparison(
    dfResults, Synthetic_Participants,
    lSettings = c(lSettings, list(baseline_visits = "Baseline", unscheduled_visit_values = c("Week 2", "Unscheduled 1")))
  )
  expect_identical(lListed$x$lSettings$unscheduled_visit_values, c("Week 2", "Unscheduled 1"))
  expect_setequal(VisitsOf(lListed$x$lStatistics$results), c("Week 4", "EARLY TERMINATION", "Week 8", "Week 12"))
  # With no pattern and no list no visit is unscheduled, and nothing is named.
  lNoRule <- Widget_GroupComparison(
    dfResults, Synthetic_Participants,
    lSettings = c(lSettings, list(baseline_visits = "Baseline", unscheduled_visit_pattern = NULL))
  )
  expect_false("unscheduled_visit_values" %in% names(lNoRule$x$lSettings))
  expect_true(all(chrUnscheduled %in% VisitsOf(lNoRule$x$lStatistics$results)))
  # A pattern R does not read as a browser does is refused, with the way out.
  expect_error(
    Widget_GroupComparison(dfResults, Synthetic_Participants, lSettings = c(lSettings, list(unscheduled_visit_pattern = "/^unsched/i"))),
    "unscheduled_visit_values"
  )
})
