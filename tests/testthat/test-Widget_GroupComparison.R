# The group comparison widget (#9): bio.viz's chart from R, with R's answers
# computed when the widget is made and stored in the page, so a saved page
# shows them with no R and no network.

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

# The view the widget tests are of: change from Baseline, by arm, with no visit
# named, so every visit is chosen. Opened on a biomarker, the chart draws one
# panel per visit after Baseline (a change at the baseline visit is the same for
# everyone, and is not drawn) and asks for one test per panel.
chrVisitsDrawn <- function() {
  setdiff(unique(Synthetic_Results$VISIT), Synthetic_Truth$GroupDifference$BaselineVisit)
}

nStoredForChange <- function() {
  length(chrSyntheticBiomarkers()) * length(chrVisitsDrawn())
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
  expect_identical(lWidget$x$lSettings, lSettings)
  expect_false(lWidget$x$bDebug)
  expect_named(lWidget$x$lStatistics, c("computed_by", "results"))

  # The participant table is optional: a group then comes from the results rows.
  dfAlone <- Synthetic_Results
  dfAlone$ARM <- Synthetic_Participants$ARM[match(dfAlone$USUBJID, Synthetic_Participants$USUBJID)]
  lAlone <- Widget_GroupComparison(dfAlone, lSettings = lSettings)
  expect_null(lAlone$x$dfParticipants)
  expect_identical(length(lAlone$x$lStatistics$results), nStoredForChange())
  expect_identical(nStoredForChange(), 48L)
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
  # A column to group by that no table has is refused by the frame.
  expect_error(lSyntheticWidget(list(groups = "NOPE", group_by = "NOPE")), "no table has the column `NOPE`")
})

test_that("the widget stores one result for each biomarker at each visit panel the chart draws, keyed to the view the settings open on (#9)", {
  lResults <- lSyntheticWidget()$x$lStatistics$results
  chrBiomarkers <- chrSyntheticBiomarkers()
  chrVisits <- unique(Synthetic_Results$VISIT)

  # Change from Baseline with every visit chosen: a panel, and so a result, for
  # each visit after Baseline, and none for Baseline itself.
  expect_identical(chrVisitsDrawn(), c("Week 2", "Week 4", "Week 8", "Week 12"))
  expect_identical(length(lResults), length(chrBiomarkers) * length(chrVisitsDrawn()))
  dfKeys <- data.frame(
    measure = vapply(lResults, function(lResult) lResult$dataId$measure, character(1)),
    visit = vapply(lResults, function(lResult) lResult$dataId$visit, character(1))
  )
  expect_identical(anyDuplicated(dfKeys), 0L)
  expect_setequal(dfKeys$measure, chrBiomarkers)
  expect_setequal(dfKeys$visit, chrVisitsDrawn())
  # Every biomarker is stored whichever the page opens on: the overview, when
  # `start_value` names none, prints no test, and a reader opens one from it.
  lFromOverview <- lSyntheticWidget(lWidgetSettings()[c("value_type", "baseline_visits", "group_by")])$x$lStatistics$results
  expect_identical(lFromOverview, lResults)
  # Visits named in the settings are the visits chosen, and the panels stored.
  lWeek4 <- lSyntheticWidget(c(lWidgetSettings(), list(visits = c("Week 4", "Baseline"))))$x$lStatistics$results
  expect_identical(length(lWeek4), length(chrBiomarkers))
  expect_identical(unique(vapply(lWeek4, function(lResult) lResult$dataId$visit, character(1))), "Week 4")
  # Against two baseline visits a change is drawn at every visit.
  lTwo <- lSyntheticWidget(list(value_type = "change", baseline_visits = c("Baseline", "Week 2"), group_by = "ARM"))$x$lStatistics$results
  expect_identical(length(lTwo), length(chrBiomarkers) * length(chrVisits))
  for (lResult in lResults) {
    expect_named(lResult, c("name", "args", "dataId", "rows", "value"))
    expect_identical(lResult$name, "Analyze_GroupDifference")
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

  # What the settings open on is what is computed: another test, a filter, a
  # panel column and a logarithmic scale each give their own keys. The result
  # itself, not a change, is drawn at every visit, Baseline included: a panel
  # per visit and per sex.
  lMore <- lSyntheticWidget(c(lWidgetSettings(), list(
    test = "wilcoxon", y_scale = "log", panel_by = "SEX", color_by = "RESPONSE", value_type = "raw",
    filters = list(list(value_col = "RESPONSE", start = "Responder"))
  ))[-2])$x$lStatistics$results
  expect_identical(length(lMore), length(chrBiomarkers) * length(chrVisits) * 2L)
  expect_identical(unique(vapply(lMore, function(lResult) lResult$args$strMethod, character(1))), "wilcoxon")
  expect_identical(lMore[[1]]$dataId$filters, list(RESPONSE = list("Responder")))
  expect_true(lMore[[1]]$dataId$positive_only)
  expect_identical(lMore[[1]]$dataId$color_by, "RESPONSE")
  expect_setequal(vapply(lMore, function(lResult) lResult$dataId$panel, character(1)), c("F", "M"))

  # A baseline value has no visit: one result per biomarker, and no `visit`.
  lBaseline <- lSyntheticWidget(list(value_type = "baseline", baseline_visits = "Baseline", group_by = "ARM"))$x$lStatistics$results
  expect_identical(length(lBaseline), length(chrBiomarkers))
  expect_false(any(vapply(lBaseline, function(lResult) "visit" %in% names(lResult$dataId), logical(1))))

  # No test asked for, or no statistics line, and nothing is computed.
  expect_identical(lSyntheticWidget(c(lWidgetSettings(), list(test = "none")))$x$lStatistics$results, list())
  expect_identical(lSyntheticWidget(c(lWidgetSettings(), list(statistic = NULL)))$x$lStatistics$results, list())
  expect_identical(Widget_GroupComparison(Synthetic_Results)$x$lStatistics$results, list())
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
  expect_identical(lSyntheticWidget(lGiven)$x$lSettings, lGiven)
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

test_that("the saved page holds the stored results, equal to Analyze_GroupDifference's answer member by member (#9)", {
  lResults <- lSavedWidget()$payload$lStatistics$results
  expect_identical(length(lResults), nStoredForChange())

  for (lResult in lResults) {
    strLabel <- paste(lResult$dataId$measure, lResult$dataId$visit)
    # The key reads back as it was written: single values single, lists lists.
    expect_identical(lResult$name, "Analyze_GroupDifference", label = strLabel)
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

  # Every status the study gives rise to went through the page.
  chrStatus <- vapply(lResults, function(lResult) lResult$value$status, character(1))
  expect_true("ok" %in% chrStatus)
  # The comparison can fail: a result is not its neighbour's answer.
  lFirst <- lResults[[1]]
  lSecond <- lResults[[2]]
  dfSecond <- dfChangeByArm(lSecond$dataId$measure, lSecond$dataId$visit)
  expect_gt(length(chrPageDifferences(lFirst$value, do.call(Analyze_GroupDifference, c(list(dfSecond), lSecond$args)), "a neighbour")), 0)
  expect_gt(length(chrPageDifferences(lFirst$value$reason, "a reason", "a reason")), 0)
  expect_gt(length(chrPageDifferences(lFirst$value$counts, unlist(lFirst$value$counts), "counts as a bare vector")), 0)
})

test_that("the saved page prints the planted difference: a test, its method and its counts (#9)", {
  lTruth <- Synthetic_Truth$GroupDifference
  lPayload <- lSavedWidget()$payload
  lFound <- Filter(function(lResult) {
    lResult$dataId$measure == lTruth$Biomarker && lResult$dataId$visit == lTruth$Visit
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
  lResults <- lSavedWidget()$payload$lStatistics$results
  chrKeys <- vapply(lResults, function(lResult) GroupComparison_KeyText(lResult[c("name", "args", "dataId")]), character(1))

  # One result per key, and every key is a view the settings open on.
  expect_identical(anyDuplicated(chrKeys), 0L)
  lOpening <- GroupComparison_Settings(lWidgetSettings())
  for (lResult in lResults) {
    expect_identical(lResult$args$strMethod, lOpening$test)
    expect_false(lResult$args$bPairwise)
    expect_identical(lResult$dataId$value_type, lOpening$value_type)
    expect_identical(lResult$dataId$group_by, lOpening$group_by)
    expect_false(any(c("filters", "color_by", "panel_by", "panel", "positive_only") %in% names(lResult$dataId)))
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
    GroupComparison_KeyText(lKey)
  }
  expect_true(Key() %in% chrKeys)
  expect_false(Key(list(strMethod = "wilcoxon")) %in% chrKeys)
  expect_false(Key(list(bPairwise = TRUE)) %in% chrKeys)
  expect_false(Key(lDataId = list(group_by = "SEX", groups = list("F", "M"))) %in% chrKeys)
  expect_false(Key(lDataId = list(filters = list(SEX = list("F")))) %in% chrKeys)
  expect_false(Key(lDataId = list(value_type = "raw")) %in% chrKeys)
  expect_false(Key(lDataId = list(positive_only = TRUE)) %in% chrKeys)
  expect_false(Key(lDataId = list(baseline_visits = list("Week 2"))) %in% chrKeys)
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
  strBinding <- paste(readLines(system.file("htmlwidgets", "Widget_GroupComparison.js", package = "gsm.bio"), warn = FALSE), collapse = "\n")
  expect_match(strBinding, "by.r_version", fixed = TRUE)
  expect_match(strBinding, "by.gsm_bio_version", fixed = TRUE)
  expect_match(strBinding, "by.computed_at", fixed = TRUE)
  expect_match(strBinding, "gsm-bio-provenance", fixed = TRUE)
})

test_that("the page's connection is made from the stored results alone: no R and no address in it (#9)", {
  strBinding <- paste(readLines(system.file("htmlwidgets", "Widget_GroupComparison.js", package = "gsm.bio"), warn = FALSE), collapse = "\n")
  expect_match(strBinding, "BioViz.r.createConnection({ results: statistics.results })", fixed = TRUE)
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
  expect_identical(lPayload$lSettings, list(start_value = "IL-6", value_type = "change", baseline_visits = "Baseline", group_by = "ARM"))
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
