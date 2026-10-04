# The stratified survival widget (#35): bio.viz's Kaplan-Meier curves by group
# from R, with R's log-rank test, medians and hazard ratio computed when the
# widget is made and stored in the page, so a saved page shows the curves and
# R's numbers with no R and no network.

lSurvivalSettings <- function() {
  list(endpoint = "EFS", group_by = list(measure = "CRP", visit = "Baseline", cut = "median"))
}

lSurvivalWidget <- function(lSettings = lSurvivalSettings(), dfOutcomes = Synthetic_Outcomes, ...) {
  Widget_StratifiedSurvival(Synthetic_Results, Synthetic_Participants, lSettings, dfOutcomes, ...)
}

# Event-free survival by CRP at Baseline cut at its median, worked out here
# from the study's tables and nothing of the widget's: quantile() and cut() on
# the results, the bound written to four digits, each participant's time and
# flag from the outcomes table.
dfSurvivalRows <- function() {
  nCrp <- nResultAt("CRP", "Baseline")
  nMedian <- stats::median(nCrp, na.rm = TRUE)
  strBound <- format(signif(nMedian, 4), scientific = FALSE, trim = TRUE)
  dfEfs <- Synthetic_Outcomes[Synthetic_Outcomes$PARAMCD == "EFS", ]
  iAt <- match(Synthetic_Participants$USUBJID, dfEfs$USUBJID)
  dfRows <- data.frame(
    USUBJID = Synthetic_Participants$USUBJID, time = dfEfs$AVAL[iAt],
    group = enc2utf8(ifelse(nCrp <= nMedian, paste0("≤ ", strBound), paste0("> ", strBound))),
    censor = dfEfs$CNSR[iAt], stringsAsFactors = FALSE
  )
  dfRows <- dfRows[!is.na(nCrp) & !is.na(iAt), ]
  attr(dfRows, "groups") <- enc2utf8(c(paste0("> ", strBound), paste0("≤ ", strBound)))
  dfRows
}

lSavedSurvival <- local({
  lSaved <- NULL
  function() {
    if (is.null(lSaved)) {
      lWidget <- lSurvivalWidget()
      strPage <- strSavedPage(lWidget)
      lSaved <<- list(widget = lWidget, page = strPage, payload = lPagePayload(strPage))
    }
    lSaved
  }
})

test_that("Widget_StratifiedSurvival returns an htmlwidget carrying the three tables, the settings and the stored results (#35)", {
  lWidget <- lSurvivalWidget()
  expect_s3_class(lWidget, c("Widget_StratifiedSurvival", "htmlwidget"))
  expect_named(lWidget$x, c("dfResults", "dfParticipants", "dfOutcomes", "lSettings", "bDebug", "bAutoWidth", "bAutoHeight", "lStatistics"))
  expect_identical(lWidget$x$dfResults, Synthetic_Results)
  expect_identical(lWidget$x$dfParticipants, Synthetic_Participants)
  expect_identical(lWidget$x$dfOutcomes, Synthetic_Outcomes)
  expect_identical(lWidget$x$lSettings$endpoint, "EFS")
  expect_named(lWidget$x$lStatistics, c("computed_by", "results"))
  lSized <- lSurvivalWidget(width = "100%", height = "600px", elementId = "survival", bDebug = TRUE)
  expect_identical(lSized[c("width", "height", "elementId")], list(width = "100%", height = "600px", elementId = "survival"))
  # The other widgets carry no outcomes table.
  expect_false("dfOutcomes" %in% names(Widget_CrossTab(Synthetic_Results, Synthetic_Participants)$x))
})

test_that("Widget_StratifiedSurvival rejects invalid inputs before a page is made (#35)", {
  expect_error(Widget_StratifiedSurvival("not a data.frame"), "dfResults is not a data.frame")
  expect_error(lSurvivalWidget(dfOutcomes = "nope"), "dfOutcomes is not a data.frame")
  expect_error(lSurvivalWidget(dfOutcomes = Synthetic_Outcomes[c("USUBJID", "PARAMCD", "AVAL")]), "dfOutcomes has no column 'CNSR'.*censor_col")
  expect_error(lSurvivalWidget(c(lSurvivalSettings(), list(event_col = "EVENT", censor_col = "CNSR"))), "exactly one of 'censor_col'")
  expect_error(lSurvivalWidget(list(statistic = "my_test")), "statistic.*Analyze_Survival")
  expect_error(lSurvivalWidget(list(connection = list())), "connection.*cannot be given")
  expect_error(lSurvivalWidget(list(group_by = list(measure = "CRP", visit = "Baseline"))), "no cut")
  expect_error(lSurvivalWidget(list(group_by = list(measure = "NOPE", visit = "Baseline", cut = "median"))), "NOPE")
})

test_that("the widget stores R's test of the curves the chart opens on, keyed as the chart asks (#35)", {
  lResults <- lSurvivalWidget()$x$lStatistics$results
  expect_length(lResults, 1L)
  lResult <- lResults[[1]]
  dfRows <- dfSurvivalRows()
  expect_identical(lResult$name, "Analyze_Survival")
  expect_identical(lResult$args, list(strTimeCol = "time", strGroupCol = "group", strCensorCol = "censor", chrGroups = as.list(attr(dfRows, "groups"))))
  expect_identical(lResult$dataId, list(
    chart = "stratified-survival", endpoint = "EFS",
    group_by = list(measure = "CRP", visit = "Baseline", value = "raw", cut = "median")
  ))
  expect_identical(lResult$rows, nrow(dfRows))
  # A column's groups; no statistic stores nothing; nor does no outcomes table.
  lArm <- lSurvivalWidget(list(endpoint = "EFS", group_by = "ARM"))$x$lStatistics$results[[1]]
  expect_identical(lArm$args$chrGroups, list("Placebo", "Treatment"))
  expect_identical(lArm$dataId$group_by, "ARM")
  expect_identical(lSurvivalWidget(c(lSurvivalSettings(), list(statistic = NULL)))$x$lStatistics$results, list())
  lNone <- lSurvivalWidget(dfOutcomes = NULL)
  expect_identical(lNone$x$lStatistics$results, list())
  expect_false("dfOutcomes" %in% names(lNone$x))
  # An event column: the flag the other way round, and R told so.
  dfEvent <- Synthetic_Outcomes
  dfEvent$EVENT <- 1L - dfEvent$CNSR
  dfEvent$CNSR <- NULL
  lEvent <- lSurvivalWidget(c(lSurvivalSettings(), list(event_col = "EVENT")), dfOutcomes = dfEvent)
  expect_identical(lEvent$x$lStatistics$results[[1]]$args$strEventCol, "event")
  expect_identical(lEvent$x$lStatistics$results[[1]]$value$p_value, lResult$value$p_value)
})

test_that("the saved survival page holds R's log-rank test, medians and hazard ratio, equal to Analyze_Survival member by member (#35)", {
  lResults <- lSavedSurvival()$payload$lStatistics$results
  expect_length(lResults, 1L)
  dfRows <- dfSurvivalRows()
  lTheirs <- Analyze_Survival(dfRows, "time", "group", strCensorCol = "censor", chrGroups = attr(dfRows, "groups"))
  expect_identical(lResults[[1]]$rows, nrow(dfRows))
  ExpectInPage(lResults[[1]]$value, lTheirs, "the curves' test")
  # What the page shows: the log-rank p-value, each group's median and the
  # hazard ratio of the higher group over the lower.
  expect_identical(lTheirs$status, "ok")
  expect_identical(lTheirs$test, "logrank")
  expect_identical(lTheirs$estimates$name, c("Median", "Median", "Hazard ratio"))
  expect_gt(lTheirs$estimates$estimate[3], 1)
  # The comparison can fail: another view's test is not this one.
  dfArm <- dfRows
  dfArm$group <- Synthetic_Participants$ARM[match(dfArm$USUBJID, Synthetic_Participants$USUBJID)]
  expect_gt(length(chrPageDifferences(lResults[[1]]$value, Analyze_Survival(dfArm, "time", "group", strCensorCol = "censor"), "another view")), 0)
})

test_that("the saved survival page holds no result under a key it was not computed for (#35)", {
  lResults <- lSavedSurvival()$payload$lStatistics$results
  chrKeys <- vapply(lResults, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  Key <- function(lArgs = list(), lDataId = list()) {
    lKey <- lResults[[1]][c("name", "args", "dataId")]
    lKey$args[names(lArgs)] <- lArgs
    lKey$dataId[names(lDataId)] <- lDataId
    Chart_KeyText(lKey)
  }
  expect_true(Key() %in% chrKeys)
  # Another grouping, another cut, another endpoint, a filter, the groups the
  # other way round, or the flag read the other way.
  expect_false(Key(lDataId = list(group_by = "ARM")) %in% chrKeys)
  expect_false(Key(lDataId = list(group_by = list(measure = "CRP", visit = "Baseline", value = "raw", cut = "tertiles"))) %in% chrKeys)
  expect_false(Key(lDataId = list(endpoint = "OS")) %in% chrKeys)
  expect_false(Key(lDataId = list(filters = list(SEX = list("F")))) %in% chrKeys)
  expect_false(Key(list(chrGroups = rev(lResults[[1]]$args$chrGroups))) %in% chrKeys)
  lEvent <- lResults[[1]]$args
  lEvent$strCensorCol <- NULL
  lEvent$strEventCol <- "event"
  expect_false(Chart_KeyText(list(name = "Analyze_Survival", args = lEvent, dataId = lResults[[1]]$dataId)) %in% chrKeys)
})

test_that("the survival page records which R computed the results, and hands the chart the outcomes table (#35)", {
  lSaved <- lSavedSurvival()
  expect_identical(lSaved$payload$lStatistics$computed_by, lSaved$widget$x$lStatistics$computed_by)
  strScripts <- strWidgetScripts("Widget_StratifiedSurvival")
  expect_match(strScripts, "BioViz.stratifiedSurvival(chart, settings)", fixed = TRUE)
  expect_match(strScripts, "BioViz.r.createConnection({ results: statistics.results })", fixed = TRUE)
  expect_match(strScripts, "tables.outcomes = toRows(x.dfOutcomes)", fixed = TRUE)
  for (strNever in c("browser", "webr", "sourceUrl", "http", "fetch(", "import(")) {
    expect_false(grepl(strNever, strScripts, fixed = TRUE), label = paste("the scripts name", strNever))
  }
  # The page's outcomes table is the study's, by column.
  expect_identical(unlist(lSaved$payload$dfOutcomes$USUBJID), Synthetic_Outcomes$USUBJID)
  expect_identical(unlist(lSaved$payload$dfOutcomes$CNSR), Synthetic_Outcomes$CNSR)
})

test_that("the survival widget saves as one self-contained file that holds both bundles and loads nothing (#35)", {
  if (!bPandoc()) {
    if (bSourceTree()) {
      fail("pandoc was not found: htmlwidgets::saveWidget(selfcontained = TRUE) needs it, and so does this test")
    }
    skip("pandoc is not available to save a self-contained page")
  }
  strDir <- tempfile("Widget_StratifiedSurvival")
  dir.create(strDir)
  strFile <- file.path(strDir, "stratified-survival.html")
  htmlwidgets::saveWidget(lSurvivalWidget(), file = strFile, selfcontained = TRUE)
  strPage <- paste(readLines(strFile, warn = FALSE), collapse = "\n")
  expect_match(strPage, "var BioViz = ", fixed = TRUE)
  expect_match(strPage, "var SafetyViz = ", fixed = TRUE)
  expect_match(strPage, "name: 'Widget_StratifiedSurvival'", fixed = TRUE)
  expect_match(strPage, "\"name\":\"Analyze_Survival\"", fixed = TRUE)
  expect_false(grepl("<(script|img|iframe|link)[^>]*\\s(src|href)\\s*=", strPage, perl = TRUE))
  expect_false(grepl("stratified-survival_files", strPage, fixed = TRUE))
  expect_identical(length(lPagePayload(strPage)$lStatistics$results), 1L)
})

test_that("a cut is handed to the page as R reads it, and so are the cuts the Groups control offers (#35)", {
  lWidget <- lSurvivalWidget(list(
    endpoint = "EFS", group_by = list(measure = "CRP", visit = "Baseline", cut = 4),
    cuts = list(list(col = "AGE", type = "number", cut = 50))
  ))
  lPage <- lPagePayload(strSavedPage(lWidget))
  expect_identical(lPage$lSettings$group_by, list(measure = "CRP", visit = "Baseline", value = "raw", cut = list(4L)))
  expect_identical(lPage$lSettings$cuts, list(list(col = "AGE", type = "number", cut = list(50L))))
  expect_identical(lPage$lStatistics$results[[1]]$dataId$group_by, lPage$lSettings$group_by)
  # The baseline visits are named to the chart outright.
  expect_identical(lPage$lSettings$baseline_visits, "Baseline")
})
