# The cross-tabulation widget (#18): bio.viz's two-way table from R, with R's
# test of it computed when the widget is made and stored in the page, so a
# saved page shows the table and its test with no R and no network.

lCrossTabWidget <- function(lSettings = list(row_by = "ARM", col_by = "RESPONSE"), ...) {
  Widget_CrossTab(Synthetic_Results, Synthetic_Participants, lSettings = lSettings, ...)
}

# Arm by response, worked out here from the participant table alone.
dfArmByResponse <- function() {
  data.frame(
    USUBJID = Synthetic_Participants$USUBJID, row = Synthetic_Participants$ARM, col = Synthetic_Participants$RESPONSE,
    stringsAsFactors = FALSE
  )
}

# Response by CRP at Baseline cut at its median, worked out here: quantile()
# and cut() on the study's results, the bound written to four digits.
dfResponseByCrp <- function() {
  nCrp <- nResultAt("CRP", "Baseline")
  nMedian <- stats::median(nCrp, na.rm = TRUE)
  strBound <- format(signif(nMedian, 4), scientific = FALSE, trim = TRUE)
  chrGroup <- ifelse(nCrp <= nMedian, paste0("\u2264 ", strBound), paste0("> ", strBound))
  dfRows <- data.frame(
    USUBJID = Synthetic_Participants$USUBJID, row = Synthetic_Participants$RESPONSE, col = enc2utf8(chrGroup),
    stringsAsFactors = FALSE
  )
  dfRows[!is.na(nCrp), ]
}

lSavedCrossTab <- local({
  lSaved <- NULL
  function() {
    if (is.null(lSaved)) {
      lWidget <- lCrossTabWidget()
      strPage <- strSavedPage(lWidget)
      lSaved <<- list(widget = lWidget, page = strPage, payload = lPagePayload(strPage))
    }
    lSaved
  }
})

test_that("Widget_CrossTab returns an htmlwidget carrying the tables, the settings and the stored results (#18)", {
  lWidget <- lCrossTabWidget()
  expect_s3_class(lWidget, c("Widget_CrossTab", "htmlwidget"))
  expect_named(lWidget$x, c("dfResults", "dfParticipants", "lSettings", "bDebug", "bAutoWidth", "bAutoHeight", "lStatistics"))
  expect_identical(lWidget$x$dfResults, Synthetic_Results)
  expect_identical(lWidget$x$dfParticipants, Synthetic_Participants)
  expect_identical(lWidget$x$lSettings[c("row_by", "col_by")], list(row_by = "ARM", col_by = "RESPONSE"))
  expect_named(lWidget$x$lStatistics, c("computed_by", "results"))
  lSized <- lCrossTabWidget(width = "100%", height = "600px", elementId = "cross-tab", bDebug = TRUE)
  expect_identical(lSized[c("width", "height", "elementId")], list(width = "100%", height = "600px", elementId = "cross-tab"))
})

test_that("Widget_CrossTab rejects invalid inputs before a page is made (#18)", {
  expect_error(Widget_CrossTab("not a data.frame"), "dfResults is not a data.frame")
  expect_error(lCrossTabWidget(list(percent = "total")), "percent.*must be one of")
  expect_error(lCrossTabWidget(list(test = "mcnemar")), "test.*must be one of")
  expect_error(lCrossTabWidget(list(statistic = "my_test")), "statistic.*Analyze_Contingency")
  expect_error(lCrossTabWidget(list(connection = list())), "connection.*cannot be given")
  expect_error(lCrossTabWidget(list(row_by = list(measure = "CRP", visit = "Baseline"))), "no cut")
  expect_error(lCrossTabWidget(list(row_by = list(measure = "NOPE", visit = "Baseline", cut = "median"))), "NOPE")
})

test_that("the widget stores the table's test by chi-square and by Fisher's exact test, keyed as the chart asks (#18)", {
  lResults <- lCrossTabWidget()$x$lStatistics$results
  expect_identical(vapply(lResults, function(lResult) lResult$args$strMethod, character(1)), c("chisq", "fisher"))
  for (lResult in lResults) {
    expect_identical(lResult$name, "Analyze_Contingency")
    expect_identical(lResult$args[c("strRowCol", "strColCol")], list(strRowCol = "row", strColCol = "col"))
    expect_identical(lResult$args$chrRowGroups, list("Placebo", "Treatment"))
    expect_identical(lResult$args$chrColGroups, list("Non-responder", "Responder"))
    expect_identical(lResult$dataId, list(chart = "cross-tab", row_by = "ARM", col_by = "RESPONSE"))
    expect_identical(lResult$rows, 200L)
  }
  # The test the settings open on comes first; none stores nothing.
  expect_identical(lCrossTabWidget(list(row_by = "ARM", col_by = "RESPONSE", test = "fisher"))$x$lStatistics$results[[1]]$args$strMethod, "fisher")
  expect_identical(lCrossTabWidget(list(row_by = "ARM", col_by = "RESPONSE", test = "none"))$x$lStatistics$results, list())
  expect_identical(lCrossTabWidget(list(row_by = "ARM", col_by = "RESPONSE", statistic = NULL))$x$lStatistics$results, list())
  # A cut: the groups low to high, the cut variable as the settings write it.
  lCut <- lCrossTabWidget(list(row_by = "RESPONSE", col_by = list(measure = "CRP", visit = "Baseline", cut = "median")))$x$lStatistics$results[[1]]
  expect_identical(lCut$dataId$col_by, list(measure = "CRP", visit = "Baseline", value = "raw", cut = "median"))
  strBound <- format(signif(stats::median(nResultAt("CRP", "Baseline"), na.rm = TRUE), 4), scientific = FALSE, trim = TRUE)
  expect_identical(unlist(lCut$args$chrColGroups), enc2utf8(c(paste0("\u2264 ", strBound), paste0("> ", strBound))))
})

test_that("the saved cross-tabulation page holds the table's tests, equal to Analyze_Contingency member by member (#18)", {
  lResults <- lSavedCrossTab()$payload$lStatistics$results
  expect_length(lResults, 2L)
  dfRows <- dfArmByResponse()
  for (lResult in lResults) {
    expect_identical(lResult$rows, nrow(dfRows))
    ExpectInPage(
      lResult$value,
      Analyze_Contingency(dfRows, "row", "col", strMethod = lResult$args$strMethod,
        chrRowGroups = c("Placebo", "Treatment"), chrColGroups = c("Non-responder", "Responder")
      ),
      lResult$args$strMethod
    )
  }
  # The comparison can fail: another table's test is not this one.
  expect_gt(length(chrPageDifferences(lResults[[1]]$value, Analyze_Contingency(dfResponseByCrp(), "row", "col"), "another table")), 0)
  # A cut table's test, on rows worked out here.
  lCutWidget <- lCrossTabWidget(list(row_by = "RESPONSE", col_by = list(measure = "CRP", visit = "Baseline", cut = "median")))
  lCut <- lPagePayload(strSavedPage(lCutWidget))$lStatistics$results[[1]]
  dfCut <- dfResponseByCrp()
  expect_identical(lCut$rows, nrow(dfCut))
  ExpectInPage(
    lCut$value,
    Analyze_Contingency(dfCut, "row", "col", chrRowGroups = unlist(lCut$args$chrRowGroups), chrColGroups = unlist(lCut$args$chrColGroups)),
    "the cut table"
  )
})

test_that("the saved cross-tabulation page holds no result under a key it was not computed for (#18)", {
  lResults <- lSavedCrossTab()$payload$lStatistics$results
  chrKeys <- vapply(lResults, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  expect_identical(anyDuplicated(chrKeys), 0L)
  Key <- function(lArgs = list(), lDataId = list()) {
    lKey <- lResults[[1]][c("name", "args", "dataId")]
    lKey$args[names(lArgs)] <- lArgs
    lKey$dataId[names(lDataId)] <- lDataId
    Chart_KeyText(lKey)
  }
  expect_true(Key() %in% chrKeys)
  expect_false(Key(lDataId = list(row_by = "SEX")) %in% chrKeys)
  expect_false(Key(lDataId = list(col_by = list(measure = "CRP", visit = "Baseline", value = "raw", cut = "median"))) %in% chrKeys)
  expect_false(Key(lDataId = list(filters = list(SEX = list("F")))) %in% chrKeys)
  expect_false(Key(list(chrRowGroups = list("Treatment", "Placebo"))) %in% chrKeys)
  expect_false(Key(list(strMethod = "none")) %in% chrKeys)
})

test_that("the cross-tabulation page records which R computed the results, and is made by the script every widget shares (#18)", {
  lSaved <- lSavedCrossTab()
  expect_identical(lSaved$payload$lStatistics$computed_by, lSaved$widget$x$lStatistics$computed_by)
  strScripts <- strWidgetScripts("Widget_CrossTab")
  expect_match(strScripts, "BioViz.crossTab(chart, settings)", fixed = TRUE)
  expect_match(strScripts, "BioViz.r.createConnection({ results: statistics.results, computedBy: statistics.computed_by })", fixed = TRUE)
  for (strNever in c("browser", "webr", "sourceUrl", "http", "fetch(", "import(")) {
    expect_false(grepl(strNever, strScripts, fixed = TRUE), label = paste("the scripts name", strNever))
  }
})

test_that("the cross-tabulation widget saves as one self-contained file that holds both bundles and loads nothing (#18)", {
  if (!bPandoc()) {
    if (bSourceTree()) {
      fail("pandoc was not found: htmlwidgets::saveWidget(selfcontained = TRUE) needs it, and so does this test")
    }
    skip("pandoc is not available to save a self-contained page")
  }
  strDir <- tempfile("Widget_CrossTab")
  dir.create(strDir)
  strFile <- file.path(strDir, "cross-tab.html")
  htmlwidgets::saveWidget(lCrossTabWidget(), file = strFile, selfcontained = TRUE)
  strPage <- paste(readLines(strFile, warn = FALSE), collapse = "\n")
  expect_match(strPage, "var BioViz = ", fixed = TRUE)
  expect_match(strPage, "var SafetyViz = ", fixed = TRUE)
  expect_match(strPage, "name: 'Widget_CrossTab'", fixed = TRUE)
  expect_match(strPage, "\"name\":\"Analyze_Contingency\"", fixed = TRUE)
  expect_false(grepl("<(script|img|iframe|link)[^>]*\\s(src|href)\\s*=", strPage, perl = TRUE))
  expect_false(grepl("cross-tab_files", strPage, fixed = TRUE))
  expect_identical(length(lPagePayload(strPage)$lStatistics$results), 2L)
})

test_that("a cut is handed to the page as R reads it: a single typed point as a list, which the chart takes (#18)", {
  lSettings <- list(
    row_by = list(measure = "CRP", visit = "Baseline", cut = 3), col_by = "RESPONSE",
    cuts = list(list(col = "AGE", type = "number", cut = 50))
  )
  lWidget <- lCrossTabWidget(lSettings)
  lPage <- lPagePayload(strSavedPage(lWidget))
  expect_identical(lPage$lSettings$row_by, list(measure = "CRP", visit = "Baseline", value = "raw", cut = list(3L)))
  expect_identical(lPage$lSettings$cuts, list(list(col = "AGE", type = "number", cut = list(50L))))
  expect_identical(lPage$lSettings$col_by, "RESPONSE")
  expect_length(lPage$lStatistics$results, 2L)
  expect_identical(lPage$lStatistics$results[[1]]$dataId$row_by, lPage$lSettings$row_by)
})

test_that("a cut with no value to cut stores nothing and stops nothing (#18)", {
  dfAgeless <- Synthetic_Participants
  dfAgeless$AGE <- NA_integer_
  dfNoCrp <- Synthetic_Results
  dfNoCrp$STRESN[dfNoCrp$TEST == "CRP" & dfNoCrp$VISIT == "Baseline"] <- NA
  for (lCase in list(
    list(results = Synthetic_Results, participants = Synthetic_Participants, col_by = list(measure = "CRP", visit = "Week 99", cut = "median")),
    list(results = dfNoCrp, participants = Synthetic_Participants, col_by = list(measure = "CRP", visit = "Baseline", cut = "median")),
    list(results = Synthetic_Results, participants = dfAgeless, col_by = list(col = "AGE", type = "number", cut = "median"))
  )) {
    lWidget <- Widget_CrossTab(lCase$results, lCase$participants, lSettings = list(row_by = "ARM", col_by = lCase$col_by))
    expect_identical(lWidget$x$lStatistics$results, list())
  }
})

test_that("every widget hands its chart which R computed the stored results, so the chart's own footnote names the versions (#37)", {
  strScripts <- strWidgetScripts("Widget_CrossTab")
  expect_match(strScripts, "BioViz.r.createConnection({ results: statistics.results, computedBy: statistics.computed_by })", fixed = TRUE)
  # The copied bundle takes the record, and its footnote says it.
  strBundle <- paste(readLines(strBioVizBundleFile(), warn = FALSE), collapse = "\n")
  expect_match(strBundle, "computedBy", fixed = TRUE)
  expect_match(strBundle, "stored with the page", fixed = TRUE)
  # The record the page carries has the members the connection checks.
  lBy <- Widget_CrossTab(Synthetic_Results, Synthetic_Participants, lSettings = list(row_by = "ARM", col_by = "RESPONSE"))$x$lStatistics$computed_by
  expect_true(all(c("r_version", "gsm_bio_version", "computed_at") %in% names(lBy)))
  expect_true(all(vapply(lBy[c("r_version", "gsm_bio_version", "computed_at")], is.character, logical(1))))
})

# The study's participants with two columns whose names sort one way by code
# point and another by name: a dose, and a letter in either case.
dfDoseParticipants <- function() {
  dfParticipants <- Synthetic_Participants
  dfParticipants$DOSE <- ifelse(seq_len(nrow(dfParticipants)) %% 2L == 0L, "10 mg", "2 mg")
  dfParticipants$LETTER <- ifelse(seq_len(nrow(dfParticipants)) %% 3L == 0L, "B", "a")
  dfParticipants
}

test_that("a column's categories are keyed in the order the chart draws them, by name with numbers as numbers, so the chart finds its stored results (#44)", {
  dfParticipants <- dfDoseParticipants()
  lResults <- Widget_CrossTab(Synthetic_Results, dfParticipants, lSettings = list(row_by = "DOSE", col_by = "LETTER"))$x$lStatistics$results
  expect_length(lResults, 2L)
  for (lResult in lResults) {
    # bio.viz draws a column's categories in gsm.bio's own order (its
    # `categoryOrder`, Core_NaturalCompare) and asks for them so.
    expect_identical(lResult$args$chrRowGroups, list("2 mg", "10 mg"))
    expect_identical(lResult$args$chrColGroups, list("a", "B"))
    expect_identical(unlist(lResult$args$chrRowGroups), Core_Levels(dfParticipants$DOSE))
    expect_identical(lResult$value$status, "ok")
  }
  # The figure and the table read the table in the same order.
  skip_if_not_installed("ggplot2")
  lTable <- CrossTab_Table(Synthetic_Results, dfParticipants, CrossTab_Settings(list(row_by = "DOSE", col_by = "LETTER")),
    CrossTab_State(Synthetic_Results, dfParticipants, CrossTab_Settings(list(row_by = "DOSE", col_by = "LETTER"))))
  expect_identical(lTable$row_levels, c("2 mg", "10 mg"))
  expect_identical(lTable$col_levels, c("a", "B"))
})

# Forty participants in a two-by-two with an empty cell: A has 17 x and 3 y,
# B has no x and 20 y, so Fisher's odds ratio is infinite and its lower bound
# 14.86.
lEmptyCell <- function() {
  dfParticipants <- Synthetic_Participants[1:40, ]
  dfParticipants$ROWV <- rep(c("A", "B"), each = 20L)
  dfParticipants$COLV <- c(rep("x", 17L), rep("y", 23L))
  list(
    results = Synthetic_Results[Synthetic_Results$USUBJID %in% dfParticipants$USUBJID, ],
    participants = dfParticipants,
    settings = list(row_by = "ROWV", col_by = "COLV", test = "fisher"),
    rows = data.frame(row = dfParticipants$ROWV, col = dfParticipants$COLV, stringsAsFactors = FALSE)
  )
}

test_that("an infinite odds ratio is stored as the text Inf, which bio.viz reads back as infinity, and not lost as null (#44)", {
  lCase <- lEmptyCell()
  lWidget <- Widget_CrossTab(lCase$results, lCase$participants, lSettings = lCase$settings)
  lTheirs <- Analyze_Contingency(lCase$rows, "row", "col", strMethod = "fisher", chrRowGroups = c("A", "B"), chrColGroups = c("x", "y"))
  expect_identical(lTheirs$estimates$estimate, Inf)
  expect_equal(lTheirs$estimates$lower, 14.85639, tolerance = 1e-6)
  lPage <- lPagePayload(strSavedPage(lWidget))$lStatistics$results[[1]]
  expect_identical(lPage$args$strMethod, "fisher")
  lEstimate <- lPage$value$estimates[[1]]
  expect_identical(lEstimate$estimate, "Inf")
  expect_identical(lEstimate$upper, "Inf")
  expect_equal(lEstimate$lower, lTheirs$estimates$lower, tolerance = 1e-12)
  # Read back by bio.viz's rule, the page holds R's answer member by member.
  ExpectInPage(lPage$value, lTheirs, "the empty-cell table")
  # Each of R's non-finite numbers has its own spelling; a missing value is null.
  expect_identical(StoredValue(c(-Inf, NaN, NA, 1)), list("-Inf", "NaN", NULL, 1))
  expect_identical(StoredValue(Inf), "Inf")
})

test_that("the figure and the table name the odds ratio by its rows and columns, and print an infinite one in words, as the chart does (#44)", {
  skip_if_not_installed("ggplot2")
  lCase <- lEmptyCell()
  strInfinite <- "odds ratio (A / B, odds of x against y): infinite, 95% confidence interval 14.86 to infinity"
  gg <- Visualize_CrossTab(lCase$results, lCase$participants, lCase$settings)
  expect_match(gg$labels$caption, paste0(strInfinite, "."), fixed = TRUE)
  dfTable <- Table_CrossTab(lCase$results, lCase$participants, lCase$settings)
  expect_identical(dfTable$Estimate, strInfinite)
  # A finite one is named the same way.
  dfArm <- Table_CrossTab(Synthetic_Results, Synthetic_Participants, list(row_by = "ARM", col_by = "RESPONSE", test = "fisher"))
  expect_match(dfArm$Estimate, "^odds ratio \\(Placebo / Treatment, odds of Non-responder against Responder\\): [0-9.]+, 95% confidence interval [0-9.]+ to [0-9.]+$")
  expect_match(Visualize_CrossTab(Synthetic_Results, Synthetic_Participants, list(row_by = "ARM", col_by = "RESPONSE", test = "fisher"))$labels$caption,
    "odds ratio (Placebo / Treatment, odds of Non-responder against Responder): ", fixed = TRUE)
  # A table bigger than two by two has no odds ratio to name; minus infinity is in words too.
  expect_identical(Output_EstimateText(list(name = "odds ratio", group = NA, estimate = -Inf, lower = -Inf, upper = 2, level = 0.95)),
    "odds ratio: minus infinity, 95% confidence interval minus infinity to 2.")
})

# Twenty participants in the two-by-three table of the task (#46): arm by
# grade, with only two at grade 3, so a column is below the minimum group size.
lSmallMargin <- function() {
  dfParticipants <- Synthetic_Participants[1:20, ]
  dfParticipants$ARMX <- rep(c("A", "B"), each = 10L)
  dfParticipants$GRADE <- c(rep(c("1", "2", "3"), times = c(6L, 3L, 1L)), rep(c("1", "2", "3"), times = c(4L, 5L, 1L)))
  list(
    results = Synthetic_Results[Synthetic_Results$USUBJID %in% dfParticipants$USUBJID, ],
    participants = dfParticipants,
    settings = list(row_by = "ARMX", col_by = "GRADE"),
    table = table(dfParticipants$ARMX, dfParticipants$GRADE)
  )
}

test_that("on a table with a small margin the widget stores Fisher's exact test as R computed it and the chi-square test as refused, and the saved page holds both (#46)", {
  lCase <- lSmallMargin()
  lBase <- stats::fisher.test(lCase$table)
  lWidget <- Widget_CrossTab(lCase$results, lCase$participants, lSettings = lCase$settings)
  lResults <- lWidget$x$lStatistics$results
  expect_identical(vapply(lResults, function(lResult) lResult$args$strMethod, character(1)), c("chisq", "fisher"))
  expect_identical(vapply(lResults, function(lResult) lResult$rows, integer(1)), c(20L, 20L))
  # Neither request sends a minimum group size: the minimum is R's.
  for (lResult in lResults) expect_false("nMinGroup" %in% names(lResult$args))
  lChisq <- lResults[[1]]$value
  expect_identical(lChisq$status, "too_small")
  expect_identical(lChisq$reason, "Not computed: col = 3 has 2. The minimum group size is 5.")
  expect_null(lChisq$p_value)
  lFisher <- lResults[[2]]$value
  expect_identical(lFisher$status, "ok")
  expect_identical(lFisher$p_value, lBase$p.value)
  expect_identical(round(lFisher$p_value, 3), 0.809)
  expect_identical(lFisher$method, "Fisher's Exact Test for Count Data")
  expect_identical(
    lFisher$notes,
    list("Fisher's exact test is exact at any count, so the minimum group size of 5 is not applied to it. Below it here: col = 3 has 2.")
  )
  # In the saved page: R's answer, member by member.
  lTheirs <- Analyze_Contingency(
    data.frame(row = lCase$participants$ARMX, col = lCase$participants$GRADE, stringsAsFactors = FALSE),
    "row", "col", strMethod = "fisher", chrRowGroups = c("A", "B"), chrColGroups = c("1", "2", "3")
  )
  lPage <- lPagePayload(strSavedPage(lWidget))$lStatistics$results
  expect_identical(lPage[[2]]$args$strMethod, "fisher")
  ExpectInPage(lPage[[2]]$value, lTheirs, "Fisher's exact test of the small-margin table")
  expect_identical(lPage[[1]]$value$status, "too_small")
})

test_that("on a table with a small margin the statistics table and the figure print Fisher's p-value, and say why the chi-square test was not computed (#46)", {
  lCase <- lSmallMargin()
  lBase <- stats::fisher.test(lCase$table)
  dfFisher <- Table_CrossTab(lCase$results, lCase$participants, c(lCase$settings, list(test = "fisher")))
  expect_identical(nrow(dfFisher), 1L)
  expect_identical(dfFisher$Method, "Fisher's Exact Test for Count Data")
  expect_identical(dfFisher$`p-value`, Output_P(lBase$p.value))
  expect_identical(dfFisher$`p-value`, "p = 0.809")
  expect_identical(dfFisher$Counts, "n = 20")
  expect_identical(attr(dfFisher, "results")[[1]]$status, "ok")
  dfChisq <- Table_CrossTab(lCase$results, lCase$participants, c(lCase$settings, list(test = "chisq")))
  expect_identical(dfChisq$`p-value`, "")
  expect_identical(dfChisq$Note, "Not computed: col = 3 has 2. The minimum group size is 5.")
  expect_identical(attr(dfChisq, "results")[[1]]$status, "too_small")
  skip_if_not_installed("ggplot2")
  strCaption <- Visualize_CrossTab(lCase$results, lCase$participants, c(lCase$settings, list(test = "fisher")))$labels$caption
  expect_match(strCaption, "Fisher's Exact Test for Count Data: p = 0.809", fixed = TRUE)
  expect_match(
    Visualize_CrossTab(lCase$results, lCase$participants, lCase$settings)$labels$caption,
    "Not computed: col = 3 has 2. The minimum group size is 5.", fixed = TRUE
  )
})
