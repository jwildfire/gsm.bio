# The widgets in a Shiny page, their statistics answered by the session (#71).
#
# Serve_Reply() is the whole of what the session does with a request, and is
# tested here with no Shiny at all: a request is text, and so is its answer.
# The last tests run an app in a second R session and drive its page in a
# headless browser, as the saved-page tests drive a file.

# The text the widget's script sends for a batch of requests: each a name, the
# rows as one array per column with null for a missing value, and arguments.
strRequests <- function(...) {
  chrEach <- vapply(list(...), function(lOne) {
    strColumns <- if (is.null(lOne$data)) {
      "{}"
    } else {
      as.character(jsonlite::toJSON(as.list(lOne$data), auto_unbox = FALSE, na = "null", digits = I(17)))
    }
    strArgs <- if (length(lOne$args) == 0L) "{}" else as.character(jsonlite::toJSON(lOne$args, auto_unbox = TRUE, null = "null", digits = I(17)))
    sprintf("{\"id\":%d,\"name\":%s,\"columns\":%s,\"args\":%s}", lOne$id, jsonlite::toJSON(lOne$name, auto_unbox = TRUE), strColumns, strArgs)
  }, character(1))
  paste0("[", paste(chrEach, collapse = ","), "]")
}

# One row per participant: a biomarker's result at a visit, beside the
# participant's own columns and outcome.
dfServeRows <- function(strBiomarker = "CRP", strVisit = "Week 4") {
  dfOne <- Synthetic_Results[Synthetic_Results$TEST == strBiomarker & Synthetic_Results$VISIT == strVisit, c("USUBJID", "STRESN")]
  dfOne <- merge(dfOne, Synthetic_Participants, by = "USUBJID")
  merge(dfOne, Synthetic_Outcomes[, c("USUBJID", "AVAL", "CNSR")], by = "USUBJID")
}

# A call of each of the nine statistics functions, on the synthetic study.
lServeCalls <- function() {
  dfRows <- dfServeRows()
  dfWide <- dfRows
  dfWide$IL6 <- dfServeRows("IL-6")$STRESN
  dfLong <- merge(
    Synthetic_Results[Synthetic_Results$TEST == "CRP", c("USUBJID", "VISIT", "STRESN")],
    Synthetic_Participants[, c("USUBJID", "ARM")],
    by = "USUBJID"
  )
  dfGrid <- merge(
    Synthetic_Results[Synthetic_Results$TEST %in% c("CRP", "IL-6"), c("USUBJID", "TEST", "VISIT", "STRESN")],
    Synthetic_Participants[, c("USUBJID", "ARM")],
    by = "USUBJID"
  )
  list(
    list(name = "Analyze_GroupDifference", data = dfRows, args = list(strValueCol = "STRESN", strGroupCol = "ARM", strMethod = "wilcoxon")),
    list(name = "Analyze_GroupDifferenceBy", data = dfLong, args = list(strValueCol = "STRESN", strGroupCol = "ARM", strByCol = "VISIT")),
    list(name = "Analyze_DifferenceGrid", data = dfGrid, args = list(
      strValueCol = "STRESN", strGroupCol = "ARM", strBiomarkerCol = "TEST", strByCol = "VISIT"
    )),
    list(name = "Analyze_Correlation", data = dfWide, args = list(strXCol = "STRESN", strYCol = "IL6", strMethod = "spearman")),
    list(name = "Analyze_CorrelationMatrix", data = dfWide, args = list(chrCols = c("STRESN", "IL6", "AGE"))),
    list(name = "Analyze_Fit", data = dfWide, args = list(strXCol = "STRESN", strYCol = "IL6")),
    list(name = "Analyze_Contingency", data = dfRows, args = list(strRowCol = "ARM", strColCol = "RESPONSE", strMethod = "fisher")),
    list(name = "Analyze_Survival", data = dfRows, args = list(strTimeCol = "AVAL", strGroupCol = "ARM", strCensorCol = "CNSR")),
    list(name = "Analyze_Screen", data = dfWide, args = list(chrCols = c("STRESN", "IL6"), strGroupCol = "ARM"))
  )
}

test_that("each of the nine statistics functions answered through the session equals the function called directly, converted as a stored result is (#71)", {
  lCalls <- lServeCalls()
  expect_setequal(vapply(lCalls, function(lCall) lCall$name, character(1)), names(Serve_Functions()))
  expect_setequal(names(Serve_Functions()), grep("^Analyze_", getNamespaceExports("gsm.bio"), value = TRUE))
  for (iCall in seq_along(lCalls)) {
    lCall <- lCalls[[iCall]]
    lAnswers <- Serve_Reply(strRequests(c(lCall, list(id = iCall))))
    expect_length(lAnswers, 1L)
    lAnswer <- lAnswers[[1]]
    expect_identical(lAnswer$id, iCall, label = paste(lCall$name, "id"))
    expect_true(lAnswer$ok, label = paste(lCall$name, "ok"))
    lDirect <- do.call(lCall$name, c(list(lCall$data), lCall$args))
    expect_identical(lDirect$status, "ok", label = paste(lCall$name, "computes on the synthetic study"))
    expect_identical(lAnswer$value, StoredValue(lDirect), label = lCall$name)
  }
})

test_that("a name outside the nine is answered with a sentence and nothing is called: not a base function, not an internal of the package, not a name that is not text (#71)", {
  strFile <- tempfile("gsm-bio-served")
  on.exit(unlink(strFile), add = TRUE)
  lOthers <- list(
    list(id = 1L, name = "file.create", args = list(strFile)),
    list(id = 2L, name = "base::system", args = list(command = paste("touch", strFile))),
    list(id = 3L, name = "Stat_Result"),
    list(id = 4L, name = "Serve_Reply"),
    list(id = 5L, name = "eval", args = list(expr = sprintf("file.create('%s')", strFile)))
  )
  lAnswers <- Serve_Reply(strRequests(lOthers[[1]], lOthers[[2]], lOthers[[3]], lOthers[[4]], lOthers[[5]]))
  expect_length(lAnswers, 5L)
  for (lAnswer in lAnswers) {
    expect_false(lAnswer$ok)
    expect_null(lAnswer$value)
    expect_match(lAnswer$message, "^This server runs gsm.bio's statistics functions and no other: Analyze_GroupDifference, ")
  }
  expect_false(file.exists(strFile))
  # A name that is a number, a list of names, or absent.
  lOdd <- Serve_Reply("[{\"id\":1,\"name\":7},{\"id\":2,\"name\":[\"Analyze_Fit\",\"Analyze_Screen\"]},{\"id\":3}]")
  expect_identical(vapply(lOdd, function(lAnswer) lAnswer$ok, logical(1)), c(FALSE, FALSE, FALSE))
  # And an argument is data: text that reads as R is handed over as text.
  lText <- Serve_Reply(strRequests(list(
    id = 9L, name = "Analyze_GroupDifference", data = dfServeRows(),
    args = list(strValueCol = sprintf("file.create('%s')", strFile), strGroupCol = "ARM")
  )))[[1]]
  expect_true(lText$ok)
  expect_identical(lText$value$status, "error")
  expect_false(file.exists(strFile))
})

test_that("a batch is answered in order under its own ids, an R error comes back as R's message, and what is not a batch of requests is answered with nothing (#71)", {
  dfRows <- dfServeRows()
  lAnswers <- Serve_Reply(strRequests(
    list(id = 41L, name = "Analyze_Contingency", data = dfRows, args = list(strRowCol = "ARM", strColCol = "RESPONSE")),
    list(id = 7L, name = "Analyze_GroupDifference", data = dfRows, args = list(strValueCol = "STRESN", strGroupCol = "ARM", bNoSuchArgument = TRUE)),
    list(id = 42L, name = "Analyze_GroupDifference", data = dfRows, args = list(strValueCol = "STRESN", strGroupCol = "SEX"))
  ))
  expect_identical(vapply(lAnswers, function(lAnswer) lAnswer$id, integer(1)), c(41L, 7L, 42L))
  expect_identical(vapply(lAnswers, function(lAnswer) lAnswer$ok, logical(1)), c(TRUE, FALSE, TRUE))
  expect_match(lAnswers[[2]]$message, "unused argument")
  expect_identical(lAnswers[[3]]$value, StoredValue(Analyze_GroupDifference(dfRows, "STRESN", "SEX")))
  # Columns of different lengths are no table: R's own message.
  lRagged <- Serve_Reply("[{\"id\":1,\"name\":\"Analyze_Fit\",\"columns\":{\"x\":[1,2,3],\"y\":[1,2]},\"args\":{\"strXCol\":\"x\",\"strYCol\":\"y\"}}]")
  expect_false(lRagged[[1]]$ok)
  expect_match(lRagged[[1]]$message, "differing number of rows")
  # Rows sent as an array, or arguments not by name, are refused.
  expect_match(Serve_Reply("[{\"id\":1,\"name\":\"Analyze_Fit\",\"columns\":[[1,2],[3,4]]}]")[[1]]$message, "named columns")
  expect_match(Serve_Reply("[{\"id\":1,\"name\":\"Analyze_Fit\",\"columns\":{},\"args\":[\"x\",\"y\"]}]")[[1]]$message, "by name")
  # Not text, not JSON, not an array, or a request with no id: nothing to answer.
  expect_identical(Serve_Reply(NULL), list())
  expect_identical(Serve_Reply(c("[]", "[]")), list())
  expect_identical(Serve_Reply("Analyze_Fit(data)"), list())
  expect_identical(Serve_Reply("{\"id\":1,\"name\":\"Analyze_Fit\"}"), list())
  expect_identical(Serve_Reply("[{\"name\":\"Analyze_Fit\"},{\"id\":\"one\",\"name\":\"Analyze_Fit\"}, 4]"), list())
  expect_identical(Serve_Reply("[]"), list())
  # The page's first question, whether the session answers at all, is answered yes.
  expect_identical(Serve_Reply("[{\"id\":3,\"hello\":true}]"), list(list(id = 3L, ok = TRUE, value = NULL)))
})

test_that("a missing value in a column reaches R as missing, whatever the column holds, and a request with no rows is answered (#71)", {
  dfRows <- dfServeRows()
  dfRows$STRESN[c(2, 5)] <- NA
  dfRows$ARM[3] <- NA
  lAnswer <- Serve_Reply(strRequests(list(id = 1L, name = "Analyze_GroupDifference", data = dfRows, args = list(strValueCol = "STRESN", strGroupCol = "ARM"))))[[1]]
  expect_identical(lAnswer$value, StoredValue(Analyze_GroupDifference(dfRows, "STRESN", "ARM")))
  expect_gt(length(lAnswer$value$dropped), 0L)
  lEmpty <- Serve_Reply("[{\"id\":2,\"name\":\"Analyze_GroupDifference\",\"columns\":{},\"args\":{\"strValueCol\":\"STRESN\",\"strGroupCol\":\"ARM\"}}]")[[1]]
  expect_true(lEmpty$ok)
  expect_identical(lEmpty$value, StoredValue(Analyze_GroupDifference(data.frame(), "STRESN", "ARM")))
})

test_that("a widget made for a session stores no result and computes none, says it is served, and names the R that made it; made any other way it stores what it did (#71)", {
  lSettings <- list(start_value = "CRP", visits = "Week 4", group_by = "ARM")
  lSaved <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lSettings)
  expect_gt(length(lSaved$x$lStatistics$results), 0L)
  expect_null(lSaved$x$lStatistics$served)
  # Served: the stored results are never computed. The function that computes
  # them stops if it is called.
  local_mocked_bindings(GroupComparison_StoredResults = function(...) stop("the stored results were computed"))
  expect_error(Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lSettings), "were computed")
  lServed <- Widget_Served(Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lSettings))
  expect_identical(lServed$x$lStatistics$results, list())
  expect_true(lServed$x$lStatistics$served)
  expect_identical(lServed$x$lStatistics$computed_by$r_version, paste(R.version$major, R.version$minor, sep = "."))
  expect_identical(lServed$x$lStatistics$computed_by$gsm_bio_version, as.character(utils::packageVersion("gsm.bio")))
  # Everything else of the widget is the same.
  lServed$x$lStatistics <- NULL
  lSaved$x$lStatistics <- NULL
  expect_identical(lServed$x, lSaved$x)
  # The mark is taken off again, also when the widget cannot be made.
  expect_false(Widget_IsServed())
  expect_error(Widget_Served(stop("no widget")), "no widget")
  expect_false(Widget_IsServed())
})

test_that("every widget has an output function and a render function, and a widget drawn by its render function is one the session answers (#71)", {
  skip_if_not_installed("shiny")
  chrWidgets <- grep("^Widget_[A-Za-z]+$", getNamespaceExports("gsm.bio"), value = TRUE)
  chrWidgets <- chrWidgets[!grepl("Output$", chrWidgets)]
  expect_length(chrWidgets, 6L)
  for (strWidget in chrWidgets) {
    fnOutput <- getExportedValue("gsm.bio", paste0(strWidget, "Output"))
    fnRender <- getExportedValue("gsm.bio", paste0("render", strWidget))
    strTag <- as.character(fnOutput("chart"))
    expect_match(strTag, "id=\"chart\"", fixed = TRUE, label = strWidget)
    expect_match(strTag, paste0("class=\"", strWidget, " html-widget html-widget-output"), fixed = TRUE, label = strWidget)
    expect_identical(names(formals(fnRender)), c("expr", "env", "quoted"))
  }
  # Drawn in a session: the payload the page is sent stores nothing and says it
  # is served, whether the expression is given as code or already quoted.
  lSettings <- list(start_value = "CRP", visits = "Week 4", group_by = "ARM")
  Server <- function(input, output, session) {
    output$chart <- renderWidget_GroupComparison(
      Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lSettings)
    )
    output$quoted <- renderWidget_CrossTab(
      quote(Widget_CrossTab(Synthetic_Results, Synthetic_Participants, lSettings = list(row_by = "ARM", column_by = "RESPONSE"))),
      quoted = TRUE
    )
  }
  shiny::testServer(Server, {
    for (strOutput in c("chart", "quoted")) {
      lPayload <- jsonlite::fromJSON(output[[strOutput]], simplifyVector = FALSE)$x
      expect_true(lPayload$lStatistics$served, label = strOutput)
      expect_identical(lPayload$lStatistics$results, list(), label = strOutput)
      expect_gt(length(lPayload$dfResults), 0L)
    }
  })
  expect_false(Widget_IsServed())
  # Outside a session there is nothing to answer.
  expect_error(Serve_Statistics(session = NULL), "inside a Shiny server function")
})

# A group comparison in an app, with the session answering.
strServedApp <- "
shiny::shinyApp(
  ui = shiny::fluidPage(Widget_GroupComparisonOutput('chart')),
  server = function(input, output, session) {
    if (!identical(shiny::parseQueryString(shiny::isolate(session$clientData$url_search))$answer, 'no')) Serve_Statistics()
    output$chart <- renderWidget_GroupComparison(Widget_GroupComparison(
      Synthetic_Results, Synthetic_Participants,
      lSettings = list(start_value = 'CRP', visits = 'Week 4', group_by = 'ARM')
    ))
  }
)
"

test_that("in a Shiny page a view the saved widget does not store is answered by the session with what the statistics function returns for the same rows, and the page says it was computed on this server (#71)", {
  NeedApp()
  lSettings <- list(start_value = "CRP", visits = "Week 4", group_by = "ARM")
  # What a saved widget holds: Welch's test, the one the settings open on, and
  # not the rank-sum test the reader is about to choose.
  KeysOf <- function(lWidget) {
    vapply(lWidget$x$lStatistics$results, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  }
  lSaved <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lSettings)
  lChosen <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = c(lSettings, list(test = "wilcoxon")))
  lExpected <- stats::setNames(lChosen$x$lStatistics$results, KeysOf(lChosen))

  lApp <- lRunApp(strServedApp)
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)

  # The view the page opens on is answered by the session, not from the page.
  lOpened <- lPage$Look()
  expect_true(lOpened$settled)
  expect_length(lOpened$statistics, 1L)
  expect_identical(lOpened$statistics[[1]]$answer$status, "ok")
  expect_identical(lOpened$statistics[[1]]$answer$form, "server")

  # The reader chooses another test: no saved widget of these settings has it.
  lView <- lPage$Move("choose", "test", "wilcoxon")
  expect_true(lView$settled)
  expect_length(lView$statistics, 1L)
  lAsked <- lView$statistics[[1]]
  strKey <- Chart_KeyText(lAsked[c("name", "args", "dataId")])
  expect_false(strKey %in% KeysOf(lSaved))
  expect_true(strKey %in% names(lExpected))
  expect_identical(lAsked$name, "Analyze_GroupDifference")
  expect_identical(lAsked$answer$status, "ok")
  expect_identical(lAsked$answer$form, "server")
  # A number crosses to the page as JSON at sixteen significant digits, as a
  # stored result's does: equal to R's own to that many.
  expect_equal(lAsked$answer$value, lExpected[[strKey]]$value, tolerance = 1e-14)
  expect_identical(lAsked$answer$value$p_value, lExpected[[strKey]]$value$p_value)
  expect_identical(lAsked$answer$value$method, "Wilcoxon rank sum test with continuity correction")
  # Which R answered: the session's, named with every answer and under the chart.
  strR <- paste(R.version$major, R.version$minor, sep = ".")
  strGsmBio <- as.character(utils::packageVersion("gsm.bio"))
  expect_identical(lAsked$answer$computedBy, list(r_version = strR, gsm_bio_version = strGsmBio))
  strServer <- sprintf("computed by R %s with gsm.bio %s on this server.", strR, strGsmBio)
  expect_match(strDrawnBy(lPage), strServer, fixed = TRUE)
  expect_match(lView$lines[[1]], "p = ", fixed = TRUE)
  expect_match(lView$provenance[[1]], sprintf("^Statistics: computed on request by R %s with gsm.bio %s on this server\\.", strR, strGsmBio))
  expect_identical(lView$errors, list())
  expect_identical(lPage$Errors(), character(0))
})

test_that("a session whose server function does not call Serve_Statistics() draws the chart and says its statistics are unavailable and why, and a session that ends is said to be out of reach, not an error of R's (#71)", {
  NeedApp()
  lApp <- lRunApp(strServedApp)
  on.exit(lApp$Stop(), add = TRUE)
  # The session that answers nothing: the chart is drawn, waits, and says so.
  lSilent <- lOpenPage(NULL, strAddress = paste0(lApp$address, "?answer=no"))
  on.exit(lSilent$Close(), add = TRUE)
  lWaiting <- lSilent$Look()
  expect_gt(lWaiting$canvases, 0L)
  expect_null(lWaiting$statistics[[1]]$answer)
  expect_true(bWaitFor(lSilent, "window.gsmBioPage.chart().statistics()[0].answer"), label = "the chart was answered in the end")
  lSaid <- lSilent$Look()
  expect_identical(lSaid$statistics[[1]]$answer$status, "unavailable")
  expect_identical(
    lSaid$statistics[[1]]$answer$message,
    "Statistics are unavailable: R on the server could not be reached (its session answers no statistics: Serve_Statistics() is not called in the server function)."
  )
  expect_false(any(grepl("p = ", unlist(lSaid$lines), fixed = TRUE)))
  lSilent$Close()

  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)
  expect_identical(lPage$Look()$statistics[[1]]$answer$form, "server")
  # The session ends under the page.
  lApp$Stop()
  expect_true(bWaitFor(lPage, "!window.Shiny.shinyapp.isConnected()"), label = "the page knows its session has ended")
  lView <- lPage$Move("choose", "test", "wilcoxon")
  expect_true(lView$settled)
  expect_identical(lView$statistics[[1]]$answer$status, "unavailable")
  expect_identical(
    lView$statistics[[1]]$answer$message,
    "Statistics are unavailable: R on the server could not be reached (the session with the server has ended; reload the page)."
  )
  expect_false(any(grepl("p = ", unlist(lView$lines), fixed = TRUE)))
})
