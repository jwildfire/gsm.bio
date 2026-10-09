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

test_that("a request is read by the exact names of its members: one whose name only begins as `id`, `hello`, `name`, `columns` or `args` does is not that member (#86)", {
  dfRows <- dfServeRows()
  # No `id`: nothing to answer under, whatever else begins with those letters.
  expect_identical(Serve_Reply("[{\"idx\":7,\"hello\":true}]"), list())
  expect_identical(Serve_Reply("[{\"identity\":7,\"name\":\"Analyze_Fit\"}]"), list())
  # `helloThere` is no hello, and `namesake` no name: each is a request that
  # names no function, and is refused as one.
  for (strRequest in c("[{\"id\":3,\"helloThere\":true}]", "[{\"id\":3,\"namesake\":\"Analyze_Fit\",\"columns\":{}}]")) {
    lAnswer <- Serve_Reply(strRequest)[[1]]
    expect_identical(lAnswer$id, 3L)
    expect_false(lAnswer$ok)
    expect_match(lAnswer$message, "^This server runs gsm.bio's statistics functions and no other: ")
  }
  # Rows and arguments under longer names are not the rows and the arguments:
  # the function is called with neither, as it is for a request that has none.
  strAsked <- strRequests(list(
    id = 5L, name = "Analyze_GroupDifference", data = dfRows, args = list(strValueCol = "STRESN", strGroupCol = "ARM")
  ))
  lWhole <- Serve_Reply(strAsked)[[1]]
  expect_identical(lWhole$value, StoredValue(Analyze_GroupDifference(dfRows, "STRESN", "ARM")))
  strLonger <- sub("\"args\":", "\"arguments\":", sub("\"columns\":", "\"columnsSent\":", strAsked, fixed = TRUE), fixed = TRUE)
  expect_false(identical(strLonger, strAsked))
  expect_identical(Serve_Reply(strLonger), Serve_Reply("[{\"id\":5,\"name\":\"Analyze_GroupDifference\"}]"))
  expect_false(identical(Serve_Reply(strLonger)[[1]], lWhole))
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
      quote(Widget_CrossTab(Synthetic_Results, Synthetic_Participants, lSettings = list(row_by = "ARM", col_by = "RESPONSE"))),
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

test_that("in a Shiny page a view the saved widget does not store is answered by the session with what the statistics function returns for the same rows, and the page says it was computed on this server, on a line that prints R's answer (#71, #86)", {
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
  lOpened <- lLookAnswered(lPage)
  expect_true(lOpened$answered)
  expect_true(lOpened$settled)
  expect_length(lOpened$statistics, 1L)
  expect_identical(lOpened$statistics[[1]]$answer$status, "ok")
  expect_identical(lOpened$statistics[[1]]$answer$form, "server")

  # The reader chooses another test: no saved widget of these settings has it.
  lPage$Move("choose", "test", "wilcoxon")
  lView <- lLookAnswered(lPage)
  expect_true(lView$answered)
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
  # The line under the chart prints the answer: R's method, the p-value of
  # the function called directly, written by the line's rule, and the counts
  # (#86).
  nP <- Analyze_GroupDifference(dfServeRows(), "STRESN", "ARM", strMethod = "wilcoxon")$p_value
  expect_identical(lExpected[[strKey]]$value$p_value, nP)
  expect_match(lView$lines[[1]], Output_P(nP), fixed = TRUE)
  expect_true(startsWith(lView$lines[[1]], Output_StatisticText(lExpected[[strKey]]$value)), label = "the line opens with R's answer written out")
  expect_match(lView$provenance[[1]], sprintf("^Statistics: computed on request by R %s with gsm.bio %s on this server\\.", strR, strGsmBio))
  expect_identical(lView$errors, list())
  expect_identical(lPage$Errors(), character(0))
})

test_that("a session whose server function does not call Serve_Statistics() draws the chart and says its statistics are unavailable because the session did not answer in twenty seconds, and a session that ends is said to be out of reach, not an error of R's (#71, #86)", {
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
    paste0(
      "Statistics are unavailable: R on the server could not be reached (its session did not answer within 20 seconds: ",
      "it may be busy, or Serve_Statistics() may not be called in the server function)."
    )
  )
  expect_false(any(grepl("p = ", unlist(lSaid$lines), fixed = TRUE)))
  lSilent$Close()

  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)
  expect_identical(lLookAnswered(lPage)$statistics[[1]]$answer$form, "server")
  # The session ends under the page.
  lApp$Stop()
  expect_true(bWaitFor(lPage, "!window.Shiny.shinyapp.isConnected()"), label = "the page knows its session has ended")
  lPage$Move("choose", "test", "wilcoxon")
  lView <- lLookAnswered(lPage)
  expect_true(lView$answered)
  expect_true(lView$settled)
  expect_identical(lView$statistics[[1]]$answer$status, "unavailable")
  expect_identical(
    lView$statistics[[1]]$answer$message,
    "Statistics are unavailable: R on the server could not be reached (the session with the server has ended; reload the page)."
  )
  expect_false(any(grepl("p = ", unlist(lView$lines), fixed = TRUE)))
})

# ---- A session that is slow to answer (#86) -----------------------------------

# A group comparison in an app whose R can be kept busy, as another reader's
# long job keeps the one R process of a server busy. R does nothing else from
# the moment the page names a file until that file is there, so the test says
# when R is free and nothing is timed.
#
# The page's own script sets how long a widget waits for its session to two
# seconds, in place of twenty; it keeps what the chart said of its statistic
# each time that changed; and it makes R busy as the chart's first value
# arrives, before the chart has asked anything.
strBusyApp <- function(strFirst) {
  strScript <- paste(
    "window.GsmBioWidget.limits.patience = 2000;",
    "window.gsmBioSeen = [];",
    "setInterval(function() {",
    "  const widget = window.HTMLWidgets && HTMLWidgets.find('#chart');",
    "  const chart = widget && widget.chart && widget.chart();",
    "  if (!chart) return;",
    "  const asked = chart.statistics()[0];",
    "  const now = !asked ? 'nothing asked' : !asked.answer ? 'waiting' : asked.answer.status + (asked.answer.message ? ': ' + asked.answer.message : '');",
    "  if (window.gsmBioSeen[window.gsmBioSeen.length - 1] !== now) window.gsmBioSeen.push(now);",
    "}, 20);",
    "window.gsmBioBusy = function(file) { Shiny.setInputValue('busy', file, { priority: 'event' }); return true; };",
    "window.gsmBioFirst = true;",
    "$(document).on('shiny:value', function(event) {",
    "  if (event.name !== 'chart' || !window.gsmBioFirst) return;",
    "  window.gsmBioFirst = false;",
    sprintf("  window.gsmBioBusy(%s);", jsonlite::toJSON(strFirst, auto_unbox = TRUE)),
    "});",
    "Shiny.addCustomMessageHandler('pong', function(count) { window.gsmBioPong = count; });",
    sep = "\n"
  )
  sprintf("
shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::tabsetPanel(
      shiny::tabPanel('Chart', Widget_GroupComparisonOutput('chart')),
      shiny::tabPanel('Other', shiny::tags$p('No chart here.'))
    ),
    shiny::tags$script(shiny::HTML(%s))
  ),
  server = function(input, output, session) {
    Serve_Statistics()
    shiny::observeEvent(input$busy, {
      nStart <- Sys.time()
      while (!file.exists(input$busy) && difftime(Sys.time(), nStart, units = 'secs') < 120) Sys.sleep(0.05)
    })
    shiny::observeEvent(input$ping, session$sendCustomMessage('pong', input$ping))
    output$chart <- renderWidget_GroupComparison({
      input$again
      Widget_GroupComparison(
        Synthetic_Results, Synthetic_Participants,
        lSettings = list(start_value = 'CRP', visits = 'Week 4', group_by = 'ARM')
      )
    })
  }
)
", paste(deparse(strScript), collapse = ""))
}

test_that("a chart that asks while R is busy past the time it waits says the session did not answer in time, which is all it knows, and prints its statistic once R is free with no control changed; a chart that is not shown then asks when it is (#86)", {
  NeedApp()
  lSettings <- list(start_value = "CRP", visits = "Week 4", group_by = "ARM")
  lStored <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lSettings)$x$lStatistics$results
  lStored <- stats::setNames(lStored, vapply(lStored, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1)))
  strDir <- tempfile("gsm-bio-busy")
  dir.create(strDir)
  chrFree <- file.path(strDir, c("first", "second"))
  strLate <- paste0(
    "unavailable: Statistics are unavailable: R on the server could not be reached (its session did not answer within 2 seconds: ",
    "it may be busy, or Serve_Statistics() may not be called in the server function)."
  )
  Seen <- function(lPage) unlist(lPage$Evaluate("window.gsmBioSeen"))
  Last <- "window.gsmBioSeen[window.gsmBioSeen.length - 1]"
  # The session has answered everything the page sent it before this returns.
  nPings <- 0L
  Answered <- function(lPage) {
    nPings <<- nPings + 1L
    lPage$Evaluate(sprintf("Shiny.setInputValue('ping', %d, { priority: 'event' })", nPings))
    bWaitFor(lPage, sprintf("window.gsmBioPong === %d", nPings))
  }
  # The chart's answer is R's, called directly, and so is the line printed.
  ExpectAnswer <- function(lView) {
    lAsked <- lView$statistics[[1]]
    strKey <- Chart_KeyText(lAsked[c("name", "args", "dataId")])
    expect_true(strKey %in% names(lStored))
    expect_identical(lAsked$answer$status, "ok")
    expect_identical(lAsked$answer$form, "server")
    expect_equal(lAsked$answer$value, lStored[[strKey]]$value, tolerance = 1e-14)
    expect_identical(lAsked$answer$value$p_value, Analyze_GroupDifference(dfServeRows(), "STRESN", "ARM")$p_value)
    expect_true(startsWith(lView$lines[[1]], Output_StatisticText(lStored[[strKey]]$value)), label = "the line opens with R's answer written out")
  }

  lApp <- lRunApp(strBusyApp(chrFree[1]))
  on.exit(lApp$Stop(), add = TRUE)
  on.exit(file.create(chrFree), add = TRUE)
  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)

  # R is busy as the chart first asks. The chart waits its two seconds and
  # says what it knows: nothing of a call that is in fact made.
  expect_true(bWaitFor(lPage, sprintf("/^unavailable/.test(%s)", Last)), label = "the chart gave up on a busy session")
  expect_identical(Seen(lPage), c("waiting", strLate))
  lGaveUp <- lPage$Look()
  expect_gt(lGaveUp$canvases, 0L)
  expect_false(any(grepl("p = |p < ", unlist(lGaveUp$lines))))
  expect_match(lGaveUp$lines[[1]], "its session did not answer within 2 seconds", fixed = TRUE)
  # R is free. Nothing in the page is touched: the chart asks again by itself.
  file.create(chrFree[1])
  expect_true(bWaitFor(lPage, sprintf("%s === 'ok'", Last)), label = "the chart printed its statistic once R was free")
  expect_identical(Seen(lPage), c("waiting", strLate, "waiting", "ok"))
  ExpectAnswer(lPage$Look())

  # The chart is drawn again while R is busy, gives up again, and its tab is
  # left before R is free. A chart that is not shown is not drawn: it keeps
  # what it said until its tab is opened, and asks then.
  lPage$Evaluate(sprintf(
    "(() => { Shiny.setInputValue('again', 1, { priority: 'event' }); setTimeout(() => window.gsmBioBusy(%s), 0); return true; })()",
    jsonlite::toJSON(chrFree[2], auto_unbox = TRUE)
  ))
  expect_true(bWaitFor(lPage, sprintf("window.gsmBioSeen.length === 6 && /^unavailable/.test(%s)", Last)), label = "the chart gave up a second time")
  expect_identical(Seen(lPage), c("waiting", strLate, "waiting", "ok", "waiting", strLate))
  lPage$Evaluate("document.querySelector('a[data-value=\"Other\"]').click()")
  expect_true(bWaitFor(lPage, "document.querySelector('#chart').offsetParent === null"), label = "the chart's tab was left")
  file.create(chrFree[2])
  expect_true(Answered(lPage), label = "the session answered what it was sent")
  expect_identical(lPage$Evaluate("window.gsmBioPage.chart().statistics()[0].answer.status"), "unavailable")
  expect_identical(Seen(lPage), c("waiting", strLate, "waiting", "ok", "waiting", strLate))
  lPage$Evaluate("document.querySelector('a[data-value=\"Chart\"]').click()")
  expect_true(bWaitFor(lPage, sprintf("%s === 'ok'", Last)), label = "the chart asked when its tab was opened")
  expect_identical(Seen(lPage), c("waiting", strLate, "waiting", "ok", "waiting", strLate, "waiting", "ok"))
  ExpectAnswer(lPage$Look())
  expect_identical(lPage$Errors(), character(0))
})
