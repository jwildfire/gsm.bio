# RunApp(): the six charts in one Shiny app, on the tables it is given (#72).
#
# The first tests build the app and its session with no browser. The last runs
# it in a second R session and opens each chart in a headless browser, as
# tests/testthat/test-serve.R does for one widget.

# The payload a chart's output sends the page, in a session of the app.
lAppPayloads <- function(xApp, chrCharts) {
  # A session of the app starts the app, which sets the size of file it
  # accepts; nothing stops it here, so the size is put back by hand.
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  lPayloads <- list()
  shiny::testServer(xApp, {
    for (strChart in chrCharts) {
      lPayloads[[strChart]] <<- jsonlite::fromJSON(output[[strChart]], simplifyVector = FALSE)$x
    }
  })
  lPayloads
}

test_that("RunApp() returns a Shiny app that lists the six charts, on the synthetic study when it is given no table (#72)", {
  skip_if_not_installed("shiny")
  xApp <- RunApp()
  expect_s3_class(xApp, "shiny.appobj")
  expect_identical(
    names(formals(RunApp)),
    c("dfResults", "dfParticipants", "dfOutcomes", "lSettings", "nMaxUploadMB")
  )
  expect_identical(formals(RunApp)$nMaxUploadMB, 100)
  strPage <- as.character(App_Ui())
  # The list, in order, each chart under its own name, after the Data view.
  expect_lt(regexpr(">Data<", strPage, fixed = TRUE)[1], regexpr(">Group comparison<", strPage, fixed = TRUE)[1])
  nAt <- vapply(unname(chrAppCharts), function(strTitle) regexpr(paste0(">", strTitle, "<"), strPage, fixed = TRUE)[1], numeric(1))
  expect_true(all(nAt > 0))
  expect_identical(order(nAt), seq_along(nAt))
  # Every chart has its place in the page; the survival chart's is filled by
  # the session, which knows whether there is an outcomes table (#73).
  for (strChart in setdiff(names(chrAppCharts), "StratifiedSurvival")) {
    expect_match(strPage, sprintf("class=\"Widget_%s html-widget html-widget-output", strChart), fixed = TRUE)
    expect_match(strPage, sprintf(" id=\"%s\"", strChart), fixed = TRUE)
  }
  expect_match(strPage, "id=\"gsm_bio_place_StratifiedSurvival\"", fixed = TRUE)
  # The chart the page opens on is the first chart, not the Data view.
  expect_match(strPage, "<li class=\"active\">\\s*<a [^>]*data-value=\"GroupComparison\"")
  expect_match(strPage, sprintf(
    "Every statistic is computed on request by R %s on this server", paste(R.version$major, R.version$minor, sep = ".")
  ), fixed = TRUE)
  # No control of a chart is made again as a Shiny input: the page's inputs
  # are the Data view's: the viewer's two buttons (#80), the three files and
  # their button (#73), and nothing else. The viewer's tabs are written by the
  # session.
  chrInputs <- regmatches(strPage, gregexpr("<(input|select|button|textarea)[^>]*>", strPage))[[1]]
  chrIds <- regmatches(chrInputs, regexpr("id=\"[^\"]*\"", chrInputs))
  expect_identical(
    chrIds,
    c(
      "id=\"gsm_bio_view_previous\"", "id=\"gsm_bio_view_next\"",
      "id=\"gsm_bio_file_results\"", "id=\"gsm_bio_file_participants\"", "id=\"gsm_bio_file_outcomes\"", "id=\"gsm_bio_apply\""
    )
  )
  # An input with no name of its own is a file input's own line saying which file.
  expect_true(all(grepl("readonly", chrInputs[!grepl("id=", chrInputs, fixed = TRUE)], fixed = TRUE)))

  # Every chart is drawn by the session on the synthetic study's tables, and is
  # one the session answers: nothing is stored with the page.
  lPayloads <- lAppPayloads(xApp, names(chrAppCharts))
  expect_named(lPayloads, names(chrAppCharts))
  for (strChart in names(chrAppCharts)) {
    lPayload <- lPayloads[[strChart]]
    expect_true(lPayload$lStatistics$served, label = strChart)
    expect_identical(lPayload$lStatistics$results, list(), label = strChart)
    expect_length(lPayload$dfResults$USUBJID, nrow(Synthetic_Results))
    expect_length(lPayload$dfParticipants$USUBJID, nrow(Synthetic_Participants))
  }
  expect_length(lPayloads$StratifiedSurvival$dfOutcomes$USUBJID, nrow(Synthetic_Outcomes))
  expect_length(lPayloads$BiomarkerScreen$dfOutcomes$USUBJID, nrow(Synthetic_Outcomes))
})

test_that("RunApp() on a results table alone draws the charts with no participants and no filters, and a sentence in place of the stratified survival chart; a chart's settings reach its chart (#72)", {
  skip_if_not_installed("shiny")
  dfResults <- Synthetic_Results[Synthetic_Results$TEST %in% c("CRP", "IL-6"), ]
  xApp <- RunApp(dfResults, lSettings = list(GroupComparison = list(start_value = "CRP", tile_summary = "mean")))
  chrDrawn <- setdiff(names(chrAppCharts), "StratifiedSurvival")
  lPayloads <- lAppPayloads(xApp, chrDrawn)
  for (strChart in chrDrawn) {
    expect_length(lPayloads[[strChart]]$dfResults$USUBJID, nrow(dfResults))
    expect_null(lPayloads[[strChart]]$dfParticipants, label = strChart)
    expect_null(lPayloads[[strChart]]$dfOutcomes, label = strChart)
    expect_null(lPayloads[[strChart]]$lSettings$filters, label = strChart)
  }
  expect_identical(lPayloads$GroupComparison$lSettings$start_value, "CRP")
  expect_identical(lPayloads$GroupComparison$lSettings$tile_summary, "mean")
  expect_null(lPayloads$AssociationScatter$lSettings$start_value)
  # The survival chart has no output: its place in the page is a sentence.
  Said <- function(xApp) {
    lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
    on.exit(options(lWas), add = TRUE)
    lSaid <- list()
    shiny::testServer(xApp, {
      lSaid$place <<- as.character(output$gsm_bio_place_StratifiedSurvival$html)
      lSaid$source <<- output$gsm_bio_source
    })
    lSaid
  }
  lSaid <- Said(xApp)
  expect_false(grepl("id=\"StratifiedSurvival\"", lSaid$place, fixed = TRUE))
  expect_match(lSaid$place, "The stratified survival chart reads an outcomes table", fixed = TRUE)
  expect_identical(lSaid$source, "Drawn on the tables this app was started with.")
  expect_error(lAppPayloads(xApp, "StratifiedSurvival"))
  # With outcomes it is drawn.
  expect_match(Said(RunApp(dfResults, dfOutcomes = Synthetic_Outcomes))$place, "id=\"StratifiedSurvival\"", fixed = TRUE)
  expect_identical(Said(RunApp())$source, "Drawn on the synthetic study that ships with gsm.bio.")
})

test_that("a table that lacks a column the charts need is refused with a sentence naming the column, and so is anything that is no table, no chart's settings or no size (#72)", {
  skip_if_not_installed("shiny")
  expect_error(RunApp(Synthetic_Results[, setdiff(names(Synthetic_Results), "VISITNUM")]), "`dfResults` has no column named `VISITNUM`", fixed = TRUE)
  expect_error(
    RunApp(Synthetic_Results[, c("USUBJID", "VISIT", "VISITNUM")]),
    "`dfResults` has no column named `TEST`, `STRESN`. The app reads the results table under gsm.bio's column names (USUBJID, TEST, STRESN, VISIT, VISITNUM): rename the columns before calling RunApp().",
    fixed = TRUE
  )
  expect_error(RunApp(Synthetic_Results, Synthetic_Participants[, "ARM", drop = FALSE]), "`dfParticipants` has no column named `USUBJID`", fixed = TRUE)
  expect_error(
    RunApp(Synthetic_Results, dfOutcomes = Synthetic_Outcomes[, c("USUBJID", "AVAL")]),
    "`dfOutcomes` has no column named `PARAMCD`, `PARAM`, `CNSR`",
    fixed = TRUE
  )
  expect_error(RunApp("results.csv"), "`dfResults` must be a data frame: the results table.", fixed = TRUE)
  expect_error(RunApp(dfParticipants = Synthetic_Participants), "`dfResults` is needed with `dfParticipants` or `dfOutcomes`", fixed = TRUE)
  expect_error(RunApp(lSettings = list(Histogram = list())), "`lSettings` names `Histogram`, which is no chart of the app.", fixed = TRUE)
  expect_error(RunApp(lSettings = list(list(group_by = "ARM"))), "`lSettings` must be a named list", fixed = TRUE)
  expect_error(RunApp(lSettings = list(CrossTab = "ARM")), "`lSettings$CrossTab` must be a list of that chart's settings", fixed = TRUE)
  for (xSize in list(0, -5, NA_real_, "100", c(10, 20))) {
    expect_error(RunApp(nMaxUploadMB = xSize), "`nMaxUploadMB` must be one number above zero", fixed = TRUE)
  }
})

test_that("without shiny RunApp() stops with one sentence naming the package to install (#72)", {
  local_mocked_bindings(Serve_HasShiny = function() FALSE)
  expect_error(
    RunApp(),
    "RunApp() draws a widget in a Shiny page, and shiny is not installed. Install it with install.packages(\"shiny\"); the widgets themselves do not need it.",
    fixed = TRUE
  )
})

test_that("the app accepts files up to the size it was given for as long as it runs, and puts Shiny's own limit back (#72)", {
  skip_if_not_installed("shiny")
  lWas <- options(shiny.maxRequestSize = 123)
  on.exit(options(lWas), add = TRUE)
  xApp <- RunApp(nMaxUploadMB = 250)
  # Making the app changes nothing: the size is set when it starts.
  expect_identical(getOption("shiny.maxRequestSize"), 123)
  # onStart is the app's, run when it starts: here by hand, with what stops it.
  lStops <- list()
  local_mocked_bindings(onStop = function(fun, ...) lStops[[length(lStops) + 1L]] <<- fun, .package = "shiny")
  xApp$onStart()
  expect_identical(getOption("shiny.maxRequestSize"), 250 * 1024^2)
  expect_length(lStops, 1L)
  lStops[[1]]()
  expect_identical(getOption("shiny.maxRequestSize"), 123)
})

# The app on the synthetic study, each chart opened on a view that asks R.
lSixCharts <- list(
  GroupComparison = list(start_value = "CRP", visits = "Week 4", group_by = "ARM"),
  AssociationScatter = list(color_by = "ARM"),
  BiomarkerScreen = list(group_by = "ARM"),
  CrossTab = list(row_by = "ARM", col_by = "RESPONSE"),
  StratifiedSurvival = list(group_by = "ARM")
)
strSixCharts <- sprintf("RunApp(lSettings = %s)", paste(deparse(lSixCharts), collapse = ""))

# What a chart of the app asks R, answered with no page and no session: the
# same chart made as a widget outside the app, on the same tables and
# settings. Every result such a widget stores is an `Analyze_*()` function's
# answer for the rows the chart draws (Chart_Answer() calls the function), so
# a stored result is R called directly. Named by what was asked.
lAskedDirectly <- function(strChart, lSettings = lSixCharts[[strChart]]) {
  fnWidget <- getExportedValue("gsm.bio", paste0("Widget_", strChart))
  lTables <- list(Synthetic_Results, Synthetic_Participants)
  if ("dfOutcomes" %in% names(formals(fnWidget))) lTables$dfOutcomes <- Synthetic_Outcomes
  lResults <- do.call(fnWidget, c(lTables, list(lSettings = if (is.null(lSettings)) list() else lSettings)))$x$lStatistics$results
  stats::setNames(lResults, vapply(lResults, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1)))
}

# How many numbers a result holds.
nNumbersIn <- function(xValue) {
  sum(rapply(list(xValue), function(xOne) sum(!is.na(xOne)), classes = c("numeric", "integer"), deflt = 0L, how = "unlist"))
}

test_that("in a browser each of the six charts is drawn when it is chosen from the list, with a statistic from the session that is its statistics function's own answer for the same rows, and the line under it saying it was computed on this server (#72, #86)", {
  NeedApp()
  lApp <- lRunApp(strSixCharts)
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)
  strServer <- sprintf(
    "computed by R %s with gsm.bio %s on this server.",
    paste(R.version$major, R.version$minor, sep = "."), as.character(utils::packageVersion("gsm.bio"))
  )
  # The statistics function each chart asks on the view it opens on.
  chrSixFunctions <- c(
    GroupComparison = "Analyze_GroupDifference", AssociationScatter = "Analyze_Correlation",
    CorrelationMatrix = "Analyze_CorrelationMatrix", BiomarkerScreen = "Analyze_Screen",
    CrossTab = "Analyze_Contingency", StratifiedSurvival = "Analyze_Survival"
  )
  chrHeld <- character(0)
  # One chart at a time: before a chart is chosen, the page has not drawn it.
  expect_identical(
    unlist(lPage$Evaluate("Array.from(document.querySelectorAll('.html-widget')).filter((node) => node.querySelector('.gsm-bio-chart')).map((node) => node.id)")),
    "GroupComparison"
  )
  for (strChart in names(chrAppCharts)) {
    strFind <- sprintf("(HTMLWidgets.find('#%s') && HTMLWidgets.find('#%s').chart())", strChart, strChart)
    lPage$Evaluate(sprintf("document.querySelector('a[data-value=\"%s\"]').click()", strChart))
    expect_true(bWaitFor(lPage, strFind), label = paste(strChart, "was drawn"))
    # Every question the chart asked R is answered, and there is one at least.
    expect_true(
      bWaitFor(lPage, sprintf("%s.statistics().length > 0 && %s.statistics().every((asked) => asked.answer)", strFind, strFind)),
      label = paste(strChart, "was answered")
    )
    lSeen <- lPage$Evaluate(sprintf(
      "({ asked: %s.statistics().map((asked) => ({ name: asked.name, status: asked.answer.status, form: asked.answer.form })),
          drawn: document.querySelector('#%s .gsm-bio-chart').childElementCount,
          foot: Array.from(document.querySelectorAll('#%s .bv-foot-line')).map((line) => line.textContent).join(' '),
          shown: document.querySelector('#%s').offsetParent !== null,
          errors: Array.from(document.querySelectorAll('#%s .gsm-bio-error')).map((node) => node.textContent) })",
      strFind, strChart, strChart, strChart, strChart
    ))
    expect_gt(lSeen$drawn, 0L, label = paste(strChart, "has a chart in the page"))
    expect_true(lSeen$shown, label = paste(strChart, "is the chart shown"))
    expect_identical(lSeen$errors, list(), label = paste(strChart, "errors"))
    for (lAsked in lSeen$asked) {
      expect_true(lAsked$name %in% names(Serve_Functions()), label = paste(strChart, "asked for", lAsked$name))
      expect_identical(lAsked$status, "ok", label = paste(strChart, lAsked$name, "status"))
      expect_identical(lAsked$form, "server", label = paste(strChart, lAsked$name, "form"))
    }
    expect_match(lSeen$foot, strServer, fixed = TRUE, label = paste(strChart, "footnote"))
    # The value itself, not only that there is one (#86): everything the chart
    # was answered is what its statistics function returns, called directly,
    # for the same rows and arguments, to the sixteen digits a number crosses
    # to the page with.
    lDirect <- lAskedDirectly(strChart)
    lAnswers <- lPage$Evaluate(sprintf("%s.statistics()", strFind))
    expect_gt(length(lAnswers), 0L, label = paste(strChart, "answers"))
    for (lAsked in lAnswers) {
      strKey <- Chart_KeyText(lAsked[c("name", "args", "dataId")])
      expect_true(strKey %in% names(lDirect), label = paste(strChart, "asks what a widget of its settings asks"))
      expect_identical(lAsked$name, chrSixFunctions[[strChart]], label = paste(strChart, "function"))
      expect_identical(lDirect[[strKey]]$value$status, "ok", label = paste(strChart, "direct status"))
      expect_gt(nNumbersIn(lDirect[[strKey]]$value), 0L, label = paste(strChart, "numbers in the direct answer"))
      expect_equal(lAsked$answer$value, lDirect[[strKey]]$value, tolerance = 1e-14, label = paste(strChart, "value"))
      expect_identical(lAsked$rows, lDirect[[strKey]]$rows, label = paste(strChart, "rows"))
    }
    chrHeld <- c(chrHeld, strChart)
  }
  expect_identical(chrHeld, names(chrAppCharts))
  expect_identical(lPage$Errors(), character(0))
})

# What a page sends the session, changed on its way: the chart's own request
# goes as the chart made it, but for what `strChange`, the body of a function
# of one request, does to it. The session is sent the changed request, so the
# answer the chart shows is the session's answer to that.
strOnTheWire <- function(strChange) {
  sprintf(
    "(() => {
      window.gsmBioSend = window.gsmBioSend || Shiny.setInputValue;
      Shiny.setInputValue = function(name, value, options) {
        if (name === 'gsm_bio_request') {
          const batch = JSON.parse(value);
          batch.forEach((request) => { if (!request.hello) { %s } });
          value = JSON.stringify(batch);
        }
        return window.gsmBioSend.call(this, name, value, options);
      };
      return true;
    })()", strChange
  )
}
strOffTheWire <- "(() => { Shiny.setInputValue = window.gsmBioSend; return true; })()"

# The app's first chart, with a control a reader changes to make it ask again.
strAppOneChart <- sprintf("RunApp(lSettings = %s)", paste(deparse(lSixCharts["GroupComparison"]), collapse = ""))

test_that("through the running app a request that names a function outside the nine is refused in the page with nothing run: the file the call would have written is not there (#86)", {
  NeedApp()
  strDir <- tempfile("gsm-bio-refused")
  dir.create(strDir)
  strFile <- file.path(strDir, "written.rds")
  # The call the page asks for, made here as the session would make one: it
  # writes the file. So a file that is not there was not written.
  do.call("saveRDS", c(list(data.frame(STRESN = 1)), list(file = strFile)))
  expect_true(file.exists(strFile))
  unlink(strFile)
  expect_false(file.exists(strFile))

  lApp <- lRunApp(strAppOneChart)
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)
  expect_identical(lLookAnswered(lPage)$statistics[[1]]$answer$status, "ok")

  # The chart's next request goes to the session as a call of saveRDS() on
  # the rows it sends, to that file.
  lPage$Evaluate(strOnTheWire(sprintf(
    "request.name = 'saveRDS'; request.args = { file: %s };", jsonlite::toJSON(strFile, auto_unbox = TRUE)
  )))
  lPage$Move("choose", "test", "wilcoxon")
  lView <- lLookAnswered(lPage)
  expect_true(lView$answered)
  strRefusal <- paste0(
    "This server runs gsm.bio's statistics functions and no other: ", paste(names(Serve_Functions()), collapse = ", "), "."
  )
  expect_identical(lView$statistics[[1]]$answer$status, "error")
  expect_identical(lView$statistics[[1]]$answer$message, strRefusal)
  expect_null(lView$statistics[[1]]$answer$value)
  # The reader is told so under the chart, where the statistic would be.
  expect_identical(lView$lines[[1]], paste("R reported an error:", strRefusal))
  expect_false(file.exists(strFile))
  expect_identical(list.files(strDir), character(0))

  # The session still answers what it does run.
  lPage$Evaluate(strOffTheWire)
  lPage$Move("choose", "test", "t")
  lAfter <- lLookAnswered(lPage)
  expect_identical(lAfter$statistics[[1]]$answer$status, "ok")
  expect_identical(lAfter$statistics[[1]]$name, "Analyze_GroupDifference")
  expect_false(file.exists(strFile))
  expect_identical(lPage$Errors(), character(0))
})

test_that("through the running app an error R raises in a statistics function is said under the chart in R's own words, and the session answers the next request (#86)", {
  NeedApp()
  lDirect <- lAskedDirectly("GroupComparison")
  lApp <- lRunApp(strAppOneChart)
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)
  expect_identical(lLookAnswered(lPage)$statistics[[1]]$answer$status, "ok")

  # The chart's next request goes with an argument its function does not
  # have: R raises an error as it calls the function.
  lPage$Evaluate(strOnTheWire("request.args.bNoSuchArgument = true;"))
  lPage$Move("choose", "test", "wilcoxon")
  lView <- lLookAnswered(lPage)
  expect_true(lView$answered)
  expect_identical(lView$statistics[[1]]$name, "Analyze_GroupDifference")
  # What R says of the same call made here.
  strR <- tryCatch(
    Analyze_GroupDifference(Synthetic_Results, "STRESN", "TEST", bNoSuchArgument = TRUE),
    error = function(cndError) conditionMessage(cndError)
  )
  expect_identical(strR, "unused argument (bNoSuchArgument = TRUE)")
  expect_identical(lView$statistics[[1]]$answer$status, "error")
  expect_identical(lView$statistics[[1]]$answer$message, strR)
  expect_identical(lView$lines[[1]], paste("R reported an error:", strR))
  expect_false(any(grepl("p = |p < ", unlist(lView$lines))))

  # The next request, sent as the chart made it, is answered with the function's own answer.
  lPage$Evaluate(strOffTheWire)
  lPage$Move("choose", "test", "t")
  lAfter <- lLookAnswered(lPage)
  lAsked <- lAfter$statistics[[1]]
  strKey <- Chart_KeyText(lAsked[c("name", "args", "dataId")])
  expect_true(strKey %in% names(lDirect))
  expect_identical(lAsked$answer$status, "ok")
  expect_equal(lAsked$answer$value, lDirect[[strKey]]$value, tolerance = 1e-14)
  expect_identical(lPage$Errors(), character(0))
})
