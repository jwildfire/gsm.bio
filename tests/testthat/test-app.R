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
  strPage <- as.character(App_Ui(App_Study(NULL, NULL, NULL)))
  # The list, in order, each chart under its own name.
  nAt <- vapply(unname(chrAppCharts), function(strTitle) regexpr(paste0(">", strTitle, "<"), strPage, fixed = TRUE)[1], numeric(1))
  expect_true(all(nAt > 0))
  expect_identical(order(nAt), seq_along(nAt))
  for (strChart in names(chrAppCharts)) {
    expect_match(strPage, sprintf("class=\"Widget_%s html-widget html-widget-output", strChart), fixed = TRUE)
    expect_match(strPage, sprintf(" id=\"%s\"", strChart), fixed = TRUE)
  }
  expect_match(strPage, "Drawn on the synthetic study that ships with gsm.bio.", fixed = TRUE)
  expect_match(strPage, sprintf(
    "Every statistic is computed on request by R %s on this server", paste(R.version$major, R.version$minor, sep = ".")
  ), fixed = TRUE)
  # No control of a chart is made again as a Shiny input: the page's only
  # input is the list of charts.
  expect_false(grepl("<select|<input|<button", strPage))

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
  strPage <- as.character(App_Ui(App_Study(dfResults, NULL, NULL)))
  expect_false(grepl("id=\"StratifiedSurvival\"", strPage, fixed = TRUE))
  expect_match(strPage, "The stratified survival chart reads an outcomes table", fixed = TRUE)
  expect_match(strPage, "Drawn on the tables this app was started with.", fixed = TRUE)
  expect_error(lAppPayloads(xApp, "StratifiedSurvival"))
  # With outcomes it is drawn.
  strWith <- as.character(App_Ui(App_Study(dfResults, NULL, Synthetic_Outcomes)))
  expect_match(strWith, "id=\"StratifiedSurvival\"", fixed = TRUE)
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
strSixCharts <- "
RunApp(lSettings = list(
  GroupComparison = list(start_value = 'CRP', visits = 'Week 4', group_by = 'ARM'),
  AssociationScatter = list(color_by = 'ARM'),
  BiomarkerScreen = list(group_by = 'ARM'),
  CrossTab = list(row_by = 'ARM', col_by = 'RESPONSE'),
  StratifiedSurvival = list(group_by = 'ARM')
))
"

test_that("in a browser each of the six charts is drawn when it is chosen from the list, with a statistic from the session and the line under it saying it was computed on this server (#72)", {
  NeedApp()
  lApp <- lRunApp(strSixCharts)
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)
  strServer <- sprintf(
    "computed by R %s with gsm.bio %s on this server.",
    paste(R.version$major, R.version$minor, sep = "."), as.character(utils::packageVersion("gsm.bio"))
  )
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
  }
  expect_identical(lPage$Errors(), character(0))
})
