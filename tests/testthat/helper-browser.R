# A saved widget page, opened in a headless browser with no network and driven
# as a reader drives it (#53). chromote speaks to a Chrome or a Chromium on
# this machine; nothing is installed by the tests. Nothing here is exported.

# Whether a headless browser can be started here: chromote is installed and
# finds a Chrome.
bBrowser <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    return(FALSE)
  }
  strChrome <- tryCatch(chromote::find_chrome(), error = function(cndError) NULL, warning = function(cndWarning) NULL)
  !is.null(strChrome) && !is.na(strChrome) && nzchar(strChrome) && file.exists(strChrome)
}

# The tests that open a page run in the source tree, which is where
# devtools::test() and CI's source-tree step run the suite. There a browser
# that cannot be started is a failure, never a skip: these are the tests that
# prove the saved page, and they must not read as a pass. R CMD check runs the
# suite against the built package and leaves them out, so the check starts no
# browser and leaves no browser's files behind it.
NeedBrowser <- function() {
  if (!bSourceTree()) {
    testthat::skip("a saved page is opened in a browser from the source tree, not under R CMD check")
  }
  if (!bPandoc()) {
    testthat::fail("pandoc was not found: htmlwidgets::saveWidget(selfcontained = TRUE) needs it, and so does this test")
    testthat::skip("pandoc is not available to save a self-contained page")
  }
  if (!bBrowser()) {
    testthat::fail("no headless browser was found: the chromote package and a Chrome or Chromium are needed to open the saved page")
    testthat::skip("no headless browser is available to open a saved page")
  }
  invisible(TRUE)
}

# What the page is driven with, defined in the page once it has loaded. Every
# move is a reader's: a click on a tile, on a visit's name, on a step of the
# trail above the chart, or a control changed. `view()` is what the chart says
# of itself: the level it draws, what it asked R and what it was answered
# (`chart.statistics()`), and what the page shows where a result would be.
strBrowserDriver <- "
window.gsmBioPage = (function () {
  const widget = () => HTMLWidgets.find('#' + document.querySelector('.html-widget').id);
  const chart = () => widget().chart();
  const said = () => JSON.stringify([chart().root.dataset.level, chart().statistics()]);
  // The chart draws at once and is answered from the stored results in a
  // turn of the page's loop: the view is settled when it has stopped changing.
  const settle = async () => {
    const start = Date.now();
    let last = null;
    let same = 0;
    while (Date.now() - start < 10000) {
      await new Promise((done) => setTimeout(done, 30));
      const now = said();
      if (now === last) {
        same += 1;
        if (same >= 3) return true;
      } else {
        same = 0;
        last = now;
      }
    }
    return false;
  };
  const texts = (selector) => [...chart().root.querySelectorAll(selector)].map((node) => node.textContent);
  const view = () => ({
    level: chart().root.dataset.level,
    measure: chart().state.measure,
    visits: chart().state.visits,
    offered: chart().visitsOffered(),
    statistics: chart().statistics(),
    tiles: [...chart().root.querySelectorAll('button.bv-tile')].map((node) => node.dataset.measure),
    canvases: chart().root.querySelectorAll('canvas').length,
    visitButtons: [...chart().root.querySelectorAll('button.bv-time-visit')].map((node) => node.dataset.visit),
    testRow: [...chart().root.querySelectorAll('tr[data-row=\"test\"] td[data-visit]')].map((node) => ({
      visit: node.dataset.visit, status: node.dataset.status, text: node.textContent
    })),
    testRowState: (chart().root.querySelector('tr[data-row=\"test\"]') || { dataset: {} }).dataset.state || null,
    testRowHead: texts('tr[data-row=\"test\"] th').join(' '),
    lines: texts('.bv-statistic'),
    panels: [...chart().root.querySelectorAll('[data-panel]')].map((node) => node.dataset.panel),
    trail: texts('nav.bv-trail li'),
    hidden: texts('.bv-hidden-visits'),
    footnote: chart().footnote ? chart().footnote.textContent : null,
    provenance: [...document.querySelectorAll('.gsm-bio-provenance')].map((node) => node.textContent),
    errors: [...document.querySelectorAll('.gsm-bio-error')].map((node) => node.textContent)
  });
  const press = (node, what) => {
    if (!node) throw new Error('the page has no ' + what);
    node.click();
  };
  const moves = {
    tile: (measure) => press([...chart().root.querySelectorAll('button.bv-tile')].find((node) => node.dataset.measure === measure), 'tile of ' + measure),
    visit: (visit) => press([...chart().root.querySelectorAll('button.bv-time-visit')].find((node) => node.dataset.visit === visit), 'visit named ' + visit),
    trail: (words) => press([...chart().root.querySelectorAll('nav.bv-trail button')].find((node) => node.textContent === words), 'step of the trail reading ' + words),
    choose: (control, value) => {
      const node = chart().root.querySelector('select[data-control=\"' + control + '\"]');
      if (!node) throw new Error('the page has no control ' + control);
      node.value = value;
      node.dispatchEvent(new Event('change', { bubbles: true }));
    },
    tick: (control) => press(chart().root.querySelector('input[data-control=\"' + control + '\"]'), 'switch ' + control)
  };
  // One move, then the view once it has settled.
  const move = async (how, ...given) => {
    moves[how](...given);
    const settled = await settle();
    return JSON.stringify({ settled, ...view() });
  };
  const look = async () => {
    const settled = await settle();
    return JSON.stringify({ settled, ...view() });
  };
  return { move, look, chart };
})();
"

# How long, in seconds, the browser is given to answer: chromote's own ten is
# too few for the first start of Chrome on a machine that has just been set up,
# as a CI runner has.
nBrowserPatience <- 60

# A browser session, started patiently and more than once if need be: a slow
# start is the machine's, and is not what these tests are of. A browser that
# does not start in three tries is an error, which fails the test.
lStartBrowser <- function(nWidth, nHeight, nTries = 3L) {
  lWas <- options(chromote.timeout = nBrowserPatience)
  on.exit(options(lWas), add = TRUE)
  chrSaid <- character(0)
  for (iTry in seq_len(nTries)) {
    xSession <- tryCatch(chromote::ChromoteSession$new(width = nWidth, height = nHeight), error = function(cndError) cndError)
    if (!inherits(xSession, "error")) {
      return(xSession)
    }
    chrSaid <- c(chrSaid, conditionMessage(xSession))
  }
  stop("the headless browser did not start in ", nTries, " tries: ", paste(unique(chrSaid), collapse = "; "), call. = FALSE)
}

# Opens a saved page in a headless browser with the network switched off, and
# returns what drives it: `Look()`, the view as it stands; `Move(how, ...)`,
# one move and the view after it; `Evaluate(strCode)`, an expression's value,
# read back from JSON; `Picture(strFile)`, a screenshot of the whole page;
# `Requests()`, the address of every request the page has made; and `Close()`.
lOpenPage <- function(strFile, nWidth = 1200L, nHeight = 900L, strAddress = NULL) {
  # A saved file is opened with the network off. A page served by a session on
  # this machine (`strAddress`, #71) is opened with it on: its server is local.
  strUrl <- if (is.null(strAddress)) paste0("file://", normalizePath(strFile)) else strAddress
  lBrowser <- lStartBrowser(nWidth, nHeight)
  chrRequests <- character(0)
  chrErrors <- character(0)
  lBrowser$Network$enable()
  lBrowser$Runtime$enable()
  lBrowser$Network$requestWillBeSent(callback_ = function(lEvent) {
    chrRequests <<- c(chrRequests, lEvent$request$url)
  })
  lBrowser$Runtime$exceptionThrown(callback_ = function(lEvent) {
    chrErrors <<- c(chrErrors, paste(lEvent$exceptionDetails$text, lEvent$exceptionDetails$exception$description))
  })
  # No network: a request to anywhere but the file itself fails.
  if (is.null(strAddress)) {
    lBrowser$Network$emulateNetworkConditions(offline = TRUE, latency = 0, downloadThroughput = 0, uploadThroughput = 0)
  }
  lLoaded <- lBrowser$Page$loadEventFired(wait_ = FALSE, timeout_ = nBrowserPatience)
  lBrowser$Page$navigate(strUrl, wait_ = FALSE)
  lBrowser$wait_for(lLoaded)
  Evaluate <- function(strCode) {
    lAnswer <- lBrowser$Runtime$evaluate(strCode, awaitPromise = TRUE, returnByValue = TRUE, timeout_ = nBrowserPatience)
    if (!is.null(lAnswer$exceptionDetails)) {
      stop("the page raised: ", lAnswer$exceptionDetails$exception$description, call. = FALSE)
    }
    lAnswer$result$value
  }
  # A page a session serves draws its widget when the session sends it: wait
  # for the chart before the driver is defined on it.
  if (!is.null(strAddress)) {
    Evaluate("new Promise((done, fail) => { const start = Date.now(); const look = () => { const node = document.querySelector('.html-widget'); const widget = node && window.HTMLWidgets && HTMLWidgets.find('#' + node.id); if (widget && widget.chart && widget.chart()) return done(true); if (Date.now() - start > 30000) return fail(new Error('no widget was drawn in 30 seconds')); setTimeout(look, 50); }; look(); })")
  }
  Evaluate(strBrowserDriver)
  Json <- function(strCode) jsonlite::fromJSON(Evaluate(strCode), simplifyVector = FALSE)
  Quote <- function(xValue) as.character(jsonlite::toJSON(xValue, auto_unbox = TRUE))
  list(
    url = strUrl,
    Look = function() Json("window.gsmBioPage.look()"),
    Move = function(strHow, ...) {
      Json(sprintf("window.gsmBioPage.move(%s)", paste(vapply(list(strHow, ...), Quote, character(1)), collapse = ", ")))
    },
    Evaluate = function(strCode) Json(sprintf("Promise.resolve(%s).then((value) => JSON.stringify(value === undefined ? null : value))", strCode)),
    Picture = function(strPicture) {
      lBrowser$screenshot(strPicture, selector = "html", scale = 1, show = FALSE)
      invisible(strPicture)
    },
    # Gives a file input of the page a file of this machine, as a reader who
    # chose it would (#73).
    Upload = function(strSelector, strUpload) {
      nRoot <- lBrowser$DOM$getDocument()$root$nodeId
      nInput <- lBrowser$DOM$querySelector(nRoot, strSelector)$nodeId
      if (is.null(nInput) || nInput == 0) stop("the page has no file input ", strSelector, call. = FALSE)
      lBrowser$DOM$setFileInputFiles(files = list(normalizePath(strUpload)), nodeId = nInput)
      invisible(strUpload)
    },
    Requests = function() chrRequests,
    Errors = function() chrErrors,
    Close = function() invisible(tryCatch(lBrowser$close(), error = function(cndError) NULL))
  )
}

# The chart's own footnote, the last under it: when and by which bio.viz it was
# drawn, and what stands behind each statistic.
strDrawnBy <- function(lPage) {
  lPage$Evaluate("Array.from(document.querySelectorAll('.bv-foot-line')).map((line) => line.textContent).join(' ')")
}

# Waits for something to be true of a page, asking it again and again: a
# browser asked one question that takes many seconds to answer can drop the
# line it is asked on. Returns whether it became true in time.
bWaitFor <- function(lPage, strCondition, nSeconds = 40) {
  nStart <- Sys.time()
  repeat {
    if (isTRUE(lPage$Evaluate(sprintf("Boolean(%s)", strCondition)))) {
      return(TRUE)
    }
    if (as.numeric(difftime(Sys.time(), nStart, units = "secs")) > nSeconds) {
      return(FALSE)
    }
    Sys.sleep(0.5)
  }
}

# The view of a page whose statistics a session answers, once every statistic
# it asked for has its answer. A page with stored results is answered in a
# turn of its own loop, so its view has settled when it stops changing; an
# answer from a session takes as long as the session does, and the view can
# stand still for a moment while it is on the way.
lLookAnswered <- function(lPage, nSeconds = 40) {
  bAnswered <- bWaitFor(
    lPage,
    "window.gsmBioPage.chart().statistics().length > 0 && window.gsmBioPage.chart().statistics().every((asked) => asked.answer)",
    nSeconds
  )
  c(list(answered = bAnswered), lPage$Look())
}

# A widget saved as one self-contained file, for a browser to open.
strSavedFile <- function(lWidget, strName = "group-comparison") {
  strDir <- tempfile("gsm-bio-page")
  dir.create(strDir)
  strFile <- file.path(strDir, paste0(strName, ".html"))
  htmlwidgets::saveWidget(lWidget, file = strFile, selfcontained = TRUE)
  strFile
}

# Walks a page as a reader does, from the trend tiles: each biomarker's tile,
# which opens it over time; there each adjustment named, by the Adjust across
# visits control; then each visit under the picture, which opens it alone, and
# the trail's way back; and the trail's way back to the tiles. Returns `asked`,
# everything the chart asked R on the way with what it was answered (an entry
# of `chart.statistics()` each, with the level it was asked at), and `views`,
# what the page showed at the first biomarker's three levels.
lWalkPage <- function(lPage, chrBiomarkers, chrAdjustments = character(0)) {
  lAsked <- list()
  Note <- function(lView) {
    for (lOne in lView$statistics) {
      lAsked[[length(lAsked) + 1L]] <<- c(lOne, list(level = lView$level))
    }
    lView
  }
  lTiles <- lPage$Look()
  lViews <- list(tiles = lTiles)
  for (strBiomarker in chrBiomarkers) {
    lTime <- Note(lPage$Move("tile", strBiomarker))
    for (strAdjustment in chrAdjustments) {
      lTime <- Note(lPage$Move("choose", "visit-adjustment", strAdjustment))
    }
    if (is.null(lViews$over_time)) lViews$over_time <- lTime
    for (strVisit in unlist(lTime$visitButtons)) {
      lVisit <- Note(lPage$Move("visit", strVisit))
      if (is.null(lViews$visit)) lViews$visit <- lVisit
      lBack <- Note(lPage$Move("trail", paste(strBiomarker, "over time")))
      if (!identical(lBack$level, "over-time")) stop("the trail did not lead back to ", strBiomarker, " over time")
    }
    lHome <- lPage$Move("trail", "All biomarkers")
    if (!identical(lHome$level, "biomarkers")) stop("the trail did not lead back to the tiles")
  }
  list(asked = lAsked, views = lViews)
}

# A Shiny app run by a second R session on this machine, for a browser to open
# (#71). `strApp` is the R code of the app: an expression whose value
# shiny::runApp() takes, written with the package loaded from the source tree.
# Returns the page's address and `Stop()`, which ends the session. Base R only:
# the second session writes the port it chose and its process id to files.
lRunApp <- function(strApp, nPatience = 90) {
  strDir <- tempfile("gsm-bio-app")
  dir.create(strDir)
  strPort <- file.path(strDir, "port")
  strPid <- file.path(strDir, "pid")
  strScript <- file.path(strDir, "app.R")
  writeLines(c(
    # The second session finds its packages where this one does.
    sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
    sprintf("pkgload::load_all(%s, quiet = TRUE, export_all = FALSE, helpers = FALSE)", deparse(strSourceRoot())),
    sprintf("writeLines(as.character(Sys.getpid()), %s)", deparse(strPid)),
    "nPort <- httpuv::randomPort()",
    sprintf("xApp <- local({\n%s\n})", strApp),
    sprintf("writeLines(as.character(nPort), %s)", deparse(strPort)),
    "shiny::runApp(xApp, port = nPort, host = '127.0.0.1', launch.browser = FALSE, quiet = TRUE)"
  ), strScript)
  strLog <- file.path(strDir, "log")
  system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(strScript)), wait = FALSE, stdout = strLog, stderr = strLog)
  Stop <- function() {
    if (file.exists(strPid)) {
      nPid <- suppressWarnings(as.integer(readLines(strPid, warn = FALSE)[1]))
      if (!is.na(nPid)) tools::pskill(nPid)
    }
    invisible(NULL)
  }
  Said <- function() if (file.exists(strLog)) paste(readLines(strLog, warn = FALSE), collapse = "\n") else ""
  nStart <- Sys.time()
  repeat {
    if (file.exists(strPort)) {
      strAddress <- sprintf("http://127.0.0.1:%s/", readLines(strPort, warn = FALSE)[1])
      bUp <- tryCatch(
        {
          xPage <- url(strAddress)
          on.exit(try(close(xPage), silent = TRUE), add = TRUE)
          length(suppressWarnings(readLines(xPage, n = 1L, warn = FALSE))) > 0L
        },
        error = function(cndError) FALSE
      )
      if (bUp) {
        return(list(address = strAddress, Stop = Stop, Said = Said))
      }
    }
    if (as.numeric(difftime(Sys.time(), nStart, units = "secs")) > nPatience) {
      Stop()
      stop("the app did not start in ", nPatience, " seconds: ", Said(), call. = FALSE)
    }
    Sys.sleep(0.25)
  }
}

# The tests that open an app run where the saved-page tests do, and need shiny
# as well: in the source tree a missing shiny is a failure, never a skip.
NeedApp <- function() {
  if (!bSourceTree()) {
    testthat::skip("an app is run and opened in a browser from the source tree, not under R CMD check")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    testthat::fail("shiny is not installed: the tests of the widgets in a Shiny page need it")
    testthat::skip("shiny is not available to run an app")
  }
  if (!bBrowser()) {
    testthat::fail("no headless browser was found: the chromote package and a Chrome or Chromium are needed to open the app")
    testthat::skip("no headless browser is available to open an app")
  }
  invisible(TRUE)
}
