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
      await new Promise((done) => setTimeout(done, 40));
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
    provenance: texts.call(null, 'p').length ? [...document.querySelectorAll('.gsm-bio-provenance')].map((node) => node.textContent) : [],
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

# Opens a saved page in a headless browser with the network switched off, and
# returns what drives it: `Look()`, the view as it stands; `Move(how, ...)`,
# one move and the view after it; `Evaluate(strCode)`, an expression's value,
# read back from JSON; `Picture(strFile)`, a screenshot of the whole page;
# `Requests()`, the address of every request the page has made; and `Close()`.
lOpenPage <- function(strFile, nWidth = 1200L, nHeight = 900L) {
  strUrl <- paste0("file://", normalizePath(strFile))
  lBrowser <- chromote::ChromoteSession$new(width = nWidth, height = nHeight)
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
  lBrowser$Network$emulateNetworkConditions(offline = TRUE, latency = 0, downloadThroughput = 0, uploadThroughput = 0)
  lLoaded <- lBrowser$Page$loadEventFired(wait_ = FALSE)
  lBrowser$Page$navigate(strUrl, wait_ = FALSE)
  lBrowser$wait_for(lLoaded)
  Evaluate <- function(strCode) {
    lAnswer <- lBrowser$Runtime$evaluate(strCode, awaitPromise = TRUE, returnByValue = TRUE)
    if (!is.null(lAnswer$exceptionDetails)) {
      stop("the page raised: ", lAnswer$exceptionDetails$exception$description, call. = FALSE)
    }
    lAnswer$result$value
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
    Requests = function() chrRequests,
    Errors = function() chrErrors,
    Close = function() invisible(tryCatch(lBrowser$close(), error = function(cndError) NULL))
  )
}

# A widget saved as one self-contained file, for a browser to open.
strSavedFile <- function(lWidget, strName = "group-comparison") {
  strDir <- tempfile("gsm-bio-page")
  dir.create(strDir)
  strFile <- file.path(strDir, paste0(strName, ".html"))
  htmlwidgets::saveWidget(lWidget, file = strFile, selfcontained = TRUE)
  strFile
}
