# The app's shell: the page around the charts (#84).
#
# A header band with the app's name and the mark gsm.bio with its version, a
# row of pills for Data and the six charts, a chip saying what the charts are
# drawn on, and one footer line. The first tests read the page and a session
# with no browser. The last run the app in a second R session and drive it in a
# headless browser, by pointer and by keyboard, at a desk's width and a
# phone's, as tests/testthat/test-app.R does for the charts.

# What a pill's hover text says of each page, by the value of its tab.
chrShellWhat <- c(
  Data = "The tables the charts are drawn on, and where a study of your own is loaded.",
  GroupComparison = "One biomarker between groups, over the visits.",
  AssociationScatter = "Two variables, one point per participant.",
  CorrelationMatrix = "Every pair of biomarkers at one visit.",
  BiomarkerScreen = "Every biomarker on one comparison.",
  CrossTab = "Two categories, with a cut on a biomarker.",
  StratifiedSurvival = "High against low, with a Kaplan-Meier curve."
)

strShellLacks <- "The stratified survival chart reads an outcomes table, with a time and a censor flag for each participant, and this app has none."

# A page's words, without its markup.
strShellText <- function(strHtml) {
  trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", strHtml)))
}

# One attribute of each tag, or NA where a tag has none.
chrShellAttr <- function(chrTags, strName) {
  lFound <- regmatches(chrTags, regexec(sprintf(" %s=\"([^\"]*)\"", strName), chrTags))
  vapply(lFound, function(chrOne) if (length(chrOne) == 2L) chrOne[2] else NA_character_, character(1))
}

# One part of the page, from the tag that opens it to the tag that closes it.
strShellPart <- function(strPage, strOpens, strCloses) {
  strFlat <- gsub("\n\\s*", "", strPage)
  chrPart <- regmatches(strFlat, regexpr(paste0(strOpens, ".*?", strCloses), strFlat, perl = TRUE))
  if (length(chrPart) == 0L) "" else chrPart
}

# The pills as the page writes them: each link's attributes, in order, with
# what the pill reads and whether it is the one chosen.
lShellPills <- function(strPage) {
  strList <- strShellPart(strPage, "<ul class=\"nav nav-pills shiny-tab-input\" id=\"gsm_bio_chart\"", "</ul>")
  chrItems <- regmatches(strList, gregexpr("<li[^>]*>\\s*<a [^>]*>.*?</a>\\s*</li>", strList, perl = TRUE))[[1]]
  chrLinks <- regmatches(chrItems, regexpr("<a [^>]*>", chrItems))
  list(
    value = chrShellAttr(chrLinks, "data-value"), title = chrShellAttr(chrLinks, "title"),
    href = chrShellAttr(chrLinks, "href"), toggle = chrShellAttr(chrLinks, "data-toggle"),
    chosen = grepl("^<li class=\"active\"", chrItems),
    dim = grepl("gsm-bio-app-pill-dim", chrItems, fixed = TRUE),
    reads = vapply(chrItems, strShellText, character(1), USE.NAMES = FALSE),
    items = chrItems
  )
}

# A reader's results file with gsm.bio's own column names, and a session's
# inputs once it is chosen and every column said.
strShellFile <- function() {
  strDir <- tempfile("gsm-bio-shell")
  dir.create(strDir)
  strFile <- file.path(strDir, "labs.csv")
  utils::write.csv(Synthetic_Results[Synthetic_Results$TEST %in% c("CRP", "IL-6"), ], strFile, row.names = FALSE, na = "")
  strFile
}

test_that("the page opens on a header band with the app's name and the mark gsm.bio with its version, and closes on one footer line with the two standing facts (#84)", {
  skip_if_not_installed("shiny")
  strPage <- as.character(App_Ui())
  strVersion <- as.character(utils::packageVersion("gsm.bio"))
  strHead <- strShellPart(strPage, "<header class=\"gsm-bio-app-head\"", "</header>")
  expect_true(nzchar(strHead))
  # The name is the page's one heading, and the mark is beside it.
  expect_identical(lengths(regmatches(strPage, gregexpr("<h1", strPage, fixed = TRUE))), 1L)
  expect_match(strHead, "<h1[^>]*>Biomarker charts</h1>")
  expect_identical(
    strShellText(strShellPart(strHead, "<span class=\"gsm-bio-app-mark\"", "</span>\\s*</span>")),
    paste("gsm.bio", strVersion)
  )
  # The pills and the chip are in the band; the pages they open are under it.
  expect_match(strHead, "id=\"gsm_bio_chart\"", fixed = TRUE)
  expect_match(strHead, "id=\"gsm_bio_study\"", fixed = TRUE)
  expect_false(grepl("class=\"tab-content\"", strHead, fixed = TRUE))
  expect_lt(regexpr("</header>", strPage, fixed = TRUE)[1], regexpr("class=\"tab-content\"", strPage, fixed = TRUE)[1])

  # One footer, after every page: which R computes the statistics, and that a
  # reader's files are held for this session only.
  expect_identical(lengths(regmatches(strPage, gregexpr("<footer", strPage, fixed = TRUE))), 1L)
  expect_lt(regexpr("class=\"tab-content\"", strPage, fixed = TRUE)[1], regexpr("<footer", strPage, fixed = TRUE)[1])
  expect_identical(
    strShellText(strShellPart(strPage, "<footer class=\"gsm-bio-app-foot\"", "</footer>")),
    sprintf(
      "Every statistic is computed on request by R %s with gsm.bio %s on this server. Files you load are held in this session's memory and nowhere else.",
      paste(R.version$major, R.version$minor, sep = "."), strVersion
    )
  )
})

test_that("the page asks for the family's two web fonts with one link, names a system font behind each, and sizes nothing of the shell from the page's root (#84)", {
  skip_if_not_installed("shiny")
  # What the page's head asks for beyond Shiny's own files: one link.
  strAsks <- as.character(App_Fonts())
  chrLinks <- regmatches(strAsks, gregexpr("<[a-z]+[^>]*>", strAsks))[[1]]
  expect_length(chrLinks, 1L)
  expect_match(chrLinks, "^<link ")
  expect_identical(chrShellAttr(chrLinks, "rel"), "stylesheet")
  strAsked <- chrShellAttr(chrLinks, "href")
  expect_match(strAsked, "^https://fonts\\.googleapis\\.com/css2\\?")
  expect_match(strAsked, "family=Instrument+Sans", fixed = TRUE)
  expect_match(strAsked, "family=Instrument+Serif", fixed = TRUE)
  expect_match(strAsked, "display=swap", fixed = TRUE)
  # The page is drawn before the fonts answer, and whether or not they do.
  expect_identical(chrShellAttr(chrLinks, "media"), "print")
  expect_identical(chrShellAttr(chrLinks, "onload"), "this.media=&#39;all&#39;")
  expect_false(grepl("@import", strAppStyle, fixed = TRUE))
  expect_match(strAppStyle, "'Instrument Sans', system-ui", fixed = TRUE)
  expect_match(strAppStyle, "'Instrument Serif', Georgia", fixed = TRUE)
  # Shiny's page sets the root font to 10 pixels and a chart's output gives it
  # back to the browser (#80): the shell reads neither, and changes neither.
  expect_false(grepl("[0-9.]rem\\b", strAppStyle))
  expect_false(grepl("(^|[\\s,}])(html|:root|body)\\s*[{,]", strAppStyle, perl = TRUE))
  expect_identical(strShinyRoot, "html { font-size: 100%; }")
})

test_that("the seven pills are Shiny's own tab links in the list's order, Data first, each with its value and a line of hover text saying what it draws, and the page opens on the first chart (#84)", {
  skip_if_not_installed("shiny")
  strPage <- as.character(App_Ui())
  lPills <- lShellPills(strPage)
  expect_identical(lPills$value, c("Data", names(chrAppCharts)))
  expect_identical(lPills$title, unname(chrShellWhat))
  expect_identical(chrAppWhat, chrShellWhat)
  expect_identical(lPills$reads, c("Data 3 tables", unname(chrAppCharts)))
  # Each is the link Shiny's tab set made: it opens its own page of the set.
  expect_identical(lPills$toggle, rep("tab", 7L))
  expect_match(lPills$href, "^#tab-[0-9]+-[1-7]$")
  expect_identical(anyDuplicated(lPills$href), 0L)
  for (iPill in seq_along(lPills$value)) {
    expect_match(strPage, sprintf("<div class=\"tab-pane[^\"]*\" data-value=\"%s\" id=\"%s\"", lPills$value[iPill], sub("#", "", lPills$href[iPill], fixed = TRUE)))
  }
  # The first chart is chosen.
  expect_identical(lPills$value[lPills$chosen], "GroupComparison")
  expect_false(any(lPills$dim))
  # The row is a named part of the page.
  expect_match(strPage, "<nav class=\"gsm-bio-app-pills\" aria-label=\"Data and the six charts\">", fixed = TRUE)
})

test_that("the study chip says what the charts are drawn on as the app opens, on the tables it was started with, and on a reader's files once they are drawn; it is a link that keeps the source line (#84)", {
  skip_if_not_installed("shiny")
  Chip <- function(strPage) strShellPart(strPage, "<a [^>]*id=\"gsm_bio_study\"", "</a>")
  strChip <- Chip(as.character(App_Ui()))
  expect_match(strChip, "^<a [^>]*href=\"#gsm_bio_chart\"")
  expect_identical(
    strShellText(strChip),
    "Synthetic study 200 participants &middot; 3 tables Drawn on the synthetic study that ships with gsm.bio."
  )
  # The source line keeps its name and its sentence, in the chip.
  expect_match(strChip, "<span id=\"gsm_bio_source\" class=\"shiny-text-output[^\"]*\">Drawn on the synthetic study that ships with gsm.bio.</span>")
  expect_match(strChip, "gsm-bio-app-study-kept", fixed = TRUE)

  # Started with a results table alone: the app's own tables, and one of them.
  dfResults <- Synthetic_Results[Synthetic_Results$USUBJID %in% unique(Synthetic_Results$USUBJID)[1:30], ]
  lAlone <- App_Study(dfResults, NULL, NULL)
  expect_identical(
    strShellText(Chip(as.character(App_Ui(lAlone)))),
    "This app's tables 30 participants &middot; 1 table Drawn on the tables this app was started with."
  )
  expect_identical(
    strShellText(as.character(App_Chip(App_Study(dfResults, Synthetic_Participants, NULL)))),
    "This app's tables 30 participants &middot; 2 tables"
  )

  # In a session: the chip as the app opens, and after a reader's file is drawn.
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  strFile <- strShellFile()
  shiny::testServer(RunApp(), {
    expect_identical(strShellText(as.character(output$gsm_bio_study_said$html)), "Synthetic study 200 participants &middot; 3 tables")
    expect_match(as.character(output$gsm_bio_study_said$html), "gsm-bio-app-study-kept", fixed = TRUE)
    expect_identical(output$gsm_bio_source, "Drawn on the synthetic study that ships with gsm.bio.")
    session$setInputs(gsm_bio_file_results = dfUploaded(strFile))
    # A file chosen and not yet drawn changes nothing of the chip.
    expect_identical(strShellText(as.character(output$gsm_bio_study_said$html)), "Synthetic study 200 participants &middot; 3 tables")
    session$setInputs(
      gsm_bio_column_results_USUBJID = "USUBJID", gsm_bio_column_results_TEST = "TEST", gsm_bio_column_results_STRESN = "STRESN",
      gsm_bio_column_results_VISIT = "VISIT", gsm_bio_column_results_VISITNUM = "VISITNUM"
    )
    session$setInputs(gsm_bio_apply = 1)
    expect_identical(strShellText(as.character(output$gsm_bio_study_said$html)), "labs.csv this session")
    expect_false(grepl("gsm-bio-app-study-kept", as.character(output$gsm_bio_study_said$html), fixed = TRUE))
    expect_identical(output$gsm_bio_source, "Drawn on labs.csv, loaded in this session.")
  })
})

test_that("with no outcomes table the stratified survival pill is dimmed and carries the reason, and is still the link to its page; the Data pill counts the tables there are (#84)", {
  skip_if_not_installed("shiny")
  dfResults <- Synthetic_Results[Synthetic_Results$TEST %in% c("CRP", "IL-6"), ]
  # The page as it is written for an app started with no outcomes table.
  lPills <- lShellPills(as.character(App_Ui(App_Study(dfResults, Synthetic_Participants, NULL))))
  expect_identical(lPills$value[lPills$dim], "StratifiedSurvival")
  strDim <- lPills$items[lPills$dim]
  # The reason is the pill's hover text and its words for a screen reader.
  expect_match(strDim, sprintf("<span class=\"gsm-bio-app-pill gsm-bio-app-pill-dim\" title=\"%s\">", strShellLacks), fixed = TRUE)
  expect_identical(strShellText(strDim), paste("Stratified survival", strShellLacks))
  # It is still a link to its page, with its own line of what it draws.
  expect_identical(lPills$toggle[lPills$dim], "tab")
  expect_match(lPills$href[lPills$dim], "^#tab-[0-9]+-7$")
  expect_identical(lPills$title[lPills$dim], chrShellWhat[["StratifiedSurvival"]])
  expect_identical(lPills$reads[1], "Data 2 tables")
  # With an outcomes table it is as the others are.
  expect_false(any(lShellPills(as.character(App_Ui(App_Study(dfResults, NULL, Synthetic_Outcomes))))$dim))
  # Only a chart that lacks a table has a reason.
  for (strChart in names(chrAppCharts)) {
    expect_null(App_Lacks(strChart, App_Study(NULL, NULL, NULL)), label = strChart)
  }
  expect_identical(App_Lacks("StratifiedSurvival", App_Study(dfResults, NULL, NULL)), strShellLacks)

  # In a session: every pill is written by the session, which knows the tables
  # there are, and the survival pill dims when a reader's files have no outcomes.
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  strFile <- strShellFile()
  shiny::testServer(RunApp(), {
    Pill <- function(strPill) as.character(output[[paste0("gsm_bio_pill_", strPill)]]$html)
    expect_identical(strShellText(Pill("Data")), "Data 3 tables")
    for (strChart in names(chrAppCharts)) {
      expect_identical(strShellText(Pill(strChart)), chrAppCharts[[strChart]])
      expect_false(grepl("gsm-bio-app-pill-dim", Pill(strChart), fixed = TRUE), label = strChart)
    }
    session$setInputs(gsm_bio_file_results = dfUploaded(strFile))
    session$setInputs(
      gsm_bio_column_results_USUBJID = "USUBJID", gsm_bio_column_results_TEST = "TEST", gsm_bio_column_results_STRESN = "STRESN",
      gsm_bio_column_results_VISIT = "VISIT", gsm_bio_column_results_VISITNUM = "VISITNUM"
    )
    session$setInputs(gsm_bio_apply = 1)
    expect_identical(strShellText(Pill("Data")), "Data 1 table")
    expect_match(Pill("StratifiedSurvival"), sprintf("<span class=\"gsm-bio-app-pill gsm-bio-app-pill-dim\" title=\"%s\">", strShellLacks), fixed = TRUE)
    expect_identical(strShellText(Pill("StratifiedSurvival")), paste("Stratified survival", strShellLacks))
    for (strChart in setdiff(names(chrAppCharts), "StratifiedSurvival")) {
      expect_false(grepl("gsm-bio-app-pill-dim", Pill(strChart), fixed = TRUE), label = strChart)
    }
    # The chart's place still holds the sentence saying what is missing.
    expect_match(as.character(output$gsm_bio_place_StratifiedSurvival$html), strShellLacks, fixed = TRUE)
  })
})

test_that("the shell adds no Shiny input: the page's inputs are the tab set of the pills, the Data view's files and buttons, and nothing else (#84)", {
  skip_if_not_installed("shiny")
  strPage <- as.character(App_Ui())
  # Everything of the page a reader can type in, choose from or press.
  chrInputs <- regmatches(strPage, gregexpr("<(input|select|button|textarea)[^>]*>", strPage))[[1]]
  expect_identical(
    chrShellAttr(chrInputs[grepl(" id=", chrInputs, fixed = TRUE)], "id"),
    c("gsm_bio_view_previous", "gsm_bio_view_next", "gsm_bio_file_results", "gsm_bio_file_participants", "gsm_bio_file_outcomes", "gsm_bio_apply")
  )
  expect_true(all(grepl("readonly", chrInputs[!grepl(" id=", chrInputs, fixed = TRUE)], fixed = TRUE)))
  # One tab set, the pills', and no link that is a button of Shiny's.
  chrSets <- regmatches(strPage, gregexpr("<ul class=\"[^\"]*shiny-tab-input[^\"]*\"[^>]*>", strPage))[[1]]
  expect_identical(chrShellAttr(chrSets, "id"), "gsm_bio_chart")
  expect_false(grepl("action-link", strPage, fixed = TRUE))
  # The header's own parts are outputs or plain links: the chip and the pills.
  strHead <- strShellPart(strPage, "<header class=\"gsm-bio-app-head\"", "</header>")
  expect_false(grepl("<(input|select|button|textarea)|action-button|shiny-bound-input", strHead))
})

# What the shell is asked of in a browser. `chosen` is the page shown and the
# pill said to be selected for it; `wide` is how far anything reaches sideways.
strShellLook <- "(() => {
  const head = document.querySelector('.gsm-bio-app-head');
  const row = document.querySelector('.gsm-bio-app-pills');
  const box = (node) => { const at = node.getBoundingClientRect(); return { left: at.left, right: at.right, top: at.top, bottom: at.bottom, width: at.width }; };
  const active = document.querySelector('#gsm_bio_chart li.active > a');
  const page = document.querySelector('.gsm-bio-app-main > .tab-content > .tab-pane.active');
  return {
    chosen: { pill: active && active.dataset.value, page: page && page.dataset.value, input: Shiny.shinyapp.$inputValues.gsm_bio_chart,
              selected: Array.from(document.querySelectorAll('#gsm_bio_chart a[role=\"tab\"][aria-selected=\"true\"]')).map((link) => link.dataset.value),
              stop: Array.from(document.querySelectorAll('#gsm_bio_chart a')).filter((link) => link.tabIndex === 0).map((link) => link.dataset.value) },
    focus: document.activeElement && (document.activeElement.dataset.value || document.activeElement.id || document.activeElement.tagName),
    window: window.innerWidth, room: document.documentElement.clientWidth, wide: Math.max(document.documentElement.scrollWidth, document.body.scrollWidth),
    name: box(head.querySelector('.gsm-bio-app-name')), study: box(document.querySelector('#gsm_bio_study')), row: box(row), pill: active && box(active),
    rowScrolls: row.scrollWidth - row.clientWidth, rowAt: row.scrollLeft, rowOverflow: getComputedStyle(row).overflowX,
    card: page && page.querySelector('.gsm-bio-app-data, .gsm-bio-app-card') && box(page.querySelector('.gsm-bio-app-data, .gsm-bio-app-card')),
    main: box(document.querySelector('.gsm-bio-app-main'))
  };
})()"

# The pages of the app, and what says a page has been drawn.
chrShellPages <- c("Data", names(chrAppCharts))
strShellDrawn <- function(strPage) {
  if (identical(strPage, "Data")) {
    return("document.querySelector('#gsm_bio_view table')")
  }
  sprintf("(HTMLWidgets.find('#%s') && HTMLWidgets.find('#%s').chart() && document.querySelector('#%s .gsm-bio-chart').childElementCount > 0)", strPage, strPage, strPage)
}

# The fonts' two hosts: a test's page does not reach them, as a server behind
# a firewall does not, so what is measured is the page on its system fonts.
chrShellFonts <- c("*fonts.googleapis.com*", "*fonts.gstatic.com*")

test_that("in a browser each pill opens its page by a click and by the keyboard, the chosen pill is said to be selected, the chip opens Data, and the chart has the page's width; the page holds when the fonts cannot be reached (#84)", {
  NeedApp()
  lApp <- lRunApp("RunApp()")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, nWidth = 1280L, nHeight = 900L, strAddress = lApp$address, chrBlocked = chrShellFonts)
  on.exit(lPage$Close(), add = TRUE)
  strVersion <- as.character(utils::packageVersion("gsm.bio"))

  # As it opens: the name and the mark, the chip's words, the seven pills.
  lSeen <- lPage$Evaluate("({
    name: document.querySelector('.gsm-bio-app-head h1').textContent,
    mark: document.querySelector('.gsm-bio-app-mark').textContent.replace(/\\s+/g, ' ').trim(),
    pills: Array.from(document.querySelectorAll('#gsm_bio_chart a')).map((link) => link.dataset.value),
    titles: Array.from(document.querySelectorAll('#gsm_bio_chart a')).map((link) => link.title),
    links: Array.from(document.querySelectorAll('#gsm_bio_chart a, #gsm_bio_study')).every((link) => link.matches('a[href]')),
    chip: document.querySelector('#gsm_bio_study').tabIndex,
    asked: Array.from(document.querySelectorAll('head link[rel=\"stylesheet\"]')).map((link) => link.href).filter((href) => !href.startsWith(location.origin)),
    foot: document.querySelector('.gsm-bio-app-foot').textContent.replace(/\\s+/g, ' ').trim(),
    serif: getComputedStyle(document.querySelector('.gsm-bio-app-head h1')).fontFamily,
    body: getComputedStyle(document.body).fontSize,
    root: getComputedStyle(document.documentElement).fontSize
  })")
  expect_identical(lSeen$name, "Biomarker charts")
  expect_identical(lSeen$mark, paste("gsm.bio", strVersion))
  expect_identical(unlist(lSeen$pills), chrShellPages)
  expect_identical(unlist(lSeen$titles), unname(chrShellWhat))
  expect_true(lSeen$links)
  expect_identical(lSeen$chip, 0L)
  # The one thing the page asks of anywhere but its own server is the fonts.
  expect_length(lSeen$asked, 1L)
  strHost <- sub("/$", "", sub("^http://", "", lApp$address))
  chrElsewhere <- grep("^https?://", lPage$Requests()[!grepl(strHost, lPage$Requests(), fixed = TRUE)], value = TRUE)
  expect_identical(unique(sub("^(https?://[^/]+)/.*$", "\\1", chrElsewhere)), "https://fonts.googleapis.com")
  expect_match(lSeen$foot, sprintf("computed on request by R %s with gsm.bio %s on this server. Files you load are held in this session's memory and nowhere else.", paste(R.version$major, R.version$minor, sep = "."), strVersion), fixed = TRUE)
  # The fonts were asked for, did not come, and the page is drawn all the same.
  expect_true(any(grepl("^https://fonts\\.googleapis\\.com/css2", lPage$Requests())))
  expect_match(lSeen$serif, "Instrument Serif.*Georgia")
  expect_identical(lSeen$root, "16px")
  expect_identical(lSeen$body, "14px")
  Said <- function(strId) lPage$Evaluate(sprintf("document.querySelector('#%s').textContent.replace(/\\s+/g, ' ').trim()", strId))
  expect_true(bWaitFor(lPage, "document.querySelector('#gsm_bio_study_said').textContent.includes('Synthetic study')"))
  expect_identical(Said("gsm_bio_study_said"), "Synthetic study 200 participants \u00b7 3 tables")
  expect_identical(Said("gsm_bio_source"), "Drawn on the synthetic study that ships with gsm.bio.")
  lOpen <- lPage$Evaluate(strShellLook)
  # The chosen pill is the one said to be selected, to a reader who cannot see
  # it, and the row's one stop of the Tab key: Shiny's tab set does both.
  expect_identical(lOpen$chosen, list(
    pill = "GroupComparison", page = "GroupComparison", input = "GroupComparison",
    selected = list("GroupComparison"), stop = list("GroupComparison")
  ))

  Chosen <- function(strPill) {
    sprintf("(() => { const seen = %s; return seen.chosen.pill === '%s' && seen.chosen.page === '%s' && seen.chosen.input === '%s'; })()", strShellLook, strPill, strPill, strPill)
  }
  # By pointer: each pill, from the last to the first, opens its page.
  for (strPill in rev(chrShellPages)) {
    lPage$Evaluate(sprintf("document.querySelector('#gsm_bio_chart a[data-value=\"%s\"]').click()", strPill))
    expect_true(bWaitFor(lPage, Chosen(strPill)), label = paste(strPill, "is chosen by a click"))
    expect_true(bWaitFor(lPage, strShellDrawn(strPill)), label = paste(strPill, "is drawn after a click"))
    lNow <- lPage$Evaluate(strShellLook)
    expect_identical(lNow$chosen$selected, list(strPill), label = paste(strPill, "is the one pill said to be selected"))
    expect_identical(lNow$chosen$stop, list(strPill), label = paste(strPill, "is the row's stop of the Tab key"))
    expect_lte(lNow$wide, lNow$window, label = paste(strPill, "reaches no wider than the window"))
    # A chart has the page's width: its card runs from one margin to the
    # other. So does the Data page, which is a rail and its cards (#85). The
    # room is the window's less a scroll bar, which takes none of it on a Mac
    # and fifteen pixels on the Linux of CI.
    expect_gte(lNow$card$width, lNow$room - 42, label = paste(strPill, "has the page's width"))
  }
  # By keyboard: from the chosen pill, where the Tab key stops, the arrow keys
  # walk the row and open each page, round to the first and back to the last.
  expect_identical(lPage$Evaluate(strShellLook)$chosen$pill, "Data")
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"Data\"]').focus()")
  for (strPill in c(chrShellPages[-1], "Data")) {
    lPage$Press("ArrowRight")
    expect_true(bWaitFor(lPage, Chosen(strPill)), label = paste(strPill, "is chosen by the right arrow"))
    lNow <- lPage$Evaluate(strShellLook)
    expect_identical(lNow$focus, strPill, label = paste(strPill, "has the focus"))
    expect_identical(lNow$chosen$selected, list(strPill), label = paste(strPill, "is said to be selected after the arrow"))
  }
  lPage$Press("ArrowLeft")
  expect_true(bWaitFor(lPage, Chosen("StratifiedSurvival")), label = "the left arrow goes back")
  # Enter on a pill that has the focus opens its page.
  for (strPill in c("AssociationScatter", "Data")) {
    lPage$Evaluate(sprintf("document.querySelector('#gsm_bio_chart a[data-value=\"%s\"]').focus()", strPill))
    expect_identical(lPage$Evaluate(strShellLook)$focus, strPill)
    lPage$Press("Enter")
    expect_true(bWaitFor(lPage, Chosen(strPill)), label = paste(strPill, "is chosen by Enter"))
  }
  # The chip opens Data, pressed with the pointer and with the keyboard.
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"CrossTab\"]').click()")
  expect_true(bWaitFor(lPage, Chosen("CrossTab")))
  lPage$Evaluate("document.querySelector('#gsm_bio_study').click()")
  expect_true(bWaitFor(lPage, Chosen("Data")), label = "the chip opens Data by a click")
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"BiomarkerScreen\"]').click()")
  expect_true(bWaitFor(lPage, Chosen("BiomarkerScreen")))
  lPage$Evaluate("document.querySelector('#gsm_bio_study').focus()")
  expect_identical(lPage$Evaluate(strShellLook)$focus, "gsm_bio_study")
  lPage$Press("Enter")
  expect_true(bWaitFor(lPage, Chosen("Data")), label = "the chip opens Data by Enter")
  expect_identical(lPage$Evaluate("location.hash"), "")

  # Every page has been opened: the inputs the page has sent the session are
  # the tab set, the Data view's, and the one the charts ask R through (#71).
  expect_setequal(
    sub(":.*$", "", unlist(lPage$Evaluate("Object.keys(Shiny.shinyapp.$inputValues).filter((name) => !name.startsWith('.clientdata'))"))),
    c(
      "gsm_bio_chart", "gsm_bio_view_table", "gsm_bio_view_previous", "gsm_bio_view_next",
      "gsm_bio_file_results", "gsm_bio_file_participants", "gsm_bio_file_outcomes", "gsm_bio_apply", strServeInput
    )
  )
  expect_identical(lPage$Errors(), character(0))
})

test_that("on a 390-pixel phone the header is two rows, the pills scroll sideways in their own row with the chosen one in view, and no page of the app scrolls sideways (#84)", {
  NeedApp()
  lApp <- lRunApp("RunApp()")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, nWidth = 390L, nHeight = 844L, strAddress = lApp$address, bPhone = TRUE, chrBlocked = chrShellFonts)
  on.exit(lPage$Close(), add = TRUE)
  lOpen <- lPage$Evaluate(strShellLook)
  expect_identical(lOpen$window, 390L)
  # Two rows: the name with the chip beside it, and the pills under both.
  expect_lt(lOpen$study$top, lOpen$name$bottom)
  expect_gte(lOpen$study$left, lOpen$name$right)
  expect_gte(lOpen$row$top, max(lOpen$name$bottom, lOpen$study$bottom) - 1)
  expect_gte(lOpen$name$left, 0)
  expect_lte(lOpen$study$right, 390)
  # The row of pills is as wide as the page and scrolls inside itself.
  expect_lte(lOpen$row$right, 390)
  expect_gt(lOpen$rowScrolls, 0)
  expect_identical(lOpen$rowOverflow, "auto")
  expect_true(bWaitFor(lPage, "document.querySelector('#gsm_bio_study_said').textContent.includes('Synthetic study')"))
  expect_true(isTRUE(lPage$Evaluate("document.querySelector('#gsm_bio_study_said').getBoundingClientRect().width > 40")), label = "the chip's words are shown")
  for (strPill in c(rev(chrShellPages), "GroupComparison")) {
    lPage$Evaluate(sprintf("document.querySelector('#gsm_bio_chart a[data-value=\"%s\"]').click()", strPill))
    expect_true(bWaitFor(lPage, sprintf("document.querySelector('.gsm-bio-app-main > .tab-content > .tab-pane.active').dataset.value === '%s'", strPill)), label = paste(strPill, "is opened"))
    expect_true(bWaitFor(lPage, strShellDrawn(strPill)), label = paste(strPill, "is drawn"))
    lNow <- lPage$Evaluate(strShellLook)
    expect_lte(lNow$wide, lNow$window, label = paste(strPill, "does not scroll sideways"))
    # The chosen pill is whole inside the row, wherever the row had been.
    expect_gte(lNow$pill$left, lNow$row$left - 1, label = paste(strPill, "is in view on its left"))
    expect_lte(lNow$pill$right, lNow$row$right + 1, label = paste(strPill, "is in view on its right"))
  }
  # The last pill was reached by scrolling the row, not the page.
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"StratifiedSurvival\"]').click()")
  expect_true(bWaitFor(lPage, sprintf("(() => { const seen = %s; return seen.chosen.page === 'StratifiedSurvival' && seen.rowAt > 0; })()", strShellLook)))
  expect_identical(lPage$Evaluate("[window.scrollX, document.documentElement.scrollLeft]"), list(0L, 0L))
  # A reader's files can have long names: the chip gives way, not the name.
  # The chip's words are set in the page here, as the session would set them.
  lLong <- lPage$Evaluate("(() => {
    document.querySelector('.gsm-bio-app-study-name').textContent = 'results_final_locked_2026.sas7bdat, participants.csv';
    const box = (node) => { const at = node.getBoundingClientRect(); return { left: at.left, right: at.right, top: at.top, bottom: at.bottom }; };
    return { name: box(document.querySelector('.gsm-bio-app-name')), title: box(document.querySelector('.gsm-bio-app-head h1')), study: box(document.querySelector('#gsm_bio_study')),
             wide: Math.max(document.documentElement.scrollWidth, document.body.scrollWidth) };
  })()")
  expect_lte(lLong$wide, 390)
  expect_lte(lLong$study$right, 390)
  expect_gte(lLong$study$left, lLong$title$right)
  expect_lte(lLong$title$right, lLong$name$right + 1)
  expect_lt(lLong$title$bottom, lLong$name$bottom + 1)
  expect_identical(lPage$Errors(), character(0))
})

test_that("in a browser an app with no outcomes table shows the stratified survival pill dimmed with its reason, still opens its page on the sentence saying what is missing, and holds at 390 pixels (#84)", {
  NeedApp()
  lApp <- lRunApp("RunApp(Synthetic_Results, Synthetic_Participants)")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, nWidth = 390L, nHeight = 844L, strAddress = lApp$address, bPhone = TRUE, chrBlocked = chrShellFonts)
  on.exit(lPage$Close(), add = TRUE)
  strPills <- "Array.from(document.querySelectorAll('#gsm_bio_chart a')).map((link) => { const pill = link.querySelector('.gsm-bio-app-pill'); return { value: link.dataset.value, dim: pill.classList.contains('gsm-bio-app-pill-dim'), why: pill.title, reads: link.textContent.replace(/\\s+/g, ' ').trim(), ink: getComputedStyle(pill).color, link: link.matches('a[href]') }; })"
  expect_true(bWaitFor(lPage, "document.querySelector('#gsm_bio_study_said').textContent.includes('tables')"))
  lPills <- lPage$Evaluate(strPills)
  bDim <- vapply(lPills, function(lPill) lPill$dim, logical(1))
  expect_identical(vapply(lPills, function(lPill) lPill$value, character(1))[bDim], "StratifiedSurvival")
  lDim <- lPills[[which(bDim)]]
  expect_identical(lDim$why, strShellLacks)
  expect_identical(lDim$reads, paste("Stratified survival", strShellLacks))
  expect_true(lDim$link)
  # Dimmed is drawn differently from a pill that can be opened.
  expect_false(identical(lDim$ink, lPills[[which(!bDim)[2]]]$ink))
  expect_identical(lPage$Evaluate("document.querySelector('#gsm_bio_pill_Data').textContent.replace(/\\s+/g, ' ').trim()"), "Data 2 tables")
  expect_identical(
    lPage$Evaluate("document.querySelector('#gsm_bio_study_said').textContent.replace(/\\s+/g, ' ').trim()"),
    "This app's tables 200 participants \u00b7 2 tables"
  )
  # It is still reached, by the keyboard as any pill is: from the chosen pill,
  # the left arrow twice, past Data and round to the last. Its page says what
  # is missing.
  lPage$Evaluate("document.querySelector('#gsm_bio_chart li.active > a').focus()")
  lPage$Press("ArrowLeft")
  lPage$Press("ArrowLeft")
  expect_true(bWaitFor(lPage, "document.activeElement.dataset.value === 'StratifiedSurvival'"), label = "the dimmed pill has the focus")
  expect_true(bWaitFor(lPage, "(document.querySelector('.tab-pane.active .gsm-bio-app-lacks') || {}).textContent"), label = "the sentence in the chart's place")
  expect_identical(lPage$Evaluate("document.querySelector('.tab-pane.active .gsm-bio-app-lacks').textContent"), strShellLacks)
  lNow <- lPage$Evaluate(strShellLook)
  expect_identical(lNow$chosen$page, "StratifiedSurvival")
  expect_lte(lNow$wide, lNow$window)
  expect_identical(lPage$Errors(), character(0))
})

# ---- What the second release review found of the page (#97) ------------------

# The outline every control of the Data page has when the keyboard is on it.
strShellOutline <- "outline: 2px solid #1f2328; outline-offset: 1px;"

# How far apart two colours are, as WCAG counts it: 1 for the same colour, 21
# for black on white. Each is three numbers from 0 to 255.
nShellContrast <- function(nInk, nGround) {
  Light <- function(nColour) {
    nPart <- nColour / 255
    nPart <- ifelse(nPart <= 0.03928, nPart / 12.92, ((nPart + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * nPart)
  }
  nBoth <- sort(c(Light(nInk), Light(nGround)), decreasing = TRUE)
  (nBoth[1] + 0.05) / (nBoth[2] + 0.05)
}

# A colour as a browser says it, `rgb(107, 93, 82)`, as its three numbers.
nShellColour <- function(strColour) {
  as.numeric(regmatches(strColour, gregexpr("[0-9.]+", strColour))[[1]][1:3])
}

# Every title a page's tags give it.
chrShellTitles <- function(xTag) {
  if (inherits(xTag, "shiny.tag")) {
    if (identical(xTag$name, "title")) {
      return(paste(unlist(xTag$children), collapse = ""))
    }
    return(unlist(lapply(xTag$children, chrShellTitles)))
  }
  if (is.list(xTag)) unlist(lapply(xTag, chrShellTitles))
}

test_that("the page says it is in English and is titled as the app is named, and its style gives a file control's drawn button and its name field the outline the page's other controls have, and 'No file chosen' ink a reader can read (#97)", {
  skip_if_not_installed("shiny")
  xPage <- App_Ui()
  expect_identical(attr(xPage, "lang"), "en")
  expect_identical(as.character(chrShellTitles(xPage)), "Biomarker charts")
  # The other controls' outline, as it was, and the file control's two parts
  # with the same: the drawn button while the real input, which is off screen,
  # has the keyboard's focus, and the field that names the chosen file.
  expect_match(strAppStyle, paste(".gsm-bio-app-data select:focus-visible {", strShellOutline, "}"), fixed = TRUE)
  expect_match(strAppStyle, paste(".gsm-bio-app-file .btn-file:has(:focus-visible) {", strShellOutline, "}"), fixed = TRUE)
  expect_match(strAppStyle, paste(".gsm-bio-app-file .input-group > .form-control:focus-visible {", strShellOutline, "}"), fixed = TRUE)
  # No rule of the file control takes an outline away.
  chrFileRules <- grep("^\\.gsm-bio-app-file ", strsplit(strAppStyle, "\n")[[1]], value = TRUE)
  expect_false(any(grepl("outline: (0|none)", chrFileRules)))
  # 'No file chosen' is written in an ink of 4.5 to 1 or better on the card's white.
  strInk <- regmatches(strAppStyle, regexec("\\.gsm-bio-app-file \\.input-group > \\.form-control::placeholder \\{ color: #([0-9a-f]{6}); opacity: 1; \\}", strAppStyle))[[1]][2]
  expect_false(is.na(strInk))
  nInk <- strtoi(substring(strInk, c(1, 3, 5), c(2, 4, 6)), 16L)
  expect_gte(nShellContrast(nInk, c(255, 255, 255)), 4.5)
  # The measure is the one the review used: the grey it found is 2.85 to 1.
  expect_equal(nShellContrast(c(153, 153, 153), c(255, 255, 255)), 2.85, tolerance = 0.002)
})

test_that("in a browser each of the three file controls shows where the keyboard is: with the control focused its drawn button has the outline the page's other controls have and has none when the focus is elsewhere, the name field beside it has the same, and 'No file chosen' is 4.5 to 1 or better on its card (#97)", {
  NeedApp()
  lApp <- lRunApp("RunApp()")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, nWidth = 1280L, nHeight = 900L, strAddress = lApp$address, chrBlocked = chrShellFonts)
  on.exit(lPage$Close(), add = TRUE)
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"Data\"]').click()")
  expect_true(bWaitFor(lPage, "document.querySelector('#gsm_bio_view table') && document.querySelector('#gsm_bio_file_outcomes')"), label = "the Data page is drawn")
  # What the keyboard is on, and how each file control is drawn: its button,
  # its name field, and where the real input and the drawn button are.
  strLook <- "(() => {
    const drawn = (node, part) => { const style = getComputedStyle(node, part); return [style.outlineStyle, style.outlineWidth, style.outlineColor, style.outlineOffset].join(' '); };
    const ground = (node) => { for (let at = node; at; at = at.parentElement) { const colour = getComputedStyle(at).backgroundColor; if (!/rgba\\(.*, 0\\)$|transparent/.test(colour)) return colour; } return 'rgb(255, 255, 255)'; };
    const seen = (node) => { const at = node.getBoundingClientRect(); return at.width > 0 && at.height > 0 && at.right > 0 && at.bottom > 0 && at.left < window.innerWidth; };
    const active = document.activeElement;
    const cards = {};
    for (const table of ['results', 'participants', 'outcomes']) {
      const card = document.querySelector('#gsm_bio_card_' + table);
      const name = card.querySelector('input[type=\"text\"]');
      const said = getComputedStyle(name, '::placeholder');
      cards[table] = { button: drawn(card.querySelector('.btn-file')), name: drawn(name), buttonSeen: seen(card.querySelector('.btn-file')), fileSeen: seen(card.querySelector('input[type=\"file\"]')),
                       placeholder: name.placeholder, ink: said.color, opacity: said.opacity, ground: ground(name) };
    }
    const card = active.closest('.gsm-bio-app-file');
    return { id: active.id, type: active.type || active.tagName, card: card && card.id, keyboard: active.matches(':focus-visible'), active: drawn(active), cards };
  })()"
  # From the control before the first file control, the Tab key.
  lPage$Evaluate("document.querySelector('#gsm_bio_view_next').focus()")
  lAway <- lPage$Evaluate(strLook)
  expect_identical(lAway$id, "gsm_bio_view_next")
  for (strTable in names(lAway$cards)) {
    # The real input is nowhere a reader sees; the button drawn for it is.
    expect_false(lAway$cards[[strTable]]$fileSeen, label = paste("the", strTable, "file input is on screen"))
    expect_true(lAway$cards[[strTable]]$buttonSeen, label = paste("the", strTable, "button is on screen"))
    expect_match(lAway$cards[[strTable]]$button, "^none ", label = paste("the", strTable, "button has no outline away from it"))
  }
  lOn <- list()
  for (strTable in names(lAway$cards)) {
    lPage$Press("Tab")
    lOn[[strTable]] <- list(file = lPage$Evaluate(strLook))
    lPage$Press("Tab")
    lOn[[strTable]]$name <- lPage$Evaluate(strLook)
  }
  # The next stop is the button, one of the page's other controls: its outline
  # is the one the file controls are held to.
  lPage$Press("Tab")
  lButton <- lPage$Evaluate(strLook)
  expect_identical(lButton$id, "gsm_bio_apply")
  expect_true(lButton$keyboard)
  strOutline <- lButton$active
  expect_identical(strOutline, "solid 2px rgb(31, 35, 40) 1px")
  for (strTable in names(lOn)) {
    lFile <- lOn[[strTable]]$file
    lName <- lOn[[strTable]]$name
    # The Tab key reached the real input, and then the name field beside it.
    expect_identical(lFile$id, paste0("gsm_bio_file_", strTable))
    expect_true(lFile$keyboard, label = paste("the keyboard is on the", strTable, "file input"))
    expect_identical(lName[c("type", "card")], list(type = "text", card = paste0("gsm_bio_card_", strTable)))
    # With the file control focused its drawn button is outlined as the
    # page's other controls are, which it is not when the focus is elsewhere.
    expect_identical(lFile$cards[[strTable]]$button, strOutline, label = paste("the", strTable, "button with its control focused"))
    expect_false(identical(lFile$cards[[strTable]]$button, lAway$cards[[strTable]]$button), label = paste("the", strTable, "button is drawn the same focused or not"))
    expect_identical(lName$cards[[strTable]]$button, lAway$cards[[strTable]]$button, label = paste("the", strTable, "button once the focus has left it"))
    # The name field shows the focus when it has it, and only then.
    expect_identical(lName$cards[[strTable]]$name, strOutline, label = paste("the", strTable, "name field with the focus"))
    expect_false(identical(lName$cards[[strTable]]$name, lAway$cards[[strTable]]$name), label = paste("the", strTable, "name field is drawn the same focused or not"))
    # No other card's control is outlined meanwhile.
    for (strOther in setdiff(names(lOn), strTable)) {
      expect_identical(lFile$cards[[strOther]]$button, lAway$cards[[strOther]]$button, label = paste("the", strOther, "button while", strTable, "has the focus"))
    }
    # 'No file chosen', against the card it is written on.
    expect_identical(lAway$cards[[strTable]]$placeholder, "No file chosen")
    expect_identical(lAway$cards[[strTable]]$opacity, "1")
    expect_gte(nShellContrast(nShellColour(lAway$cards[[strTable]]$ink), nShellColour(lAway$cards[[strTable]]$ground)), 4.5, label = paste("the contrast of 'No file chosen' in the", strTable, "card"))
  }
  expect_identical(lPage$Errors(), character(0))
})

test_that("in a browser the page says it is in English and is titled Biomarker charts, and a chart opened from the Data page's link takes the keyboard's focus with it, to the chart's pill, where a reader sees it (#97)", {
  NeedApp()
  lApp <- lRunApp("RunApp()")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, nWidth = 1280L, nHeight = 900L, strAddress = lApp$address, chrBlocked = chrShellFonts)
  on.exit(lPage$Close(), add = TRUE)
  expect_identical(lPage$Evaluate("({ lang: document.documentElement.lang, title: document.title })"), list(lang = "en", title = "Biomarker charts"))
  Chosen <- function(strPill) {
    sprintf("(() => { const seen = %s; return seen.chosen.pill === '%s' && seen.chosen.page === '%s' && seen.chosen.input === '%s'; })()", strShellLook, strPill, strPill, strPill)
  }
  strFocus <- "(() => { const active = document.activeElement; const at = active.getBoundingClientRect();
    return { tag: active.tagName, pill: active.closest('#gsm_bio_chart') ? active.dataset.value : null, seen: active.getClientRects().length > 0 && at.top >= 0 && at.bottom <= window.innerHeight,
             keyboard: active.matches(':focus-visible'), outline: getComputedStyle(active).outlineStyle, scrolled: window.scrollY }; })()"
  lPage$Evaluate("document.querySelector('#gsm_bio_chart a[data-value=\"Data\"]').click()")
  expect_true(bWaitFor(lPage, Chosen("Data")))
  expect_true(bWaitFor(lPage, "document.querySelector('#gsm_bio_rail a[data-gsm-bio-open=\"GroupComparison\"]')"), label = "the rail has its link")
  # By the keyboard: the link has the focus, and Enter follows it.
  lPage$Evaluate("document.querySelector('#gsm_bio_rail a[data-gsm-bio-open]').focus()")
  expect_identical(lPage$Evaluate("document.activeElement.dataset.gsmBioOpen"), "GroupComparison")
  lPage$Press("Enter")
  expect_true(bWaitFor(lPage, Chosen("GroupComparison")), label = "the link opens the group comparison")
  lAfter <- lPage$Evaluate(strFocus)
  # The link is on a page that is now hidden. The focus is not left there, or
  # nowhere: it is on the pill of the chart that was opened, outlined.
  expect_identical(lAfter[c("tag", "pill", "seen")], list(tag = "A", pill = "GroupComparison", seen = TRUE))
  expect_true(lAfter$keyboard)
  expect_identical(lAfter$outline, "solid")
  expect_identical(lAfter$scrolled, 0L)
  expect_identical(lPage$Evaluate(strShellLook)$chosen$stop, list("GroupComparison"))
  # From there the arrow keys walk the pills, as they do from any pill.
  lPage$Press("ArrowRight")
  expect_true(bWaitFor(lPage, Chosen("AssociationScatter")), label = "the right arrow goes on from the pill")
  expect_identical(lPage$Errors(), character(0))
})
