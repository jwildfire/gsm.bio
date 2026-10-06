# The saved group comparison page, opened in a headless browser with no
# network and driven as a reader drives it (#53). The other widget tests read
# what R wrote into the page; these run the copied bundle itself on it, and
# hold what R stored to what the chart asks: the stored-results key R computes
# has to equal the key the chart computes, or the page says that statistics are
# unavailable where a result should be.
#
# They run in the source tree (devtools::test(), and CI's source-tree step),
# with the chromote package and a Chrome or Chromium on the machine; see
# tests/testthat/helper-browser.R. Set GSM_BIO_PAGE_PICTURES to a folder and
# the first test also saves a picture of each of the three levels there.

# The stored results of a saved file, read back out of it, by key.
lStoredInFile <- function(strFile) {
  strPage <- paste(readLines(strFile, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  lStored <- lPagePayload(strPage)$lStatistics$results
  stats::setNames(lStored, vapply(lStored, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1)))
}

strAskedKey <- function(lAsked) {
  Chart_KeyText(lAsked[c("name", "args", "dataId")])
}

# Every request the chart made on a walk was answered, from the page's stored
# results, with the result stored under its key and no other.
ExpectAnsweredFromStore <- function(lAsked, lStored) {
  expect_gt(length(lAsked), 0L)
  for (lOne in lAsked) {
    strLabel <- paste(c(lOne$level, lOne$dataId$measure, lOne$dataId$visit, lOne$args$strPAdjust), collapse = " ")
    strKey <- strAskedKey(lOne)
    expect_true(strKey %in% names(lStored), label = paste("the page holds a result for", strLabel))
    expect_false(is.null(lOne$answer), label = paste("the chart was answered for", strLabel))
    if (!strKey %in% names(lStored) || is.null(lOne$answer)) next
    # The connection's answer: the stored result, said to be precomputed.
    expect_identical(lOne$answer$status, "ok", label = paste(strLabel, "status"))
    expect_identical(lOne$answer$form, "precomputed", label = paste(strLabel, "form"))
    expect_identical(lOne$answer$value, lStored[[strKey]]$value, label = paste(strLabel, "value"))
    expect_identical(lOne$rows, lStored[[strKey]]$rows, label = paste(strLabel, "rows"))
  }
}

# A page that asked for nothing over the network: every request it made is for
# the file itself, or for data the file holds.
ExpectNoNetwork <- function(lPage) {
  chrRequests <- unique(lPage$Requests())
  expect_true(lPage$url %in% chrRequests, label = "the page was loaded from its file")
  chrElsewhere <- chrRequests[chrRequests != lPage$url & !grepl("^(data|blob):", chrRequests)]
  expect_identical(chrElsewhere, character(0), label = "requests to anywhere but the file")
  expect_identical(lPage$Errors(), character(0), label = "errors the page raised")
}

test_that("a saved page, opened with no network, shows the three levels and answers every request they make from its stored results (#53)", {
  NeedBrowser()
  chrBiomarkers <- chrSyntheticBiomarkers()
  chrTested <- c("Week 2", "Week 4", "Week 8", "Week 12")
  # Change from Baseline, by arm, with an adjustment named, so the page holds
  # each row of tests both ways. No biomarker is named: the page opens on the
  # trend tiles.
  lWidget <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = list(
    value_type = "change", baseline_visits = "Baseline", group_by = "ARM", visit_adjustment = "holm"
  ))
  strFile <- strSavedFile(lWidget)
  lStored <- lStoredInFile(strFile)
  lPage <- lOpenPage(strFile)
  on.exit(lPage$Close(), add = TRUE)
  strPictures <- Sys.getenv("GSM_BIO_PAGE_PICTURES")
  Picture <- function(strName) {
    if (nzchar(strPictures)) lPage$Picture(file.path(strPictures, strName))
  }

  # Every biomarker: a tile each, in the Biomarker control's order, a picture
  # in each, no test, and nothing asked of R.
  lTiles <- lPage$Look()
  expect_true(lTiles$settled)
  expect_identical(lTiles$errors, list())
  expect_identical(lTiles$level, "biomarkers")
  expect_identical(unlist(lTiles$tiles), Chart_Measures(Synthetic_Results, GroupComparison_Settings()))
  expect_setequal(unlist(lTiles$tiles), chrBiomarkers)
  expect_gte(lTiles$canvases, length(chrBiomarkers))
  expect_identical(lTiles$statistics, list())
  expect_false(any(nzchar(unlist(lTiles$lines))), label = "a statistics line on the tiles")
  expect_match(lTiles$provenance[[1]], "stored with this page. No R runs here", fixed = TRUE)
  Picture("1-trend-tiles.png")

  # A tile opens its biomarker over time: the visits along the bottom, and
  # under each visit after Baseline R's test, from the page.
  lTime <- lPage$Move("tile", "IL-6")
  expect_identical(lTime$level, "over-time")
  expect_identical(unlist(lTime$trail), c("All biomarkers", "IL-6 over time"))
  expect_identical(unlist(lTime$visitButtons), chrTested)
  expect_identical(
    vapply(lTime$testRow, function(lCell) lCell$visit, character(1)),
    c("Baseline", chrTested)
  )
  expect_identical(vapply(lTime$testRow, function(lCell) lCell$status, character(1)), c("untested", rep("shown", 4L)))
  expect_true(all(grepl("^p [<=] ?[0-9.]+$", vapply(lTime$testRow[-1], function(lCell) lCell$text, character(1)))))
  expect_match(lTime$testRowHead, "p, adjusted (Holm)", fixed = TRUE)
  expect_length(lTime$statistics, 1L)
  expect_identical(lTime$statistics[[1]]$name, "Analyze_GroupDifferenceBy")
  expect_identical(lTime$statistics[[1]]$args$strPAdjust, "holm")
  Picture("2-one-biomarker-over-time.png")

  # A visit's name opens that visit alone, with its stored test under it.
  lVisit <- lPage$Move("visit", "Week 4")
  expect_identical(lVisit$level, "visits")
  expect_identical(unlist(lVisit$trail), c("All biomarkers", "IL-6 over time", "Week 4"))
  expect_length(lVisit$statistics, 1L)
  expect_identical(lVisit$statistics[[1]]$name, "Analyze_GroupDifference")
  expect_identical(lVisit$statistics[[1]]$dataId$visit, "Week 4")
  expect_match(paste(unlist(lVisit$lines), collapse = " "), "Welch Two Sample t-test: p < 0.001 (Placebo n = 95, Treatment n = 91)", fixed = TRUE)
  Picture("3-one-visit.png")

  # A view that was not computed says so where the result would be, at both
  # levels, and shows no number: another test was not stored.
  lOther <- lPage$Move("choose", "test", "wilcoxon")
  expect_length(lOther$statistics, 1L)
  expect_identical(lOther$statistics[[1]]$answer[c("status", "reason")], list(status = "unavailable", reason = "not-precomputed"))
  expect_false(strAskedKey(lOther$statistics[[1]]) %in% names(lStored))
  expect_match(
    paste(unlist(lOther$lines), collapse = " "),
    "Statistics are unavailable for this view: the page holds no stored result for it, and no R is attached to compute one.",
    fixed = TRUE
  )
  lOtherTime <- lPage$Move("trail", "IL-6 over time")
  expect_identical(lOtherTime$testRowState, "unavailable")
  expect_identical(lOtherTime$statistics[[1]]$answer$status, "unavailable")
  expect_match(paste(unlist(lOtherTime$lines), collapse = " "), "Statistics are unavailable for this view", fixed = TRUE)
  expect_identical(lOtherTime$testRow, list())
  # Back to what the page opened on.
  lAgain <- lPage$Move("choose", "test", "t")
  expect_identical(lAgain$testRowState, "shown")
  expect_identical(lPage$Move("trail", "All biomarkers")$level, "biomarkers")

  # The whole page, as a reader can walk it: every biomarker over time, under
  # each adjustment, and every visit alone. Everything asked was answered from
  # the page, and the page holds nothing the chart did not ask for.
  lWalk <- lWalkPage(lPage, chrBiomarkers, c("none", "holm"))
  ExpectAnsweredFromStore(lWalk$asked, lStored)
  chrAsked <- unique(vapply(lWalk$asked, strAskedKey, character(1)))
  expect_setequal(chrAsked, names(lStored))
  expect_identical(length(lStored), length(chrBiomarkers) * (2L + length(chrTested)))
  # The three levels were the ones walked.
  expect_setequal(
    unique(vapply(lWalk$asked, function(lOne) paste(lOne$level, lOne$name), character(1))),
    c("over-time Analyze_GroupDifferenceBy", "visits Analyze_GroupDifference")
  )
  ExpectNoNetwork(lPage)
})

test_that("with unscheduled rows added, the page draws the visits R stored results for and asks for nothing else (#53)", {
  NeedBrowser()
  dfResults <- dfWithUnscheduled()
  chrBiomarkers <- c("IL-6", "CRP")
  chrScheduled <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
  # No baseline visit is named: R names the first visit the chart draws.
  lSettings <- list(value_type = "change", group_by = "ARM", measures = chrBiomarkers)

  # Left out, the default. The page says how many are not drawn, offers the
  # scheduled visits alone, and every request it makes is one R stored.
  strFile <- strSavedFile(Widget_GroupComparison(dfResults, Synthetic_Participants, lSettings = lSettings))
  lStored <- lStoredInFile(strFile)
  lPage <- lOpenPage(strFile)
  on.exit(lPage$Close(), add = TRUE)
  lTiles <- lPage$Look()
  expect_identical(lTiles$level, "biomarkers")
  expect_identical(unlist(lTiles$tiles), chrBiomarkers)
  expect_identical(unlist(lTiles$offered), chrScheduled)
  expect_length(lTiles$hidden, 1L)
  expect_match(lTiles$hidden[[1]], "2 unscheduled visits not drawn: Unscheduled 1, EARLY TERMINATION.", fixed = TRUE)
  # The rows the bundle's own rule sets aside are the rows R set aside.
  lTheirs <- lPage$Evaluate(
    "(() => { const chart = window.gsmBioPage.chart(); const found = BioViz.core.scheduledResults(chart.tables.results, chart.settings); return { visits: found.visits, rows: found.rows, kept: found.results.length }; })()"
  )
  lMine <- Core_Scheduled(dfResults, c(list(visit_col = "VISIT"), lCoreUnscheduledDefaults))
  expect_identical(unlist(lTheirs$visits), lMine$visits)
  expect_identical(lTheirs$rows, lMine$rows)
  expect_identical(lTheirs$kept, nrow(lMine$results))

  lWalk <- lWalkPage(lPage, chrBiomarkers)
  expect_identical(unlist(lWalk$views$over_time$offered), chrScheduled)
  expect_identical(vapply(lWalk$views$over_time$testRow, function(lCell) lCell$visit, character(1)), chrScheduled)
  expect_identical(lWalk$views$over_time$testRow[[1]]$status, "untested")
  ExpectAnsweredFromStore(lWalk$asked, lStored)
  expect_setequal(unique(vapply(lWalk$asked, strAskedKey, character(1))), names(lStored))
  chrAskedVisits <- unique(unlist(lapply(lWalk$asked, function(lOne) c(lOne$dataId$visit, unlist(lOne$dataId$visits)))))
  expect_setequal(chrAskedVisits, chrScheduled[-1])
  expect_false(any(vapply(lWalk$asked, function(lOne) "unscheduled_visits" %in% names(lOne$dataId), logical(1))))
  # A reader who switches them on leaves the views that were computed, and is
  # told so: R stored results for the visits the page opens on and no others.
  lPage$Move("tile", "IL-6")
  lSwitched <- lPage$Move("tick", "unscheduled-visits")
  expect_identical(lSwitched$level, "over-time")
  expect_true(all(c("Unscheduled 1", "EARLY TERMINATION") %in% unlist(lSwitched$offered)))
  expect_true(lSwitched$statistics[[1]]$dataId$unscheduled_visits)
  expect_identical(lSwitched$statistics[[1]]$answer$status, "unavailable")
  expect_identical(lSwitched$testRowState, "unavailable")
  ExpectNoNetwork(lPage)
  lPage$Close()

  # Switched on by the settings: the page draws them, the baseline is the first
  # visit of them all, and again every request is one R stored.
  strFileOn <- strSavedFile(Widget_GroupComparison(dfResults, Synthetic_Participants, lSettings = c(lSettings, list(unscheduled_visits = TRUE))))
  lStoredOn <- lStoredInFile(strFileOn)
  lPageOn <- lOpenPage(strFileOn)
  on.exit(lPageOn$Close(), add = TRUE)
  chrEvery <- c("Unscheduled 1", "Baseline", "Week 2", "Week 4", "EARLY TERMINATION", "Week 8", "Week 12")
  lTilesOn <- lPageOn$Look()
  expect_identical(unlist(lTilesOn$offered), chrEvery)
  expect_identical(lTilesOn$hidden, list())
  lWalkOn <- lWalkPage(lPageOn, chrBiomarkers)
  expect_identical(vapply(lWalkOn$views$over_time$testRow, function(lCell) lCell$visit, character(1)), chrEvery)
  expect_identical(lWalkOn$views$over_time$testRow[[1]]$status, "untested")
  ExpectAnsweredFromStore(lWalkOn$asked, lStoredOn)
  expect_setequal(unique(vapply(lWalkOn$asked, strAskedKey, character(1))), names(lStoredOn))
  expect_true(all(vapply(lWalkOn$asked, function(lOne) isTRUE(lOne$dataId$unscheduled_visits), logical(1))))
  expect_true(all(vapply(lWalkOn$asked, function(lOne) identical(lOne$dataId$baseline_visits, list("Unscheduled 1")), logical(1))))
  ExpectNoNetwork(lPageOn)
})

test_that("R reads an unscheduled-visit pattern as the copied bundle's own rule reads it in a browser (#53)", {
  NeedBrowser()
  strFile <- strSavedFile(Widget_GroupComparison(Synthetic_Results, lSettings = list(measures = "IL-6")))
  lPage <- lOpenPage(strFile)
  on.exit(lPage$Close(), add = TRUE)
  # Names a study could hold, and names that tell a browser's reading of `i`
  # from another's: only the letters A to Z match in either case, so the long
  # s, the dotless i, the dotted capital I and the Kelvin sign are themselves.
  chrNames <- c(
    "Unscheduled", "UNSCHEDULED 2", "Visit 3 (unscheduled)", "unscheduled", "Early Termination", "EARLY TERMINATION",
    "early  termination", "early termination", "Early Term", "ET", "Week 4", "Week 12", "Baseline", "Screening",
    "Unſcheduled", "early termınation", "EARLY TERMİNATION", "weeK 4", "Unscheduled|x", "a|b", " ", "x"
  )
  chrPatterns <- c(
    lCoreUnscheduledDefaults$unscheduled_visit_pattern, "/unscheduled/i", "/Unscheduled/", "unscheduled", "Week|Base",
    "/week|base/i", "/WEEK 4|et/i", "/week|/", "//", "/ /", "x", "/Early Term|Screening/", "/k/i", "/s/i", "/I/i"
  )
  for (strPattern in chrPatterns) {
    expect_false(is.null(Core_ReadPattern(strPattern)), label = paste("R reads", strPattern))
    lTheirs <- lPage$Evaluate(sprintf(
      "%s.map((name) => BioViz.core.isUnscheduledVisit(name, { unscheduled_visit_pattern: %s }))",
      jsonlite::toJSON(chrNames), jsonlite::toJSON(strPattern, auto_unbox = TRUE)
    ))
    expect_identical(
      Core_IsUnscheduled(chrNames, list(unscheduled_visit_pattern = strPattern)), unlist(lTheirs),
      label = paste("R's reading of", strPattern), expected.label = "the bundle's"
    )
  }
  # Each name is unscheduled under one pattern or another, and scheduled under
  # one or another: the comparison could fail either way.
  expect_true(any(Core_IsUnscheduled(chrNames, lCoreUnscheduledDefaults)) && !all(Core_IsUnscheduled(chrNames, lCoreUnscheduledDefaults)))
  # A list is read the same everywhere, by name.
  lListed <- lPage$Evaluate(sprintf(
    "%s.map((name) => BioViz.core.isUnscheduledVisit(name, { unscheduled_visit_values: ['Week 4', 'ET'], unscheduled_visit_pattern: '/x/' }))",
    jsonlite::toJSON(chrNames)
  ))
  expect_identical(Core_IsUnscheduled(chrNames, list(unscheduled_visit_values = c("Week 4", "ET"), unscheduled_visit_pattern = "/x/")), unlist(lListed))
  # The default is the bundle's own.
  expect_identical(
    lPage$Evaluate("BioViz.core.UNSCHEDULED_DEFAULTS"),
    lCoreUnscheduledDefaults[c("unscheduled_visits", "unscheduled_visit_pattern", "unscheduled_visit_values")]
  )
})
