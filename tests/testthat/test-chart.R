# What every chart shares, held to bio.viz's own code (#19). The filters a chart
# opens on decide which participants R computes on, so Chart_Filters() is held
# to the bundles the widgets ship: fixtures/filter-states/states.json holds what
# the safety.viz kit and bio.viz's addFilterControls opened each case in
# cases.json on, written by data-raw/filter-states.mjs from the vendored bundles.

lFilterCases <- function() {
  lReadJson(testthat::test_path("fixtures", "filter-states"), "cases.json")$cases
}

lFilterStates <- function() {
  lReadJson(testthat::test_path("fixtures", "filter-states"), "states.json")
}

# A case's participant table: its own, given by column as JSON gives it, or the
# synthetic study's.
dfFilterParticipants <- function(lCase) {
  if (is.null(lCase$participants)) {
    return(Synthetic_Participants)
  }
  as.data.frame(lapply(lCase$participants, function(lColumn) {
    unlist(lapply(lColumn, function(xValue) if (is.null(xValue)) NA else xValue))
  }), stringsAsFactors = FALSE, check.names = FALSE)
}

lFilterOpened <- function(lCase) {
  dfParticipants <- dfFilterParticipants(lCase)
  lConfig <- GroupComparison_Settings(list(filters = lCase$filters))
  list(
    participants = dfParticipants,
    config = lConfig,
    state = Chart_Filters(dfParticipants, lConfig, Chart_Categories(Synthetic_Results, dfParticipants, lConfig))
  )
}

KitState <- function(lState) {
  lapply(lState, function(xValue) if (is.null(xValue)) NULL else as.character(unlist(xValue)))
}

test_that("the filters open on what the bundles open them on: a start the data lacks is All, and all = FALSE is the first value (#19)", {
  lCases <- lFilterCases()
  lStates <- lFilterStates()$states
  expect_gte(length(lCases), 27L)
  expect_identical(
    vapply(lStates, function(lState) lState$case, character(1)),
    vapply(lCases, function(lCase) lCase$case, character(1))
  )
  for (iCase in seq_along(lCases)) {
    lCase <- lCases[[iCase]]
    if (!is.null(lCase$differs)) next
    lMine <- lFilterOpened(lCase)$state
    lTheirs <- KitState(lStates[[iCase]]$state)
    expect_identical(names(lMine), names(lTheirs), label = paste(lCase$case, "filters"))
    for (strColumn in names(lTheirs)) {
      expect_identical(lMine[[strColumn]], lTheirs[[strColumn]], label = paste(lCase$case, strColumn))
    }
  }
  # The cases reach every turn of the rule: a start kept and one dropped, the
  # first value in force, several values kept in part and dropped whole, the
  # participant's id, which is no filter, and two specs on one column.
  chrWarnings <- unlist(lapply(lStates, function(lState) lState$warnings))
  expect_true(any(grepl("opens on All", chrWarnings, fixed = TRUE)))
  expect_true(any(grepl("opens on [ Placebo ]", chrWarnings, fixed = TRUE)))
  expect_true(any(grepl("opens without it", chrWarnings, fixed = TRUE)))
})

test_that("a filter of several values given no value opens on none, as bio.viz's startFilters opens it, and lets nobody through (#39)", {
  lCase <- Filter(function(lCase) identical(lCase$case, "multiple-with-an-empty-start"), lFilterCases())[[1]]
  lOpened <- lFilterOpened(lCase)
  expect_identical(lOpened$state$RESPONSE, character(0))
  lKept <- Chart_KeepFiltered(Synthetic_Results, Synthetic_Participants, lOpened$config, lOpened$state)
  expect_identical(nrow(lKept$participants), 0L)
  expect_identical(nrow(lKept$results), 0L)
  # It is in force, and named in no title: bio.viz's filtersInForce leaves out
  # a filter of no value.
  expect_identical(Chart_FiltersInForce(lOpened$state), list())
  # A filter of one value given none, or one set to all, still lets everyone through.
  lAll <- Chart_KeepFiltered(Synthetic_Results, Synthetic_Participants, lOpened$config, list(RESPONSE = NULL))
  expect_identical(nrow(lAll$participants), nrow(Synthetic_Participants))
  lSingle <- Chart_Filters(Synthetic_Participants, GroupComparison_Settings(list(filters = list(list(value_col = "RESPONSE", start = list())))), NULL)
  expect_null(lSingle$RESPONSE)
})

test_that("a control offers the values the chart lists: text that is only white space is a value, and only empty text is none (#19)", {
  lCases <- lFilterCases()
  lStates <- lFilterStates()$states
  for (iCase in seq_along(lCases)) {
    lCase <- lCases[[iCase]]
    dfParticipants <- dfFilterParticipants(lCase)
    for (strColumn in names(lStates[[iCase]]$offered)) {
      expect_identical(
        length(Chart_FilterValues(dfParticipants, strColumn)), as.integer(lStates[[iCase]]$offered[[strColumn]]),
        label = paste(lCase$case, strColumn, "values offered")
      )
    }
  }
  expect_identical(Chart_FilterValues(data.frame(SEX = c(" ", "M", NA, "", "F")), "SEX"), c(" ", "F", "M"))
})

test_that("where R's order of the values is not the browser's, R's first value is pinned, and named as the start it is the kit's too (#19)", {
  lCases <- Filter(function(lCase) !is.null(lCase$differs), lFilterCases())
  lStates <- lFilterStates()$states
  names(lStates) <- vapply(lStates, function(lState) lState$case, character(1))
  # Text that is not plain letters and digits: a letter with an accent, and punctuation.
  expect_gte(length(lCases), 2L)
  for (lCase in lCases) {
    lOpened <- lFilterOpened(lCase)
    strColumn <- lCase$filters[[1]]$value_col
    # What R opens on, pinned: R's order is its own, whatever the session's locale.
    expect_identical(lOpened$state[[strColumn]], lCase$differs$r, label = paste(lCase$case, "R's first value"))
    # The browser's first value is another.
    expect_false(identical(KitState(lStates[[lCase$case]]$state)[[strColumn]], lCase$differs$r), label = paste(lCase$case, "the kit's first value"))
    # Named as the filter's start, R's value is the one the kit opens on.
    expect_identical(KitState(lStates[[lCase$case]]$named)[[strColumn]], lCase$differs$r, label = paste(lCase$case, "named"))
    lNamed <- Widget_NameFilters(lOpened$config, list(filters = lCase$filters), Synthetic_Results, lOpened$participants)
    expect_identical(lNamed$settings$filters[[1]]$start, lCase$differs$r, label = paste(lCase$case, "the start the page is given"))
    expect_identical(lNamed$config$filters[[1]]$start, lCase$differs$r)
  }
})

test_that("a widget names the value an all = FALSE filter opens on as its start, and leaves every other filter as it was given (#19)", {
  dfParticipants <- Synthetic_Participants
  dfParticipants$COUNTRY <- ifelse(seq_len(nrow(dfParticipants)) %% 2L == 0L, "France", "\u00c9ire")
  lWidget <- Widget_GroupComparison(Synthetic_Results, dfParticipants, lSettings = list(
    start_value = "IL-6", filters = list("ARM", list(value_col = "COUNTRY", all = FALSE))
  ))
  lFilters <- lWidget$x$lSettings$filters
  expect_identical(vapply(lFilters, function(lSpec) lSpec$value_col, character(1)), c("ARM", "COUNTRY"))
  expect_null(lFilters[[1]]$start)
  expect_identical(lFilters[[2]]$start, "France")
  expect_false(lFilters[[2]]$all)
  expect_true(all(vapply(lWidget$x$lStatistics$results, function(lResult) identical(lResult$dataId$filters, list(COUNTRY = list("France"))), logical(1))))
  # Two specs on one column: both are named, so the kit opens where R does.
  lTwo <- Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = list(
    start_value = "IL-6", filters = list(list(value_col = "SEX", all = FALSE), "SEX")
  ))$x$lSettings$filters
  expect_identical(vapply(lTwo, function(lSpec) lSpec$start, character(1)), c("F", "F"))
  # Filters that name no first value are handed to the page as they were given.
  lGiven <- list(start_value = "IL-6", filters = list("ARM", list(value_col = "SEX", start = "F")))
  expect_identical(Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = lGiven)$x$lSettings$filters, lGiven$filters)
})

test_that("a session whose locale is not UTF-8 makes every widget with text that is not ASCII (#19, #22)", {
  strCtype <- Sys.getlocale("LC_CTYPE")
  strCollate <- Sys.getlocale("LC_COLLATE")
  on.exit({
    Sys.setlocale("LC_CTYPE", strCtype)
    Sys.setlocale("LC_COLLATE", strCollate)
  })
  Sys.setlocale("LC_CTYPE", "C")
  Sys.setlocale("LC_COLLATE", "C")
  # Text read in such a session is held unmarked: its UTF-8 bytes, which R
  # does not know to be UTF-8.
  strEire <- rawToChar(as.raw(c(0xc3, 0x89, 0x69, 0x72, 0x65)))
  strCafe <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xc3, 0xa9)))
  expect_identical(Encoding(c(strEire, strCafe)), c("unknown", "unknown"))
  dfParticipants <- Synthetic_Participants
  dfParticipants$COUNTRY <- ifelse(seq_len(nrow(dfParticipants)) %% 2L == 0L, "France", strEire)
  dfParticipants$CITY <- ifelse(seq_len(nrow(dfParticipants)) %% 3L == 0L, strCafe, "cafe")
  for (lSettings in list(
    list(),
    list(filters = list(list(value_col = "COUNTRY", all = FALSE), list(value_col = "CITY", start = strCafe)))
  )) {
    expect_no_error(Widget_GroupComparison(Synthetic_Results, dfParticipants, lSettings = c(list(start_value = "IL-6"), lSettings)))
    expect_no_error(Widget_AssociationScatter(Synthetic_Results, dfParticipants, lSettings = lSettings))
    expect_no_error(Widget_CorrelationMatrix(Synthetic_Results, dfParticipants, lSettings = lSettings))
    expect_no_error(Widget_BiomarkerScreen(Synthetic_Results, dfParticipants, lSettings = c(list(visit = "Week 4", value_type = "change", group_by = "ARM"), lSettings)))
  }
  lStored <- Widget_GroupComparison(Synthetic_Results, dfParticipants, lSettings = list(
    start_value = "IL-6", filters = list(list(value_col = "COUNTRY", all = FALSE), list(value_col = "CITY", start = strCafe))
  ))$x$lStatistics$results
  # The widget holds the text marked UTF-8, the bytes it was read as (#22).
  strCafeUtf8 <- strCafe
  Encoding(strCafeUtf8) <- "UTF-8"
  expect_identical(lStored[[1]]$dataId$filters, list(COUNTRY = list("France"), CITY = list(strCafeUtf8)))
  expect_identical(charToRaw(lStored[[1]]$dataId$filters$CITY[[1]]), charToRaw(strCafe))
  # The order is the same as in a UTF-8 session: by the text's UTF-8 bytes.
  for (strAccented in list(strEire, "\u00c9ire")) {
    expect_identical(Core_NaturalCompare("France", strAccented), -1)
    expect_identical(Core_Levels(c(strAccented, "France", "b", "(d)", "_a")), c("(d)", "_a", "b", "France", strAccented))
  }
  expect_identical(Core_NaturalCompare("cafe", strCafe), -1)
  expect_identical(Core_NaturalCompare("cafe", "caf\u00e9"), -1)
  expect_identical(Core_SortText(c(strCafe, "cafe", "Cafe")), c("Cafe", "cafe", strCafe))
})

test_that("the filter states were written by the vendored kit, from the study the package ships (#19)", {
  lStates <- lFilterStates()
  lRecord <- lReadJson(system.file("htmlwidgets", "lib", package = "gsm.bio"), "SOURCE.json")
  lBundle <- Filter(function(lFile) lFile$library == "safety.viz", lRecord$files)[[1]]
  expect_identical(lStates$written_by, "data-raw/filter-states.mjs")
  expect_identical(lStates$bundle$sha256, lBundle$sha256)
  expect_identical(lStates$bundle$bio_viz_commit, lRecord$commit)
  lBioViz <- Filter(function(lFile) lFile$library == "bio.viz", lRecord$files)[[1]]
  expect_identical(lStates$bio_viz_bundle$sha256, lBioViz$sha256)
  expect_identical(
    strSha256(system.file("extdata", "synthetic_participants.csv", package = "gsm.bio")),
    lStates$study[[1]]$sha256
  )
})

# The study with numbers where it has names: the visit is its number, and the
# arm, a dose and the sex are numeric participant columns.
lNumericStudy <- function() {
  dfResults <- Synthetic_Results
  dfResults$VISIT <- dfResults$VISITNUM
  dfParticipants <- Synthetic_Participants
  dfParticipants$ARMN <- ifelse(dfParticipants$ARM == "Placebo", 0L, 1L)
  dfParticipants$DOSE <- ifelse(dfParticipants$ARM == "Placebo", 0, 2.5)
  dfParticipants$SEXN <- ifelse(dfParticipants$SEX == "F", 1, 2)
  list(results = dfResults, participants = dfParticipants)
}

# Every value in a stored result's key, as the chart writes its request's: text
# for a name, a visit, a panel, a group or a filter value, and a number only
# where the chart writes one.
chrKeyNumbers <- function(lResults) {
  Leaves <- function(xValue, strPath) {
    if (is.list(xValue)) {
      return(unlist(lapply(seq_along(xValue), function(i) Leaves(xValue[[i]], paste0(strPath, "/", if (is.null(names(xValue))) i else names(xValue)[i])))))
    }
    if (is.numeric(xValue)) strPath else character(0)
  }
  unlist(lapply(lResults, function(lResult) Leaves(lResult$dataId, lResult$name)))
}

test_that("a numeric visit, panel, group or colour column is keyed as text, as the chart sends it (#19)", {
  lStudy <- lNumericStudy()
  Make <- function(Widget, lSettings) Widget(lStudy$results, lStudy$participants, lSettings = lSettings)$x$lStatistics$results

  lGroup <- Make(Widget_GroupComparison, list(start_value = "IL-6", value_type = "change", group_by = "DOSE", color_by = "SEXN", panel_by = "SEXN", visits = c(4, 12)))
  expect_gt(length(lGroup), 0L)
  expect_identical(chrKeyNumbers(lGroup), character(0))
  expect_setequal(vapply(lGroup, function(lResult) lResult$dataId$visit, character(1)), c("4", "12"))
  expect_setequal(vapply(lGroup, function(lResult) lResult$dataId$panel, character(1)), c("1", "2"))
  expect_identical(lGroup[[1]]$dataId$groups, list("0", "2.5"))
  expect_identical(lGroup[[1]]$dataId$baseline_visits, list("0"))

  lScatter <- Make(Widget_AssociationScatter, list(
    x = list(measure = "TNF-alpha", visit = "0"), y = list(measure = "IL-10", visit = "0"), color_by = "ARMN", panel_by = "SEXN"
  ))
  expect_gt(length(lScatter), 0L)
  expect_identical(chrKeyNumbers(lScatter), character(0))
  expect_setequal(unique(vapply(lScatter, function(lResult) lResult$dataId$panel, character(1))), c("1", "2"))
  expect_identical(lScatter[[1]]$dataId$groups, list("0", "1"))

  lMatrix <- Make(Widget_CorrelationMatrix, list(visit = "4", limit = 3))
  expect_identical(length(lMatrix), 1L + 6L)
  expect_identical(chrKeyNumbers(lMatrix), character(0))
  expect_identical(lMatrix[[1]]$dataId$variables[[1]]$visit, "4")

  lScreen <- Make(Widget_BiomarkerScreen, list(visit = "4", value_type = "change", group_by = "ARMN", filters = list(list(value_col = "SEXN", start = 2))))
  expect_identical(length(lScreen), 13L)
  expect_identical(chrKeyNumbers(lScreen), character(0))
  expect_identical(lScreen[[1]]$args$chrGroups, list("0", "1"))
  expect_identical(lScreen[[1]]$dataId$visit, "4")
  expect_identical(lScreen[[1]]$dataId$filters, list(SEXN = list("2")))
  expect_identical(lScreen[[2]]$dataId$visit, "4")
})
