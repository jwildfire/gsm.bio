# The frame in R (#9): named variables resolved to one row per participant, as
# bio.viz's core resolves them in the page. It is the one rule gsm.bio writes a
# second time, so it is held to frames the core itself wrote:
# fixtures/core-frames/frames.json holds what `BioViz.core.frame` of the
# vendored bundle returned for each case in cases.json, and R is run on the
# same tables, variables and settings.

lCoreCases <- function() {
  lReadJson(testthat::test_path("fixtures", "core-frames"), "cases.json")
}

lCoreFrames <- function() {
  lFrames <- lReadJson(testthat::test_path("fixtures", "core-frames"), "frames.json")$frames
  stats::setNames(lFrames, vapply(lFrames, function(lFrame) lFrame$case, character(1)))
}

# A case's tables: the participant table left out when the case says so, with
# the columns it names carried onto the results rows first.
lCaseTables <- function(lCase, lSources) {
  lTables <- lSources[[lCase$tables]]
  if (!identical(lCase$participants, FALSE)) {
    return(lTables)
  }
  dfResults <- lTables$results
  for (strColumn in unlist(lCase$carry)) {
    dfResults[[strColumn]] <- lTables$participants[[strColumn]][match(dfResults$USUBJID, lTables$participants$USUBJID)]
  }
  list(results = dfResults, participants = NULL)
}

# A setting written as null in the cases is a setting given as NULL.
lCaseSettings <- function(lCase) {
  lSettings <- lCase$settings
  if (!is.null(lSettings$baseline_visits)) {
    lSettings$baseline_visits <- unlist(lSettings$baseline_visits)
  }
  # `required: []` is every variable optional, which is not NULL, all required.
  if ("required" %in% names(lSettings)) {
    lSettings$required <- as.character(unlist(lSettings$required))
  }
  lSettings
}

# Holds R's frame to the frame the core wrote: the same participants in the
# same order, the same value of every variable, and the same counts of who was
# left out, for the same reasons, in the same order.
ExpectSameFrame <- function(lMade, lTheirs, strCase) {
  dfTheirs <- dfFromRecords(lTheirs$data)
  expect_identical(nrow(lMade$data), nrow(dfTheirs), label = paste(strCase, "rows"))
  expect_identical(names(lMade$data), names(dfTheirs), label = paste(strCase, "columns"))
  for (strColumn in names(dfTheirs)) {
    if (is.numeric(dfTheirs[[strColumn]])) {
      expect_true(is.numeric(lMade$data[[strColumn]]), label = paste(strCase, strColumn, "is a number"))
      expect_equal(
        as.numeric(lMade$data[[strColumn]]), as.numeric(dfTheirs[[strColumn]]),
        tolerance = 1e-12, label = paste(strCase, strColumn)
      )
    } else {
      expect_identical(Core_Text(lMade$data[[strColumn]]), dfTheirs[[strColumn]], label = paste(strCase, strColumn))
    }
  }
  expect_identical(as.numeric(lMade$participants), as.numeric(lTheirs$participants), label = paste(strCase, "participants"))
  expect_identical(
    lMade$dropped$reason, vapply(lTheirs$dropped, function(lEntry) lEntry$reason, character(1)),
    label = paste(strCase, "reasons")
  )
  expect_identical(
    lMade$dropped$variable,
    vapply(lTheirs$dropped, function(lEntry) if (is.null(lEntry$variable)) NA_character_ else lEntry$variable, character(1)),
    label = paste(strCase, "variables left out by")
  )
  expect_identical(
    as.numeric(lMade$dropped$n), vapply(lTheirs$dropped, function(lEntry) as.numeric(lEntry$n), numeric(1)),
    label = paste(strCase, "counts left out")
  )
  expect_identical(lMade$baseline_visits, unlist(lTheirs$baseline_visits), label = paste(strCase, "baseline visits"))
  expect_identical(nrow(lMade$data) + sum(lMade$dropped$n), as.integer(lMade$participants), label = paste(strCase, "adds up"))
}

test_that("R's frame equals the frame bio.viz's core writes, for every value type, with and without a participant table (#9)", {
  lFile <- lCoreCases()
  lTheirs <- lCoreFrames()
  lSources <- list(
    study = lStudyAsText(),
    edge = list(results = dfFromRecords(lFile$edge$results), participants = dfFromRecords(lFile$edge$participants))
  )

  for (lCase in lFile$cases) {
    lTables <- lCaseTables(lCase, lSources)
    lMade <- Core_Frame(lTables$results, lTables$participants, lCase$variables, lCaseSettings(lCase))
    ExpectSameFrame(lMade, lTheirs[[lCase$case]], lCase$case)
  }

  # The cases cover what the widget can be asked for: every value type, the
  # results table alone, and every reason a participant is left out.
  chrValueTypes <- unlist(lapply(lFile$cases, function(lCase) {
    lapply(lCase$variables, function(lVariable) if (is.null(lVariable$measure)) NULL else c(lVariable$value, "raw")[1])
  }))
  expect_setequal(chrValueTypes, chrCoreValueTypes)
  for (strTables in c("study", "edge")) {
    bAlone <- vapply(lFile$cases, function(lCase) lCase$tables == strTables && identical(lCase$participants, FALSE), logical(1))
    expect_true(any(bAlone), label = paste("a results-alone case on the", strTables, "tables"))
  }
  chrReasons <- unique(unlist(lapply(lTheirs, function(lFrame) lapply(lFrame$dropped, function(lEntry) lEntry$reason))))
  expect_setequal(chrReasons, unname(chrCoreDropped))
  chrStats <- unlist(lapply(lFile$cases, function(lCase) lCase$settings$baseline_stat))
  expect_setequal(chrStats, chrCoreBaselineStats)
})

test_that("R's frame is the same from the typed study as from its text (#9)", {
  # The widget is handed R's own tables, where a result is a number and a
  # missing one is NA; the fixtures were written from the CSV files' text.
  lFile <- lCoreCases()
  lTheirs <- lCoreFrames()
  lSources <- list(study = list(results = Synthetic_Results, participants = Synthetic_Participants))

  lStudyCases <- Filter(function(lCase) lCase$tables == "study", lFile$cases)
  expect_gt(length(lStudyCases), 10)
  for (lCase in lStudyCases) {
    lTables <- lCaseTables(lCase, lSources)
    lMade <- Core_Frame(lTables$results, lTables$participants, lCase$variables, lCaseSettings(lCase))
    ExpectSameFrame(lMade, lTheirs[[lCase$case]], paste(lCase$case, "(typed)"))
  }

  # A factor is its text, to the frame.
  dfParticipants <- Synthetic_Participants
  dfParticipants$ARM <- factor(dfParticipants$ARM)
  lCase <- lStudyCases[[3]]
  lMade <- Core_Frame(Synthetic_Results, dfParticipants, lCase$variables, lCaseSettings(lCase))
  ExpectSameFrame(lMade, lTheirs[[lCase$case]], paste(lCase$case, "(factor)"))
})

test_that("the frame refuses a column no table has, a malformed variable and an unknown setting (#9)", {
  lY <- list(measure = "IL-6", visit = "Week 4")
  expect_error(
    Core_Frame(Synthetic_Results, Synthetic_Participants, list(y = lY, x = list(col = "NOPE"))),
    "no table has the column `NOPE`"
  )
  expect_error(Core_Frame(Synthetic_Results, NULL, list(y = lY), list(value_col = "NOPE")), "no column `NOPE` \\(`value_col`\\)")
  expect_error(Core_Frame(Synthetic_Results, NULL, list(USUBJID = lY)), "cannot be named `USUBJID`")
  expect_error(Core_Frame(Synthetic_Results, NULL, list(y = list(measure = "IL-6"))), "must name its visit")
  expect_error(Core_Frame(Synthetic_Results, NULL, list(y = list(measure = "IL-6", visit = "Week 4", value = "delta"))), "`value` must be one of")
  expect_error(Core_Frame(Synthetic_Results, NULL, list(y = list(measure = "IL-6", col = "ARM"))), "names both")
  expect_error(Core_Frame(Synthetic_Results, NULL, list(y = lY), list(requires = "y")), "`requires` is not a setting")
  expect_error(Core_Frame(Synthetic_Results, NULL, list(y = lY), list(required = "z")), "`required` names `z`, which is not one of the variables")
  expect_error(Core_Frame(Synthetic_Results, NULL, list(y = lY), list(baseline_stat = "median")), "`baseline_stat` must be one of")
  # A biomarker or a visit no row has is not refused: everyone is counted.
  lNone <- Core_Frame(Synthetic_Results, Synthetic_Participants, list(y = list(measure = "IL-6", visit = "Week 99")))
  expect_identical(nrow(lNone$data), 0L)
  expect_identical(lNone$dropped$reason, "No result at the visit")
  expect_identical(lNone$dropped$n, nrow(Synthetic_Participants))
})

test_that("visits are ordered by the visit-order column, and by name with numbers as numbers without one (#9)", {
  expect_identical(Core_Visits(Synthetic_Results), c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12"))
  expect_identical(
    Core_Visits(Synthetic_Results, list(visit_order_col = NULL)),
    c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
  )
  # A visit with no usable result is not a visit.
  dfResults <- Synthetic_Results
  dfResults$STRESN[dfResults$VISIT == "Week 8"] <- NA
  expect_identical(Core_Visits(dfResults), c("Baseline", "Week 2", "Week 4", "Week 12"))
  expect_identical(Core_Visits(Synthetic_Results[0, ]), character(0))
  expect_identical(Core_Levels(c("b10", "b9", "", NA, "a", "B1", "b9")), c("a", "B1", "b9", "b10"))
})

test_that("a value is compared as the text the chart writes it as (#9)", {
  expect_identical(Core_Text(c("a", NA)), c("a", NA))
  expect_identical(Core_Text(factor(c("b", "a"))), c("b", "a"))
  expect_identical(Core_Text(c(1L, 20L, NA)), c("1", "20", NA))
  expect_identical(Core_Text(c(10, 2.5, 0.1 + 0.2, 1e5, -3, NA)), c("10", "2.5", "0.30000000000000004", "100000", "-3", NA))
  expect_identical(Core_Text(c(TRUE, FALSE, NA)), c("true", "false", NA))
  expect_identical(Core_IsBlank(c("a", "", "  ", NA)), c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(Core_IsBlank(c(1, NA, NaN)), c(FALSE, TRUE, TRUE))
  expect_identical(Core_Number(c(" 7 ", "<0.5", "", "1e1", "NA", "Inf", NA)), c(7, NA, NA, 10, NA, NA, NA))
  expect_identical(Core_Number(c(1.5, Inf, NA)), c(1.5, NA, NA))
})
