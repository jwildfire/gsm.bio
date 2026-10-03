# What the biomarker screen draws and asks R, worked out in R (#16). The widget
# stores R's answers ahead of time, so it has to know the frame the screen hands
# R and how it asks, and what the chart each row opens asks. The screen is held
# to bio.viz's own fixtures, copied with their record: the frames its chart
# wrote for views of its demo (ten when this was written; every case the
# fixture lists is run), the request the chart makes for each, and what desktop
# R answered.

strScreenFixture <- function(...) {
  testthat::test_path("fixtures", "bio.viz", ...)
}

dfScreenCases <- function() {
  utils::read.csv(
    strScreenFixture("screen-statistics", "cases.csv"),
    colClasses = "character", na.strings = "", check.names = FALSE
  )
}

lRecordedScreenRequests <- function() {
  lResults <- lReadJson(strScreenFixture("screen-statistics-r.json"))$results
  stats::setNames(lResults, vapply(lResults, function(lResult) lResult$case, character(1)))
}

# The tables and the settings of bio.viz's demo page for this chart
# (site/demo/biomarker-screen.js), without what only the page has.
lScreenDemo <- function() {
  lLabelled <- list(
    list(value_col = "ARM", label = "Arm"),
    list(value_col = "SEX", label = "Sex"),
    list(value_col = "RESPONSE", label = "Response")
  )
  lNumbers <- list(list(value_col = "AGE", label = "Age"), list(value_col = "BMIBL", label = "BMI at baseline"))
  list(
    results = Synthetic_Results,
    participants = Synthetic_Participants,
    settings = list(
      comparison = "difference",
      visit = "Week 4",
      value_type = "change",
      group_by = "ARM",
      baseline_visits = "Baseline",
      groups = lLabelled,
      numbers = lNumbers,
      filters = lLabelled,
      group_comparison = list(groups = lLabelled),
      association_scatter = list(groups = lLabelled, numbers = lNumbers)
    )
  )
}

# What a case says of its view, as the settings R reads; the filters, which no
# setting opens on in bio.viz's cases, are set as the controls would set them.
lScreenCaseView <- function(lCase) {
  One <- function(strValue) if (is.na(strValue)) NULL else strValue
  Several <- function(strValue) if (is.na(strValue)) NULL else strsplit(strValue, "|", fixed = TRUE)[[1]]
  lFilters <- list()
  if (!is.na(lCase$filters)) {
    for (chrPart in strsplit(strsplit(lCase$filters, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)) {
      lFilters[[chrPart[1]]] <- Several(chrPart[2])
    }
  }
  lTables <- lScreenDemo()
  lSettings <- lTables$settings
  lSettings$comparison <- lCase$comparison
  lSettings$value_type <- lCase$value_type
  lSettings$visit <- One(lCase$visit)
  lSettings$baseline_visits <- Several(lCase$baseline_visits)
  lSettings$baseline_stat <- lCase$baseline_stat
  lSettings$method <- lCase$method
  lSettings$adjustment <- lCase$adjustment
  if (lCase$comparison == "difference") {
    lSettings$group_by <- lCase$group_by
    lSettings$levels <- Several(lCase$groups)
  } else if (!is.na(lCase$with_col)) {
    lSettings$with <- list(col = lCase$with_col)
  } else {
    lSettings$with <- list(measure = lCase$with_measure, value = lCase$with_value, visit = One(lCase$with_visit))
  }
  lConfig <- BiomarkerScreen_Settings(lSettings)
  lState <- BiomarkerScreen_State(lTables$results, lTables$participants, lConfig)
  lState$filters <- lFilters
  list(results = lTables$results, participants = lTables$participants, config = lConfig, state = lState)
}

lScreenCaseRequest <- function(lCase) {
  lView <- lScreenCaseView(lCase)
  lRequests <- BiomarkerScreen_Requests(lView$results, lView$participants, lView$config, lView$state)
  expect_identical(length(lRequests), 1L, label = paste(lCase$case, "asks once"))
  lRequests[[1]]
}

test_that("the settings R reads have the defaults of the vendored biomarker screen (#16)", {
  lBundle <- lBundleDefaults("association_scatter")
  expect_false(is.null(lBundle), label = "the bundle's chart defaults were found")
  expect_true(all(names(lBiomarkerScreenDefaults) %in% names(lBundle)))
  for (strSetting in names(lBiomarkerScreenDefaults)) {
    expect_identical(
      lBiomarkerScreenDefaults[[strSetting]], lBundle[[strSetting]],
      label = paste("R's default for", strSetting), expected.label = "the bundle's"
    )
  }
  expect_true("connection" %in% names(lBundle))
  expect_identical(lBundle$statistic, strBiomarkerScreenStatistic)
  expect_identical(BiomarkerScreen_Settings()[names(lBiomarkerScreenDefaults)], lBiomarkerScreenDefaults)
  expect_identical(lCoreDefaults, lBiomarkerScreenDefaults[names(lCoreDefaults)])
})

test_that("the settings R reads are refused as the chart refuses them (#16)", {
  expect_error(BiomarkerScreen_Settings(list(comparison = "ratio")), "comparison")
  expect_error(BiomarkerScreen_Settings(list(adjustment = "bonferroni")), "adjustment")
  expect_error(BiomarkerScreen_Settings(list(method = "kendall")), "method")
  expect_error(BiomarkerScreen_Settings(list(levels = c("A", "B", "C"))), "levels")
  expect_error(BiomarkerScreen_Settings(list(statistic = "Analyze_Other")), "statistic")
  expect_error(BiomarkerScreen_Settings(list(group_comparison = "t")), "group_comparison")
  expect_error(BiomarkerScreen_Settings(list(with = "IL-10")), "with")
  expect_null(BiomarkerScreen_Settings(list(statistic = NULL))$statistic)
  expect_error(BiomarkerScreen_Settings(list(with = list(measure = "IL-10", value = "baseline", visit = "Week 4"))), "with")
  expect_identical(
    BiomarkerScreen_Settings(list(with = list(measure = "IL-10", value = "baseline")))$with,
    list(measure = "IL-10", value = "baseline")
  )
})

test_that("R resolves the frame bio.viz's screen hands R, gaps kept, for every view of its demo bio.viz recorded (#16)", {
  dfCases <- dfScreenCases()
  expect_gte(nrow(dfCases), 10L)
  expect_identical(anyDuplicated(dfCases$case), 0L)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    dfTheirs <- utils::read.csv(
      strScreenFixture("screen-statistics", lCase$file),
      colClasses = "character", na.strings = "", check.names = FALSE
    )
    dfMine <- lScreenCaseRequest(lCase)$data

    expect_identical(names(dfMine), names(dfTheirs), label = paste(lCase$case, "columns"))
    expect_identical(nrow(dfMine), nrow(dfTheirs), label = paste(lCase$case, "rows"))
    expect_identical(Core_Text(dfMine$USUBJID), dfTheirs$USUBJID, label = paste(lCase$case, "participants"))
    strExtra <- names(dfTheirs)[ncol(dfTheirs)]
    for (strColumn in setdiff(names(dfTheirs), "USUBJID")) {
      if (lCase$comparison == "difference" && strColumn == strExtra) {
        expect_identical(Core_Text(dfMine[[strColumn]]), dfTheirs[[strColumn]], label = paste(lCase$case, strColumn))
      } else {
        # A gap is NA here and an empty cell there.
        expect_equal(dfMine[[strColumn]], as.numeric(dfTheirs[[strColumn]]), tolerance = 1e-12, label = paste(lCase$case, strColumn))
      }
    }
  }
  # The cases reach both comparisons, a change, a baseline visit, a filter,
  # both adjustments, both coefficients, a biomarker and a number to correlate
  # with, and frames with gaps.
  expect_setequal(dfCases$comparison, c("difference", "correlation"))
  expect_true(all(c("change", "raw") %in% dfCases$value_type))
  expect_true(any(!is.na(dfCases$filters)))
  expect_setequal(dfCases$adjustment, c("BH", "holm"))
  expect_setequal(dfCases$method, c("pearson", "spearman"))
  expect_true(any(!is.na(dfCases$with_col)) && any(!is.na(dfCases$with_measure)))
  bGaps <- vapply(dfCases$file, function(strFile) {
    anyNA(utils::read.csv(strScreenFixture("screen-statistics", strFile), colClasses = "character", na.strings = ""))
  }, logical(1))
  expect_true(any(bGaps))
})

test_that("R keys the screen's stored result exactly as the chart keys its request (#16)", {
  dfCases <- dfScreenCases()
  lRecorded <- lRecordedScreenRequests()
  expect_setequal(names(lRecorded), dfCases$case)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lMine <- lScreenCaseRequest(lCase)
    lTheirs <- lRecorded[[lCase$case]]
    expect_identical(lMine$name, lTheirs$name, label = paste(lCase$case, "name"))
    expect_identical(lMine$args, lTheirs$args, label = paste(lCase$case, "args"))
    expect_identical(lMine$dataId, lTheirs$dataId, label = paste(lCase$case, "dataId"))
    expect_identical(lMine$rows, lTheirs$rows, label = paste(lCase$case, "rows"))
    expect_identical(
      as.character(jsonlite::toJSON(lMine[c("name", "args", "dataId", "rows")], auto_unbox = TRUE)),
      as.character(jsonlite::toJSON(lTheirs[c("name", "args", "dataId", "rows")], auto_unbox = TRUE)),
      label = paste(lCase$case, "as JSON")
    )
  }
  chrKeys <- vapply(lRecorded, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  expect_identical(anyDuplicated(chrKeys), 0L)
})

test_that("R's answers for those screens are the answers bio.viz recorded from desktop R, row by row (#16)", {
  dfCases <- dfScreenCases()
  lRecorded <- lRecordedScreenRequests()
  strRecordedR <- lReadJson(strScreenFixture("screen-statistics-r.json"))$made_by$r_version
  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lRequest <- lScreenCaseRequest(lCase)
    lMine <- do.call(Analyze_Screen, c(list(lRequest$data), lRequest$args))
    lTheirs <- lRecorded[[lCase$case]]$value

    ExpectResultShape(lMine)
    expect_identical(lMine$status, lTheirs$status, label = paste(lCase$case, "status"))
    expect_identical(lMine$counts, lTheirs$counts, label = paste(lCase$case, "counts"))
    expect_identical(nrow(lMine$rows), length(lTheirs$rows), label = paste(lCase$case, "rows"))
    Column <- function(strColumn, xType) {
      vapply(lTheirs$rows, function(lRow) if (is.null(lRow[[strColumn]])) methods::as(NA, class(xType)) else lRow[[strColumn]], xType)
    }
    for (strColumn in c("biomarker", "method", "adjustment", "status", "reason")) {
      expect_identical(lMine$rows[[strColumn]], Column(strColumn, character(1)), label = paste(lCase$case, strColumn))
    }
    # A warning is in R's own words, which change between R versions (R 4.6
    # capitalises cor.test()'s "Cannot compute exact p-value with ties"): the
    # same rows warn whatever the R, and in the same words under the R that
    # recorded them.
    expect_identical(is.na(lMine$rows$warning), is.na(Column("warning", character(1))), label = paste(lCase$case, "rows that warn"))
    if (identical(as.character(getRversion()), strRecordedR)) {
      expect_identical(lMine$rows$warning, Column("warning", character(1)), label = paste(lCase$case, "warning"))
    }
    for (strColumn in c("counts", "n_1", "n_2", "dropped", "adjusted_over")) {
      expect_identical(lMine$rows[[strColumn]], Column(strColumn, integer(1)), label = paste(lCase$case, strColumn))
    }
    for (strColumn in c("estimate", "lower", "upper", "level", "statistic", "p_unadjusted", "p_value")) {
      expect_equal(lMine$rows[[strColumn]], Column(strColumn, numeric(1)), tolerance = 1e-8, label = paste(lCase$case, strColumn))
    }
  }
})

test_that("the screen opens on the settings, where the tables have what they name (#16)", {
  lTables <- lScreenDemo()
  State <- function(lMore) {
    lConfig <- BiomarkerScreen_Settings(c(lTables$settings[setdiff(names(lTables$settings), names(lMore))], lMore))
    BiomarkerScreen_State(lTables$results, lTables$participants, lConfig)
  }
  lOpening <- State(list())
  expect_identical(lOpening$visit, "Week 4")
  expect_identical(lOpening$group_by, "ARM")
  expect_identical(lOpening$levels, c("Placebo", "Treatment"))
  # The variable to correlate with falls back to the first number offered.
  expect_identical(lOpening$with, list(col = "AGE"))
  # A visit, a column or groups the tables do not have fall back.
  expect_identical(State(list(visit = "Week 99"))$visit, "Baseline")
  expect_identical(State(list(group_by = "SITE"))$group_by, "ARM")
  expect_identical(State(list(levels = c("Treatment", "Placebo")))$levels, c("Treatment", "Placebo"))
  expect_identical(State(list(levels = c("Treatment", "Other")))$levels, c("Placebo", "Treatment"))
  expect_identical(State(list(with = list(measure = "IL-99", value = "raw", visit = "Baseline")))$with, list(col = "AGE"))
  expect_identical(State(list(with = list(measure = "IL-10", value = "baseline")))$with, list(measure = "IL-10", value = "baseline"))
  # With no numbers, the first biomarker at the first visit.
  expect_identical(
    State(list(numbers = list("STUDYID")))$with,
    list(col = "STUDYID")
  )
  dfNoNumbers <- lTables$participants[c("USUBJID", "ARM", "SEX", "RESPONSE")]
  lConfig <- BiomarkerScreen_Settings(lTables$settings[setdiff(names(lTables$settings), "numbers")])
  expect_identical(
    BiomarkerScreen_State(lTables$results, dfNoNumbers, lConfig)$with,
    list(measure = "CRP", value = "raw", visit = "Baseline")
  )
})

test_that("the screen has no rows, and asks nothing, where the chart draws none (#16)", {
  lTables <- lScreenDemo()
  Requests <- function(lMore, dfParticipants = lTables$participants) {
    lConfig <- BiomarkerScreen_Settings(c(lTables$settings[setdiff(names(lTables$settings), names(lMore))], lMore))
    BiomarkerScreen_Requests(lTables$results, dfParticipants, lConfig, BiomarkerScreen_State(lTables$results, dfParticipants, lConfig))
  }
  expect_length(Requests(list()), 1L)
  # A change at the one baseline visit is the same for everyone.
  expect_length(Requests(list(visit = "Baseline")), 0L)
  # No statistic, no request.
  expect_length(Requests(list(statistic = NULL)), 0L)
  # A difference needs a column of two groups.
  dfOneArm <- lTables$participants
  dfOneArm$ARM <- "Placebo"
  dfOneArm$SEX <- "F"
  dfOneArm$RESPONSE <- "Responder"
  expect_length(Requests(list(), dfOneArm), 0L)
  # A filter nobody passes leaves nothing to screen.
  lConfig <- BiomarkerScreen_Settings(lTables$settings)
  lState <- BiomarkerScreen_State(lTables$results, lTables$participants, lConfig)
  lState$filters$SEX <- "X"
  expect_length(BiomarkerScreen_Requests(lTables$results, lTables$participants, lConfig, lState), 0L)
  # A correlation with a biomarker leaves that biomarker out of the rows.
  lRequest <- Requests(list(comparison = "correlation", value_type = "raw", with = list(measure = "IL-6", value = "raw", visit = "Week 4")))[[1]]
  expect_false("IL-6" %in% unlist(lRequest$args$chrCols))
  expect_identical(lRequest$args$strWithCol, "IL-6 at Week 4")
  expect_identical(length(lRequest$args$chrCols), 11L)
  # A variable named like a biomarker is a frame with two columns of one name.
  dfClash <- lTables$participants
  dfClash$CRP <- dfClash$AGE
  expect_length(Requests(list(comparison = "correlation", numbers = list("CRP"), with = list(col = "CRP")), dfClash), 0L)
  # A baseline value is not read at a visit.
  lBaseline <- Requests(list(value_type = "baseline"))[[1]]
  expect_null(lBaseline$dataId$visit)
  expect_identical(lBaseline$dataId$value_type, "baseline")
})

test_that("a variable is named in the frame as the chart names it (#16)", {
  expect_identical(BiomarkerScreen_VariableName(list(col = "AGE")), "AGE")
  expect_identical(BiomarkerScreen_VariableName(list(measure = "IL-10", value = "baseline")), "IL-10, baseline value")
  expect_identical(BiomarkerScreen_VariableName(list(measure = "IL-10", value = "raw", visit = "Week 4")), "IL-10 at Week 4")
  expect_identical(
    BiomarkerScreen_VariableName(list(measure = "IL-10", value = "percent_change", visit = "Week 4")),
    "IL-10, percent change from baseline at Week 4"
  )
})

test_that("a row of a difference opens the group comparison on its biomarker, one visit, the two groups and Welch's test (#16)", {
  lTables <- lScreenDemo()
  lSettings <- lTables$settings
  lSettings$filters <- list(list(value_col = "SEX", start = "F"), "ARM")
  lSettings$group_comparison <- list(groups = lTables$settings$groups, y_scale = "log")
  lConfig <- BiomarkerScreen_Settings(lSettings)
  lState <- BiomarkerScreen_State(lTables$results, lTables$participants, lConfig)
  lSpecs <- Chart_FilterSpecs(lTables$results, lTables$participants, lConfig)
  lHanded <- BiomarkerScreen_OpenedSettings(lConfig, lState, lSpecs, "IL-6", list(measure = "IL-6", value = "change", visit = "Week 4"))

  expect_identical(lHanded$start_value, "IL-6")
  expect_identical(lHanded$visits, "Week 4")
  expect_identical(lHanded$value_type, "change")
  expect_identical(lHanded$group_by, "ARM")
  expect_identical(lHanded$levels, c("Placebo", "Treatment"))
  # The screen's test is Welch's, and the page cannot set another; the page's
  # other settings for the chart are kept.
  expect_identical(lHanded$test, "t")
  expect_error(BiomarkerScreen_Settings(list(group_comparison = list(test = "wilcoxon"))), "'test'")
  expect_error(BiomarkerScreen_Settings(list(group_comparison = list(levels = c("A", "B"), back = NULL))), "'levels', 'back'")
  expect_identical(lHanded$y_scale, "log")
  expect_identical(lHanded$baseline_visits, "Baseline")
  expect_identical(vapply(lHanded$filters, function(lSpec) lSpec$value_col, character(1)), c("SEX", "ARM"))
  expect_identical(lHanded$filters[[1]]$start, "F")
  expect_true(lHanded$filters[[1]]$all)
  expect_null(lHanded$filters[[2]]$start)

  # The chart it opens asks for one test: that biomarker, at that visit, the
  # two groups, Welch's, on the screen's participants.
  lRequests <- BiomarkerScreen_OpenedRequests(
    lTables$results, lTables$participants, lConfig, lState, lSpecs, "IL-6", list(measure = "IL-6", value = "change", visit = "Week 4")
  )
  expect_length(lRequests, 1L)
  expect_identical(lRequests[[1]]$name, "Analyze_GroupDifference")
  expect_identical(lRequests[[1]]$args$strMethod, "t")
  expect_identical(lRequests[[1]]$dataId$measure, "IL-6")
  expect_identical(lRequests[[1]]$dataId$visit, "Week 4")
  expect_identical(lRequests[[1]]$dataId$filters, list(SEX = list("F")))
  expect_identical(lRequests[[1]]$dataId$groups, list("Placebo", "Treatment"))
  expect_setequal(Core_Text(lRequests[[1]]$data$x), c("Placebo", "Treatment"))
  expect_identical(lRequests[[1]]$rows, nrow(lRequests[[1]]$data))

  # A baseline value opens on no visit, and the chart draws its baseline panel.
  lBaselineState <- lState
  lBaselineState$value_type <- "baseline"
  lBaseline <- BiomarkerScreen_OpenedSettings(lConfig, lBaselineState, lSpecs, "IL-6", list(measure = "IL-6", value = "baseline"))
  expect_true("visits" %in% names(lBaseline))
  expect_null(lBaseline$visits)
})

test_that("a row of a correlation opens the scatter with the biomarker along the bottom and the variable up the side (#16)", {
  lTables <- lScreenDemo()
  lSettings <- lTables$settings
  lSettings$comparison <- "correlation"
  lSettings$value_type <- "raw"
  lSettings$visit <- "Baseline"
  lSettings$method <- "spearman"
  lSettings$with <- list(measure = "IL-10", value = "raw", visit = "Baseline")
  lSettings$association_scatter <- list(groups = lTables$settings$groups, fit = "linear")
  lConfig <- BiomarkerScreen_Settings(lSettings)
  lState <- BiomarkerScreen_State(lTables$results, lTables$participants, lConfig)
  lSpecs <- Chart_FilterSpecs(lTables$results, lTables$participants, lConfig)
  lAxis <- list(measure = "TNF-alpha", value = "raw", visit = "Baseline")
  lHanded <- BiomarkerScreen_OpenedSettings(lConfig, lState, lSpecs, "TNF-alpha", lAxis)

  # What the screen carries across is over what the page set for the scatter.
  expect_identical(lHanded$x, lAxis)
  expect_identical(lHanded$y, list(measure = "IL-10", value = "raw", visit = "Baseline"))
  expect_identical(lHanded$method, "spearman")
  expect_identical(lHanded$fit, "linear")
  expect_identical(lHanded$numbers, lConfig$numbers)
  expect_error(BiomarkerScreen_Settings(list(association_scatter = list(x = list(col = "AGE"), method = "pearson"))), "'x', 'method'")

  lRequests <- BiomarkerScreen_OpenedRequests(lTables$results, lTables$participants, lConfig, lState, lSpecs, "TNF-alpha", lAxis)
  expect_identical(vapply(lRequests, function(lRequest) lRequest$name, character(1)), c("Analyze_Correlation", "Analyze_Fit"))
  expect_identical(lRequests[[1]]$args, list(strXCol = "x", strYCol = "y", strMethod = "spearman"))
  expect_identical(lRequests[[1]]$dataId$x, lAxis)
  expect_identical(lRequests[[1]]$dataId$y, lHanded$y)
})

test_that("the stored results are the screen's answer and, for each row, what the chart it opens asks (#16)", {
  lTables <- lScreenDemo()
  lConfig <- BiomarkerScreen_Settings(lTables$settings)
  lStored <- BiomarkerScreen_StoredResults(lTables$results, lTables$participants, lConfig)
  chrNames <- vapply(lStored, function(lResult) lResult$name, character(1))
  expect_identical(chrNames, c("Analyze_Screen", rep("Analyze_GroupDifference", 12L)))
  expect_identical(
    vapply(lStored[-1], function(lResult) lResult$dataId$measure, character(1)),
    unlist(lStored[[1]]$args$chrCols)
  )
  # Each row's own test agrees with the screen's row for it: the same
  # participants, and the same Welch p-value before adjustment.
  dfRows <- lStored[[1]]$value$rows
  for (lResult in lStored[-1]) {
    iRow <- match(lResult$dataId$measure, dfRows$biomarker)
    expect_identical(sum(unlist(lResult$value$counts)), dfRows$counts[iRow])
    expect_equal(lResult$value$p_value, dfRows$p_unadjusted[iRow], tolerance = 1e-12)
  }

  lCorrelation <- BiomarkerScreen_Settings(c(
    lTables$settings[setdiff(names(lTables$settings), c("comparison", "with"))],
    list(comparison = "correlation", with = list(measure = "IL-10", value = "change", visit = "Week 4"))
  ))
  lStored <- BiomarkerScreen_StoredResults(lTables$results, lTables$participants, lCorrelation)
  expect_identical(vapply(lStored, function(lResult) lResult$name, character(1)), c("Analyze_Screen", rep("Analyze_Correlation", 11L)))
  dfRows <- lStored[[1]]$value$rows
  for (lResult in lStored[-1]) {
    expect_identical(lResult$dataId$y, list(measure = "IL-10", value = "change", visit = "Week 4"))
    iRow <- match(lResult$dataId$x$measure, dfRows$biomarker)
    expect_equal(lResult$value$estimates$estimate, dfRows$estimate[iRow], tolerance = 1e-12)
    expect_equal(lResult$value$p_value, dfRows$p_unadjusted[iRow], tolerance = 1e-12)
  }

  # No screen, nothing stored.
  expect_identical(BiomarkerScreen_StoredResults(lTables$results[0, ], lTables$participants, lConfig), list())
  expect_identical(
    BiomarkerScreen_StoredResults(lTables$results, lTables$participants, BiomarkerScreen_Settings(c(lTables$settings, list(statistic = NULL)))),
    list()
  )
})
