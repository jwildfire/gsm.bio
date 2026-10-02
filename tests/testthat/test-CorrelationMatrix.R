# What the correlation matrix draws and asks R, worked out in R (#13). The
# widget stores R's answers ahead of time, so it has to know the frame the
# chart hands R for its grid and how it asks, and what each cell's scatter asks.
# The grid is held to bio.viz's own fixtures, copied with their record: the
# frames its core wrote for views of its demo (ten when this was written; every
# case the fixture lists is run), and the request the chart makes for each.

strMatrixFixture <- function(...) {
  testthat::test_path("fixtures", "bio.viz", ...)
}

dfMatrixCases <- function() {
  utils::read.csv(
    strMatrixFixture("matrix-statistics", "cases.csv"),
    colClasses = "character", na.strings = "", check.names = FALSE
  )
}

lRecordedMatrixRequests <- function() {
  lResults <- lReadJson(strMatrixFixture("matrix-statistics-r.json"))$results
  stats::setNames(lResults, vapply(lResults, function(lResult) lResult$case, character(1)))
}

# The tables and the settings of bio.viz's demo page for this chart.
lMatrixDemo <- function() {
  lLabelled <- list(
    list(value_col = "ARM", label = "Arm"),
    list(value_col = "SEX", label = "Sex"),
    list(value_col = "RESPONSE", label = "Response")
  )
  list(
    results = Synthetic_Results,
    participants = Synthetic_Participants,
    settings = list(
      baseline_visits = "Baseline",
      filters = lLabelled,
      scatter = list(
        groups = lLabelled,
        numbers = list(list(value_col = "AGE", label = "Age"), list(value_col = "BMIBL", label = "BMI at baseline"))
      )
    )
  )
}

# What a case says of its view, as the settings and the controls R reads.
lMatrixCaseView <- function(lCase) {
  One <- function(strValue) if (is.na(strValue)) NULL else strValue
  Several <- function(strValue) if (is.na(strValue)) NULL else strsplit(strValue, "|", fixed = TRUE)[[1]]
  lFilters <- list()
  if (!is.na(lCase$filters)) {
    for (chrPart in strsplit(strsplit(lCase$filters, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)) {
      lFilters[[chrPart[1]]] <- Several(chrPart[2])
    }
  }
  lTables <- lMatrixDemo()
  lSettings <- lTables$settings
  lSettings$baseline_visits <- Several(lCase$baseline_visits)
  lSettings$baseline_stat <- lCase$baseline_stat
  lSettings$mode <- lCase$mode
  lSettings$value_type <- lCase$value_type
  lSettings$method <- lCase$method
  if (!is.na(lCase$min_pairs)) lSettings$min_pairs <- as.numeric(lCase$min_pairs)
  if (lCase$mode == "biomarkers") {
    lSettings$visit <- One(lCase$visit)
    lSettings$biomarkers <- Several(lCase$variables)
  } else {
    lSettings$measure <- lCase$measure
    lSettings$visits <- Several(lCase$variables)
  }
  lConfig <- CorrelationMatrix_Settings(lSettings)
  lState <- CorrelationMatrix_State(lTables$results, lTables$participants, lConfig)
  lState$filters <- lFilters
  list(results = lTables$results, participants = lTables$participants, config = lConfig, state = lState)
}

lMatrixCaseRequest <- function(lCase) {
  lView <- lMatrixCaseView(lCase)
  lRequests <- CorrelationMatrix_Requests(lView$results, lView$participants, lView$config, lView$state)
  expect_identical(length(lRequests), 1L, label = paste(lCase$case, "asks once"))
  lRequests[[1]]
}

test_that("the settings R reads have the defaults of the vendored correlation matrix (#13)", {
  lBundle <- lBundleDefaults("min_pairs")
  expect_false(is.null(lBundle), label = "the bundle's chart defaults were found")
  expect_true(all(names(lCorrelationMatrixDefaults) %in% names(lBundle)))
  for (strSetting in names(lCorrelationMatrixDefaults)) {
    expect_identical(
      lCorrelationMatrixDefaults[[strSetting]], lBundle[[strSetting]],
      label = paste("R's default for", strSetting), expected.label = "the bundle's"
    )
  }
  expect_true("connection" %in% names(lBundle))
  expect_identical(lBundle$statistic, strCorrelationMatrixStatistic)
  expect_identical(CorrelationMatrix_Settings()[names(lCorrelationMatrixDefaults)], lCorrelationMatrixDefaults)
  expect_identical(lCoreDefaults, lCorrelationMatrixDefaults[names(lCoreDefaults)])
})

test_that("R resolves the frame bio.viz's grid hands R, gaps kept, for every view of its demo bio.viz recorded (#13)", {
  dfCases <- dfMatrixCases()
  expect_gte(nrow(dfCases), 10L)
  expect_identical(anyDuplicated(dfCases$case), 0L)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    dfTheirs <- utils::read.csv(
      strMatrixFixture("matrix-statistics", lCase$file),
      colClasses = "character", na.strings = "", check.names = FALSE
    )
    dfMine <- lMatrixCaseRequest(lCase)$data

    expect_identical(names(dfMine), names(dfTheirs), label = paste(lCase$case, "columns"))
    expect_identical(nrow(dfMine), nrow(dfTheirs), label = paste(lCase$case, "rows"))
    expect_identical(Core_Text(dfMine$USUBJID), dfTheirs$USUBJID, label = paste(lCase$case, "participants"))
    for (strColumn in setdiff(names(dfTheirs), "USUBJID")) {
      # A gap is NA here and an empty cell there.
      expect_equal(dfMine[[strColumn]], as.numeric(dfTheirs[[strColumn]]), tolerance = 1e-12, label = paste(lCase$case, strColumn))
    }
  }
  # The cases reach both modes, a change, a filter, a minimum and both
  # coefficients, and at least one frame with gaps.
  expect_setequal(dfCases$mode, c("biomarkers", "visits"))
  expect_true(all(c("change", "raw") %in% dfCases$value_type))
  expect_true(any(!is.na(dfCases$filters)) && any(!is.na(dfCases$min_pairs)))
  expect_setequal(dfCases$method, c("pearson", "spearman"))
  bGaps <- vapply(seq_len(nrow(dfCases)), function(iCase) {
    dfTheirs <- utils::read.csv(strMatrixFixture("matrix-statistics", dfCases$file[iCase]), colClasses = "character", na.strings = "")
    anyNA(dfTheirs)
  }, logical(1))
  expect_true(any(bGaps))
})

test_that("R keys the grid's stored result exactly as the chart keys its request, with a minimum only when one is set (#13)", {
  dfCases <- dfMatrixCases()
  lRecorded <- lRecordedMatrixRequests()
  expect_setequal(names(lRecorded), dfCases$case)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lMine <- lMatrixCaseRequest(lCase)
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
    expect_identical("nMinPairs" %in% names(lMine$args), !is.na(lCase$min_pairs), label = paste(lCase$case, "minimum sent"))
  }
  chrKeys <- vapply(lRecorded, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  expect_identical(anyDuplicated(chrKeys), 0L)
})

test_that("R's answers for those grids are the answers bio.viz recorded from desktop R, and hold no p-value (#13)", {
  dfCases <- dfMatrixCases()
  lRecorded <- lRecordedMatrixRequests()
  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lRequest <- lMatrixCaseRequest(lCase)
    lMine <- do.call(Analyze_CorrelationMatrix, c(list(lRequest$data), lRequest$args))
    lTheirs <- lRecorded[[lCase$case]]$value

    ExpectResultShape(lMine)
    expect_identical(lMine$status, lTheirs$status, label = paste(lCase$case, "status"))
    expect_identical(lMine$counts, lTheirs$counts, label = paste(lCase$case, "counts"))
    Column <- function(strColumn) {
      vapply(lTheirs$rows, function(lRow) if (is.null(lRow[[strColumn]])) NA else lRow[[strColumn]], if (strColumn %in% c("x", "y", "status")) character(1) else numeric(1))
    }
    expect_identical(lMine$rows$x, Column("x"), label = paste(lCase$case, "pairs"))
    expect_identical(lMine$rows$y, Column("y"), label = paste(lCase$case, "pairs"))
    expect_identical(lMine$rows$counts, as.integer(Column("counts")), label = paste(lCase$case, "pair counts"))
    expect_identical(lMine$rows$status, Column("status"), label = paste(lCase$case, "pair status"))
    for (strColumn in c("estimate", "lower", "upper")) {
      expect_equal(lMine$rows[[strColumn]], Column(strColumn), tolerance = 1e-8, label = paste(lCase$case, strColumn))
    }
    # A grid prints no p-value: R returns none.
    expect_true(is.na(lMine$p_value))
    expect_false(any(grepl("^p_", names(lMine$rows))))
  }
  # The pair counts differ from cell to cell somewhere: the gaps are kept.
  lWeek12 <- lMatrixCaseRequest(as.list(dfCases[dfCases$case == "week-12-minimum-183", ]))
  lAnswer <- do.call(Analyze_CorrelationMatrix, c(list(lWeek12$data), lWeek12$args))
  expect_gt(length(unique(lAnswer$rows$counts)), 1L)
  expect_true("too_small" %in% lAnswer$rows$status)
})

test_that("the grid's variables follow its mode, its value and its limit (#13)", {
  lTables <- lMatrixDemo()
  Variables <- function(lMore) {
    lConfig <- CorrelationMatrix_Settings(c(lTables$settings, lMore))
    lState <- CorrelationMatrix_State(lTables$results, lTables$participants, lConfig)
    CorrelationMatrix_Variables(lTables$results, lConfig, lState)
  }
  Said <- function(lVariables) vapply(lVariables, function(lAxis) paste(lAxis$measure, lAxis$value, if (is.null(lAxis$visit)) "-" else lAxis$visit), character(1))

  # Across biomarkers: every biomarker the control offers, at the first visit.
  expect_identical(length(Variables(list())), 12L)
  expect_identical(Said(Variables(list()))[1:2], c("CRP raw Baseline", "D-dimer raw Baseline"))
  expect_identical(Said(Variables(list(biomarkers = c("IL-6", "CRP"), visit = "Week 4"))), c("CRP raw Week 4", "IL-6 raw Week 4"))
  # A baseline value has no visit; a change at the baseline visit is no grid.
  expect_identical(Said(Variables(list(value_type = "baseline", biomarkers = c("IL-6", "CRP")))), c("CRP baseline -", "IL-6 baseline -"))
  expect_identical(Variables(list(value_type = "change")), list())
  expect_identical(length(Variables(list(value_type = "change", visit = "Week 4"))), 12L)
  # The limit draws the first of those chosen; fewer than two is no grid.
  expect_identical(length(Variables(list(limit = 5))), 5L)
  expect_identical(Variables(list(biomarkers = "IL-6")), list())
  # Across visits: every visit, and for a change not the baseline visit.
  expect_identical(Said(Variables(list(mode = "visits", measure = "IL-6"))), paste("IL-6 raw", c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")))
  expect_identical(Said(Variables(list(mode = "visits", measure = "IL-6", value_type = "change"))), paste("IL-6 change", c("Week 2", "Week 4", "Week 8", "Week 12")))
  expect_identical(Variables(list(mode = "visits", value_type = "baseline")), list())
  expect_identical(Said(Variables(list(mode = "visits")))[1], "CRP raw Baseline")
})

test_that("a cell's scatter is handed the pair, the method, the filters and the page's scatter settings (#13)", {
  lTables <- lMatrixDemo()
  lSettings <- lTables$settings
  lSettings$method <- "spearman"
  lSettings$filters <- list(list(value_col = "SEX", start = "F"), "ARM")
  lConfig <- CorrelationMatrix_Settings(lSettings)
  lState <- CorrelationMatrix_State(lTables$results, lTables$participants, lConfig)
  lSpecs <- CorrelationMatrix_FilterSpecs(lTables$results, lTables$participants, lConfig)
  lX <- list(measure = "TNF-alpha", value = "raw", visit = "Baseline")
  lY <- list(measure = "IL-10", value = "raw", visit = "Baseline")
  lHanded <- CorrelationMatrix_ScatterSettings(lConfig, lState, lSpecs, lX, lY)

  expect_identical(lHanded$x, lX)
  expect_identical(lHanded$y, lY)
  expect_identical(lHanded$method, "spearman")
  expect_identical(lHanded$baseline_visits, "Baseline")
  expect_identical(lHanded$groups, lTables$settings$scatter$groups)
  expect_identical(vapply(lHanded$filters, function(lSpec) lSpec$value_col, character(1)), c("SEX", "ARM"))
  expect_identical(lHanded$filters[[1]]$start, "F")
  expect_null(lHanded$filters[[2]]$start)

  # The scatter it opens asks for that pair's coefficient, on the grid's
  # participants, under the grid's filters, and for nothing else.
  lScatter <- AssociationScatter_Settings(lHanded)
  lRequests <- AssociationScatter_Requests(
    lTables$results, lTables$participants, lScatter,
    AssociationScatter_State(lTables$results, lTables$participants, lScatter)
  )
  expect_identical(length(lRequests), 1L)
  expect_identical(lRequests[[1]]$args, list(strXCol = "x", strYCol = "y", strMethod = "spearman"))
  expect_identical(lRequests[[1]]$dataId$filters, list(SEX = list("F")))
  expect_identical(lRequests[[1]]$rows, sum(Synthetic_Participants$SEX == "F"))

  # A fitted line in the page's scatter settings is asked for too.
  lFit <- AssociationScatter_Settings(CorrelationMatrix_ScatterSettings(
    CorrelationMatrix_Settings(c(lSettings[setdiff(names(lSettings), "scatter")], list(scatter = list(fit = "linear", color_by = "ARM")))),
    lState, lSpecs, lX, lY
  ))
  expect_identical(
    vapply(AssociationScatter_Requests(lTables$results, lTables$participants, lFit, AssociationScatter_State(lTables$results, lTables$participants, lFit)), function(lRequest) lRequest$name, character(1)),
    c("Analyze_Correlation", "Analyze_Fit")
  )
})
