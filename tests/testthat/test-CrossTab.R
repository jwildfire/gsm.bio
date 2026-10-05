# What bio.viz's cross-tabulation draws and asks R, worked out in R (#18). The
# widget stores R's answer ahead of time, so it has to know the table the chart
# draws and how it asks for its test. Both are held to bio.viz's recorded
# cases, fixtures/bio.viz/cross-tab-r.json, written by bio.viz's
# tools/r-cross-tab.R (`cross_tab_key`) from the synthetic study, whose keys
# bio.viz's own tests hold to the chart's requests.

lCrossTabCases <- function() {
  lReadJson(testthat::test_path("fixtures", "bio.viz"), "cross-tab-r.json")$cases
}

# The tables and the settings of bio.viz's demo page for this chart.
lCrossTabDemo <- function() {
  lLabelled <- list(
    list(value_col = "ARM", label = "Arm"),
    list(value_col = "SEX", label = "Sex"),
    list(value_col = "RESPONSE", label = "Response")
  )
  list(
    results = Synthetic_Results,
    participants = Synthetic_Participants,
    settings = list(
      row_by = "ARM", col_by = "RESPONSE", percent = "row", test = "chisq", baseline_visits = "Baseline",
      groups = lLabelled, filters = lLabelled, cuts = list(list(measure = "CRP", visit = "Baseline", cut = "median"))
    )
  )
}

# A case's view: the demo's settings with the case's rows, columns and test,
# and its filters set as the controls would set them.
lCrossTabView <- function(lCase) {
  lTables <- lCrossTabDemo()
  lSettings <- lTables$settings
  # A case with tables of its own: those, as records, with no setting that
  # names the study's columns.
  if (!is.null(lCase$tables)) {
    lTables$results <- dfFromRecords(lCase$tables$results)
    lTables$participants <- dfFromRecords(lCase$tables$participants)
    lSettings <- list(baseline_visits = "Baseline")
  }
  lSettings$row_by <- lCase$dataId$row_by
  lSettings$col_by <- lCase$dataId$col_by
  lSettings$test <- lCase$args$strMethod
  lConfig <- CrossTab_Settings(lSettings)
  lState <- CrossTab_State(lTables$results, lTables$participants, lConfig)
  for (strColumn in names(lCase$dataId$filters)) {
    lState$filters[[strColumn]] <- unlist(lCase$dataId$filters[[strColumn]])
  }
  list(results = lTables$results, participants = lTables$participants, config = lConfig, state = lState)
}

test_that("the settings R reads have the defaults of the vendored cross-tabulation (#18)", {
  lBundle <- lBundleDefaults("row_by")
  expect_false(is.null(lBundle), label = "the bundle's chart defaults were found")
  expect_true(all(names(lCrossTabDefaults) %in% names(lBundle)))
  for (strSetting in names(lCrossTabDefaults)) {
    expect_identical(
      lCrossTabDefaults[[strSetting]], lBundle[[strSetting]],
      label = paste("R's default for", strSetting), expected.label = "the bundle's"
    )
  }
  expect_true("connection" %in% names(lBundle))
  expect_identical(lBundle$statistic, strCrossTabStatistic)
  expect_identical(CrossTab_Settings()[names(lCrossTabDefaults)], lCrossTabDefaults)
  expect_identical(lCoreDefaults, lCrossTabDefaults[names(lCoreDefaults)])
})

test_that("R draws the table bio.viz recorded for every case: its categories in order, counts and totals (#18)", {
  lCases <- lCrossTabCases()
  expect_gte(length(lCases), 9L)
  for (lCase in lCases) {
    lView <- lCrossTabView(lCase)
    lTable <- CrossTab_Table(lView$results, lView$participants, lView$config, lView$state)
    expect_identical(lTable$row_levels, unlist(lCase$row_levels), label = paste(lCase$case, "rows"))
    expect_identical(lTable$col_levels, unlist(lCase$col_levels), label = paste(lCase$case, "columns"))
    nCounts <- do.call(rbind, lapply(lCase$counts, function(lRow) as.integer(unlist(lRow))))
    expect_identical(unname(lTable$counts), nCounts, label = paste(lCase$case, "counts"))
    expect_identical(lTable$total, as.integer(lCase$total), label = paste(lCase$case, "total"))
    expect_identical(nrow(lTable$records), as.integer(lCase$rows), label = paste(lCase$case, "participants"))
  }
  # A column's categories are handed to R in the order the chart draws them,
  # gsm.bio's Core_Levels() by name with numbers as numbers, whatever the
  # locale (#44, bio.viz#79), and text that is only white space is no category.
  lStage <- Filter(function(lCase) identical(lCase$case, "stage-by-grade-chisq"), lCases)
  expect_length(lStage, 1L)
  expect_identical(unlist(lStage[[1]]$args$chrRowGroups), enc2utf8(c("week 1", "Week 2", "Week 10", "\u00d6dem")))
  expect_identical(unlist(lStage[[1]]$args$chrColGroups), c("a", "B"))
  expect_false(" " %in% unlist(lStage[[1]]$args$chrColGroups))
  lDose <- Filter(function(lCase) identical(lCase$case, "dose-by-response-fisher"), lCases)
  expect_identical(unlist(lDose[[1]]$args$chrRowGroups), c("2 mg", "10 mg"))
  # The cases reach a column each way, a cut at its median and at a typed
  # point, a change from baseline, a filter, and both tests.
  expect_true(any(vapply(lCases, function(lCase) is.list(lCase$dataId$col_by), logical(1))))
  expect_true(any(vapply(lCases, function(lCase) !is.null(lCase$dataId$filters), logical(1))))
  expect_true(any(vapply(lCases, function(lCase) !is.null(lCase$dataId$baseline_stat), logical(1))))
  expect_setequal(vapply(lCases, function(lCase) lCase$args$strMethod, character(1)), c("chisq", "fisher"))
})

test_that("R keys the table's stored result exactly as the chart keys its request (#18)", {
  for (lCase in lCrossTabCases()) {
    lView <- lCrossTabView(lCase)
    lRequests <- CrossTab_Requests(lView$results, lView$participants, lView$config, lView$state)
    expect_length(lRequests, 1L)
    lMine <- lRequests[[1]]
    expect_identical(lMine$name, lCase$name, label = paste(lCase$case, "name"))
    expect_identical(lMine$args, lCase$args, label = paste(lCase$case, "args"))
    expect_identical(lJsonRoundTrip(lMine$dataId), lCase$dataId, label = paste(lCase$case, "dataId"))
    expect_identical(lMine$rows, lCase$rows, label = paste(lCase$case, "rows"))
    expect_identical(
      as.character(jsonlite::toJSON(lMine[c("name", "args", "dataId", "rows")], auto_unbox = TRUE, digits = NA)),
      as.character(jsonlite::toJSON(lCase[c("name", "args", "dataId", "rows")], auto_unbox = TRUE, digits = NA)),
      label = paste(lCase$case, "as JSON")
    )
  }
})

test_that("R's answer for each table is the answer bio.viz recorded from desktop R (#18)", {
  for (lCase in lCrossTabCases()) {
    lView <- lCrossTabView(lCase)
    lRequest <- CrossTab_Requests(lView$results, lView$participants, lView$config, lView$state)[[1]]
    lMine <- do.call(Analyze_Contingency, c(list(lRequest$data), lRequest$args))
    lTheirs <- lCase$value
    ExpectResultShape(lMine)
    expect_identical(lMine$status, lTheirs$status, label = paste(lCase$case, "status"))
    # JSON writes NA as null.
    expect_identical(lMine$method, if (is.null(lTheirs$method)) NA_character_ else lTheirs$method, label = paste(lCase$case, "method"))
    expect_identical(lMine$reason, if (is.null(lTheirs$reason)) NA_character_ else lTheirs$reason, label = paste(lCase$case, "reason"))
    expect_identical(lMine$counts, lTheirs$counts, label = paste(lCase$case, "counts"))
    expect_equal(lMine$p_value, if (is.null(lTheirs$p_value)) NA_real_ else lTheirs$p_value, tolerance = 1e-8, label = paste(lCase$case, "p-value"))
    expect_equal(
      lMine$statistic$value, vapply(lTheirs$statistic, function(lRow) lRow$value, numeric(1)),
      tolerance = 1e-8, label = paste(lCase$case, "statistic")
    )
  }
})

test_that("the order bio.viz draws a column's categories in is Core_Levels(), on the sample R wrote for it (#44)", {
  # bio.viz's `categoryOrder` is held to this sample, written by its
  # tools/r-order.R from gsm.bio's own rule; R is held to it here.
  lOrder <- lReadJson(testthat::test_path("fixtures", "bio.viz"), "cross-tab-r.json")$category_order
  expect_gte(length(lOrder$given), 20L)
  expect_identical(Core_Levels(enc2utf8(unlist(lOrder$given))), enc2utf8(unlist(lOrder$sorted)))
})

test_that("an infinite odds ratio is stored as bio.viz's recorded answer writes it, the text Inf (#44)", {
  lCase <- Filter(function(lCase) identical(lCase$case, "empty-cell-fisher"), lCrossTabCases())[[1]]
  lView <- lCrossTabView(lCase)
  lRequest <- CrossTab_Requests(lView$results, lView$participants, lView$config, lView$state)[[1]]
  lMine <- do.call(Analyze_Contingency, c(list(lRequest$data), lRequest$args))
  expect_identical(lMine$estimates$estimate, Inf)
  # As the widget writes it to the page, and as bio.viz's tools/r-json.R wrote
  # the recorded answer.
  lStored <- jsonlite::fromJSON(
    as.character(jsonlite::toJSON(StoredValue(lMine), auto_unbox = TRUE, null = "null", na = "null", digits = NA)),
    simplifyVector = FALSE
  )
  lTheirs <- lCase$value$estimates[[1]]
  expect_identical(lStored$estimates[[1]][c("name", "estimate", "upper")], lTheirs[c("name", "estimate", "upper")])
  expect_identical(lTheirs$estimate, "Inf")
  expect_equal(lStored$estimates[[1]]$lower, lTheirs$lower, tolerance = 1e-10)
  expect_identical(round(lTheirs$lower, 2), 14.86)
})

test_that("the table opens on the settings, or the first two columns offered, and asks nothing where there is no test (#18)", {
  lTables <- lCrossTabDemo()
  State <- function(lMore) {
    lSettings <- lTables$settings
    for (strName in names(lMore)) lSettings[strName] <- list(lMore[[strName]])
    CrossTab_State(lTables$results, lTables$participants, CrossTab_Settings(lSettings))
  }
  Requests <- function(lMore, lFilters = NULL) {
    lSettings <- lTables$settings
    for (strName in names(lMore)) lSettings[strName] <- list(lMore[[strName]])
    lConfig <- CrossTab_Settings(lSettings)
    lState <- CrossTab_State(lTables$results, lTables$participants, lConfig)
    if (!is.null(lFilters)) lState$filters <- lFilters
    CrossTab_Requests(lTables$results, lTables$participants, lConfig, lState)
  }
  expect_identical(State(list(row_by = NULL, col_by = NULL))[c("row_by", "col_by")], list(row_by = "ARM", col_by = "SEX"))
  expect_identical(State(list(row_by = "NOPE", col_by = NULL))[c("row_by", "col_by")], list(row_by = "ARM", col_by = "SEX"))
  expect_identical(State(list(row_by = "SEX", col_by = NULL))$col_by, "ARM")
  expect_identical(
    State(list(col_by = list(measure = "CRP", visit = "Baseline", cut = "median")))$col_by,
    list(measure = "CRP", visit = "Baseline", value = "raw", cut = "median")
  )
  # No test, no statistic, a filter nobody passes, one category one way.
  expect_length(Requests(list()), 1L)
  expect_length(Requests(list(test = "none")), 0L)
  expect_length(Requests(list(statistic = NULL)), 0L)
  expect_length(Requests(list(), list(SEX = "X")), 0L)
  expect_length(Requests(list(col_by = "SEX"), list(SEX = "F")), 0L)
  # A change from baseline puts the baseline settings in the key; a result does not.
  expect_null(Requests(list(col_by = list(measure = "CRP", visit = "Baseline", cut = "median")))[[1]]$dataId$baseline_stat)
  expect_identical(
    Requests(list(col_by = list(measure = "CRP", visit = "Week 4", value = "change", cut = "median")))[[1]]$dataId$baseline_visits,
    list("Baseline")
  )
})

test_that("the cross-tabulation's settings are refused as the chart refuses them (#18)", {
  expect_error(CrossTab_Settings(list(percent = "total")), "percent")
  expect_error(CrossTab_Settings(list(test = "mcnemar")), "test")
  expect_error(CrossTab_Settings(list(statistic = "Analyze_Other")), "statistic")
  expect_error(CrossTab_Settings(list(row_by = list(measure = "CRP", visit = "Baseline"))), "no cut")
  expect_error(CrossTab_Settings(list(cuts = list("ARM"))), "cut variable")
  expect_error(CrossTab_Settings(list(max_levels = 0)), "max_levels")
})
