# What bio.viz's stratified survival chart draws and asks R, worked out in R
# (#35). The widget stores R's answer ahead of time, so it has to know who the
# chart draws, in which group, with which time and flag, and how it asks for
# its test. Both are held to bio.viz's recorded cases,
# fixtures/bio.viz/stratified-survival-r.json, written by bio.viz's
# tools/r-survival.R (`survival_key`) from the synthetic study, whose keys
# bio.viz's own tests hold to the chart's requests.

lSurvivalCases <- function() {
  lReadJson(testthat::test_path("fixtures", "bio.viz"), "stratified-survival-r.json")$cases
}

# The tables and the settings of bio.viz's demo page for this chart
# (site/demo/stratified-survival.js), without what only the page has.
lSurvivalDemo <- function() {
  lLabelled <- list(
    list(value_col = "ARM", label = "Arm"),
    list(value_col = "SEX", label = "Sex"),
    list(value_col = "RESPONSE", label = "Response")
  )
  list(
    results = Synthetic_Results,
    participants = Synthetic_Participants,
    outcomes = Synthetic_Outcomes,
    settings = list(
      endpoint = "EFS",
      group_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
      baseline_visits = "Baseline",
      groups = lLabelled,
      filters = lLabelled,
      cuts = list(list(measure = "CRP", visit = "Baseline", cut = "tertiles"))
    )
  )
}

# A case's view: the demo's settings with the case's endpoint and groups, its
# filters set as the controls would set them. A case with tables of its own
# has those, as records; an outcomes table with an event column is read by it.
lSurvivalView <- function(lCase) {
  lTables <- lSurvivalDemo()
  lSettings <- lTables$settings
  if (!is.null(lCase$tables$participants)) {
    lTables$participants <- dfFromRecords(lCase$tables$participants)
    lSettings <- list()
  }
  if (!is.null(lCase$tables$results)) {
    lTables$results <- dfFromRecords(lCase$tables$results)
  }
  if (!is.null(lCase$tables$outcomes)) {
    lTables$outcomes <- dfFromRecords(lCase$tables$outcomes)
    if ("EVENT" %in% names(lTables$outcomes)) lSettings$event_col <- "EVENT"
  }
  lSettings$endpoint <- lCase$dataId$endpoint
  lSettings["group_by"] <- list(lCase$dataId$group_by)
  lConfig <- StratifiedSurvival_Settings(lSettings)
  lState <- StratifiedSurvival_State(lTables$results, lTables$participants, lTables$outcomes, lConfig)
  for (strColumn in names(lCase$dataId$filters)) {
    lState$filters[[strColumn]] <- unlist(lCase$dataId$filters[[strColumn]])
  }
  c(lTables[c("results", "participants", "outcomes")], list(config = lConfig, state = lState))
}

test_that("the settings R reads have the defaults of the vendored stratified survival chart (#35)", {
  lBundle <- lBundleDefaults("at_risk_times")
  expect_false(is.null(lBundle), label = "the bundle's chart defaults were found")
  expect_true(all(names(lStratifiedSurvivalDefaults) %in% names(lBundle)))
  for (strSetting in names(lStratifiedSurvivalDefaults)) {
    expect_identical(
      lStratifiedSurvivalDefaults[[strSetting]], lBundle[[strSetting]],
      label = paste("R's default for", strSetting), expected.label = "the bundle's"
    )
  }
  expect_true("connection" %in% names(lBundle))
  expect_identical(lBundle$statistic, strStratifiedSurvivalStatistic)
  expect_identical(StratifiedSurvival_Settings()[names(lStratifiedSurvivalDefaults)], lStratifiedSurvivalDefaults)
  expect_identical(lCoreDefaults, lStratifiedSurvivalDefaults[names(lCoreDefaults)])
  expect_identical(lOutcomeDefaults, lStratifiedSurvivalDefaults[names(lOutcomeDefaults)])
})

test_that("the settings R reads are refused as the chart refuses them, and an event column alone is read as one (#35)", {
  expect_error(StratifiedSurvival_Settings(list(event_col = "EVENT", censor_col = "CNSR")), "exactly one of 'censor_col'")
  expect_error(StratifiedSurvival_Settings(list(censor_col = NULL)), "exactly one of 'censor_col'")
  expect_error(StratifiedSurvival_Settings(list(time_col = NULL)), "time_col")
  expect_error(StratifiedSurvival_Settings(list(endpoint = 3)), "endpoint")
  expect_error(StratifiedSurvival_Settings(list(statistic = "Analyze_Other")), "statistic")
  expect_error(StratifiedSurvival_Settings(list(group_by = list(measure = "CRP", visit = "Baseline"))), "no cut")
  expect_error(StratifiedSurvival_Settings(list(at_risk_times = c(6, 3))), "at_risk_times")
  expect_error(StratifiedSurvival_Settings(list(cuts = "CRP")), "cuts")
  expect_null(StratifiedSurvival_Settings(list(statistic = NULL))$statistic)
  # An event column named alone is the flag, as the chart's flaggedSettings()
  # reads it: the default censor column is then none.
  lEvent <- StratifiedSurvival_Settings(list(event_col = "EVENT"))
  expect_identical(lEvent$event_col, "EVENT")
  expect_null(lEvent$censor_col)
  expect_true("censor_col" %in% names(lEvent))
  expect_identical(Chart_FlagOf(lEvent), list(col = "EVENT", field = "event"))
  expect_identical(Chart_FlagOf(StratifiedSurvival_Settings()), list(col = "CNSR", field = "censor"))
  # A cut is read as the settings write it.
  expect_identical(
    StratifiedSurvival_Settings(list(group_by = list(measure = "CRP", visit = "Baseline", cut = 4)))$group_by,
    list(measure = "CRP", visit = "Baseline", value = "raw", cut = list(4))
  )
})

test_that("each participant's outcome is read as the chart reads it, and left out for the chart's reasons (#35)", {
  lConfig <- StratifiedSurvival_Settings()
  dfOutcomes <- data.frame(
    USUBJID = c("A", "B", "B", "C", "D", "E", "F", "G", "H", "I", " "),
    PARAMCD = c("EFS", "EFS", "EFS", "EFS", "EFS", "EFS", "EFS", "OS", "EFS", "EFS", "EFS"),
    PARAM = c("", "Event-free", "x", "x", "x", "x", "x", "Overall", "x", "x", "x"),
    AVAL = c("3.5", "1", "2", NA, "4", "-1", " 7 ", "2", "1e1", "abc", "1"),
    CNSR = c(0, 1, 0, 0, 2, 0, 1, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  dfRead <- Chart_Outcomes(dfOutcomes, lConfig, "EFS", c("A", "B", "C", "D", "E", "F", "G", "H", "I", " "))
  expect_identical(dfRead$time, c(3.5, NA, NA, NA, NA, 7, NA, 10, NA, NA))
  expect_identical(dfRead$flag, c(0, NA, NA, NA, NA, 1, NA, 0, NA, NA))
  expect_identical(dfRead$event, c(TRUE, NA, NA, NA, NA, FALSE, NA, TRUE, NA, NA))
  expect_identical(dfRead$reason, unname(c(
    NA, chrOutcomeLeftOut["several"], chrOutcomeLeftOut["missing"], chrOutcomeLeftOut["flag"],
    chrOutcomeLeftOut["negative"], NA, chrOutcomeLeftOut["none"], NA, chrOutcomeLeftOut["missing"],
    chrOutcomeLeftOut["none"]
  )))
  expect_identical(unname(chrOutcomeLeftOut["none"]), "No outcome for the endpoint")
  # Read the other way round, a flag of 1 is an event.
  lEvent <- StratifiedSurvival_Settings(list(event_col = "CNSR"))
  expect_identical(Chart_Outcomes(dfOutcomes, lEvent, "EFS", c("A", "F"))$event, c(FALSE, TRUE))
  # The endpoints, by name with numbers as numbers, each with the first label
  # that is written.
  expect_identical(
    Chart_Endpoints(dfOutcomes, lConfig),
    data.frame(endpoint = c("EFS", "OS"), label = c("Event-free", "Overall"), stringsAsFactors = FALSE)
  )
  expect_identical(Chart_Endpoints(Synthetic_Outcomes, lConfig)$label, "Event-free survival (months)")
  # A table without a column the settings name is refused, naming it.
  expect_error(Chart_CheckOutcomes(dfOutcomes[c("USUBJID", "PARAMCD", "AVAL")], lConfig), "no column 'CNSR' \\(setting 'censor_col'\\)")
  expect_error(Chart_CheckOutcomes(dfOutcomes, StratifiedSurvival_Settings(list(outcome_id_col = "SUBJ"))), "'SUBJ' \\(setting 'outcome_id_col'\\)")
  # An empty table is no outcomes table, and is not checked.
  expect_silent(Chart_CheckOutcomes(dfOutcomes[0, "USUBJID", drop = FALSE], lConfig))
})

test_that("R draws the participants bio.viz recorded for every case: each one's group, time and event, and the groups in order (#35)", {
  lCases <- lSurvivalCases()
  expect_gte(length(lCases), 8L)
  for (lCase in lCases) {
    lView <- lSurvivalView(lCase)
    lTable <- StratifiedSurvival_Table(lView$results, lView$participants, lView$outcomes, lView$config, lView$state)
    dfRecords <- lTable$records
    expect_identical(dfRecords$USUBJID, unlist(lCase$ids), label = paste(lCase$case, "participants"))
    expect_identical(dfRecords$group, unlist(lCase$group_of), label = paste(lCase$case, "groups"))
    expect_identical(dfRecords$time, as.numeric(unlist(lCase$time_of)), label = paste(lCase$case, "times"))
    expect_identical(dfRecords$event, unlist(lCase$event_of), label = paste(lCase$case, "events"))
    expect_identical(lTable$levels, unlist(lCase$groups), label = paste(lCase$case, "levels"))
    if (!is.null(lCase$points)) {
      expect_equal(lTable$cut$points, as.numeric(unlist(lCase$points)), tolerance = 1e-12, label = paste(lCase$case, "points"))
      expect_identical(lTable$cut$n, as.integer(lCase$n_values), label = paste(lCase$case, "values cut"))
    } else {
      expect_null(lTable$cut, label = paste(lCase$case, "cut"))
    }
    expect_identical(lTable$filtered, as.integer(lCase$participants), label = paste(lCase$case, "participants filtered"))
  }
  # The cases reach a column, a cut at its median, its tertiles and a typed
  # point, a filter, a flag of either kind, and tables of their own.
  expect_true(any(vapply(lCases, function(lCase) is.character(lCase$dataId$group_by), logical(1))))
  expect_setequal(
    unlist(lapply(lCases, function(lCase) if (is.list(lCase$dataId$group_by)) lCase$dataId$group_by$cut)),
    c("median", "tertiles", 4L, 10L)
  )
  expect_true(any(vapply(lCases, function(lCase) !is.null(lCase$dataId$filters), logical(1))))
  expect_true(any(vapply(lCases, function(lCase) !is.null(lCase$args$strEventCol), logical(1))))
})

test_that("R keys the curves' stored result exactly as the chart keys its request (#35)", {
  for (lCase in lSurvivalCases()) {
    lView <- lSurvivalView(lCase)
    lRequests <- StratifiedSurvival_Requests(lView$results, lView$participants, lView$outcomes, lView$config, lView$state)
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
    # R is handed the id, the time, the group and the flag, under the names
    # the arguments give.
    strFlag <- if (is.null(lCase$args$strEventCol)) "censor" else "event"
    expect_identical(names(lMine$data), c("USUBJID", "time", "group", strFlag), label = paste(lCase$case, "columns"))
  }
  # A cut's groups are handed to R high to low, so the hazard ratio is the
  # higher group's over the lower's; a column's by code point.
  lMedian <- Filter(function(lCase) lCase$case == "crp-median", lSurvivalCases())[[1]]
  expect_identical(unlist(lMedian$args$chrGroups), rev(unlist(lMedian$groups)))
})

test_that("R's answer for each case is the answer bio.viz recorded from desktop R (#35)", {
  for (lCase in lSurvivalCases()) {
    lView <- lSurvivalView(lCase)
    lRequest <- StratifiedSurvival_Requests(lView$results, lView$participants, lView$outcomes, lView$config, lView$state)[[1]]
    lMine <- do.call(Analyze_Survival, c(list(lRequest$data), lRequest$args))
    lTheirs <- lCase$value
    ExpectResultShape(lMine)
    expect_identical(lMine$status, lTheirs$status, label = paste(lCase$case, "status"))
    expect_identical(lMine$reason, if (is.null(lTheirs$reason)) NA_character_ else lTheirs$reason, label = paste(lCase$case, "reason"))
    expect_identical(lMine$counts, lTheirs$counts, label = paste(lCase$case, "counts"))
    expect_equal(lMine$p_value, if (is.null(lTheirs$p_value)) NA_real_ else lTheirs$p_value, tolerance = 1e-8, label = paste(lCase$case, "p-value"))
    if (identical(lMine$status, "ok")) {
      Estimate <- function(lRow) if (is.null(lRow$estimate)) NA_real_ else lRow$estimate
      expect_identical(lMine$estimates$name, vapply(lTheirs$estimates, function(lRow) lRow$name, character(1)), label = paste(lCase$case, "estimates"))
      expect_equal(lMine$estimates$estimate, vapply(lTheirs$estimates, Estimate, numeric(1)), tolerance = 1e-8, label = paste(lCase$case, "estimates"))
    }
  }
})

test_that("the chart opens on the settings, where the tables have what they name (#35)", {
  lTables <- lSurvivalDemo()
  State <- function(lMore, dfOutcomes = lTables$outcomes) {
    lSettings <- lTables$settings
    for (strName in names(lMore)) lSettings[strName] <- list(lMore[[strName]])
    StratifiedSurvival_State(lTables$results, lTables$participants, dfOutcomes, StratifiedSurvival_Settings(lSettings))
  }
  lOpening <- State(list())
  expect_identical(lOpening$endpoint, "EFS")
  expect_identical(lOpening$group_by, list(measure = "CRP", visit = "Baseline", value = "raw", cut = "median"))
  # An endpoint the table does not have is the first it has; a column the
  # controls do not offer is the first they do.
  expect_identical(State(list(endpoint = "OS"))$endpoint, "EFS")
  expect_identical(State(list(endpoint = NULL))$endpoint, "EFS")
  expect_identical(State(list(group_by = "SITE"))$group_by, "ARM")
  expect_identical(State(list(group_by = NULL))$group_by, "ARM")
  expect_identical(State(list(group_by = "SEX"))$group_by, "SEX")
  expect_null(State(list(), Synthetic_Outcomes[0, ])$endpoint)
  expect_error(State(list(group_by = list(measure = "NOPE", visit = "Baseline", cut = "median"))), "NOPE")
})

test_that("the chart asks nothing where it draws no test (#35)", {
  lTables <- lSurvivalDemo()
  Requests <- function(lMore = list(), dfOutcomes = lTables$outcomes, lFilters = NULL) {
    lSettings <- lTables$settings
    for (strName in names(lMore)) lSettings[strName] <- list(lMore[[strName]])
    lConfig <- StratifiedSurvival_Settings(lSettings)
    lState <- StratifiedSurvival_State(lTables$results, lTables$participants, dfOutcomes, lConfig)
    lState$filters[names(lFilters)] <- lFilters
    StratifiedSurvival_Requests(lTables$results, lTables$participants, dfOutcomes, lConfig, lState)
  }
  expect_length(Requests(), 1L)
  expect_length(Requests(list(statistic = NULL)), 0L)
  # A filter nobody passes, no outcome for anyone, or one group drawn.
  expect_length(Requests(lFilters = list(SEX = "X")), 0L)
  expect_length(Requests(dfOutcomes = Synthetic_Outcomes[0, ]), 0L)
  dfOneArm <- lTables$participants
  dfOneArm$ARM <- "Placebo"
  lConfig <- StratifiedSurvival_Settings(c(lTables$settings[setdiff(names(lTables$settings), "group_by")], list(group_by = "ARM")))
  lState <- StratifiedSurvival_State(lTables$results, dfOneArm, lTables$outcomes, lConfig)
  expect_length(StratifiedSurvival_Requests(lTables$results, dfOneArm, lTables$outcomes, lConfig, lState), 0L)
  # A participant without an outcome is not drawn, and not handed to R.
  dfFewer <- lTables$outcomes[-(1:10), ]
  expect_identical(Requests(dfOutcomes = dfFewer)[[1]]$rows, 190L)
  # A cut that reads a baseline names the baseline settings; a raw value does not.
  lChange <- Requests(list(group_by = list(measure = "CRP", visit = "Week 4", value = "change", cut = "median")))[[1]]
  expect_identical(lChange$dataId$baseline_visits, list("Baseline"))
  expect_identical(lChange$dataId$baseline_stat, "mean")
  expect_null(Requests()[[1]]$dataId$baseline_stat)
})

test_that("the stored results are R's answer for the view the chart opens on (#35)", {
  lTables <- lSurvivalDemo()
  lConfig <- StratifiedSurvival_Settings(lTables$settings)
  lStored <- StratifiedSurvival_StoredResults(lTables$results, lTables$participants, lTables$outcomes, lConfig)
  expect_length(lStored, 1L)
  expect_identical(lStored[[1]]$name, "Analyze_Survival")
  expect_identical(lStored[[1]]$value$status, "ok")
  expect_identical(StratifiedSurvival_StoredResults(lTables$results, lTables$participants, NULL, lConfig), list())
  expect_identical(StratifiedSurvival_StoredResults(lTables$results[0, ], lTables$participants, lTables$outcomes, lConfig), list())
})
