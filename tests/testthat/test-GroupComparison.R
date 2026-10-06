# What the group comparison chart draws and asks R, worked out in R (#9). The
# widget stores R's answers ahead of time, so it has to know which rows the
# chart will hand R for each panel and how the chart will ask. Both are held to
# bio.viz's own fixtures, copied with their record: the rows its core wrote for
# panels of its demo chart (thirteen when this was written; every case the
# fixture lists is run), and the request the chart makes for each.

strStatisticsFixture <- function(...) {
  testthat::test_path("fixtures", "bio.viz", ...)
}

# The cases as bio.viz lists them: one panel of its demo in one view each.
dfDemoCases <- function() {
  utils::read.csv(
    strStatisticsFixture("group-statistics", "cases.csv"),
    colClasses = "character", na.strings = "", check.names = FALSE
  )
}

lRecordedRequests <- function() {
  lResults <- lReadJson(strStatisticsFixture("group-statistics-r.json"))$results
  stats::setNames(lResults, vapply(lResults, function(lResult) lResult$case, character(1)))
}

# What a case says of its view, as the settings and the controls R reads.
lCaseView <- function(lCase) {
  One <- function(strValue) if (is.na(strValue)) NULL else strValue
  Several <- function(strValue) if (is.na(strValue)) NULL else strsplit(strValue, "|", fixed = TRUE)[[1]]
  lFilters <- list()
  if (!is.na(lCase$filters)) {
    for (chrPart in strsplit(strsplit(lCase$filters, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)) {
      lFilters[[chrPart[1]]] <- Several(chrPart[2])
    }
  }
  lDemoTables <- lDemo()
  lSettings <- lDemoTables$settings
  lSettings$baseline_visits <- Several(lCase$baseline_visits)
  lSettings$baseline_stat <- lCase$baseline_stat
  list(
    results = lDemoTables$results,
    participants = lDemoTables$participants,
    config = GroupComparison_Settings(lSettings),
    state = list(
      measure = lCase$measure, visits = if (is.na(lCase$visit)) character(0) else lCase$visit,
      value_type = lCase$value_type, group_by = One(lCase$group_by), levels = NULL,
      color_by = One(lCase$color_by), panel_by = One(lCase$panel_by), y_scale = lCase$y_scale,
      test = lCase$test, pairwise = identical(lCase$pairwise, "TRUE"), filters = lFilters
    ),
    panel = One(lCase$panel)
  )
}

# The request R works out for a case's panel.
lCaseRequest <- function(lCase) {
  lView <- lCaseView(lCase)
  lRequests <- GroupComparison_Requests(lView$results, lView$participants, lView$config, lView$state)
  lFound <- Filter(function(lRequest) identical(lRequest$dataId$panel, lView$panel), lRequests)
  expect_identical(length(lFound), 1L, label = paste(lCase$case, "has one request for its panel"))
  lFound[[1]]
}

test_that("the settings R reads have the defaults of the vendored chart (#9)", {
  lBundle <- lBundleDefaults("group_by")
  expect_false(is.null(lBundle), label = "the bundle's chart defaults were found")
  expect_true(all(names(lGroupComparisonDefaults) %in% names(lBundle)))
  for (strSetting in names(lGroupComparisonDefaults)) {
    expect_identical(
      lGroupComparisonDefaults[[strSetting]], lBundle[[strSetting]],
      label = paste("R's default for", strSetting), expected.label = "the bundle's"
    )
  }
  # The setting the widget makes itself, and the functions it stores results of.
  expect_true("connection" %in% names(lBundle))
  expect_identical(lBundle$statistic, strGroupComparisonStatistic)
  expect_identical(lBundle$statistic_by_visit, strGroupComparisonByVisit)
  # The unscheduled-visit rule's settings are the core's, under the same names.
  expect_identical(lCoreUnscheduledDefaults, lGroupComparisonDefaults[names(lCoreUnscheduledDefaults)])
  expect_identical(GroupComparison_Settings()[names(lGroupComparisonDefaults)], lGroupComparisonDefaults)
  # The frame's own settings are the chart's, under the same names.
  expect_identical(lCoreDefaults, lGroupComparisonDefaults[names(lCoreDefaults)])
})

test_that("R resolves the rows bio.viz's chart hands R, for every panel of its demo bio.viz recorded (#9)", {
  dfCases <- dfDemoCases()
  # Every case bio.viz lists is run, however many it comes to list.
  expect_gte(nrow(dfCases), 13L)
  expect_identical(anyDuplicated(dfCases$case), 0L)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    dfTheirs <- utils::read.csv(
      strStatisticsFixture("group-statistics", lCase$file),
      colClasses = "character", na.strings = character(0), check.names = FALSE
    )
    dfMine <- lCaseRequest(lCase)$data

    expect_identical(names(dfMine), names(dfTheirs), label = paste(lCase$case, "columns"))
    expect_identical(nrow(dfMine), nrow(dfTheirs), label = paste(lCase$case, "rows"))
    expect_equal(dfMine$y, as.numeric(dfTheirs$y), tolerance = 1e-12, label = paste(lCase$case, "y"))
    for (strColumn in setdiff(names(dfTheirs), "y")) {
      expect_identical(Core_Text(dfMine[[strColumn]]), dfTheirs[[strColumn]], label = paste(lCase$case, strColumn))
    }
  }

  # The cases reach a filter, a colour, a panel, several visits, a logarithmic
  # scale and a baseline value, and three of the five value types.
  expect_true(all(c("change", "raw", "baseline") %in% dfCases$value_type))
  expect_true(all(c("SEX=F", "AGE=57") %in% dfCases$filters))
  expect_true("log" %in% dfCases$y_scale && "SEX" %in% dfCases$color_by && "SEX" %in% dfCases$panel_by)
})

test_that("R keys a stored result exactly as the chart keys its request (#9)", {
  dfCases <- dfDemoCases()
  lRecorded <- lRecordedRequests()
  expect_setequal(names(lRecorded), dfCases$case)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lMine <- lCaseRequest(lCase)
    lTheirs <- lRecorded[[lCase$case]]

    # Member for member, in the order the chart writes them: a list is a list
    # whatever its length, and a member that is not set is not there.
    expect_identical(lMine$name, lTheirs$name, label = paste(lCase$case, "name"))
    expect_identical(lMine$args, lTheirs$args, label = paste(lCase$case, "args"))
    expect_identical(lMine$dataId, lTheirs$dataId, label = paste(lCase$case, "dataId"))
    expect_identical(lMine$rows, lTheirs$rows, label = paste(lCase$case, "rows"))
    # And as JSON, which is how the page compares them.
    expect_identical(
      as.character(jsonlite::toJSON(lMine[c("name", "args", "dataId", "rows")], auto_unbox = TRUE)),
      as.character(jsonlite::toJSON(lTheirs[c("name", "args", "dataId", "rows")], auto_unbox = TRUE)),
      label = paste(lCase$case, "as JSON")
    )
  }

  # Two views are two keys: no two of the distinct requests share one.
  chrKeys <- vapply(lRecorded, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  expect_identical(anyDuplicated(chrKeys), 0L)
  expect_identical(
    Chart_KeyText(list(b = list("x", "y"), a = 1L)),
    Chart_KeyText(list(a = 1L, b = list("x", "y")))
  )
  expect_false(identical(Chart_KeyText(list(a = "ARM")), Chart_KeyText(list(a = list("ARM")))))
})

test_that("R's answer on those rows is the answer bio.viz recorded from desktop R (#9)", {
  dfCases <- dfDemoCases()
  lRecorded <- lRecordedRequests()

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lRequest <- lCaseRequest(lCase)
    lMine <- do.call(Analyze_GroupDifference, c(list(lRequest$data), lRequest$args))
    lTheirs <- lRecorded[[lCase$case]]$value

    ExpectResultShape(lMine)
    expect_identical(lMine$status, lTheirs$status, label = paste(lCase$case, "status"))
    expect_identical(lMine$method, if (is.null(lTheirs$method)) NA_character_ else lTheirs$method, label = paste(lCase$case, "method"))
    expect_identical(lMine$counts, lTheirs$counts, label = paste(lCase$case, "counts"))
    if (lMine$status == "ok") {
      # To 1 part in 10^8, the tolerance bio.viz holds the chart to.
      expect_equal(lMine$p_value, lTheirs$p_value, tolerance = 1e-8, label = paste(lCase$case, "p_value"))
    } else {
      expect_identical(lMine$reason, lTheirs$reason, label = paste(lCase$case, "reason"))
    }
  }
  # One case is below the minimum group size, so a reason is exercised too.
  expect_true("too_small" %in% vapply(lRecorded, function(lResult) lResult$value$status, character(1)))
})

test_that("the controls open on what the chart's open on: biomarkers, visits, groups and filters (#9)", {
  lTables <- lDemo()
  lPlain <- GroupComparison_Settings()

  # By name, with numbers inside a name as numbers: the order the chart lists.
  expect_identical(
    Chart_Measures(Synthetic_Results, lPlain),
    c("CRP", "D-dimer", "Ferritin", "IFN-gamma", "IL-1beta", "IL-2", "IL-6", "IL-8", "IL-10", "LDH", "TNF-alpha", "VEGF")
  )
  expect_identical(
    Chart_Measures(Synthetic_Results, GroupComparison_Settings(list(measures = c("IL-6", "NOPE", "CRP")))),
    c("IL-6", "CRP")
  )
  # Every visit, or the visits asked for that the table has, in the order asked.
  lVisits <- GroupComparison_Visits(Synthetic_Results, lPlain)
  expect_identical(lVisits$all, c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12"))
  expect_identical(lVisits$start, lVisits$all)
  expect_identical(
    GroupComparison_Visits(Synthetic_Results, GroupComparison_Settings(list(visits = c("Week 12", "Week 99", "Week 4"))))$start,
    c("Week 12", "Week 4")
  )
  expect_identical(
    GroupComparison_Visits(Synthetic_Results, GroupComparison_Settings(list(visits = "Week 99")))$start,
    lVisits$all
  )

  # The participant table's category columns, and no column with too many values.
  dfCategories <- Chart_Categories(Synthetic_Results, Synthetic_Participants, lPlain)
  expect_identical(dfCategories$value_col, c("ARM", "SEX", "RESPONSE"))
  expect_identical(unique(dfCategories$table), "participants")
  lState <- GroupComparison_State(Synthetic_Results, Synthetic_Participants, lPlain)
  # No biomarker named, or one the table does not have: the overview of every
  # biomarker, which is a biomarker of NULL. A biomarker named is the one opened.
  expect_null(lState$measure)
  expect_true("measure" %in% names(lState))
  expect_null(GroupComparison_State(Synthetic_Results, Synthetic_Participants, GroupComparison_Settings(list(start_value = "NOPE")))$measure)
  expect_identical(
    GroupComparison_State(Synthetic_Results, Synthetic_Participants, GroupComparison_Settings(list(start_value = "IL-6")))$measure,
    "IL-6"
  )
  expect_identical(lState$visits, lVisits$all)
  expect_identical(lState$group_by, "ARM")
  expect_null(lState$color_by)
  expect_identical(names(lState$filters), c("ARM", "SEX", "RESPONSE"))
  expect_true(all(vapply(lState$filters, is.null, logical(1))))

  # With the results alone a group comes from a column carried on their rows
  # that holds one value for each participant; a mapped column is not offered.
  dfAlone <- Synthetic_Results
  dfAlone$ARM <- Synthetic_Participants$ARM[match(dfAlone$USUBJID, Synthetic_Participants$USUBJID)]
  dfAlone$ROW <- seq_len(nrow(dfAlone)) %% 2L
  dfCategories <- Chart_Categories(dfAlone, NULL, lPlain)
  expect_identical(dfCategories$value_col, "ARM")
  expect_identical(dfCategories$table, "results")
  expect_identical(GroupComparison_State(dfAlone, NULL, lPlain)$filters, list())
  expect_null(GroupComparison_State(Synthetic_Results, NULL, lPlain)$group_by)

  # The setting `groups` is the list, and a filter opens on its `start`.
  lConfig <- GroupComparison_Settings(list(
    groups = lTables$settings$groups, group_by = "ARM_SEX", color_by = "NOPE",
    filters = list("ARM", list(value_col = "SEX", start = "F"), list(value_col = "RESPONSE", start = c("Responder", "Non-responder"), multiple = TRUE), "NOPE")
  ))
  lState <- GroupComparison_State(lTables$results, lTables$participants, lConfig)
  expect_identical(lState$group_by, "ARM_SEX")
  expect_null(lState$color_by)
  expect_identical(lState$filters, list(ARM = NULL, SEX = "F", RESPONSE = c("Responder", "Non-responder")))
  expect_identical(
    Chart_FiltersInForce(lState$filters),
    list(SEX = "F", RESPONSE = c("Non-responder", "Responder"))
  )
})

test_that("the baseline visit of a change is not a panel, and the overview asks R for nothing (#9)", {
  chrAll <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
  # At the one baseline visit a change is the same for everyone: not drawn.
  for (strValueType in c("change", "fold_change", "percent_change")) {
    expect_identical(GroupComparison_VisitsDrawn(chrAll, strValueType, "Baseline"), chrAll[-1], label = strValueType)
  }
  expect_identical(GroupComparison_VisitsDrawn("Baseline", "change", "Baseline"), character(0))
  # A result or a baseline value is drawn at every visit, and so is a change
  # measured against several baseline visits.
  expect_identical(GroupComparison_VisitsDrawn(chrAll, "raw", character(0)), chrAll)
  expect_identical(GroupComparison_VisitsDrawn(chrAll, "change", c("Baseline", "Week 2")), chrAll)

  lTables <- lDemo()
  Visits <- function(lMore, strMeasure = "IL-6") {
    lConfig <- GroupComparison_Settings(c(lTables$settings[setdiff(names(lTables$settings), c("visits", names(lMore)))], lMore))
    lState <- GroupComparison_State(lTables$results, lTables$participants, lConfig)
    lState["measure"] <- list(strMeasure)
    lRequests <- GroupComparison_Requests(lTables$results, lTables$participants, lConfig, lState)
    unlist(lapply(lRequests, function(lRequest) lRequest$dataId$visit))
  }
  # One biomarker open, every visit chosen: a request per panel drawn.
  expect_identical(Visits(list()), chrAll[-1])
  expect_identical(Visits(list(value_type = "percent_change")), chrAll[-1])
  expect_identical(Visits(list(value_type = "raw")), chrAll)
  expect_identical(Visits(list(baseline_visits = c("Baseline", "Week 2"))), chrAll)
  # With no baseline visit named the baseline is the first visit, and it is not drawn.
  expect_identical(Visits(list(baseline_visits = NULL)), chrAll[-1])
  expect_identical(Visits(list(visits = "Baseline")), NULL)
  expect_identical(Visits(list(visits = c("Week 8", "Baseline", "Week 2"))), c("Week 8", "Week 2"))
  # A baseline value has no visit: one request, with no visit in its identity.
  expect_null(Visits(list(value_type = "baseline")))
  # The overview, where no biomarker is open, prints no test.
  expect_null(Visits(list(), NULL))
})

test_that("a test that does not fit the number of groups gives way to its counterpart, and none asks nothing (#9)", {
  expect_identical(GroupComparison_FitTest("t", 2L), "t")
  expect_identical(GroupComparison_FitTest("t", 4L), "anova")
  expect_identical(GroupComparison_FitTest("anova", 2L), "t")
  expect_identical(GroupComparison_FitTest("wilcoxon", 3L), "kruskal")
  expect_identical(GroupComparison_FitTest("kruskal", 2L), "wilcoxon")
  expect_identical(GroupComparison_FitTest("none", 2L), "none")
  expect_null(GroupComparison_FitTest("t", 1L))

  # The view these requests are of: IL-6 open, change from Baseline, at Week 4
  # alone, which the demo's settings name (`start_value`, `visits`).
  lTables <- lDemo()
  expect_identical(lTables$settings[c("start_value", "visits")], list(start_value = "IL-6", visits = "Week 4"))
  Requests <- function(lMore) {
    lConfig <- GroupComparison_Settings(c(lTables$settings[setdiff(names(lTables$settings), names(lMore))], lMore))
    GroupComparison_Requests(lTables$results, lTables$participants, lConfig, GroupComparison_State(lTables$results, lTables$participants, lConfig))
  }
  # The default test, a t-test, gives way to a one-way ANOVA among four groups,
  # and pairs are compared only among more than two.
  expect_identical(Requests(list(group_by = "ARM_SEX", pairwise = TRUE))[[1]]$args$strMethod, "anova")
  expect_true(Requests(list(group_by = "ARM_SEX", pairwise = TRUE))[[1]]$args$bPairwise)
  expect_false(Requests(list(pairwise = TRUE))[[1]]$args$bPairwise)
  expect_identical(Requests(list(test = "none")), list())
  expect_identical(Requests(list(statistic = NULL)), list())
  # One level drawn is not two groups, and one panel per level of the panel column.
  expect_identical(Requests(list(levels = "Placebo")), list())
  expect_identical(
    vapply(Requests(list(panel_by = "SEX")), function(lRequest) lRequest$dataId$panel, character(1)),
    c("F", "M")
  )
})

# A cut category (#18): the groups are a biomarker or a number cut by the
# shared cut rule. bio.viz's recipes in group-statistics-r.json record two
# panels by CRP at Baseline cut, with the key and R's answer.

lCutRecipes <- function() {
  lRecipes <- lReadJson(strStatisticsFixture("group-statistics-r.json"))$recipes
  Filter(function(lRecipe) startsWith(lRecipe$case, "cut-"), lRecipes)
}

lCutView <- function(lGroupBy, lMore = list()) {
  lTables <- lDemo()
  lSettings <- lTables$settings
  lSettings$group_by <- lGroupBy
  for (strName in names(lMore)) lSettings[strName] <- list(lMore[[strName]])
  lConfig <- GroupComparison_Settings(lSettings)
  lState <- GroupComparison_State(lTables$results, lTables$participants, lConfig)
  list(tables = lTables, config = lConfig, state = lState)
}

test_that("R keys a cut category's panel as the chart does: the groups low to high in chrGroups, by code point in the identity (#18)", {
  lRecipes <- lCutRecipes()
  expect_setequal(vapply(lRecipes, function(lRecipe) lRecipe$case, character(1)), c("cut-median", "cut-too-small"))
  for (lRecipe in lRecipes) {
    lView <- lCutView(lRecipe$dataId$group_by)
    lRequests <- GroupComparison_Requests(lView$tables$results, lView$tables$participants, lView$config, lView$state)
    expect_length(lRequests, 1L)
    lMine <- lRequests[[1]]
    expect_identical(lMine$name, lRecipe$name, label = paste(lRecipe$case, "name"))
    expect_identical(lMine$args, lRecipe$args, label = paste(lRecipe$case, "args"))
    # As JSON reads it back: a typed point is a number, whole or not.
    expect_identical(lJsonRoundTrip(lMine$dataId), lRecipe$dataId, label = paste(lRecipe$case, "dataId"))
    expect_identical(lMine$rows, lRecipe$rows, label = paste(lRecipe$case, "rows"))
    expect_identical(
      as.character(jsonlite::toJSON(lMine[c("name", "args", "dataId", "rows")], auto_unbox = TRUE, digits = NA)),
      as.character(jsonlite::toJSON(lRecipe[c("name", "args", "dataId", "rows")], auto_unbox = TRUE, digits = NA)),
      label = paste(lRecipe$case, "as JSON")
    )
    # The rows: the participants and the group each is in.
    expect_identical(Core_Text(lMine$data$USUBJID), unlist(lRecipe$ids), label = paste(lRecipe$case, "participants"))
    expect_identical(Core_Text(lMine$data$x), unlist(lRecipe$groups), label = paste(lRecipe$case, "groups"))
    # R's answer, the first group's mean less the second's: low less high.
    lAnswer <- do.call(Analyze_GroupDifference, c(list(lMine$data), lMine$args))
    lTheirs <- lRecipe$value
    expect_identical(lAnswer$status, lTheirs$status, label = paste(lRecipe$case, "status"))
    expect_identical(lAnswer$counts, lTheirs$counts, label = paste(lRecipe$case, "counts"))
    if (identical(lAnswer$status, "ok")) {
      expect_equal(lAnswer$p_value, lTheirs$p_value, tolerance = 1e-8, label = paste(lRecipe$case, "p-value"))
      expect_identical(lAnswer$estimates$group, vapply(lTheirs$estimates, function(lRow) lRow$group, character(1)))
      expect_equal(lAnswer$estimates$estimate, vapply(lTheirs$estimates, function(lRow) lRow$estimate, numeric(1)), tolerance = 1e-8)
    }
  }
  # The difference is the lower group less the higher.
  lMedian <- Filter(function(lRecipe) lRecipe$case == "cut-median", lRecipes)[[1]]
  expect_match(tail(vapply(lMedian$value$estimates, function(lRow) lRow$group, character(1)), 1), "^\u2264 .* - > ")
})

test_that("a cut category's groups are worked out once, on every participant the filters keep, and every group is drawn (#18)", {
  lCut <- list(measure = "CRP", visit = "Baseline", cut = "tertiles")
  # Levels are for a column: a cut draws every group it makes.
  lView <- lCutView(lCut, list(visits = c("Week 4", "Week 8"), levels = c("Placebo")))
  lModel <- GroupComparison_Panels(lView$tables$results, lView$tables$participants, lView$config, lView$state)
  expect_identical(lModel$groups, 3L)
  lRequests <- GroupComparison_Requests(lView$tables$results, lView$tables$participants, lView$config, lView$state)
  expect_length(lRequests, 2L)
  # The same groups in every visit's panel, low to high.
  expect_identical(lRequests[[1]]$args$chrGroups, lRequests[[2]]$args$chrGroups)
  expect_identical(lRequests[[1]]$args$chrGroups, list("\u2264 2.167", "> 2.167, \u2264 3.467", "> 3.467"))
  expect_identical(lRequests[[1]]$args$strMethod, "anova")
  # A filter moves the points.
  lWomen <- lCutView(lCut, list(filters = list(list(value_col = "SEX", start = "F"))))
  lWomenRequest <- GroupComparison_Requests(lWomen$tables$results, lWomen$tables$participants, lWomen$config, lWomen$state)[[1]]
  lPoints <- Chart_Cut(Synthetic_Results, Synthetic_Participants, lWomen$config, list(SEX = "F"), Core_Variable(lCut))
  expect_identical(lWomenRequest$args$chrGroups, as.list(lPoints$labels))
  expect_false(identical(lWomenRequest$args$chrGroups, lRequests[[1]]$args$chrGroups))
  # A cut panel: its level is the group's label.
  lPanels <- lCutView("ARM", list(panel_by = list(col = "AGE", type = "number", cut = c(40, 60))))
  lPanelRequests <- GroupComparison_Requests(lPanels$tables$results, lPanels$tables$participants, lPanels$config, lPanels$state)
  expect_identical(
    vapply(lPanelRequests, function(lRequest) lRequest$dataId$panel, character(1)),
    c("\u2264 40", "> 40, \u2264 60", "> 60")
  )
  expect_identical(lPanelRequests[[1]]$dataId$panel_by, list(col = "AGE", type = "number", cut = list(40, 60)))
  expect_null(lPanelRequests[[1]]$args$chrGroups)
})

test_that("a group or a panel that is a variable must be cut, and must be of a biomarker or a column the tables have (#18)", {
  expect_error(GroupComparison_Settings(list(group_by = list(measure = "CRP", visit = "Baseline"))), "no cut")
  expect_error(GroupComparison_Settings(list(panel_by = list(col = "AGE", cut = "median"))), "number")
  expect_error(lCutView(list(measure = "NOPE", visit = "Baseline", cut = "median")), "NOPE")
  expect_error(lCutView(list(col = "NOPE", type = "number", cut = "median")), "NOPE")
})

# One biomarker over time (#53): the chart draws a biomarker across every
# visit it has in one picture, and asks R for the test under every visit in
# one request. bio.viz records the rows its own code hands R for that request
# (group-statistics/over-time-*.csv), the request, and what desktop R answered.

dfOverTimeCases <- function() {
  utils::read.csv(
    strStatisticsFixture("group-statistics", "over-time-cases.csv"),
    colClasses = "character", na.strings = "", check.names = FALSE
  )
}

lRecordedOverTime <- function() {
  lResults <- lReadJson(strStatisticsFixture("group-statistics-r.json"))$over_time
  stats::setNames(lResults, vapply(lResults, function(lResult) lResult$case, character(1)))
}

# The requests R works out for a case: the biomarker open with every visit
# chosen, as the chart is when it draws the biomarker over time.
lOverTimeCaseRequest <- function(lCase) {
  Several <- function(strValue) if (is.na(strValue)) NULL else strsplit(strValue, "|", fixed = TRUE)[[1]]
  lFilters <- list()
  if (!is.na(lCase$filters)) {
    for (chrPart in strsplit(strsplit(lCase$filters, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)) {
      lFilters[[chrPart[1]]] <- Several(chrPart[2])
    }
  }
  lTables <- lDemo()
  lSettings <- lTables$settings[setdiff(names(lTables$settings), "visits")]
  lSettings$baseline_visits <- Several(lCase$baseline_visits)
  lSettings$baseline_stat <- lCase$baseline_stat
  lSettings$value_type <- lCase$value_type
  lSettings$group_by <- lCase$group_by
  lSettings$test <- lCase$test
  lSettings$y_scale <- lCase$y_scale
  lSettings$visit_adjustment <- lCase$visit_adjustment
  lConfig <- GroupComparison_Settings(lSettings)
  lState <- GroupComparison_State(lTables$results, lTables$participants, lConfig)
  lState$measure <- lCase$measure
  lState$filters <- lFilters
  lRequests <- GroupComparison_OverTimeRequests(lTables$results, lTables$participants, lConfig, lState)
  expect_identical(length(lRequests), 1L, label = paste(lCase$case, "is one request"))
  lRequests[[1]]
}

test_that("R resolves the rows the chart hands R under one biomarker over time, for every case bio.viz recorded (#53)", {
  dfCases <- dfOverTimeCases()
  expect_gte(nrow(dfCases), 12L)
  expect_identical(anyDuplicated(dfCases$case), 0L)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    dfTheirs <- utils::read.csv(
      strStatisticsFixture("group-statistics", lCase$file),
      colClasses = "character", na.strings = character(0), check.names = FALSE
    )
    dfMine <- lOverTimeCaseRequest(lCase)$data

    # Long rows: a participant, a value, a group and a visit to a row, the
    # visits in visit order and the participants in the frame's within each.
    expect_identical(names(dfMine), c("USUBJID", "y", "x", "visit"), label = paste(lCase$case, "columns"))
    expect_identical(names(dfMine), names(dfTheirs), label = paste(lCase$case, "columns"))
    expect_identical(nrow(dfMine), nrow(dfTheirs), label = paste(lCase$case, "rows"))
    expect_equal(dfMine$y, as.numeric(dfTheirs$y), tolerance = 1e-12, label = paste(lCase$case, "y"))
    for (strColumn in setdiff(names(dfTheirs), "y")) {
      expect_identical(Core_Text(dfMine[[strColumn]]), dfTheirs[[strColumn]], label = paste(lCase$case, strColumn))
    }
  }
  # The cases reach a result and a change, two groups and four, a filter that
  # leaves groups too small to test, and each adjustment.
  expect_true(all(c("raw", "change") %in% dfCases$value_type))
  expect_true(all(c("ARM", "ARM_SEX") %in% dfCases$group_by))
  expect_setequal(dfCases$visit_adjustment, chrGroupComparisonAdjustments)
  expect_true("AGE=57" %in% dfCases$filters)
  # The baseline visit of a change is drawn and not tested: its rows are not sent.
  lChange <- lOverTimeCaseRequest(as.list(dfCases[dfCases$case == "over-time-change", ]))
  expect_false("Baseline" %in% lChange$data$visit)
  expect_identical(unique(lChange$data$visit), c("Week 2", "Week 4", "Week 8", "Week 12"))
})

test_that("R keys the test under one biomarker's visits exactly as the chart keys its request (#53)", {
  dfCases <- dfOverTimeCases()
  lRecorded <- lRecordedOverTime()
  expect_setequal(names(lRecorded), dfCases$case)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lMine <- lOverTimeCaseRequest(lCase)
    lTheirs <- lRecorded[[lCase$case]]
    expect_identical(lMine$name, lTheirs$name, label = paste(lCase$case, "name"))
    expect_identical(lMine$args, lTheirs$args, label = paste(lCase$case, "args"))
    expect_identical(lMine$dataId, lTheirs$dataId, label = paste(lCase$case, "dataId"))
    expect_identical(lMine$rows, lTheirs$rows, label = paste(lCase$case, "rows"))
    # And as JSON, which is how the page compares them.
    expect_identical(
      as.character(jsonlite::toJSON(lMine[c("name", "args", "dataId", "rows")], auto_unbox = TRUE)),
      as.character(jsonlite::toJSON(lTheirs[c("name", "args", "dataId", "rows")], auto_unbox = TRUE)),
      label = paste(lCase$case, "as JSON")
    )
  }
  # The arguments in the order the chart writes them, the visits always named
  # and the adjustment always said; the identity lists the visits tested.
  lOne <- lRecorded[["over-time-change-holm"]]
  expect_identical(names(lOne$args), c("strValueCol", "strGroupCol", "strByCol", "strMethod", "chrBy", "strPAdjust"))
  expect_identical(lOne$args$strPAdjust, "holm")
  expect_identical(lOne$args$chrBy, lOne$dataId$visits)
  expect_identical(
    names(lOne$dataId),
    c("chart", "measure", "value_type", "visits", "baseline_visits", "baseline_stat", "group_by", "groups")
  )
  # An adjustment is another key, and so is a panel's request for one visit.
  chrKeys <- vapply(lRecorded, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  expect_identical(anyDuplicated(chrKeys), 0L)
})

test_that("R's answer for those rows is the answer bio.viz recorded from desktop R, visit by visit (#53)", {
  dfCases <- dfOverTimeCases()
  lRecorded <- lRecordedOverTime()
  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lRequest <- lOverTimeCaseRequest(lCase)
    lMine <- do.call(Analyze_GroupDifferenceBy, c(list(lRequest$data), lRequest$args))
    lTheirs <- lRecorded[[lCase$case]]$value

    ExpectResultShape(lMine)
    expect_identical(lMine$status, lTheirs$status, label = paste(lCase$case, "status"))
    expect_identical(lMine$counts, lTheirs$counts, label = paste(lCase$case, "counts"))
    expect_identical(lMine$adjustment, lTheirs$adjustment, label = paste(lCase$case, "adjustment"))
    expect_identical(nrow(lMine$rows), length(lTheirs$rows), label = paste(lCase$case, "a row per visit"))
    for (iVisit in seq_along(lTheirs$rows)) {
      lTheirRow <- lTheirs$rows[[iVisit]]
      strLabel <- paste(lCase$case, lTheirRow$by)
      expect_identical(lMine$rows$by[iVisit], lTheirRow$by, label = strLabel)
      expect_identical(lMine$rows$status[iVisit], lTheirRow$status, label = paste(strLabel, "status"))
      for (strNumber in c("p_unadjusted", "p_value")) {
        if (is.null(lTheirRow[[strNumber]])) {
          expect_true(is.na(lMine$rows[[strNumber]][iVisit]), label = paste(strLabel, strNumber))
        } else {
          # To 1 part in 10^8, the tolerance bio.viz holds the chart to.
          expect_equal(lMine$rows[[strNumber]][iVisit], lTheirRow[[strNumber]], tolerance = 1e-8, label = paste(strLabel, strNumber))
        }
      }
    }
    # As the page stores it, it is the shape bio.viz's connection reads.
    expect_identical(names(StoredValue(lMine)), names(lTheirs), label = paste(lCase$case, "members"))
    expect_identical(names(StoredValue(lMine)$rows[[1]]), names(lTheirs$rows[[1]]), label = paste(lCase$case, "a row's members"))
  }
  # Groups too small to test at a visit are said, not hidden.
  expect_true("too_small" %in% vapply(lRecorded, function(lResult) lResult$value$status, character(1)))
})

test_that("the level drawn is the chart's: tiles, one biomarker over time, or its visits as panels (#53)", {
  chrAll <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
  State <- function(strMeasure, chrVisits, strValueType = "change") list(measure = strMeasure, visits = chrVisits, value_type = strValueType)
  expect_identical(chrGroupComparisonLevels, c("biomarkers", "over-time", "visits"))
  # No biomarker chosen: every biomarker, whatever the visits.
  expect_identical(GroupComparison_Level(State(NULL, chrAll), chrAll), "biomarkers")
  # A biomarker and every visit it has: over time. Fewer: panels.
  expect_identical(GroupComparison_Level(State("IL-6", chrAll), chrAll), "over-time")
  expect_identical(GroupComparison_Level(State("IL-6", chrAll[2:5]), chrAll), "visits")
  expect_identical(GroupComparison_Level(State("IL-6", "Week 4"), chrAll), "visits")
  # A visit chosen that the biomarker lacks does not count against it.
  expect_identical(GroupComparison_Level(State("IL-6", chrAll), chrAll[1:3]), "over-time")
  # One visit is not a picture over time, and a baseline value has no visit.
  expect_identical(GroupComparison_Level(State("IL-6", "Baseline"), "Baseline"), "visits")
  expect_identical(GroupComparison_Level(State("IL-6", chrAll, "baseline"), chrAll), "visits")

  # Over time asks R once, and only at that level.
  lTables <- lDemo()
  OverTime <- function(lMore = list(), strMeasure = "IL-6") {
    lConfig <- GroupComparison_Settings(c(lTables$settings[setdiff(names(lTables$settings), c("visits", names(lMore)))], lMore))
    lState <- GroupComparison_State(lTables$results, lTables$participants, lConfig)
    lState["measure"] <- list(strMeasure)
    GroupComparison_OverTimeRequests(lTables$results, lTables$participants, lConfig, lState)
  }
  expect_length(OverTime(), 1L)
  expect_identical(OverTime()[[1]]$dataId$visits, as.list(chrAll[-1]))
  expect_identical(OverTime(list(value_type = "raw"))[[1]]$dataId$visits, as.list(chrAll))
  # Against two baseline visits a change is tested at every visit.
  expect_identical(OverTime(list(baseline_visits = c("Baseline", "Week 2")))[[1]]$dataId$visits, as.list(chrAll))
  expect_identical(OverTime(list(visits = c("Week 4", "Week 8"))), list())
  expect_identical(OverTime(list(value_type = "baseline")), list())
  expect_identical(OverTime(strMeasure = NULL), list())
  # No test chosen, no function named for either line, or one group: nothing asked.
  expect_identical(OverTime(list(test = "none")), list())
  expect_identical(OverTime(list(statistic_by_visit = NULL)), list())
  expect_identical(OverTime(list(statistic = NULL)), list())
  expect_identical(OverTime(list(levels = "Placebo")), list())
  # The picture takes no second grouping and no panels: neither is in the key.
  lPlain <- OverTime()[[1]]
  lGrouped <- OverTime(list(color_by = "SEX", panel_by = "SEX", pairwise = TRUE))[[1]]
  expect_identical(lGrouped[c("name", "args", "dataId", "rows")], lPlain[c("name", "args", "dataId", "rows")])
  # The test fits the groups drawn, and a logarithmic scale is in the identity.
  expect_identical(OverTime(list(group_by = "ARM_SEX"))[[1]]$args$strMethod, "anova")
  expect_true(OverTime(list(y_scale = "log", value_type = "raw"))[[1]]$dataId$positive_only)
  # A cut's groups are handed to R low to high, every group drawn.
  lCut <- OverTime(list(group_by = list(measure = "CRP", visit = "Baseline", cut = "tertiles")))[[1]]
  expect_identical(lCut$args$chrGroups, list("\u2264 2.167", "> 2.167, \u2264 3.467", "> 3.467"))
  expect_identical(names(lCut$args), c("strValueCol", "strGroupCol", "strByCol", "strMethod", "chrBy", "strPAdjust", "chrGroups"))
  expect_identical(lCut$args$strMethod, "anova")
})

# Unscheduled visits (#53): left out at every level unless switched on, by
# safety.viz's rule under safety.viz's setting names (bio.viz,
# src/core/unscheduled.js). The synthetic study has none, so these tests add
# rows of their own to a copy of it.

test_that("a visit is unscheduled by the list when there is one, and otherwise by the pattern, as bio.viz's rule reads them (#53)", {
  chrNames <- c("Unscheduled", "UNSCHEDULED 2", "Visit 3 (unscheduled)", "Early Termination", "early  termination", "Week 4", "Baseline")
  # The default: a name holding either word, in either case.
  expect_identical(lCoreUnscheduledDefaults$unscheduled_visit_pattern, "/unscheduled|early termination/i")
  expect_identical(Core_IsUnscheduled(chrNames, lCoreUnscheduledDefaults), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  # A list decides alone, by name: the pattern is then not read, and a list of
  # none names none.
  expect_identical(
    Core_IsUnscheduled(chrNames, list(unscheduled_visit_values = c("Week 4", "Nope"), unscheduled_visit_pattern = "/unscheduled/i")),
    chrNames == "Week 4"
  )
  expect_false(any(Core_IsUnscheduled(chrNames, list(unscheduled_visit_values = character(0), unscheduled_visit_pattern = "/unscheduled/i"))))
  # With neither, no visit is unscheduled.
  expect_false(any(Core_IsUnscheduled(chrNames, list(unscheduled_visit_pattern = NULL))))
  expect_false(any(Core_IsUnscheduled(chrNames, list())))
  # A plain source is read in the case it is written in; a word of nothing is
  # in every name, as an empty alternative is to a browser.
  expect_identical(Core_IsUnscheduled(chrNames, list(unscheduled_visit_pattern = "Week|Base")), chrNames %in% c("Week 4", "Baseline"))
  expect_false(any(Core_IsUnscheduled(chrNames, list(unscheduled_visit_pattern = "week"))))
  expect_true(all(Core_IsUnscheduled(chrNames, list(unscheduled_visit_pattern = "/week|/"))))
  # Only the letters A to Z match in either case, as in a browser without the
  # flag `u`: the long s is not an s.
  expect_false(Core_IsUnscheduled("Un\u017fcheduled", lCoreUnscheduledDefaults))
  # A pattern that could mean something else to a browser is not read.
  for (strPattern in c("/^un/i", "/unscheduled/g", "/un.*/", "/a(b)/", "/\\d/", "/a/b/")) {
    expect_null(Core_ReadPattern(strPattern), label = strPattern)
    expect_error(Core_IsUnscheduled(chrNames, list(unscheduled_visit_pattern = strPattern)), "unscheduled_visit_values", label = strPattern)
  }

  # The settings: each of the three, checked.
  expect_error(GroupComparison_Settings(list(unscheduled_visits = "yes")), "unscheduled_visits.*TRUE or FALSE")
  expect_error(GroupComparison_Settings(list(unscheduled_visit_pattern = 3)), "unscheduled_visit_pattern.*regular expression")
  expect_error(GroupComparison_Settings(list(unscheduled_visit_pattern = "/^un/i")), "Name the unscheduled visits in 'unscheduled_visit_values'")
  expect_error(GroupComparison_Settings(list(unscheduled_visit_values = list(TRUE))), "unscheduled_visit_values.*name")
  # A pattern R does not read is no matter when the visits are named.
  lNamed <- GroupComparison_Settings(list(unscheduled_visit_pattern = "/^un/i", unscheduled_visit_values = list("Unscheduled 1")))
  expect_identical(lNamed$unscheduled_visit_values, "Unscheduled 1")
  expect_identical(GroupComparison_Settings(list(unscheduled_visit_values = list()))$unscheduled_visit_values, character(0))
  # And the others the three levels add.
  expect_error(GroupComparison_Settings(list(time_mark = "line")), "time_mark.*must be one of box, mean_se, median_iqr")
  expect_error(GroupComparison_Settings(list(tile_summary = "max")), "tile_summary.*must be one of median, mean")
  expect_error(GroupComparison_Settings(list(tile_min_spread = -1)), "tile_min_spread.*zero or more")
  expect_error(GroupComparison_Settings(list(visit_adjustment = "bonferroni")), "visit_adjustment.*must be one of none, holm, BH")
  expect_error(GroupComparison_Settings(list(statistic_by_visit = "my_test")), "statistic_by_visit.*Analyze_GroupDifferenceBy")
})

test_that("the rows at unscheduled visits are set aside before anything is read, and a row with no visit is kept (#53)", {
  dfResults <- dfWithUnscheduled()
  lFound <- Core_Scheduled(dfResults, c(list(visit_col = "VISIT"), lCoreUnscheduledDefaults))
  expect_identical(lFound$visits, c("Unscheduled 1", "EARLY TERMINATION"))
  expect_identical(lFound$rows, sum(dfResults$VISIT %in% lFound$visits))
  expect_identical(nrow(lFound$results), nrow(Synthetic_Results))
  expect_identical(unique(lFound$results$VISIT), unique(Synthetic_Results$VISIT))
  # Nothing to set aside: the table itself.
  expect_identical(Core_Scheduled(Synthetic_Results, c(list(visit_col = "VISIT"), lCoreUnscheduledDefaults))$results, Synthetic_Results)
  # A row with no visit is kept.
  dfBlank <- dfResults
  dfBlank$VISIT[1:3] <- c(NA, "", "  ")
  expect_identical(nrow(Core_Scheduled(dfBlank, c(list(visit_col = "VISIT"), lCoreUnscheduledDefaults))$results), nrow(Synthetic_Results))

  # What the chart draws: the scheduled visits, or every visit when switched on.
  lOff <- GroupComparison_Settings(list(value_type = "change", group_by = "ARM"))
  lOn <- GroupComparison_Settings(list(value_type = "change", group_by = "ARM", unscheduled_visits = TRUE))
  chrScheduled <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
  expect_identical(GroupComparison_Visits(dfResults, lOff)$all, chrScheduled)
  expect_identical(
    GroupComparison_Visits(dfResults, lOn)$all,
    c("Unscheduled 1", "Baseline", "Week 2", "Week 4", "EARLY TERMINATION", "Week 8", "Week 12")
  )
  # The unscheduled visits with a result to draw, in visit order; the identity
  # says they are among the rows only when they are drawn.
  expect_identical(Chart_Unscheduled(dfResults, lOff)[c("visits", "drawn")], list(visits = c("Unscheduled 1", "EARLY TERMINATION"), drawn = FALSE))
  expect_true(Chart_Unscheduled(dfResults, lOn)$drawn)
  expect_false(Chart_Unscheduled(Synthetic_Results, lOn)$drawn)
  # Named in a list, the visits are those and no others.
  lListed <- GroupComparison_Settings(list(unscheduled_visit_values = "Week 2"))
  expect_identical(GroupComparison_Visits(dfResults, lListed)$all[1:3], c("Unscheduled 1", "Baseline", "Week 4"))
  # A chart without the rule's settings draws every row.
  expect_identical(Chart_Unscheduled(dfResults, list(visit_col = "VISIT"))$results, dfResults)

  # The baseline a change is measured from is the first visit drawn: Baseline
  # with the unscheduled visits left out, though one of them sorts before it.
  expect_identical(Widget_NameBaseline(lOff, list(), dfResults)$config$baseline_visits, "Baseline")
  expect_identical(Widget_NameBaseline(lOn, list(), dfResults)$config$baseline_visits, "Unscheduled 1")
  # And the visits a biomarker has, of those drawn.
  expect_identical(GroupComparison_MeasureVisits(Chart_Unscheduled(dfResults, lOff)$results, lOff, "IL-6"), chrScheduled)
  expect_identical(GroupComparison_MeasureVisits(dfResults, lOff, "NOPE"), character(0))
})

test_that("a visit a biomarker has no value at is not one of its panels, and does not stop its picture over time (#53)", {
  lTables <- lDemo()
  # CRP has no result at Week 8 in this copy.
  dfResults <- lTables$results[!(lTables$results$TEST == "CRP" & lTables$results$VISIT == "Week 8"), ]
  lConfig <- GroupComparison_Settings(lTables$settings[setdiff(names(lTables$settings), "visits")])
  lState <- GroupComparison_State(dfResults, lTables$participants, lConfig)
  lState$measure <- "CRP"
  expect_identical(lState$visits, c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12"))
  expect_identical(GroupComparison_MeasureVisits(dfResults, lConfig, "CRP"), c("Baseline", "Week 2", "Week 4", "Week 12"))
  lModel <- GroupComparison_Panels(dfResults, lTables$participants, lConfig, lState)
  expect_identical(unlist(lapply(lModel$panels, `[[`, "visit")), c("Week 2", "Week 4", "Week 12"))
  lOverTime <- GroupComparison_OverTimeRequests(dfResults, lTables$participants, lConfig, lState)
  expect_length(lOverTime, 1L)
  expect_identical(lOverTime[[1]]$dataId$visits, list("Week 2", "Week 4", "Week 12"))
  # The picture keeps the baseline visit of a change, which the panels leave out.
  lPicture <- GroupComparison_Panels(dfResults, lTables$participants, lConfig, lState, bKeepBaseline = TRUE)
  expect_identical(unlist(lapply(lPicture$panels, `[[`, "visit")), c("Baseline", "Week 2", "Week 4", "Week 12"))
  expect_identical(lPicture$baseline_visits, "Baseline")
})
