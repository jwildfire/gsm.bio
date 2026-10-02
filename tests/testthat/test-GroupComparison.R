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
  # The setting the widget makes itself, and the function it stores results of.
  expect_true("connection" %in% names(lBundle))
  expect_identical(lBundle$statistic, strGroupComparisonStatistic)
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
