# What the association scatter draws and asks R, worked out in R (#13). The
# widget stores R's answers ahead of time, so it has to know which rows the
# chart will hand R for each panel and how the chart will ask. Both are held to
# bio.viz's own fixtures, copied with their record: the rows its core wrote for
# panels of its demo chart (twenty-four when this was written; every case the
# fixture lists is run), and the request the chart makes for each.

strAssociationFixture <- function(...) {
  testthat::test_path("fixtures", "bio.viz", ...)
}

# The cases as bio.viz lists them: one panel of its demo in one view each, asked
# for its coefficient or for its fitted line.
dfScatterCases <- function() {
  utils::read.csv(
    strAssociationFixture("association-statistics", "cases.csv"),
    colClasses = "character", na.strings = "", check.names = FALSE
  )
}

lRecordedScatterRequests <- function() {
  lResults <- lReadJson(strAssociationFixture("association-statistics-r.json"))$results
  stats::setNames(lResults, vapply(lResults, function(lResult) lResult$case, character(1)))
}

# The tables and the settings of bio.viz's demo page for this chart.
lScatterDemo <- function() {
  lLabelled <- list(
    list(value_col = "ARM", label = "Arm"),
    list(value_col = "SEX", label = "Sex"),
    list(value_col = "RESPONSE", label = "Response")
  )
  list(
    results = Synthetic_Results,
    participants = Synthetic_Participants,
    settings = list(
      x = list(measure = "TNF-alpha", visit = "Baseline"),
      y = list(measure = "IL-10", visit = "Baseline"),
      baseline_visits = "Baseline",
      color_by = "ARM",
      fit = "linear",
      groups = lLabelled,
      filters = lLabelled,
      numbers = list(list(value_col = "AGE", label = "Age"), list(value_col = "BMIBL", label = "BMI at baseline"))
    )
  )
}

# What a case says of its view, as the settings and the controls R reads.
lScatterCaseView <- function(lCase) {
  One <- function(strValue) if (is.na(strValue)) NULL else strValue
  Several <- function(strValue) if (is.na(strValue)) NULL else strsplit(strValue, "|", fixed = TRUE)[[1]]
  Axis <- function(strAxis) {
    if (!is.na(lCase[[paste0(strAxis, "_col")]])) {
      return(list(col = lCase[[paste0(strAxis, "_col")]]))
    }
    lAxis <- list(measure = lCase[[paste0(strAxis, "_measure")]], value = lCase[[paste0(strAxis, "_value")]])
    if (!is.na(lCase[[paste0(strAxis, "_visit")]])) lAxis$visit <- lCase[[paste0(strAxis, "_visit")]]
    lAxis
  }
  lFilters <- list()
  if (!is.na(lCase$filters)) {
    for (chrPart in strsplit(strsplit(lCase$filters, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)) {
      lFilters[[chrPart[1]]] <- Several(chrPart[2])
    }
  }
  lTables <- lScatterDemo()
  lSettings <- lTables$settings
  lSettings$baseline_visits <- Several(lCase$baseline_visits)
  lSettings$baseline_stat <- lCase$baseline_stat
  bLine <- lCase$kind == "fit"
  list(
    results = lTables$results,
    participants = lTables$participants,
    config = AssociationScatter_Settings(lSettings),
    state = list(
      x = Axis("x"), y = Axis("y"), color_by = One(lCase$color_by), panel_by = One(lCase$panel_by),
      x_scale = lCase$x_scale, y_scale = lCase$y_scale,
      fit = if (bLine) lCase$method else "none", method = if (bLine) "pearson" else lCase$method,
      filters = lFilters
    ),
    panel = One(lCase$panel)
  )
}

# The request R works out for a case: its panel's coefficient, or its line.
lScatterCaseRequest <- function(lCase) {
  lView <- lScatterCaseView(lCase)
  lRequests <- AssociationScatter_Requests(lView$results, lView$participants, lView$config, lView$state)
  lFound <- Filter(function(lRequest) {
    identical(lRequest$dataId$panel, lView$panel) && lRequest$name == lCase$statistic
  }, lRequests)
  expect_identical(length(lFound), 1L, label = paste(lCase$case, "has one request of its kind for its panel"))
  lFound[[1]]
}

test_that("the settings R reads have the defaults of the vendored association scatter (#13)", {
  lBundle <- lBundleDefaults("fit_statistic")
  expect_false(is.null(lBundle), label = "the bundle's chart defaults were found")
  expect_true(all(names(lAssociationScatterDefaults) %in% names(lBundle)))
  for (strSetting in names(lAssociationScatterDefaults)) {
    expect_identical(
      lAssociationScatterDefaults[[strSetting]], lBundle[[strSetting]],
      label = paste("R's default for", strSetting), expected.label = "the bundle's"
    )
  }
  # The settings the widget makes itself or refuses, and the functions it stores results of.
  expect_true(all(c("connection", "back") %in% names(lBundle)))
  expect_identical(lBundle$statistic, strAssociationScatterStatistic)
  expect_identical(lBundle$fit_statistic, strAssociationScatterFitStatistic)
  expect_identical(AssociationScatter_Settings()[names(lAssociationScatterDefaults)], lAssociationScatterDefaults)
  expect_identical(lCoreDefaults, lAssociationScatterDefaults[names(lCoreDefaults)])
})

test_that("R resolves the rows bio.viz's scatter hands R, for every panel of its demo bio.viz recorded (#13)", {
  dfCases <- dfScatterCases()
  # Every case bio.viz lists is run, however many it comes to list.
  expect_gte(nrow(dfCases), 24L)
  expect_identical(anyDuplicated(dfCases$case), 0L)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    dfTheirs <- utils::read.csv(
      strAssociationFixture("association-statistics", lCase$file),
      colClasses = "character", na.strings = character(0), check.names = FALSE
    )
    dfMine <- lScatterCaseRequest(lCase)$data

    expect_identical(names(dfMine), names(dfTheirs), label = paste(lCase$case, "columns"))
    expect_identical(nrow(dfMine), nrow(dfTheirs), label = paste(lCase$case, "rows"))
    # The files hold the values themselves. On a logarithmic axis R is handed
    # their base-10 logarithm, taken in R from the value: so it is here.
    for (strAxis in c("x", "y")) {
      nTheirs <- as.numeric(dfTheirs[[strAxis]])
      if (lCase[[paste0(strAxis, "_scale")]] == "log") {
        nTheirs <- log10(nTheirs)
      }
      expect_equal(dfMine[[strAxis]], nTheirs, tolerance = 1e-12, label = paste(lCase$case, strAxis))
    }
    for (strColumn in setdiff(names(dfTheirs), c("x", "y"))) {
      expect_identical(Core_Text(dfMine[[strColumn]]), dfTheirs[[strColumn]], label = paste(lCase$case, strColumn))
    }
  }

  # The cases reach a colour, a panel, a filter, a logarithmic axis on one side
  # and on both, a participant-level number, a change from baseline, and both
  # coefficients and both lines.
  expect_setequal(dfCases$method, c("pearson", "spearman", "linear", "smooth"))
  expect_true(all(c("SEX=F", "AGE=57", "AGE=35") %in% dfCases$filters))
  expect_true("log" %in% dfCases$x_scale && "log" %in% dfCases$y_scale && any(dfCases$x_scale == "log" & dfCases$y_scale == "linear"))
  expect_true("AGE" %in% dfCases$x_col && "change" %in% dfCases$y_value)
  expect_true("ARM" %in% dfCases$color_by && "SEX" %in% dfCases$panel_by)
})

test_that("R keys a scatter's stored result exactly as the chart keys its request (#13)", {
  dfCases <- dfScatterCases()
  lRecorded <- lRecordedScatterRequests()
  expect_setequal(names(lRecorded), dfCases$case)

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lMine <- lScatterCaseRequest(lCase)
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

  # A coefficient and a line of the same panel share an identity and differ in
  # the function and its method; no two recorded requests share a key.
  chrKeys <- vapply(lRecorded, function(lResult) Chart_KeyText(lResult[c("name", "args", "dataId")]), character(1))
  expect_identical(anyDuplicated(chrKeys), 0L)
  expect_identical(lRecorded$pearson$dataId, lRecorded$linear$dataId)
  expect_false(identical(lRecorded$pearson$dataId, lRecorded$`pearson-log`$dataId))
})

test_that("R's answers on those rows are the answers bio.viz recorded from desktop R (#13)", {
  dfCases <- dfScatterCases()
  lRecorded <- lRecordedScatterRequests()

  for (iCase in seq_len(nrow(dfCases))) {
    lCase <- as.list(dfCases[iCase, ])
    lRequest <- lScatterCaseRequest(lCase)
    lMine <- do.call(lRequest$name, c(list(lRequest$data), lRequest$args))
    lTheirs <- lRecorded[[lCase$case]]$value

    ExpectResultShape(lMine)
    expect_identical(lMine$status, lTheirs$status, label = paste(lCase$case, "status"))
    expect_identical(lMine$counts, lTheirs$counts, label = paste(lCase$case, "counts"))
    if (lMine$status != "ok") {
      expect_identical(lMine$reason, lTheirs$reason, label = paste(lCase$case, "reason"))
      next
    }
    # To 1 part in 10^8, the tolerance bio.viz holds the chart to. The
    # estimates are held for every case; a p-value where R's own answer does
    # not depend on its version (Spearman's with ties does).
    expect_equal(
      lMine$estimates$estimate, vapply(lTheirs$estimates, function(lRow) lRow$estimate, numeric(1)),
      tolerance = 1e-8, label = paste(lCase$case, "estimates")
    )
    if (lCase$method %in% c("pearson", "linear")) {
      expect_equal(lMine$p_value, lTheirs$p_value, tolerance = 1e-8, label = paste(lCase$case, "p_value"))
    }
    if (lCase$kind == "fit") {
      # The line's points, where the fit was made: null in the file is NA here.
      Column <- function(strColumn) {
        vapply(lTheirs$rows, function(lRow) if (is.null(lRow[[strColumn]])) NA_real_ else as.numeric(lRow[[strColumn]]), numeric(1))
      }
      expect_identical(nrow(lMine$rows), length(lTheirs$rows), label = paste(lCase$case, "points"))
      for (strColumn in c("x", "fit", "lower", "upper")) {
        expect_equal(lMine$rows[[strColumn]], Column(strColumn), tolerance = 1e-8, label = paste(lCase$case, strColumn))
      }
    }
  }
  expect_true("too_small" %in% vapply(lRecorded, function(lResult) lResult$value$status, character(1)))
})

test_that("the scatter's axes open on the pair the settings name, or the first two biomarkers (#13)", {
  lPlain <- AssociationScatter_Settings()
  lTables <- lScatterDemo()

  # The numbers an axis can take: columns that are all numbers, with more than
  # one value; the setting `numbers` is the list when given.
  expect_identical(Chart_Numbers(Synthetic_Results, Synthetic_Participants, lPlain), c("AGE", "BMIBL"))
  expect_identical(
    Chart_Numbers(Synthetic_Results, Synthetic_Participants, AssociationScatter_Settings(list(numbers = "AGE"))),
    "AGE"
  )
  dfAlone <- Synthetic_Results
  dfAlone$AGE <- Synthetic_Participants$AGE[match(dfAlone$USUBJID, Synthetic_Participants$USUBJID)]
  dfAlone$ARM <- Synthetic_Participants$ARM[match(dfAlone$USUBJID, Synthetic_Participants$USUBJID)]
  dfAlone$ONE <- 1
  expect_identical(Chart_Numbers(dfAlone, NULL, lPlain), "AGE")
  # A column the participant table has is the participant table's, numbers or not.
  expect_identical(Chart_Numbers(dfAlone, Synthetic_Participants["USUBJID"], lPlain), c("AGE"))
  expect_identical(Chart_Numbers(dfAlone, Synthetic_Participants[c("USUBJID", "ARM", "AGE")], lPlain), "AGE")

  # With nothing named: the first two biomarkers at the first visit, as results.
  lState <- AssociationScatter_State(Synthetic_Results, Synthetic_Participants, lPlain)
  expect_identical(lState$x, list(measure = "CRP", value = "raw", visit = "Baseline"))
  expect_identical(lState$y, list(measure = "D-dimer", value = "raw", visit = "Baseline"))
  expect_null(lState$color_by)
  expect_identical(lState[c("x_scale", "y_scale", "fit", "method")], list(x_scale = "linear", y_scale = "linear", fit = "none", method = "pearson"))
  expect_identical(names(lState$filters), c("ARM", "SEX", "RESPONSE"))

  # The pair the settings name, written in full; a baseline value has no visit.
  lDemoState <- AssociationScatter_State(lTables$results, lTables$participants, AssociationScatter_Settings(lTables$settings))
  expect_identical(lDemoState$x, list(measure = "TNF-alpha", value = "raw", visit = "Baseline"))
  expect_identical(lDemoState$y, list(measure = "IL-10", value = "raw", visit = "Baseline"))
  expect_identical(lDemoState$color_by, "ARM")
  expect_identical(lDemoState$fit, "linear")
  lOther <- AssociationScatter_Settings(list(
    x = list(col = "AGE"), y = list(measure = "IL-6", value = "baseline"), color_by = "NOPE"
  ))
  lOtherState <- AssociationScatter_State(Synthetic_Results, Synthetic_Participants, lOther)
  expect_identical(lOtherState$x, list(col = "AGE"))
  expect_identical(lOtherState$y, list(measure = "IL-6", value = "baseline"))
  expect_null(lOtherState$color_by)
  # A variable the tables do not have gives way to the chart's own choice.
  lMissing <- AssociationScatter_Settings(list(
    x = list(measure = "NOPE", visit = "Baseline"), y = list(measure = "IL-6", visit = "Week 99")
  ))
  lMissingState <- AssociationScatter_State(Synthetic_Results, Synthetic_Participants, lMissing)
  expect_identical(lMissingState$x, lState$x)
  expect_identical(lMissingState$y, lState$y)
  # One biomarker: y is that biomarker at the second visit.
  dfOne <- Synthetic_Results[Synthetic_Results$TEST == "IL-6", ]
  expect_identical(
    AssociationScatter_State(dfOne, NULL, lPlain)[c("x", "y")],
    list(x = list(measure = "IL-6", value = "raw", visit = "Baseline"), y = list(measure = "IL-6", value = "raw", visit = "Week 2"))
  )
})

test_that("the scatter asks for a coefficient and a line per panel, and for nothing where nothing is drawn (#13)", {
  lTables <- lScatterDemo()
  Requests <- function(lMore) {
    lConfig <- AssociationScatter_Settings(c(lTables$settings[setdiff(names(lTables$settings), names(lMore))], lMore))
    AssociationScatter_Requests(
      lTables$results, lTables$participants, lConfig,
      AssociationScatter_State(lTables$results, lTables$participants, lConfig)
    )
  }
  Said <- function(lRequests) vapply(lRequests, function(lRequest) paste(lRequest$name, lRequest$args$strMethod), character(1))

  # The demo's view: Pearson's coefficient and a linear fit, within each arm too.
  lOpening <- Requests(list())
  expect_identical(Said(lOpening), c("Analyze_Correlation pearson", "Analyze_Fit linear"))
  expect_identical(lOpening[[1]]$args, list(strXCol = "x", strYCol = "y", strMethod = "pearson", strGroupCol = "color"))
  expect_identical(lOpening[[1]]$dataId, lOpening[[2]]$dataId)
  expect_identical(names(lOpening[[1]]$data), c("USUBJID", "x", "y", "color"))
  # No colour: no group argument, and no colour in the identity.
  lPlainView <- Requests(list(color_by = NULL))
  expect_identical(lPlainView[[1]]$args, list(strXCol = "x", strYCol = "y", strMethod = "pearson"))
  expect_false(any(c("color_by", "groups") %in% names(lPlainView[[1]]$dataId)))
  # The line y = x and no line ask R for nothing but the coefficient.
  expect_identical(Said(Requests(list(fit = "identity"))), "Analyze_Correlation pearson")
  expect_identical(Said(Requests(list(fit = "none", method = "spearman"))), "Analyze_Correlation spearman")
  # No function named for one, and that one is not asked for.
  expect_identical(Said(Requests(list(statistic = NULL))), "Analyze_Fit linear")
  expect_identical(Said(Requests(list(fit_statistic = NULL))), "Analyze_Correlation pearson")
  # A panel column: each panel asks for itself.
  lPanels <- Requests(list(panel_by = "SEX", fit = "smooth"))
  expect_identical(Said(lPanels), rep(c("Analyze_Correlation pearson", "Analyze_Fit smooth"), 2))
  expect_identical(vapply(lPanels, function(lRequest) lRequest$dataId$panel, character(1)), c("F", "F", "M", "M"))
  expect_identical(sum(vapply(lPanels[c(1, 3)], function(lRequest) lRequest$rows, integer(1))), 200L)

  # On a logarithmic axis R is handed the logarithm, and the identity says so.
  lLog <- Requests(list(x = list(measure = "CRP", visit = "Baseline"), y = list(measure = "IFN-gamma", visit = "Baseline"), x_scale = "log"))
  lLinear <- Requests(list(x = list(measure = "CRP", visit = "Baseline"), y = list(measure = "IFN-gamma", visit = "Baseline")))
  expect_identical(lLog[[1]]$dataId$x_scale, "log")
  expect_false("y_scale" %in% names(lLog[[1]]$dataId))
  expect_false("x_scale" %in% names(lLinear[[1]]$dataId))
  expect_identical(lLog[[1]]$data$x, log10(lLinear[[1]]$data$x))
  expect_identical(lLog[[1]]$data$y, lLinear[[1]]$data$y)
  # Zero or less has no place on a logarithmic axis: those participants are left out.
  lChange <- list(x = list(measure = "IL-6", visit = "Week 4", value = "change"), y = list(measure = "IL-10", visit = "Week 4"))
  nPositive <- sum(Requests(lChange)[[1]]$data$x > 0)
  expect_lt(nPositive, Requests(lChange)[[1]]$rows)
  expect_identical(Requests(c(lChange, list(x_scale = "log")))[[1]]$rows, nPositive)

  # A change read at the one baseline visit is the same for everyone: nothing
  # is drawn and nothing is asked. A filter that lets nobody through, the same.
  expect_identical(Requests(list(x = list(measure = "IL-6", visit = "Baseline", value = "change"))), list())
  expect_gt(length(Requests(list(x = list(measure = "IL-6", visit = "Baseline", value = "change"), baseline_visits = c("Baseline", "Week 2")))), 0)
  expect_identical(Requests(list(filters = list(list(value_col = "SEX", start = "X")))), list())
})
