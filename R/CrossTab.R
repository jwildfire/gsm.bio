# What bio.viz's cross-tabulation draws and asks R, worked out in R.
#
# The chart draws a two-way table of counts and asks its connection to R once
# for a test of it, chi-square or Fisher's exact (bio.viz, docs/cross-tab.md,
# "What R is asked"). Either variable is a column, or a biomarker or a number
# cut by the shared cut rule. To ship R's answer with a page,
# Widget_CrossTab() has to know, before there is a page, the table the chart
# draws and how it asks. This file follows the chart's own code for that
# (bio.viz, src/cross-tab/): the settings R reads, what the controls open on,
# the table, and the request (bio.viz's `cross_tab_key`, tools/r-cross-tab.R).
# What every chart shares is R/chart.R, and the cut rule is R/core.R.
#
# It computes no statistic: the rows come from Core_Frame(), and the answer is
# Analyze_Contingency() on them. Nothing here is exported.

# The settings of the chart that R reads, with the chart's own defaults. The
# chart has more (its listing, the profile); those pass through to the page
# untouched. tests/testthat/test-CrossTab.R holds these to the vendored bundle.
lCrossTabDefaults <- list(
  id_col = "USUBJID",
  measure_col = "TEST",
  value_col = "STRESN",
  visit_col = "VISIT",
  visit_order_col = "VISITNUM",
  unit_col = "STRESU",
  participant_id_col = NULL,
  baseline_visits = NULL,
  baseline_stat = "mean",
  row_by = NULL,
  col_by = NULL,
  percent = "row",
  cuts = NULL,
  groups = NULL,
  max_levels = 12L,
  filters = NULL,
  statistic = "Analyze_Contingency",
  test = "chisq"
)

# The one R function the widget stores results of, and the tests it asks for.
strCrossTabStatistic <- "Analyze_Contingency"
chrCrossTabTests <- c("chisq", "fisher")

# The settings R reads, in full: the caller's over the chart's defaults, each
# checked, in the forms the functions below read them.
CrossTab_Settings <- function(lSettings = list()) {
  lConfig <- Core_Overlay(lCrossTabDefaults, lSettings[intersect(names(lSettings), names(lCrossTabDefaults))])
  IsName <- function(xValue) is.character(xValue) && length(xValue) == 1L && !is.na(xValue) && nzchar(trimws(xValue))
  for (strKey in c("id_col", "measure_col", "value_col", "visit_col")) {
    if (!IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single column name (a character string)")
    }
  }
  for (strKey in c("visit_order_col", "unit_col", "participant_id_col")) {
    if (!is.null(lConfig[[strKey]]) && !IsName(lConfig[[strKey]])) {
      Core_Stop("Setting '", strKey, "' must be a single name (a character string), or NULL")
    }
  }
  # The rows and the columns: a column, or a biomarker or a number cut into
  # groups by the shared cut rule.
  for (strKey in c("row_by", "col_by")) {
    lConfig[strKey] <- list(Chart_Grouping(lConfig[[strKey]], strKey))
  }
  if (!is.null(lConfig$cuts)) {
    if (!is.list(lConfig$cuts) || is.data.frame(lConfig$cuts)) {
      Core_Stop("Setting 'cuts' must be a list of cut variables, or NULL")
    }
    lConfig$cuts <- lapply(seq_along(lConfig$cuts), function(iCut) {
      if (!is.list(lConfig$cuts[[iCut]])) {
        Core_Stop("Setting 'cuts[", iCut, "]' must be a cut variable: list(measure, visit, cut) or list(col, type = 'number', cut)")
      }
      Chart_Grouping(lConfig$cuts[[iCut]], paste0("cuts[", iCut, "]"))
    })
  }
  Choice <- function(strKey, chrChoices) {
    if (!IsName(lConfig[[strKey]]) || !lConfig[[strKey]] %in% chrChoices) {
      Core_Stop("Setting '", strKey, "' must be one of ", paste(chrChoices, collapse = ", "))
    }
  }
  Choice("percent", c("row", "col", "none"))
  Choice("test", c(chrCrossTabTests, "none"))
  Choice("baseline_stat", chrCoreBaselineStats)
  nMaxLevels <- lConfig$max_levels
  if (!(is.numeric(nMaxLevels) && length(nMaxLevels) == 1L && !is.na(nMaxLevels) && nMaxLevels >= 1 && nMaxLevels == round(nMaxLevels))) {
    Core_Stop("Setting 'max_levels' must be a whole number, one or more")
  }
  if (!is.null(lConfig$statistic) && !identical(lConfig$statistic, strCrossTabStatistic)) {
    Core_Stop(
      "Setting 'statistic' must be '", strCrossTabStatistic, "', the function the widget stores results of, ",
      "or NULL for no statistics line"
    )
  }
  lConfig["baseline_visits"] <- list(Chart_Names(lConfig$baseline_visits, "baseline_visits"))
  for (strKey in c("groups", "filters")) {
    lConfig[strKey] <- list(Chart_Fields(lConfig[[strKey]], strKey))
  }
  lConfig
}

# What the controls open on: the rows and the columns the settings name, where
# the tables have them (a cut variable is offered as it is named); otherwise
# the first category column for the rows and the next for the columns.
CrossTab_State <- function(dfResults, dfParticipants, lConfig) {
  dfCategories <- Chart_Categories(dfResults, dfParticipants, lConfig)
  for (strKey in c("row_by", "col_by")) {
    Chart_CheckCut(lConfig[[strKey]], strKey, dfResults, dfParticipants, lConfig)
  }
  Opening <- function(xBy, xFallback) {
    if (Chart_IsCut(xBy)) {
      return(xBy)
    }
    if (!is.null(xBy) && xBy %in% dfCategories$value_col) {
      return(xBy)
    }
    xFallback
  }
  xRowBy <- Opening(lConfig$row_by, Core_First(dfCategories$value_col))
  if (length(xRowBy) == 0L) xRowBy <- NULL
  chrOther <- dfCategories$value_col[!vapply(dfCategories$value_col, identical, logical(1), xRowBy)]
  xColBy <- Opening(lConfig$col_by, Core_First(chrOther))
  if (length(xColBy) == 0L) xColBy <- NULL
  list(
    row_by = xRowBy,
    col_by = xColBy,
    percent = lConfig$percent,
    test = lConfig$test,
    filters = Chart_Filters(dfParticipants, lConfig, dfCategories)
  )
}

# A column's category as the table takes it: as text, the chart's String(),
# with text that is only white space blanked, as nothing was written.
CrossTab_Category <- function(xValue) {
  chrText <- Core_Text(xValue)
  chrText[Core_IsBlank(chrText)] <- NA_character_
  chrText
}

# The table the chart draws (bio.viz, src/cross-tab/structureData.js,
# `buildTable`): one record per participant with a category each way, the id,
# `row` and `col` as text (a cut's as its group's label); the categories each
# way, a cut's low to high and a column's by name with numbers as numbers
# (Core_Levels()), those with someone in them; and the counts. `filtered` is how many participants pass the filters,
# NULL with no participant table.
CrossTab_Table <- function(dfResults, dfParticipants, lConfig, lState) {
  lKept <- Chart_KeepFiltered(dfResults, dfParticipants, lConfig, lState$filters)
  nFiltered <- if (is.null(lKept$participants)) NULL else nrow(lKept$participants)
  lEmpty <- list(
    records = data.frame(), row_levels = character(0), col_levels = character(0),
    counts = matrix(integer(0), 0L, 0L), total = 0L, filtered = nFiltered
  )
  if (nrow(lKept$results) == 0L || is.null(lState$row_by) || is.null(lState$col_by)) {
    return(lEmpty)
  }
  lCuts <- list()
  for (strField in c("row", "col")) {
    xBy <- if (strField == "row") lState$row_by else lState$col_by
    if (Chart_IsCut(xBy)) {
      lCuts[[strField]] <- Chart_Cut(dfResults, dfParticipants, lConfig, lState$filters, xBy)
    }
  }
  dfData <- Core_Frame(
    lKept$results, lKept$participants,
    list(row = Chart_GroupingVariable(lState$row_by), col = Chart_GroupingVariable(lState$col_by)),
    Chart_CoreSettings(lConfig)
  )$data
  dfRecords <- data.frame(id = Core_Text(dfData[[lConfig$id_col]]), stringsAsFactors = FALSE)
  names(dfRecords) <- lConfig$id_col
  for (strField in c("row", "col")) {
    dfRecords[[strField]] <- if (!is.null(lCuts[[strField]])) {
      Core_CutGroups(dfData[[strField]], lCuts[[strField]]$points)
    } else {
      CrossTab_Category(dfData[[strField]])
    }
  }
  dfRecords <- dfRecords[!is.na(dfRecords$row) & !is.na(dfRecords$col), , drop = FALSE]
  rownames(dfRecords) <- NULL
  Levels <- function(strField) {
    if (!is.null(lCuts[[strField]])) {
      return(lCuts[[strField]]$labels[lCuts[[strField]]$labels %in% dfRecords[[strField]]])
    }
    # A column's categories by name, numbers as numbers: the order bio.viz
    # draws them in (its `categoryOrder`, this rule) and asks for them in.
    Core_Levels(dfRecords[[strField]])
  }
  chrRows <- Levels("row")
  chrCols <- Levels("col")
  nCounts <- unclass(table(factor(dfRecords$row, levels = chrRows), factor(dfRecords$col, levels = chrCols)))
  nCounts <- matrix(as.integer(nCounts), nrow = length(chrRows), dimnames = NULL)
  list(
    records = dfRecords, row_levels = chrRows, col_levels = chrCols, counts = nCounts,
    total = as.integer(sum(nCounts)), filtered = nFiltered
  )
}

# Whether a variable of the table is a cut biomarker that reads a baseline: its
# value is a baseline, or a change from one.
CrossTab_ReadsBaseline <- function(xBy) {
  Chart_IsCut(xBy) && !is.null(xBy$measure) && !identical(xBy$value, "raw")
}

# What the chart asks R for the table, with one test: bio.viz's
# `cross_tab_key` recipe (tools/r-cross-tab.R). The baseline settings are in the
# identity only when a cut biomarker reads a baseline.
CrossTab_Key <- function(lTable, lView) {
  lDataId <- list(chart = "cross-tab", row_by = lView$row_by, col_by = lView$col_by)
  if (CrossTab_ReadsBaseline(lView$row_by) || CrossTab_ReadsBaseline(lView$col_by)) {
    if (!is.null(lView$baseline_visits)) lDataId$baseline_visits <- as.list(lView$baseline_visits)
    lDataId$baseline_stat <- lView$baseline_stat
  }
  if (length(lView$filters) > 0L) {
    lDataId$filters <- lapply(lView$filters, as.list)
  }
  list(
    name = lView$statistic,
    args = list(
      strRowCol = "row", strColCol = "col", strMethod = lView$test,
      chrRowGroups = as.list(lTable$row_levels), chrColGroups = as.list(lTable$col_levels)
    ),
    dataId = lDataId,
    rows = nrow(lTable$records)
  )
}

# The requests the chart makes for its table, one per test in `chrTests` (the
# Test control's, by default the one the controls are set to), or none: with no
# statistic, no test, nobody through the filters, or fewer than two categories
# either way.
CrossTab_Requests <- function(dfResults, dfParticipants, lConfig, lState, chrTests = lState$test) {
  chrTests <- setdiff(chrTests, "none")
  if (is.null(lConfig$statistic) || length(chrTests) == 0L) {
    return(list())
  }
  lTable <- CrossTab_Table(dfResults, dfParticipants, lConfig, lState)
  if (identical(lTable$filtered, 0L) || lTable$total == 0L ||
    length(lTable$row_levels) < 2L || length(lTable$col_levels) < 2L) {
    return(list())
  }
  lapply(chrTests, function(strTest) {
    lKey <- CrossTab_Key(lTable, list(
      statistic = lConfig$statistic, test = strTest, row_by = lState$row_by, col_by = lState$col_by,
      baseline_visits = lConfig$baseline_visits, baseline_stat = lConfig$baseline_stat,
      filters = Chart_FiltersInForce(lState$filters)
    ))
    c(lKey, list(data = lTable$records))
  })
}

# The stored results a page ships: R's answer for the table the settings open
# on, for the test it opens on and for the other test the Test control offers,
# so either test is answered in the saved page.
CrossTab_StoredResults <- function(dfResults, dfParticipants, lConfig) {
  if (nrow(dfResults) == 0L) {
    return(list())
  }
  lState <- CrossTab_State(dfResults, dfParticipants, lConfig)
  if (identical(lState$test, "none")) {
    return(list())
  }
  chrTests <- c(lState$test, setdiff(chrCrossTabTests, lState$test))
  Chart_Answer(
    CrossTab_Requests(dfResults, dfParticipants, lConfig, lState, chrTests),
    list(Analyze_Contingency = Analyze_Contingency)
  )
}

# An answer's estimates with Fisher's odds ratio named by the categories R was
# handed, as the chart's line names it. Returns the answer's value.
CrossTab_Oriented <- function(lResult) {
  lValue <- lResult$value
  dfEstimates <- lValue$estimates
  if (is.data.frame(dfEstimates) && nrow(dfEstimates) > 0L) {
    chrRows <- unlist(lResult$args$chrRowGroups)
    chrCols <- unlist(lResult$args$chrColGroups)
    for (iRow in seq_len(nrow(dfEstimates))) {
      lRow <- Output_Oriented(as.list(dfEstimates[iRow, ]), chrRows, chrCols)
      dfEstimates$group[iRow] <- lRow$group
    }
    lValue$estimates <- dfEstimates
  }
  lValue
}

# What the chart prints for one answer under its table: the test, then each
# estimate with an interval, as `describeAnswer` does, once R computed it.
CrossTab_Lines <- function(lResult) {
  lValue <- CrossTab_Oriented(lResult)
  chrLines <- Output_StatisticText(lValue)
  bShown <- identical(lValue$status, "ok") && (is.null(lValue$reason) || is.na(lValue$reason))
  dfEstimates <- lValue$estimates
  if (bShown && is.data.frame(dfEstimates) && nrow(dfEstimates) > 0L) {
    bInterval <- !is.na(dfEstimates$lower) & !is.na(dfEstimates$upper)
    chrLines <- c(chrLines, vapply(which(bInterval), function(iRow) Output_EstimateText(as.list(dfEstimates[iRow, ])), character(1)))
  }
  chrLines
}
