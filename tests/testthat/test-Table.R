# The statistics tables (#38): each chart's statistics as a data frame, one
# row per statistic R computed for the view the chart opens on, written as the
# chart prints them (R/output.R): the method and the counts, the estimate and
# its interval, the p-value labelled exploratory with its adjustment named, and
# no stars. Each table is held member by member to the Analyze_*() function on
# rows worked out here from the study's tables and nothing of the table's, and
# Write_RTF() is held to an RTF file that parses and carries the titles.

lTableColumns <- function() {
  list(
    list(value_col = "ARM", label = "Arm"),
    list(value_col = "SEX", label = "Sex"),
    list(value_col = "RESPONSE", label = "Response")
  )
}

# The six tables as the reference pages make them.
lTableCalls <- function() {
  list(
    group_comparison = function() {
      Table_GroupComparison(Synthetic_Results, Synthetic_Participants, list(
        start_value = "IL-6", visits = c("Week 4", "Week 8"), value_type = "change", baseline_visits = "Baseline",
        group_by = "ARM", groups = lTableColumns(),
        title = "{measure}: {value} by {group}", subtitle = "{n} participants", footnotes = "Synthetic study from gsm.bio."
      ))
    },
    association_scatter = function() {
      Table_AssociationScatter(Synthetic_Results, Synthetic_Participants, list(
        x = list(measure = "TNF-alpha", visit = "Baseline"), y = list(measure = "IL-10", visit = "Baseline"),
        fit = "linear", title = "{y} against {x}"
      ))
    },
    correlation_matrix = function() {
      Table_CorrelationMatrix(Synthetic_Results, Synthetic_Participants, list(visit = "Baseline", title = "{heading}"))
    },
    biomarker_screen = function() {
      Table_BiomarkerScreen(Synthetic_Results, Synthetic_Participants, list(
        visit = "Week 4", value_type = "change", group_by = "ARM", baseline_visits = "Baseline", title = "{heading}"
      ))
    },
    cross_tab = function() {
      Table_CrossTab(Synthetic_Results, Synthetic_Participants, list(
        row_by = "RESPONSE", col_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
        groups = lTableColumns(), title = "{rows} by {columns}"
      ))
    },
    stratified_survival = function() {
      Table_StratifiedSurvival(Synthetic_Results, Synthetic_Participants, list(
        endpoint = "EFS", group_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
        title = "{endpoint} by {group}", subtitle = "{n} participants"
      ), dfOutcomes = Synthetic_Outcomes)
    }
  )
}

chrTableStatisticColumns <- c("Method", "Estimate", "Counts", "p-value", "Note")

# R's answer and the table's row for it say the same thing: the method, the
# counts, each estimate with its interval, the p-value and the note.
ExpectRowSays <- function(dfRow, lValue, strLabel) {
  expect_identical(dfRow$Method, if (is.na(lValue$method)) "" else lValue$method, label = paste(strLabel, "method"))
  strCounts <- Output_LineCounts(lValue$counts)
  expect_identical(dfRow$Counts, if (is.null(strCounts)) "" else strCounts, label = paste(strLabel, "counts"))
  expect_identical(dfRow$`p-value`, if (is.na(lValue$p_value)) "" else Output_P(lValue$p_value), label = paste(strLabel, "p-value"))
  for (iEstimate in seq_len(nrow(lValue$estimates))) {
    strEstimate <- sub("\\.$", "", Output_EstimateText(as.list(lValue$estimates[iEstimate, ])), perl = TRUE)
    expect_true(grepl(enc2utf8(strEstimate), enc2utf8(dfRow$Estimate), fixed = TRUE, useBytes = TRUE), label = paste(strLabel, "estimate", strEstimate))
  }
  # No stars, ever.
  expect_false(any(grepl("*", unlist(dfRow[chrTableStatisticColumns]), fixed = TRUE)), label = paste(strLabel, "has no stars"))
}

test_that("each table is a data frame of its chart's statistics, titled from its placeholders, with its own footnote last (#38)", {
  for (strTable in names(lTableCalls())) {
    dfTable <- lTableCalls()[[strTable]]()
    expect_s3_class(dfTable, "data.frame")
    expect_true(all(chrTableStatisticColumns %in% names(dfTable)), label = paste(strTable, "columns"))
    expect_true(all(vapply(dfTable, is.character, logical(1))), label = paste(strTable, "is text, as printed"))
    expect_gt(nrow(dfTable), 0L)
    expect_false(grepl("\\{[A-Za-z_]+\\}", attr(dfTable, "title")), label = paste(strTable, "title filled"))
    chrFootnotes <- attr(dfTable, "footnotes")
    expect_match(chrFootnotes[length(chrFootnotes)], paste0("^Drawn on ", Output_DateDrawn(), " by gsm\\.bio "))
    expect_identical(length(attr(dfTable, "results")), length(unique(attr(dfTable, "result_of"))))
  }
  dfGroup <- lTableCalls()$group_comparison()
  expect_identical(attr(dfGroup, "title"), "IL-6: Change from baseline by Arm")
  expect_identical(attr(dfGroup, "subtitle"), "200 participants")
  expect_identical(attr(dfGroup, "footnotes")[1], "Synthetic study from gsm.bio.")
  expect_identical(attr(lTableCalls()$cross_tab(), "title"), "Response by CRP at Baseline, cut at the median")
  expect_identical(attr(lTableCalls()$stratified_survival(), "title"), "Event-free survival (months) by CRP at Baseline, cut at the median")
})

test_that("the group comparison's table has a row per visit, equal to Analyze_GroupDifference on the rows worked out here (#38)", {
  dfTable <- lTableCalls()$group_comparison()
  expect_identical(dfTable$Visit, c("Week 4", "Week 8"))
  for (iVisit in 1:2) {
    strVisit <- dfTable$Visit[iVisit]
    dfRows <- data.frame(y = nResultAt("IL-6", strVisit) - nResultAt("IL-6", "Baseline"), x = Synthetic_Participants$ARM, stringsAsFactors = FALSE)
    dfRows <- dfRows[!is.na(dfRows$y), ]
    lTheirs <- Analyze_GroupDifference(dfRows, "y", "x", strMethod = "t")
    lMine <- attr(dfTable, "results")[[iVisit]]
    for (strMember in c("status", "method", "counts", "p_value", "estimates", "statistic")) {
      expect_equal(lMine[[strMember]], lTheirs[[strMember]], tolerance = 1e-12, label = paste(strVisit, strMember))
    }
    ExpectRowSays(dfTable[iVisit, ], lTheirs, strVisit)
    expect_identical(dfTable$Note[iVisit], "Exploratory, unadjusted.")
  }
})

test_that("the scatter's table has the coefficient and the line, equal to Analyze_Correlation and Analyze_Fit (#38)", {
  dfTable <- lTableCalls()$association_scatter()
  dfRows <- data.frame(x = nResultAt("TNF-alpha", "Baseline"), y = nResultAt("IL-10", "Baseline"))
  dfRows <- dfRows[stats::complete.cases(dfRows), ]
  lCor <- Analyze_Correlation(dfRows, "x", "y", strMethod = "pearson")
  lFit <- Analyze_Fit(dfRows, "x", "y", strMethod = "linear")
  expect_identical(dfTable$Statistic, c("Correlation", "Fitted line"))
  expect_equal(attr(dfTable, "results")[[1]][c("estimates", "p_value", "counts")], lCor[c("estimates", "p_value", "counts")], tolerance = 1e-12)
  expect_equal(attr(dfTable, "results")[[2]][c("estimates", "p_value", "counts")], lFit[c("estimates", "p_value", "counts")], tolerance = 1e-12)
  # The coefficient is named as the chart names it.
  lNamed <- lCor
  lNamed$estimates$name <- paste0("Pearson", intToUtf8(0x2019L), "s r")
  ExpectRowSays(dfTable[1, ], lNamed, "coefficient")
  ExpectRowSays(dfTable[2, ], lFit, "line")
})

test_that("the correlation matrix's table has a row per pair, equal to Analyze_CorrelationMatrix on the frame worked out here (#38)", {
  dfTable <- lTableCalls()$correlation_matrix()
  chrBiomarkers <- c("CRP", "D-dimer", "Ferritin", "IFN-gamma", "IL-1beta", "IL-2", "IL-6", "IL-8", "IL-10", "LDH", "TNF-alpha", "VEGF")
  dfFrame <- as.data.frame(stats::setNames(lapply(chrBiomarkers, nResultAt, strVisit = "Baseline"), chrBiomarkers), check.names = FALSE)
  dfTheirs <- Analyze_CorrelationMatrix(dfFrame, chrBiomarkers)$rows
  expect_identical(nrow(dfTable), nrow(dfTheirs))
  expect_identical(dfTable$`Variable 1`, dfTheirs$x)
  expect_identical(dfTable$`Variable 2`, dfTheirs$y)
  expect_identical(dfTable$Counts, paste("n =", dfTheirs$counts))
  expect_identical(
    dfTable$Estimate,
    paste0(Output_Figure(dfTheirs$estimate), ", 95% confidence interval ", Output_Figure(dfTheirs$lower), " to ", Output_Figure(dfTheirs$upper))
  )
  expect_true(all(dfTable$`p-value` == ""))
  expect_equal(attr(dfTable, "results")[[1]]$rows$estimate, dfTheirs$estimate, tolerance = 1e-12)
})

test_that("the screen's table has a row per biomarker, both p-values by the display rules, equal to Analyze_Screen (#38)", {
  dfTable <- lTableCalls()$biomarker_screen()
  chrBiomarkers <- c("CRP", "D-dimer", "Ferritin", "IFN-gamma", "IL-1beta", "IL-2", "IL-6", "IL-8", "IL-10", "LDH", "TNF-alpha", "VEGF")
  dfFrame <- data.frame(ARM = Synthetic_Participants$ARM, stringsAsFactors = FALSE)
  for (strBiomarker in chrBiomarkers) {
    dfFrame[[strBiomarker]] <- nResultAt(strBiomarker, "Week 4") - nResultAt(strBiomarker, "Baseline")
  }
  dfFrame <- dfFrame[rowSums(!is.na(dfFrame[chrBiomarkers])) > 0, ]
  dfTheirs <- Analyze_Screen(dfFrame, chrBiomarkers, strComparison = "difference", strGroupCol = "ARM", chrGroups = c("Placebo", "Treatment"))$rows
  expect_identical(dfTable$Biomarker, dfTheirs$biomarker)
  expect_identical(dfTable$`p-value`, vapply(dfTheirs$p_unadjusted, Output_P, character(1)))
  expect_identical(dfTable$`Adjusted p-value`, vapply(dfTheirs$p_value, Output_P, character(1)))
  expect_identical(dfTable$Counts, paste0("Placebo n = ", dfTheirs$n_1, ", Treatment n = ", dfTheirs$n_2))
  expect_identical(
    dfTable$Estimate,
    paste0(Output_Figure(dfTheirs$estimate), ", 95% confidence interval ", Output_Figure(dfTheirs$lower), " to ", Output_Figure(dfTheirs$upper))
  )
  expect_true(all(dfTable$Note == "Exploratory, adjusted (Benjamini-Hochberg)."))
  expect_equal(attr(dfTable, "results")[[1]]$rows$p_value, dfTheirs$p_value, tolerance = 1e-12)
})

test_that("the cross-tabulation's table has the test of the table, equal to Analyze_Contingency (#38)", {
  dfTable <- lTableCalls()$cross_tab()
  nCrp <- nResultAt("CRP", "Baseline")
  nMedian <- stats::median(nCrp, na.rm = TRUE)
  strBound <- format(signif(nMedian, 4), scientific = FALSE, trim = TRUE)
  chrLowHigh <- c(paste0(intToUtf8(0x2264L), " ", strBound), paste0("> ", strBound))
  dfRows <- data.frame(row = Synthetic_Participants$RESPONSE, col = ifelse(nCrp <= nMedian, chrLowHigh[1], chrLowHigh[2]), stringsAsFactors = FALSE)
  lTheirs <- Analyze_Contingency(dfRows, "row", "col", chrRowGroups = c("Non-responder", "Responder"), chrColGroups = chrLowHigh)
  expect_identical(nrow(dfTable), 1L)
  expect_equal(attr(dfTable, "results")[[1]][c("p_value", "counts", "statistic")], lTheirs[c("p_value", "counts", "statistic")], tolerance = 1e-12)
  ExpectRowSays(dfTable[1, ], lTheirs, "cross-tab")
})

test_that("the survival table has the log-rank test, each median and the hazard ratio, equal to Analyze_Survival (#38)", {
  dfTable <- lTableCalls()$stratified_survival()
  nCrp <- nResultAt("CRP", "Baseline")
  nMedian <- stats::median(nCrp, na.rm = TRUE)
  strBound <- format(signif(nMedian, 4), scientific = FALSE, trim = TRUE)
  chrLowHigh <- c(paste0(intToUtf8(0x2264L), " ", strBound), paste0("> ", strBound))
  dfEfs <- Synthetic_Outcomes[Synthetic_Outcomes$PARAMCD == "EFS", ]
  iAt <- match(Synthetic_Participants$USUBJID, dfEfs$USUBJID)
  dfRows <- data.frame(time = dfEfs$AVAL[iAt], group = ifelse(nCrp <= nMedian, chrLowHigh[1], chrLowHigh[2]), censor = dfEfs$CNSR[iAt], stringsAsFactors = FALSE)
  lTheirs <- Analyze_Survival(dfRows, "time", "group", strCensorCol = "censor", chrGroups = rev(chrLowHigh))
  expect_equal(attr(dfTable, "results")[[1]][c("p_value", "counts", "estimates", "statistic")], lTheirs[c("p_value", "counts", "estimates", "statistic")], tolerance = 1e-12)
  # One row for the test, then the medians in the legend's order, then the
  # hazard ratio, high over low.
  expect_identical(dfTable$Statistic, enc2utf8(c("Log-rank test", paste0("Median (", chrLowHigh, ")"), paste0("Hazard ratio, high over low (", chrLowHigh[2], " / ", chrLowHigh[1], ")"))))
  expect_identical(dfTable$`p-value`, c(Output_P(lTheirs$p_value), "", "", ""))
  expect_identical(dfTable$Counts[1], Output_LineCounts(lTheirs$counts))
  dfEstimates <- lTheirs$estimates
  for (iRow in seq_len(nrow(dfEstimates))) {
    lRow <- as.list(dfEstimates[iRow, ])
    strSaid <- sub("^[^:]*: ", "", sub("\\.$", "", Output_EstimateText(lRow)))
    expect_true(strSaid %in% dfTable$Estimate, label = strSaid)
  }
})

test_that("a statistic R did not compute is a row with R's reason, and no number of its own (#38)", {
  dfTable <- Table_StratifiedSurvival(Synthetic_Results, Synthetic_Participants, list(
    endpoint = "EFS", group_by = list(measure = "CRP", visit = "Baseline", cut = list(10))
  ), dfOutcomes = Synthetic_Outcomes)
  lValue <- attr(dfTable, "results")[[1]]
  expect_identical(lValue$status, "too_small")
  expect_identical(nrow(dfTable), 1L)
  expect_identical(dfTable$`p-value`, "")
  expect_identical(dfTable$Note, lValue$reason)
})

test_that("the p-value text follows the display rules: three decimals, bounds at 0.001 and 0.999, labelled exploratory, the adjustment named, no stars (#38)", {
  lValue <- list(status = "ok", method = "Welch Two Sample t-test", counts = list(A = 10L, B = 12L), p_value = 0.04, adjustment = "none", estimates = data.frame())
  for (nP in c(0.04, 0.0004, 0.9996, 0.05)) {
    lValue$p_value <- nP
    dfRow <- Table_Row(lValue)
    expect_identical(dfRow$`p-value`, c("p = 0.040", "p < 0.001", "p > 0.999", "p = 0.050")[match(nP, c(0.04, 0.0004, 0.9996, 0.05))])
    expect_identical(dfRow$Note, "Exploratory, unadjusted.")
    expect_false(grepl("*", dfRow$`p-value`, fixed = TRUE))
  }
  lValue$adjustment <- "holm"
  expect_identical(Table_Row(lValue)$Note, "Exploratory, adjusted (Holm).")
})

test_that("Write_RTF writes a table to an RTF file that parses and carries its title, subtitle, footnotes and numbers (#38)", {
  skip_if_not_installed("r2rtf")
  dfTable <- lTableCalls()$stratified_survival()
  strFile <- tempfile(fileext = ".rtf")
  expect_identical(Write_RTF(dfTable, strFile), strFile)
  strRtf <- paste(readLines(strFile, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # An RTF document: one group, opened by the header, whose braces balance.
  expect_match(strRtf, "^\\{\\\\rtf1")
  chrChars <- strsplit(gsub("\\\\[\\\\{}]", "", strRtf), "")[[1]]
  nDepth <- cumsum((chrChars == "{") - (chrChars == "}"))
  expect_identical(nDepth[length(nDepth)], 0L)
  expect_true(all(nDepth[-length(nDepth)] > 0L))
  # The title, the subtitle and the footnotes, and the table's numbers. Text
  # that is not ASCII is written as RTF's \u escapes.
  strPlain <- gsub("\\\\u8804\\??", intToUtf8(0x2264L), strRtf)
  expect_true(grepl("Event-free survival (months) by CRP at Baseline, cut at the median", strPlain, fixed = TRUE))
  expect_true(grepl("200 participants", strPlain, fixed = TRUE))
  expect_true(grepl("Drawn on ", strPlain, fixed = TRUE))
  expect_true(grepl(dfTable$`p-value`[1], strPlain, fixed = TRUE))
  expect_true(grepl(enc2utf8(dfTable$Statistic[2]), strPlain, fixed = TRUE))
  for (strTable in names(lTableCalls())) {
    strOne <- tempfile(fileext = ".rtf")
    Write_RTF(lTableCalls()[[strTable]](), strOne)
    expect_match(readLines(strOne, n = 1L, warn = FALSE), "^\\{\\\\rtf1", label = strTable)
  }
})

test_that("Write_RTF says r2rtf is needed when it is not installed, and refuses what is not a table (#38)", {
  local_mocked_bindings(Table_HasR2rtf = function() FALSE)
  expect_error(Write_RTF(lTableCalls()$cross_tab(), tempfile()), "Write_RTF\\(\\) writes RTF with r2rtf, which is not installed")
  local_mocked_bindings(Table_HasR2rtf = function() TRUE)
  expect_error(Write_RTF("not a table", tempfile()), "dfTable is not a data.frame")
})
