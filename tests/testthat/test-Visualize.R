# The static figures (#37): each of the six charts as a ggplot2 figure of the
# view the chart opens on, from the same settings, with its statistics from the
# same Analyze_*() calls on the same rows, and its title, subtitle and
# footnotes written by bio.viz's rules (R/output.R). Each figure is held to:
# what it prints, against the Analyze_*() function on rows worked out here from
# the study's tables and nothing of the figure's; and a snapshot of what it
# draws (helper-figure.R).
#
# The snapshots are of each figure's own data and labels, not SVG files drawn
# by vdiffr: an SVG depends on the fonts and the graphics engine of the session
# that drew it, while the data and labels are the same in every locale, on
# every platform and with every ggplot2 version, so a change in one is a change
# in what the figure draws.

skip_if_not_installed("ggplot2")

lFigureColumns <- function() {
  list(
    list(value_col = "ARM", label = "Arm"),
    list(value_col = "SEX", label = "Sex"),
    list(value_col = "RESPONSE", label = "Response")
  )
}

# The six figures as the gallery draws them.
lFigureCalls <- function() {
  list(
    group_comparison = function() {
      Visualize_GroupComparison(Synthetic_Results, Synthetic_Participants, list(
        start_value = "IL-6", visits = c("Week 4", "Week 8"), value_type = "change", baseline_visits = "Baseline",
        group_by = "ARM", groups = lFigureColumns(),
        title = "{measure}: {value} by {group}", subtitle = "{n} participants, at {visits}",
        footnotes = c("Synthetic study from gsm.bio.", "Filters: {filters}.")
      ))
    },
    association_scatter = function() {
      Visualize_AssociationScatter(Synthetic_Results, Synthetic_Participants, list(
        x = list(measure = "TNF-alpha", visit = "Baseline"), y = list(measure = "IL-10", visit = "Baseline"),
        fit = "linear", title = "{y} against {x}", subtitle = "{n} participants"
      ))
    },
    correlation_matrix = function() {
      Visualize_CorrelationMatrix(Synthetic_Results, Synthetic_Participants, list(
        visit = "Baseline", title = "{heading}", subtitle = "{variables} biomarkers, {n} participants"
      ))
    },
    biomarker_screen = function() {
      Visualize_BiomarkerScreen(Synthetic_Results, Synthetic_Participants, list(
        visit = "Week 4", value_type = "change", group_by = "ARM", baseline_visits = "Baseline",
        title = "{heading}", subtitle = "{biomarkers} biomarkers, {n} participants"
      ))
    },
    cross_tab = function() {
      Visualize_CrossTab(Synthetic_Results, Synthetic_Participants, list(
        row_by = "RESPONSE", col_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
        groups = lFigureColumns(), title = "{rows} by {columns}", subtitle = "{n} participants"
      ))
    },
    stratified_survival = function() {
      Visualize_StratifiedSurvival(Synthetic_Results, Synthetic_Participants, list(
        endpoint = "EFS", group_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
        title = "{endpoint} by {group}", subtitle = "{n} participants"
      ), dfOutcomes = Synthetic_Outcomes)
    }
  )
}

# CRP at Baseline split at its median, as its two groups are labelled.
lCrpMedian <- function() {
  nCrp <- nResultAt("CRP", "Baseline")
  nMedian <- stats::median(nCrp, na.rm = TRUE)
  strBound <- format(signif(nMedian, 4), scientific = FALSE, trim = TRUE)
  list(
    group = enc2utf8(ifelse(nCrp <= nMedian, paste0("≤ ", strBound), paste0("> ", strBound))),
    low_high = enc2utf8(c(paste0("≤ ", strBound), paste0("> ", strBound)))
  )
}

test_that("each of the six figures is a ggplot of its chart's opening view, titled from its placeholders (#37)", {
  for (strFigure in names(lFigureCalls())) {
    gg <- lFigureCalls()[[strFigure]]()
    expect_s3_class(gg, "ggplot")
    expect_false(is.null(gg$labels$title), label = paste(strFigure, "has a title"))
    # No placeholder is left unfilled in the titles: each name is the figure's.
    expect_false(grepl("\\{[A-Za-z_]+\\}", gg$labels$title), label = paste(strFigure, "title filled"))
    expect_false(grepl("\\{[A-Za-z_]+\\}", gg$labels$subtitle), label = paste(strFigure, "subtitle filled"))
    # The figure's own footnote is last.
    chrUnder <- strsplit(gg$labels$caption, "\n", fixed = TRUE)[[1]]
    expect_match(strFigureCaption(gg), paste0("Drawn on ", Output_DateDrawn(), " by gsm.bio "), fixed = TRUE)
    expect_match(chrUnder[length(chrUnder)], "gsm\\.bio [0-9.]+\\.$")
  }
  lFigures <- lapply(lFigureCalls(), function(fnFigure) fnFigure())
  expect_identical(lFigures$group_comparison$labels$title, "IL-6: Change from baseline by Arm")
  expect_identical(lFigures$group_comparison$labels$subtitle, "200 participants, at Week 4, Week 8")
  expect_match(strFigureCaption(lFigures$group_comparison), "Synthetic study from gsm.bio. Filters: none.", fixed = TRUE)
  expect_identical(lFigures$association_scatter$labels$title, "IL-10 at Baseline (pg/mL) against TNF-alpha at Baseline (pg/mL)")
  expect_identical(lFigures$correlation_matrix$labels$title, "Result at Baseline, biomarker against biomarker")
  expect_identical(lFigures$correlation_matrix$labels$subtitle, "12 biomarkers, 200 participants")
  expect_identical(lFigures$biomarker_screen$labels$title, "Change from baseline at Week 4: Placebo against Treatment, standardised difference")
  expect_identical(lFigures$cross_tab$labels$title, "Response by CRP at Baseline, cut at the median")
  expect_identical(lFigures$stratified_survival$labels$title, "Event-free survival (months) by CRP at Baseline, cut at the median")
  # A name the figure does not have is left as written, braces and all.
  gg <- Visualize_CrossTab(Synthetic_Results, Synthetic_Participants, list(row_by = "ARM", col_by = "RESPONSE", title = "{rows} by {arm}"))
  expect_identical(gg$labels$title, "ARM by {arm}")
})

test_that("the group comparison prints each visit's test, equal to Analyze_GroupDifference on the rows worked out here (#37)", {
  gg <- lFigureCalls()$group_comparison()
  chrHeadings <- levels(gg$data$heading)
  expect_length(chrHeadings, 2L)
  for (strVisit in c("Week 4", "Week 8")) {
    dfRows <- data.frame(
      y = nResultAt("IL-6", strVisit) - nResultAt("IL-6", "Baseline"), x = Synthetic_Participants$ARM,
      stringsAsFactors = FALSE
    )
    dfRows <- dfRows[!is.na(dfRows$y), ]
    strTheirs <- Output_StatisticText(Analyze_GroupDifference(dfRows, "y", "x", strMethod = "t"))
    strHeading <- chrHeadings[startsWith(chrHeadings, strVisit)]
    expect_true(startsWith(gsub("\n", " ", strHeading, fixed = TRUE), paste(strVisit, strTheirs)), label = paste(strVisit, "heading"))
    expect_identical(sum(gg$data$heading == strHeading), nrow(dfRows))
  }
  expect_error(
    Visualize_GroupComparison(Synthetic_Results, Synthetic_Participants, list(group_by = "ARM")),
    "start_value"
  )
})

test_that("the association scatter prints R's coefficient and draws R's line, equal to Analyze_Correlation and Analyze_Fit (#37)", {
  gg <- lFigureCalls()$association_scatter()
  dfRows <- data.frame(x = nResultAt("TNF-alpha", "Baseline"), y = nResultAt("IL-10", "Baseline"))
  dfRows <- dfRows[stats::complete.cases(dfRows), ]
  lTheirs <- Analyze_Correlation(dfRows, "x", "y", strMethod = "pearson")
  lEstimate <- as.list(lTheirs$estimates[1, ])
  expect_identical(lEstimate$name, "cor")
  lEstimate$name <- enc2utf8("Pearson\u2019s r")
  expect_match(strFigureCaption(gg), paste(Output_StatisticText(lTheirs), Output_EstimateText(lEstimate)), fixed = TRUE)
  # The line's own sentence follows, with its slope and intercept.
  lLine <- Analyze_Fit(dfRows, "x", "y", strMethod = "linear")
  expect_match(strFigureCaption(gg), Output_StatisticText(lLine), fixed = TRUE)
  expect_identical(nrow(gg$data), nrow(dfRows))
  dfLine <- gg$layers[[2]]$data
  lFit <- Analyze_Fit(dfRows, "x", "y", strMethod = "linear")
  expect_equal(dfLine$fit, lFit$rows$fit, tolerance = 1e-12)
  expect_equal(dfLine$lower, lFit$rows$lower, tolerance = 1e-12)
})

test_that("the correlation matrix writes each pair's coefficient, equal to Analyze_CorrelationMatrix on the frame worked out here (#37)", {
  gg <- lFigureCalls()$correlation_matrix()
  chrBiomarkers <- c("CRP", "D-dimer", "Ferritin", "IFN-gamma", "IL-1beta", "IL-2", "IL-6", "IL-8", "IL-10", "LDH", "TNF-alpha", "VEGF")
  dfFrame <- as.data.frame(stats::setNames(lapply(chrBiomarkers, nResultAt, strVisit = "Baseline"), chrBiomarkers), check.names = FALSE)
  lTheirs <- Analyze_CorrelationMatrix(dfFrame, chrBiomarkers)
  dfCells <- gg$data[gg$data$label != "", ]
  expect_identical(nrow(dfCells), nrow(lTheirs$rows))
  for (iRow in seq_len(nrow(lTheirs$rows))) {
    lPair <- lTheirs$rows[iRow, ]
    strLabel <- dfCells$label[as.character(dfCells$y) == lPair$x & as.character(dfCells$x) == lPair$y]
    expect_identical(strLabel, Output_Coefficient(lPair$estimate), label = paste(lPair$x, lPair$y))
  }
  expect_identical(Output_Coefficient(c(0.6384, -0.1274, -0.001, NA)), enc2utf8(c("0.64", "−0.13", "0.00", NA)))
})

test_that("the biomarker screen prints each row's estimate and p-values, equal to Analyze_Screen on the frame worked out here (#37)", {
  gg <- lFigureCalls()$biomarker_screen()
  chrBiomarkers <- c("CRP", "D-dimer", "Ferritin", "IFN-gamma", "IL-1beta", "IL-2", "IL-6", "IL-8", "IL-10", "LDH", "TNF-alpha", "VEGF")
  dfFrame <- data.frame(ARM = Synthetic_Participants$ARM, stringsAsFactors = FALSE)
  for (strBiomarker in chrBiomarkers) {
    dfFrame[[strBiomarker]] <- nResultAt(strBiomarker, "Week 4") - nResultAt(strBiomarker, "Baseline")
  }
  dfFrame <- dfFrame[rowSums(!is.na(dfFrame[chrBiomarkers])) > 0, ]
  dfTheirs <- Analyze_Screen(dfFrame, chrBiomarkers, strComparison = "difference", strGroupCol = "ARM", chrGroups = c("Placebo", "Treatment"))$rows
  for (iRow in seq_len(nrow(dfTheirs))) {
    lRow <- dfTheirs[iRow, ]
    strSaid <- paste0(
      lRow$biomarker, "   ", Output_Figure(lRow$estimate), " (", Output_Figure(lRow$lower), " to ", Output_Figure(lRow$upper), "); ",
      "Unadjusted: ", Output_P(lRow$p_unadjusted), "; Benjamini-Hochberg: ", Output_P(lRow$p_value),
      "; n, Placebo / Treatment: ", lRow$n_1, " / ", lRow$n_2
    )
    expect_true(strSaid %in% levels(gg$data$row), label = strSaid)
  }
  expect_match(
    strFigureCaption(gg),
    "Welch Two Sample t-test, one row per biomarker: 12 of 12 computed. The adjusted p-values are adjusted by Benjamini-Hochberg across the 12 biomarkers that have a p-value.",
    fixed = TRUE
  )
  # Largest first, from the top.
  chrTop <- rev(levels(gg$data$row))[1]
  expect_true(startsWith(chrTop, dfTheirs$biomarker[which.max(dfTheirs$estimate)]))
})

test_that("the cross-tabulation prints R's test of the table, equal to Analyze_Contingency on the rows worked out here (#37)", {
  gg <- lFigureCalls()$cross_tab()
  lCrp <- lCrpMedian()
  dfRows <- data.frame(row = Synthetic_Participants$RESPONSE, col = lCrp$group, stringsAsFactors = FALSE)
  dfRows <- dfRows[!is.na(dfRows$col), ]
  lTheirs <- Analyze_Contingency(dfRows, "row", "col", chrRowGroups = c("Non-responder", "Responder"), chrColGroups = lCrp$low_high)
  expect_match(strFigureCaption(gg), Output_StatisticText(lTheirs), fixed = TRUE)
  expect_identical(sum(gg$data$n), nrow(dfRows))
  expect_identical(as.integer(gg$data$n), as.integer(table(factor(dfRows$row, c("Non-responder", "Responder")), factor(dfRows$col, lCrp$low_high))))
})

test_that("the survival figure draws R's own survfit() curves and band, and prints Analyze_Survival's test, medians and hazard ratio (#37)", {
  gg <- lFigureCalls()$stratified_survival()
  lCrp <- lCrpMedian()
  dfEfs <- Synthetic_Outcomes[Synthetic_Outcomes$PARAMCD == "EFS", ]
  iAt <- match(Synthetic_Participants$USUBJID, dfEfs$USUBJID)
  dfRows <- data.frame(time = dfEfs$AVAL[iAt], group = lCrp$group, censor = dfEfs$CNSR[iAt], stringsAsFactors = FALSE)
  lTheirs <- Analyze_Survival(dfRows, "time", "group", strCensorCol = "censor", chrGroups = rev(lCrp$low_high))
  strCaption <- strFigureCaption(gg)
  expect_match(strCaption, Output_StatisticText(lTheirs), fixed = TRUE)
  dfEstimates <- lTheirs$estimates
  for (iRow in seq_len(nrow(dfEstimates))) {
    lRow <- as.list(dfEstimates[iRow, ])
    if (lRow$name == "Hazard ratio") lRow$name <- "Hazard ratio, high over low"
    expect_match(strCaption, Output_EstimateText(lRow), fixed = TRUE)
  }
  # The medians in the legend's order, low to high.
  expect_lt(regexpr(paste0("Median (", lCrp$low_high[1]), strCaption, fixed = TRUE), regexpr(paste0("Median (", lCrp$low_high[2]), strCaption, fixed = TRUE))
  # Each curve is survfit()'s, with its log-log band, a step from 1 at time 0.
  for (strLevel in lCrp$low_high) {
    dfIn <- dfRows[dfRows$group == strLevel, ]
    lFit <- survival::survfit(survival::Surv(dfIn$time, dfIn$censor == 0) ~ 1, conf.type = "log-log")
    dfCurve <- gg$data[gg$data$group == strLevel, ]
    expect_equal(dfCurve$time, c(0, lFit$time), tolerance = 1e-12)
    expect_equal(dfCurve$surv, c(1, lFit$surv), tolerance = 1e-12)
    expect_equal(dfCurve$lower, c(1, lFit$lower), tolerance = 1e-12)
  }
  expect_error(Visualize_StratifiedSurvival(Synthetic_Results, Synthetic_Participants, list(group_by = "ARM")), "needs an outcomes table")
})

test_that("a figure says ggplot2 is needed when it is not installed, and refuses settings as its chart does (#37)", {
  local_mocked_bindings(Figure_HasGgplot = function() FALSE)
  expect_error(Visualize_CrossTab(Synthetic_Results, Synthetic_Participants), "Visualize_CrossTab\\(\\) draws a static figure with ggplot2, which is not installed")
})

test_that("a figure's title, subtitle and footnotes are refused as the chart refuses them (#37)", {
  expect_error(Visualize_CrossTab(Synthetic_Results, Synthetic_Participants, list(title = 42)), "'title' must be text")
  expect_error(Visualize_CrossTab(Synthetic_Results, Synthetic_Participants, list(footnotes = list("One.", 2))), "'footnotes' must be text")
  expect_error(Visualize_CrossTab("nope"), "dfResults is not a data.frame")
})

test_that("each figure draws what its snapshot holds (#37)", {
  for (strFigure in names(lFigureCalls())) {
    gg <- lFigureCalls()[[strFigure]]()
    expect_snapshot(writeLines(strFigureStable(chrFigureDescription(gg))), variant = NULL)
  }
})

test_that("the group comparison prints each panel's difference with its interval and, with pairs, each pair's Holm-adjusted p-value, as the chart does (#37)", {
  gg <- lFigureCalls()$group_comparison()
  chrHeadings <- gsub("\n", " ", levels(gg$data$heading), fixed = TRUE)
  dfRows <- data.frame(y = nResultAt("IL-6", "Week 4") - nResultAt("IL-6", "Baseline"), x = Synthetic_Participants$ARM, stringsAsFactors = FALSE)
  dfRows <- dfRows[!is.na(dfRows$y), ]
  lWelch <- stats::t.test(dfRows$y[dfRows$x == "Placebo"], dfRows$y[dfRows$x == "Treatment"])
  strDifference <- paste0(
    "Difference in means (Placebo - Treatment): ", Output_Figure(unname(lWelch$estimate[1] - lWelch$estimate[2])),
    ", 95% confidence interval ", Output_Figure(lWelch$conf.int[1]), " to ", Output_Figure(lWelch$conf.int[2]), "."
  )
  expect_true(grepl(strDifference, chrHeadings[1], fixed = TRUE), label = strDifference)
  # Three groups, with pairs: CRP at Baseline at its tertiles, Wilcoxon's test.
  ggPairs <- Visualize_GroupComparison(Synthetic_Results, Synthetic_Participants, list(
    start_value = "IL-6", visits = "Week 4", value_type = "change", baseline_visits = "Baseline",
    group_by = list(measure = "CRP", visit = "Baseline", cut = "tertiles"), test = "wilcoxon", pairwise = TRUE
  ))
  strHeading <- gsub("\n", " ", levels(ggPairs$data$heading)[1], fixed = TRUE)
  nCrp <- nResultAt("CRP", "Baseline")
  nPoints <- stats::quantile(nCrp, c(1, 2) / 3, type = 7, names = FALSE)
  chrLabels <- Core_CutLabels(nPoints)
  dfThree <- data.frame(y = nResultAt("IL-6", "Week 4") - nResultAt("IL-6", "Baseline"), x = Core_CutGroups(nCrp, nPoints), stringsAsFactors = FALSE)
  dfThree <- dfThree[!is.na(dfThree$y) & !is.na(dfThree$x), ]
  lTheirs <- Analyze_GroupDifference(dfThree, "y", "x", strMethod = "kruskal", bPairwise = TRUE, chrGroups = chrLabels)
  expect_match(strHeading, Output_StatisticText(lTheirs), fixed = TRUE)
  expect_match(strHeading, "Pairwise comparisons, each by Wilcoxon rank sum", fixed = TRUE)
  expect_match(strHeading, "Exploratory, adjusted (Holm).", fixed = TRUE)
  mHolm <- stats::pairwise.wilcox.test(dfThree$y, factor(dfThree$x, chrLabels), p.adjust.method = "holm", exact = FALSE)$p.value
  for (iPair in seq_len(nrow(lTheirs$rows))) {
    lPair <- lTheirs$rows[iPair, ]
    nHolm <- mHolm[lPair$group_2, lPair$group_1]
    if (is.na(nHolm)) nHolm <- mHolm[lPair$group_1, lPair$group_2]
    strPair <- paste0(lPair$group_1, " and ", lPair$group_2, " (n = ", lPair$n_1, ", ", lPair$n_2, "): ", Output_P(nHolm))
    expect_true(grepl(strPair, strHeading, fixed = TRUE), label = strPair)
  }
})

test_that("the scatter's line lists the slope, the intercept and R-squared, as the chart does, and a logarithmic axis draws R's line back on the values (#37)", {
  gg <- lFigureCalls()$association_scatter()
  strCaption <- strFigureCaption(gg)
  dfRows <- data.frame(x = nResultAt("TNF-alpha", "Baseline"), y = nResultAt("IL-10", "Baseline"))
  dfRows <- dfRows[stats::complete.cases(dfRows), ]
  lLm <- stats::lm(y ~ x, data = dfRows)
  expect_lt(regexpr("Slope:", strCaption, fixed = TRUE), regexpr("Intercept:", strCaption, fixed = TRUE))
  expect_match(strCaption, paste0("R-squared: ", Output_Figure(summary(lLm)$r.squared), "."), fixed = TRUE)
  # On a logarithmic x axis R is handed log10(x), and its line is drawn back at
  # the values themselves.
  ggLog <- Visualize_AssociationScatter(Synthetic_Results, Synthetic_Participants, list(
    x = list(measure = "CRP", visit = "Baseline"), y = list(measure = "IL-10", visit = "Baseline"), fit = "linear", x_scale = "log"
  ))
  dfLog <- data.frame(x = nResultAt("CRP", "Baseline"), y = nResultAt("IL-10", "Baseline"))
  dfLog <- dfLog[stats::complete.cases(dfLog) & dfLog$x > 0, ]
  lLog <- stats::lm(y ~ log10(x), data = dfLog)
  dfLine <- ggLog$layers[[2]]$data
  expect_equal(dfLine$fit, unname(stats::coef(lLog)[1] + stats::coef(lLog)[2] * log10(dfLine$x)), tolerance = 1e-10)
  expect_equal(range(dfLine$x), range(dfLog$x), tolerance = 1e-10)
})

test_that("the screen writes each row and its caption in the chart's words: the interval's level, both p-values by name, and the exploratory label (#37)", {
  gg <- lFigureCalls()$biomarker_screen()
  strCaption <- strFigureCaption(gg)
  expect_match(
    strCaption,
    enc2utf8(paste0(
      "Each row: Standardised difference (Hedges", intToUtf8(0x2019L), " g), Placebo less Treatment, with its 95% confidence interval on one axis without units. ",
      "p: Welch Two Sample t-test, unadjusted, and adjusted by Benjamini-Hochberg across the 12 biomarkers with a p-value. Exploratory, adjusted (Benjamini-Hochberg)."
    )),
    fixed = TRUE
  )
  chrRows <- levels(gg$data$row)
  expect_true(any(grepl("; Unadjusted: p = 0.530; Benjamini-Hochberg: p = 0.636; n, Placebo / Treatment: 94 / 91$", chrRows)))
})

test_that("R's counts are named group by group up to four, and as a range from five (#37)", {
  lCounts <- list(A = 10L, B = 12L, C = 9L, D = 11L, E = 8L)
  expect_identical(Output_CountsText(lCounts[1:3]), "A n = 10, B n = 12, C n = 9")
  expect_identical(Output_CountsText(lCounts[1:4]), "A n = 10, B n = 12, C n = 9, D n = 11")
  expect_identical(Output_CountsText(lCounts), "n = 8 to 12 across 5 groups")
})

test_that("each of the gallery's figures has a text alternative (#37)", {
  strGallery <- testthat::test_path("..", "..", "vignettes", "articles", "gallery.Rmd")
  skip_if_not(file.exists(strGallery), "the gallery is in the source tree, not the built package")
  chrLines <- readLines(strGallery, warn = FALSE)
  chrChunks <- grep("^```\\{r [a-z-]+", chrLines, value = TRUE)
  chrFigures <- grep("setup", chrChunks, value = TRUE, invert = TRUE)
  expect_length(chrFigures, 6L)
  expect_true(all(grepl("fig.alt = ", chrFigures, fixed = TRUE)), label = paste(chrFigures, collapse = " / "))
})
