# Analyze_Screen (#4): one row per biomarker for a comparison chosen once, with
# a raw p-value and a p-value adjusted across the rows by p.adjust(). Every row
# is the matching single function's answer, so every comparison with it and
# with base R below is exact (expect_identical). The standardised difference is
# the package's own arithmetic and is compared with effectsize::hedges_g() to a
# stated tolerance.

dfFrame <- dfSyntheticFrame()
chrBiomarkers <- chrSyntheticBiomarkers()
chrChangeCols <- paste(chrBiomarkers, "change")
chrBaselineCols <- paste(chrBiomarkers, "@ Baseline")
lTruth <- Synthetic_Truth
chrArms <- lTruth$GroupDifference$Groups
chrScreenRowCols <- c(
  "biomarker", "counts", "n_1", "n_2", "events", "dropped", "estimate", "lower", "upper", "level",
  "method", "statistic", "p_unadjusted", "p_value", "adjustment", "adjusted_over", "status", "reason", "warning"
)

test_that("each difference row is Analyze_GroupDifference()'s Welch t-test for that biomarker (#4)", {
  lResult <- Analyze_Screen(dfFrame, chrChangeCols, "difference", strGroupCol = "ARM", chrGroups = chrArms)

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$test, "difference")
  expect_identical(lResult$method, "Welch Two Sample t-test")
  expect_true(is.na(lResult$p_value))
  expect_named(lResult$rows, chrScreenRowCols)
  # One row per biomarker, in the order they were asked for.
  expect_identical(lResult$rows$biomarker, chrChangeCols)

  for (iRow in seq_along(chrChangeCols)) {
    lSingle <- Analyze_GroupDifference(dfFrame, chrChangeCols[iRow], "ARM", strMethod = "t", chrGroups = chrArms)
    nValue <- dfFrame[[chrChangeCols[iRow]]]
    lBase <- stats::t.test(nValue[dfFrame$ARM == chrArms[1]], nValue[dfFrame$ARM == chrArms[2]])
    expect_identical(lResult$rows$p_unadjusted[iRow], lSingle$p_value)
    expect_identical(lResult$rows$p_unadjusted[iRow], lBase$p.value)
    expect_identical(lResult$rows$statistic[iRow], unname(lBase$statistic))
    expect_identical(lResult$rows$method[iRow], lSingle$method)
    expect_identical(c(lResult$rows$n_1[iRow], lResult$rows$n_2[iRow]), unname(unlist(lSingle$counts)))
    expect_identical(lResult$rows$counts[iRow], sum(!is.na(nValue)))
    expect_identical(lResult$rows$dropped[iRow], sum(is.na(nValue)))
  }
  expect_identical(lResult$counts, stats::setNames(as.list(lResult$rows$counts), chrChangeCols))
  expect_true(all(is.na(lResult$rows$events)))
})

test_that("the screen's adjusted p-values equal p.adjust() across the rows (#4)", {
  lDefault <- Analyze_Screen(dfFrame, chrChangeCols, "difference", strGroupCol = "ARM", chrGroups = chrArms)
  # Benjamini-Hochberg unless the caller says otherwise.
  expect_identical(formals(Analyze_Screen)$strPAdjust, "BH")
  expect_identical(lDefault$rows$p_value, stats::p.adjust(lDefault$rows$p_unadjusted, method = "BH"))
  expect_identical(lDefault$rows$adjustment, rep("BH", 12))
  expect_identical(lDefault$rows$adjusted_over, rep(12L, 12))
  expect_identical(lDefault$adjustment, "none")
  expect_match(lDefault$notes[[2]], "adjusted across the 12 rows that have a p-value by p.adjust(method = 'BH')", fixed = TRUE)

  for (strPAdjust in c("holm", "none")) {
    lOther <- Analyze_Screen(
      dfFrame, chrChangeCols, "difference",
      strGroupCol = "ARM", chrGroups = chrArms, strPAdjust = strPAdjust
    )
    expect_identical(lOther$rows$p_unadjusted, lDefault$rows$p_unadjusted)
    expect_identical(lOther$rows$p_value, stats::p.adjust(lDefault$rows$p_unadjusted, method = strPAdjust))
    expect_identical(lOther$rows$adjustment, rep(strPAdjust, 12))
  }

  # The same holds for the other two comparisons.
  lCorrelation <- Analyze_Screen(dfFrame, chrBaselineCols[-4], "correlation", strWithCol = chrBaselineCols[4], strPAdjust = "holm")
  expect_identical(lCorrelation$rows$p_value, stats::p.adjust(lCorrelation$rows$p_unadjusted, method = "holm"))
  lHazard <- Analyze_Screen(dfFrame, chrBaselineCols, "hazard", strTimeCol = "AVAL", strCensorCol = "CNSR")
  expect_identical(lHazard$rows$p_value, stats::p.adjust(lHazard$rows$p_unadjusted, method = "BH"))
})

test_that("the standardised difference equals effectsize::hedges_g(), estimate and both bounds (#4)", {
  skip_if_not_installed("effectsize")
  lResult <- Analyze_Screen(dfFrame, chrChangeCols, "difference", strGroupCol = "ARM", chrGroups = chrArms)

  nWorst <- 0
  for (iRow in seq_along(chrChangeCols)) {
    nValue <- dfFrame[[chrChangeCols[iRow]]]
    nFirst <- nValue[dfFrame$ARM == chrArms[1] & !is.na(nValue)]
    nSecond <- nValue[dfFrame$ARM == chrArms[2] & !is.na(nValue)]
    dfTheirs <- effectsize::hedges_g(nFirst, nSecond)
    # The estimate is the same arithmetic: equal to the last few bits.
    expect_equal(lResult$rows$estimate[iRow], dfTheirs$Hedges_g, tolerance = 1e-12)
    # The bounds are found by different root-finders: equal to six decimals.
    expect_lt(abs(lResult$rows$lower[iRow] - dfTheirs$CI_low), 1e-6)
    expect_lt(abs(lResult$rows$upper[iRow] - dfTheirs$CI_high), 1e-6)
    expect_identical(lResult$rows$level[iRow], dfTheirs$CI)
    nWorst <- max(nWorst, abs(lResult$rows$lower[iRow] - dfTheirs$CI_low), abs(lResult$rows$upper[iRow] - dfTheirs$CI_high))
  }
  expect_lt(nWorst, 1e-6)

  # Another level, and the other way round, agree too.
  lNinety <- Analyze_Screen(dfFrame, chrChangeCols[1], "difference", strGroupCol = "ARM", chrGroups = rev(chrArms), nConfLevel = 0.9)
  nValue <- dfFrame[[chrChangeCols[1]]]
  dfTheirs <- effectsize::hedges_g(
    nValue[dfFrame$ARM == chrArms[2] & !is.na(nValue)], nValue[dfFrame$ARM == chrArms[1] & !is.na(nValue)],
    ci = 0.9
  )
  expect_equal(lNinety$rows$estimate, dfTheirs$Hedges_g, tolerance = 1e-12)
  expect_lt(abs(lNinety$rows$lower - dfTheirs$CI_low), 1e-6)
  expect_lt(abs(lNinety$rows$upper - dfTheirs$CI_high), 1e-6)
})

test_that("the standardised difference is Hedges' g by its definition, with a noncentral t interval (#4)", {
  # This one needs no other package: the definition, written out.
  lResult <- Analyze_Screen(dfFrame, chrChangeCols[1], "difference", strGroupCol = "ARM", chrGroups = chrArms)
  nValue <- dfFrame[[chrChangeCols[1]]]
  nFirst <- nValue[dfFrame$ARM == chrArms[1] & !is.na(nValue)]
  nSecond <- nValue[dfFrame$ARM == chrArms[2] & !is.na(nValue)]
  nDf <- length(nFirst) + length(nSecond) - 2
  nPooled <- sqrt(((length(nFirst) - 1) * stats::var(nFirst) + (length(nSecond) - 1) * stats::var(nSecond)) / nDf)
  nCorrection <- gamma(nDf / 2) / (sqrt(nDf / 2) * gamma((nDf - 1) / 2))
  nScale <- sqrt(1 / length(nFirst) + 1 / length(nSecond))

  expect_equal(lResult$rows$estimate, (mean(nFirst) - mean(nSecond)) / nPooled * nCorrection, tolerance = 1e-12)
  # Each bound is the noncentrality at which the observed t statistic sits in
  # the matching tail, put back on the scale of the estimate.
  nT <- unname(stats::t.test(nFirst, nSecond, var.equal = TRUE)$statistic)
  expect_equal(stats::pt(nT, nDf, ncp = lResult$rows$lower / (nScale * nCorrection)), 0.975, tolerance = 1e-8)
  expect_equal(stats::pt(nT, nDf, ncp = lResult$rows$upper / (nScale * nCorrection)), 0.025, tolerance = 1e-8)
  # Unit-free: the same on any scale.
  dfScaled <- dfFrame
  dfScaled[[chrChangeCols[1]]] <- dfScaled[[chrChangeCols[1]]] * 1000
  lScaled <- Analyze_Screen(dfScaled, chrChangeCols[1], "difference", strGroupCol = "ARM", chrGroups = chrArms)
  expect_equal(lScaled$rows$estimate, lResult$rows$estimate, tolerance = 1e-12)
})

test_that("each correlation row is Analyze_Correlation()'s answer for that biomarker (#4)", {
  strWith <- paste(lTruth$Correlation$Biomarkers[2], "@ Week 4")
  chrOthers <- setdiff(paste(chrBiomarkers, "@ Week 4"), strWith)
  for (strMethod in c("pearson", "spearman")) {
    lResult <- Analyze_Screen(dfFrame, chrOthers, "correlation", strWithCol = strWith, strCorMethod = strMethod)
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "ok")
    expect_named(lResult$rows, chrScreenRowCols)

    for (iRow in seq_along(chrOthers)) {
      lSingle <- Analyze_Correlation(dfFrame, chrOthers[iRow], strWith, strMethod = strMethod)
      lBase <- lWithWarnings(function() stats::cor.test(dfFrame[[chrOthers[iRow]]], dfFrame[[strWith]], method = strMethod))
      expect_identical(lResult$rows$p_unadjusted[iRow], lSingle$p_value)
      expect_identical(lResult$rows$p_unadjusted[iRow], lBase$value$p.value)
      expect_identical(lResult$rows$estimate[iRow], unname(lBase$value$estimate))
      expect_identical(lResult$rows$estimate[iRow], lSingle$estimates$estimate)
      expect_identical(lResult$rows$lower[iRow], lSingle$estimates$lower)
      expect_identical(lResult$rows$upper[iRow], lSingle$estimates$upper)
      expect_identical(lResult$rows$counts[iRow], lSingle$counts)
      expect_identical(lResult$rows$dropped[iRow], 200L - lSingle$counts)
    }
    expect_true(all(is.na(lResult$rows$n_1)) && all(is.na(lResult$rows$events)))
  }
})

test_that("each hazard row is Analyze_Survival()'s answer for that biomarker split at its median (#4)", {
  lResult <- Analyze_Screen(dfFrame, chrBaselineCols, "hazard", strTimeCol = "AVAL", strCensorCol = "CNSR")

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$method, "Log-rank test")
  expect_named(lResult$rows, chrScreenRowCols)

  for (iRow in seq_along(chrBaselineCols)) {
    nValue <- dfFrame[[chrBaselineCols[iRow]]]
    dfSplit <- data.frame(
      AVAL = dfFrame$AVAL, CNSR = dfFrame$CNSR,
      Level = ifelse(nValue > stats::median(nValue), "High", "Low")
    )
    lSingle <- Analyze_Survival(dfSplit, "AVAL", "Level", strCensorCol = "CNSR", chrGroups = c("High", "Low"))
    dfRatio <- lSingle$estimates[lSingle$estimates$name == "Hazard ratio", ]
    expect_identical(lResult$rows$p_unadjusted[iRow], lSingle$p_value)
    expect_identical(lResult$rows$statistic[iRow], lSingle$statistic$value[1])
    expect_identical(lResult$rows$estimate[iRow], dfRatio$estimate)
    expect_identical(lResult$rows$lower[iRow], dfRatio$lower)
    expect_identical(lResult$rows$upper[iRow], dfRatio$upper)
    expect_identical(c(lResult$rows$n_1[iRow], lResult$rows$n_2[iRow]), c(sum(dfSplit$Level == "High"), sum(dfSplit$Level == "Low")))
    expect_identical(lResult$rows$events[iRow], sum(dfFrame$CNSR == 0))

    # And the survival package's own calls, directly.
    dfSplit$Against <- factor(dfSplit$Level, levels = c("Low", "High"))
    lCox <- summary(survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ Against, data = dfSplit))
    lLogRank <- survival::survdiff(survival::Surv(AVAL, CNSR == 0) ~ factor(Level, levels = c("High", "Low")), data = dfSplit)
    expect_identical(lResult$rows$estimate[iRow], unname(lCox$conf.int[1, "exp(coef)"]))
    expect_identical(lResult$rows$statistic[iRow], lLogRank$chisq)
  }
  # The planted row is the single chart's answer on the planted split.
  lPlanted <- Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = c("High", "Low"))
  iPlanted <- match("CRP @ Baseline", lResult$rows$biomarker)
  expect_identical(lResult$rows$p_unadjusted[iPlanted], lPlanted$p_value)
  expect_match(lResult$notes[[4]], "a value on the median is low", fixed = TRUE)
})

test_that("a value on the median is low, and the split is among the rows used (#4)", {
  # Eleven values, the sixth on the median: five are above it and six are not.
  dfTied <- data.frame(
    Marker = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 99, NA),
    Time = c(5, 9, 4, 8, 6, 7, 3, 2, 4, 1, 2, NA, 3),
    Event = c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1)
  )
  lResult <- Analyze_Screen(dfTied, "Marker", "hazard", strTimeCol = "Time", strEventCol = "Event")

  ExpectResultShape(lResult)
  expect_identical(lResult$rows$status, "ok")
  # The row with no time is not used, so its 99 does not move the median, and
  # the row with no value has no level.
  expect_identical(c(lResult$rows$n_1, lResult$rows$n_2), c(5L, 6L))
  expect_identical(lResult$rows$counts, 11L)
  expect_identical(lResult$rows$dropped, 2L)
})

test_that("the planted biomarkers are the top rows of the screen, for each comparison (#4)", {
  # A difference between the arms in change from Baseline: IL-6.
  lDifference <- Analyze_Screen(
    dfFrame, chrChangeCols, "difference",
    strGroupCol = lTruth$GroupDifference$GroupCol, chrGroups = lTruth$GroupDifference$Groups
  )
  dfTop <- lDifference$rows[order(lDifference$rows$p_value), ]
  expect_identical(dfTop$biomarker[1], paste(lTruth$GroupDifference$Biomarker, "change"))
  expect_lt(dfTop$p_value[1], 0.001)
  expect_gt(dfTop$p_value[2], 0.05)
  expect_identical(which.max(abs(lDifference$rows$estimate)), match(dfTop$biomarker[1], chrChangeCols))
  # The planted effect is on the scale of the data; here it is unit-free, and
  # negative because Treatment is lower.
  expect_lt(dfTop$upper[1], 0)

  # A correlation with one of the planted pair: the other one.
  for (iFixed in 1:2) {
    strFixed <- paste(lTruth$Correlation$Biomarkers[iFixed], "@", lTruth$Correlation$Visit)
    strOther <- paste(lTruth$Correlation$Biomarkers[3 - iFixed], "@", lTruth$Correlation$Visit)
    lCorrelation <- Analyze_Screen(
      dfFrame, setdiff(chrBaselineCols, strFixed), "correlation",
      strWithCol = strFixed, strCorMethod = lTruth$Correlation$Method
    )
    dfTop <- lCorrelation$rows[order(lCorrelation$rows$p_value), ]
    expect_identical(dfTop$biomarker[1], strOther)
    expect_lt(dfTop$p_value[1], 0.001)
    expect_gt(dfTop$p_value[2], 0.05)
    expect_gt(lTruth$Correlation$Value, dfTop$lower[1])
    expect_lt(lTruth$Correlation$Value, dfTop$upper[1])
  }

  # A hazard ratio for high against low: CRP.
  lHazard <- Analyze_Screen(dfFrame, chrBaselineCols, "hazard", strTimeCol = "AVAL", strCensorCol = "CNSR")
  dfTop <- lHazard$rows[order(lHazard$rows$p_value), ]
  expect_identical(dfTop$biomarker[1], paste(lTruth$Survival$Biomarker, "@", lTruth$Survival$Visit))
  expect_lt(dfTop$p_value[1], 0.001)
  expect_gt(dfTop$p_value[2], 0.05)
  expect_gt(lTruth$Survival$Value, dfTop$lower[1])
  expect_lt(lTruth$Survival$Value, dfTop$upper[1])
})

test_that("one biomarker that cannot be computed does not sink the screen, and is left out of the adjustment (#4)", {
  dfPatchy <- dfFrame
  # Too few Treatment participants with a value, and a column that is not a number.
  dfPatchy$Sparse <- dfPatchy[[chrChangeCols[1]]]
  dfPatchy$Sparse[dfPatchy$ARM == "Treatment"][-(1:3)] <- NA
  dfPatchy$Text <- "x"
  chrCols <- c(chrChangeCols[1:4], "Sparse", "Text", "Nope")
  expect_silent(lResult <- Analyze_Screen(dfPatchy, chrCols, "difference", strGroupCol = "ARM", chrGroups = chrArms))

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$rows$status, c(rep("ok", 4), "too_small", "error", "error"))
  expect_identical(lResult$rows$reason[5], "Not computed: Treatment has 3. The minimum group size is 5.")
  expect_identical(lResult$rows$reason[6], "Column 'Text' (strValueCol) is not numeric.")
  expect_true(all(is.na(lResult$rows$estimate[5:7])) && all(is.na(lResult$rows$p_unadjusted[5:7])))
  expect_identical(lResult$rows$n_1[5], 3L)

  # The adjustment covers the four rows that are tests, and the result says so.
  expect_identical(lResult$rows$p_value[1:4], stats::p.adjust(lResult$rows$p_unadjusted[1:4], method = "BH"))
  expect_true(all(is.na(lResult$rows$p_value[5:7])))
  expect_identical(lResult$rows$adjusted_over, c(rep(4L, 4), NA, NA, NA))
  expect_identical(
    lResult$notes[[2]],
    "p_value is adjusted across the 4 rows that have a p-value by p.adjust(method = 'BH'); 3 of the 7 rows have none and are left out of the adjustment."
  )

  # When no row can be computed, the screen says which kind of nothing it is.
  lSmall <- Analyze_Screen(dfPatchy, "Sparse", "difference", strGroupCol = "ARM", chrGroups = chrArms)
  ExpectResultShape(lSmall)
  expect_identical(lSmall$status, "too_small")
  lBroken <- Analyze_Screen(dfPatchy, c("Text", "Nope"), "difference", strGroupCol = "ARM", chrGroups = chrArms)
  ExpectResultShape(lBroken)
  expect_identical(lBroken$status, "error")
  expect_identical(lBroken$reason, "No biomarker could be computed. Each row gives its reason.")
})

test_that("a screen request that cannot be met is an error status, never an R error (#4)", {
  lCases <- list(
    list(dfData = dfFrame, chrCols = chrChangeCols, strComparison = "median"),
    list(dfData = dfFrame, chrCols = chrChangeCols, strComparison = "difference"),
    list(dfData = dfFrame, chrCols = chrChangeCols, strComparison = "difference", strGroupCol = "ARM_SEX"),
    list(dfData = dfFrame, chrCols = chrChangeCols, strComparison = "difference", strGroupCol = "ARM", strPAdjust = "fdr2"),
    list(dfData = dfFrame, chrCols = character(0), strComparison = "difference", strGroupCol = "ARM"),
    list(dfData = dfFrame, chrCols = chrBaselineCols, strComparison = "correlation"),
    list(dfData = dfFrame, chrCols = chrBaselineCols, strComparison = "correlation", strWithCol = "ARM"),
    list(dfData = dfFrame, chrCols = chrBaselineCols, strComparison = "correlation", strWithCol = "AGE", strCorMethod = "kendall"),
    list(dfData = dfFrame, chrCols = chrBaselineCols, strComparison = "hazard", strTimeCol = "AVAL"),
    list(dfData = dfFrame, chrCols = chrBaselineCols, strComparison = "hazard", strCensorCol = "CNSR"),
    list(dfData = dfFrame, chrCols = chrBaselineCols, strComparison = "hazard", strTimeCol = "AVAL", strCensorCol = "AGE")
  )
  for (lArgs in lCases) {
    expect_silent(lResult <- do.call(Analyze_Screen, lArgs))
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "error")
  }
  expect_identical(
    Analyze_Screen(dfFrame, chrChangeCols, "difference", strGroupCol = "ARM_SEX")$reason,
    "A standardised difference compares exactly two groups and 4 were found. Name two in chrGroups."
  )

  # The biomarker columns as JSON's list of single values; one is still a row.
  expect_identical(
    Analyze_Screen(dfFrame, as.list(chrChangeCols), "difference", strGroupCol = "ARM"),
    Analyze_Screen(dfFrame, chrChangeCols, "difference", strGroupCol = "ARM")
  )
  lOne <- Analyze_Screen(dfFrame, chrChangeCols[1], "difference", strGroupCol = "ARM")
  ExpectResultShape(lOne)
  expect_identical(nrow(lOne$rows), 1L)
  expect_identical(lOne$rows$p_value, lOne$rows$p_unadjusted)
})
