# Degenerate inputs, found by the v0.1.0 release candidate's review (#24). A
# statistic whose input does not allow it answers with a reason in place of the
# numbers, as t.test() does for data that are essentially constant, never with
# "ok" and a rounding-noise p-value, and never with "ok" and no number at all.

# Not computed: the status given, a reason in words, and no number.
ExpectReason <- function(lResult, strStatus, strPattern, strLabel) {
  ExpectResultShape(lResult)
  expect_identical(lResult$status, strStatus, label = paste(strLabel, "status"))
  expect_match(lResult$reason, strPattern, label = paste(strLabel, "reason"))
  expect_true(is.na(lResult$p_value), label = paste(strLabel, "p-value"))
  expect_identical(nrow(lResult$estimates), 0L, label = paste(strLabel, "estimates"))
}

# An answer that is "ok" has a number: a p-value, or an estimate.
ExpectNumbers <- function(lResult, strLabel) {
  expect_identical(lResult$status, "ok", label = paste(strLabel, "status"))
  expect_true(is.finite(lResult$p_value) || any(is.finite(lResult$estimates$estimate)), label = paste(strLabel, "has a number"))
}

chrThree <- rep(c("A", "B", "C"), each = 6)

test_that("a one-way ANOVA on values that do not vary within the groups is not computed, as t.test() refuses them (#24)", {
  ExpectReason(
    Analyze_GroupDifference(data.frame(v = 1, g = chrThree), "v", "g", strMethod = "anova"),
    "error", "essentially constant", "identical values"
  )
  ExpectReason(
    Analyze_GroupDifference(data.frame(v = 0, g = chrThree), "v", "g", strMethod = "anova"),
    "error", "essentially constant", "every value zero"
  )
  ExpectReason(
    Analyze_GroupDifference(data.frame(v = rep(c(1, 2, 3), each = 6), g = chrThree), "v", "g", strMethod = "anova"),
    "error", "essentially constant", "constant within each group"
  )
  # Two groups were already refused, by t.test() itself, for every test.
  for (strMethod in c("t", "wilcoxon", "anova", "kruskal")) {
    ExpectReason(
      Analyze_GroupDifference(data.frame(v = 1, g = rep(c("A", "B"), each = 6)), "v", "g", strMethod = strMethod),
      "error", "essentially constant", paste(strMethod, "two groups")
    )
  }
  # Values that vary in one group are an ANOVA.
  dfOne <- data.frame(v = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3.5, 2.5, 3, 4, 3), g = chrThree)
  lOne <- Analyze_GroupDifference(dfOne, "v", "g", strMethod = "anova")
  ExpectNumbers(lOne, "one group varies")
  expect_equal(lOne$p_value, summary(stats::aov(v ~ factor(g), dfOne))[[1]][1, "Pr(>F)"], tolerance = 1e-12)
})

test_that("Kruskal-Wallis with every value tied is not computed, and a test with no p-value is never ok (#24)", {
  ExpectReason(
    Analyze_GroupDifference(data.frame(v = 1, g = chrThree), "v", "g", strMethod = "kruskal"),
    "error", "every value is 1", "every value tied"
  )
  # Values tied within groups but not between them are a rank test.
  ExpectNumbers(
    Analyze_GroupDifference(data.frame(v = rep(c(1, 2, 3), each = 6), g = chrThree), "v", "g", strMethod = "kruskal"),
    "tied within groups"
  )
})

test_that("a linear fit with a y that does not vary is not computed, overall and within each group (#24)", {
  set.seed(24)
  dfData <- data.frame(x = stats::rnorm(20), y = 3, g = rep(c("A", "B"), 10))
  for (strMethod in c("linear", "smooth")) {
    ExpectReason(Analyze_Fit(dfData, "x", "y", strMethod = strMethod), "error", "every y value is 3", paste(strMethod, "constant y"))
  }
  ExpectReason(Analyze_Fit(transform(dfData, y = 0), "x", "y"), "error", "every y value is 0", "every y zero")
  # Within a group: the group's rows say why, and the other groups are fitted.
  dfData$y2 <- stats::rnorm(20)
  dfData$y2[dfData$g == "B"] <- 5
  lFit <- Analyze_Fit(dfData, "x", "y2", strGroupCol = "g")
  ExpectNumbers(lFit, "overall")
  dfB <- lFit$rows[!is.na(lFit$rows$group) & lFit$rows$group == "B", ]
  expect_identical(nrow(dfB), 1L)
  expect_identical(dfB$status, "error")
  expect_match(dfB$reason, "every y value is 5")
  expect_true(is.na(dfB$p_value) && is.na(dfB$fit))
  expect_false("B" %in% lFit$estimates$group)
  expect_identical(unique(lFit$rows$status[!is.na(lFit$rows$group) & lFit$rows$group == "A"]), "ok")
})

test_that("a correlation with a variable that does not vary is not computed: alone, in a matrix and in a screen (#24)", {
  set.seed(24)
  dfData <- data.frame(x = stats::rnorm(20), y = 3, z = stats::rnorm(20), g = rep(c("A", "B"), 10))
  for (strMethod in c("pearson", "spearman")) {
    ExpectReason(Analyze_Correlation(dfData, "x", "y", strMethod = strMethod), "error", "does not vary", strMethod)
  }
  # Within a group.
  dfData$w <- dfData$z
  dfData$w[dfData$g == "A"] <- 1
  lGroups <- Analyze_Correlation(dfData, "x", "w", strGroupCol = "g")
  ExpectNumbers(lGroups, "overall")
  expect_identical(lGroups$rows$status, c("error", "ok"))
  expect_match(lGroups$rows$reason[1], "does not vary")
  # A cell of a matrix, and a matrix with no cell computed.
  lMatrix <- Analyze_CorrelationMatrix(dfData, c("x", "y", "z"))
  expect_identical(lMatrix$status, "ok")
  expect_identical(lMatrix$rows$status, c("error", "ok", "error"))
  expect_true(all(grepl("does not vary", lMatrix$rows$reason[c(1, 3)])))
  expect_true(all(is.na(lMatrix$rows$estimate[c(1, 3)])))
  lNone <- Analyze_CorrelationMatrix(transform(dfData, z = 2), c("y", "z"))
  expect_identical(lNone$status, "error")
  expect_match(lNone$reason, "no pair of columns could be computed")
  # A row of the correlation screen is not ok, and is not in the adjustment.
  lScreen <- Analyze_Screen(dfData, c("x", "y", "z"), strComparison = "correlation", strWithCol = "z")
  expect_identical(lScreen$rows$status, c("ok", "error", "ok"))
  expect_true(is.na(lScreen$rows$p_unadjusted[2]) && is.na(lScreen$rows$adjusted_over[2]))
  expect_identical(lScreen$rows$adjusted_over[c(1, 3)], c(2L, 2L))
})

test_that("survival with no event in any group is not computed (#24)", {
  set.seed(24)
  for (nGroups in c(2L, 3L)) {
    dfData <- data.frame(t = stats::rexp(10 * nGroups), e = 0, g = rep(LETTERS[seq_len(nGroups)], 10))
    lNone <- Analyze_Survival(dfData, "t", "g", strEventCol = "e")
    ExpectReason(lNone, "error", "no event", paste(nGroups, "groups"))
    expect_true(all(is.na(lNone$rows$hr_test)))
  }
})

test_that("survival where one of two groups has no events keeps the log-rank test and reports no hazard ratio (#24)", {
  set.seed(24)
  dfData <- data.frame(t = stats::rexp(20), e = 0L, g = rep(c("A", "B"), 10))
  dfData$e[dfData$g == "A"] <- c(1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L)
  lResult <- Analyze_Survival(dfData, "t", "g", strEventCol = "e")
  ExpectResultShape(lResult)
  # The log-rank test is R's, and does not need the hazard ratio.
  expect_identical(lResult$status, "ok")
  lLogRank <- survival::survdiff(survival::Surv(t, e) ~ factor(g), data = dfData)
  expect_equal(lResult$p_value, stats::pchisq(lLogRank$chisq, 1, lower.tail = FALSE), tolerance = 1e-12)
  # No hazard ratio: no row, no number, no test named, and a note naming the group.
  expect_false("Hazard ratio" %in% lResult$estimates$name)
  expect_true(all(is.na(lResult$rows$hazard_ratio)) && all(is.na(lResult$rows$hr_lower)) && all(is.na(lResult$rows$hr_upper)))
  expect_true(all(is.na(lResult$rows$hr_p_value)) && all(is.na(lResult$rows$hr_test)))
  expect_true(any(grepl("not estimable: B has no events", unlist(lResult$notes), fixed = TRUE)))
  expect_false(any(grepl("infinite", unlist(lResult$warnings))))
  # The screen by hazard ratio says why the row has no number.
  dfScreen <- data.frame(Time = dfData$t, Event = dfData$e, Marker = ifelse(dfData$g == "A", 2, 1))
  lScreen <- Analyze_Screen(dfScreen, "Marker", strComparison = "hazard", strTimeCol = "Time", strEventCol = "Event")
  expect_identical(lScreen$rows$status, "error")
  expect_match(lScreen$rows$reason, "not estimable")
  expect_true(is.na(lScreen$rows$estimate) && is.na(lScreen$rows$p_unadjusted))
})

test_that("a smooth needs seven complete pairs, and one whose band is not finite is not ok (#24)", {
  set.seed(24)
  for (nPairs in 5:6) {
    dfData <- data.frame(x = seq_len(nPairs), y = stats::rnorm(nPairs))
    lSmooth <- Analyze_Fit(dfData, "x", "y", strMethod = "smooth")
    ExpectReason(lSmooth, "too_small", "a smooth needs at least 7", paste(nPairs, "pairs"))
    expect_length(lSmooth$warnings, 0L)
    # A line needs only the minimum group size.
    ExpectNumbers(Analyze_Fit(dfData, "x", "y"), paste(nPairs, "pairs, a line"))
  }
  lSeven <- Analyze_Fit(data.frame(x = c(1, 2, 3, 5, 6, 8, 9), y = c(2, 1, 4, 3, 6, 5, 7)), "x", "y", strMethod = "smooth")
  expect_identical(lSeven$status, "ok")
  expect_true(all(is.finite(c(lSeven$rows$fit, lSeven$rows$lower, lSeven$rows$upper))))
  # Seven pairs on which loess() gives a band that is not finite somewhere.
  dfNotFinite <- data.frame(x = c(6.44, 2.81, 9.58, 1.58, 4.18, 2.52, 0.94), y = c(0.95, 0.43, 1.01, -0.39, 0.38, 0.24, -1.43))
  lNotFinite <- Analyze_Fit(dfNotFinite, "x", "y", strMethod = "smooth")
  ExpectReason(lNotFinite, "error", "not finite", "a band that is not finite")
  expect_true(all(is.na(lNotFinite$rows$fit)))
  # The minimum group size, when it is larger, is the minimum.
  expect_identical(Analyze_Fit(data.frame(x = 1:8, y = stats::rnorm(8)), "x", "y", strMethod = "smooth", nMinGroup = 9)$status, "too_small")
})

test_that("the Wilcoxon rank-sum test on small groups with no ties is wilcox.test()'s exact test (#24)", {
  dfData <- data.frame(v = c(1.1, 2.2, 2.5, 3, 4, 3.1, 4.2, 5.3, 5.5, 6), g = rep(c("A", "B"), each = 5))
  lResult <- Analyze_GroupDifference(dfData, "v", "g", strMethod = "wilcoxon")
  lExact <- stats::wilcox.test(dfData$v[1:5], dfData$v[6:10])
  expect_identical(lResult$method, "Wilcoxon rank sum exact test")
  expect_identical(lResult$p_value, lExact$p.value)
  expect_false(isTRUE(all.equal(lResult$p_value, stats::wilcox.test(dfData$v[1:5], dfData$v[6:10], exact = FALSE)$p.value)))
})

test_that("on a logarithmic axis the group comparison leaves out zero and less, and asks R for the rest (#24)", {
  dfResults <- Synthetic_Results
  bIL6 <- dfResults$TEST == "IL-6" & dfResults$VISIT == "Baseline"
  iZero <- which(bIL6)[1:7]
  iNegative <- which(bIL6)[8:9]
  dfResults$STRESN[iZero] <- 0
  dfResults$STRESN[iNegative] <- -1
  lConfig <- GroupComparison_Settings(list(start_value = "IL-6", visits = "Baseline", group_by = "ARM", y_scale = "log"))
  lRequests <- GroupComparison_Requests(dfResults, Synthetic_Participants, lConfig, GroupComparison_State(dfResults, Synthetic_Participants, lConfig))
  expect_length(lRequests, 1L)
  nPositive <- sum(bIL6 & dfResults$STRESN > 0)
  expect_identical(lRequests[[1]]$rows, nPositive)
  expect_true(all(lRequests[[1]]$data$y > 0))
  expect_identical(nPositive, sum(bIL6) - 9L)
  expect_true(lRequests[[1]]$dataId$positive_only)
})
