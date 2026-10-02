# Analyze_GroupDifference (#3): t.test (Welch), wilcox.test, aov and
# kruskal.test, pairwise comparisons adjusted by p.adjust, and the difference in
# means with its interval. Every comparison with base R below is exact
# (expect_identical), except the two cross-checks against pairwise.t.test() and
# pairwise.wilcox.test(), which order each pair the other way round.

dfFrame <- dfSyntheticFrame()
lTruth <- Synthetic_Truth$GroupDifference
chrArms <- lTruth$Groups
nFirst <- dfFrame$Change[dfFrame$ARM == chrArms[1] & !is.na(dfFrame$Change)]
nSecond <- dfFrame$Change[dfFrame$ARM == chrArms[2] & !is.na(dfFrame$Change)]
chrFour <- sort(unique(dfFrame$ARM_SEX), method = "radix")
dfComplete <- dfFrame[!is.na(dfFrame$Change), ]
dfComplete$Group <- factor(dfComplete$ARM_SEX, levels = chrFour)

test_that("the Welch t-test equals t.test() on the synthetic study (#3)", {
  lResult <- Analyze_GroupDifference(dfFrame, "Change", "ARM", strMethod = "t", chrGroups = chrArms)
  lBase <- stats::t.test(nFirst, nSecond)

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$test, "t")
  expect_identical(lResult$method, "Welch Two Sample t-test")
  expect_identical(lResult$method, unname(lBase$method))
  expect_identical(
    lResult$statistic,
    data.frame(name = c("t", "df"), value = c(unname(lBase$statistic), unname(lBase$parameter)))
  )
  expect_identical(lResult$p_value, lBase$p.value)
  expect_identical(lResult$adjustment, "none")
  expect_identical(
    lResult$estimates,
    data.frame(
      name = c("Mean", "Mean", "Difference in means"),
      group = c(chrArms, paste(chrArms[1], "-", chrArms[2])),
      estimate = c(mean(nFirst), mean(nSecond), unname(lBase$estimate[1] - lBase$estimate[2])),
      lower = c(NA, NA, lBase$conf.int[1]),
      upper = c(NA, NA, lBase$conf.int[2]),
      level = c(NA, NA, 0.95)
    )
  )
  # The counts are the participants actually used, per group, and the rest are
  # accounted for.
  expect_identical(lResult$counts, stats::setNames(list(length(nFirst), length(nSecond)), chrArms))
  expect_identical(lResult$dropped, data.frame(reason = "Missing value", n = sum(is.na(dfFrame$Change))))
  expect_identical(nrow(lResult$rows), 0L)

  # The formula interface gives the same numbers, the other way round.
  lDefault <- Analyze_GroupDifference(dfFrame, "Change", "ARM")
  lFormula <- stats::t.test(Change ~ ARM, data = dfFrame)
  expect_identical(lDefault$p_value, lFormula$p.value)
  expect_identical(lDefault$estimates$lower[3], lFormula$conf.int[1])
  expect_identical(lDefault$estimates$group[3], "Placebo - Treatment")
})

test_that("the planted group difference falls inside its interval (#3)", {
  lResult <- Analyze_GroupDifference(
    dfFrame, "Change", lTruth$GroupCol,
    strMethod = "t", chrGroups = lTruth$Groups
  )
  dfDifference <- lResult$estimates[lResult$estimates$name == "Difference in means", ]

  expect_identical(dfDifference$group, paste(lTruth$Groups[1], "-", lTruth$Groups[2]))
  expect_gt(lTruth$Value, dfDifference$lower)
  expect_lt(lTruth$Value, dfDifference$upper)
  expect_lt(lResult$p_value, 0.001)
})

test_that("the Wilcoxon rank-sum test equals wilcox.test() on the synthetic study (#3)", {
  lResult <- Analyze_GroupDifference(dfFrame, "Change", "ARM", strMethod = "wilcoxon", chrGroups = chrArms)
  lBase <- lWithWarnings(function() stats::wilcox.test(nFirst, nSecond))
  lWelch <- stats::t.test(nFirst, nSecond)

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$method, unname(lBase$value$method))
  expect_identical(lResult$statistic, data.frame(name = "W", value = unname(lBase$value$statistic)))
  expect_identical(lResult$p_value, lBase$value$p.value)
  expect_identical(lResult$warnings, lBase$warnings)
  # The difference in means and its interval are t.test()'s, and a note says so.
  expect_identical(lResult$estimates$estimate[3], unname(lWelch$estimate[1] - lWelch$estimate[2]))
  expect_identical(lResult$estimates$lower[3], lWelch$conf.int[1])
  expect_identical(lResult$estimates$upper[3], lWelch$conf.int[2])
  expect_match(lResult$notes[[1]], "t.test() (Welch)", fixed = TRUE)
})

test_that("one-way ANOVA equals aov(), and its pairwise comparisons equal t.test() and p.adjust() (#3)", {
  lResult <- Analyze_GroupDifference(dfFrame, "Change", "ARM_SEX", strMethod = "anova")
  mBase <- summary(stats::aov(Change ~ Group, data = dfComplete))[[1]]

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$method, "One-way analysis of variance")
  expect_identical(
    lResult$statistic,
    data.frame(name = c("F", "num df", "denom df"), value = c(mBase[1, "F value"], mBase[, "Df"]))
  )
  expect_identical(lResult$p_value, mBase[1, "Pr(>F)"])
  expect_identical(lResult$estimates$group, chrFour)
  expect_identical(
    lResult$estimates$estimate,
    unname(vapply(chrFour, function(strGroup) mean(dfComplete$Change[dfComplete$ARM_SEX == strGroup]), numeric(1)))
  )
  expect_identical(lResult$counts, as.list(table(dfComplete$ARM_SEX)[chrFour]))

  # Six pairs of four groups, each a Welch t-test, adjusted across the six.
  dfRows <- lResult$rows
  expect_identical(nrow(dfRows), 6L)
  nRaw <- numeric(6)
  for (iPair in seq_len(6)) {
    nOne <- dfComplete$Change[dfComplete$ARM_SEX == dfRows$group_1[iPair]]
    nTwo <- dfComplete$Change[dfComplete$ARM_SEX == dfRows$group_2[iPair]]
    lPair <- stats::t.test(nOne, nTwo)
    nRaw[iPair] <- lPair$p.value
    expect_identical(dfRows$estimate[iPair], unname(lPair$estimate[1] - lPair$estimate[2]))
    expect_identical(c(dfRows$lower[iPair], dfRows$upper[iPair]), as.numeric(lPair$conf.int))
    expect_identical(dfRows$statistic[iPair], unname(lPair$statistic))
    expect_identical(dfRows$method[iPair], unname(lPair$method))
    expect_identical(c(dfRows$n_1[iPair], dfRows$n_2[iPair]), c(length(nOne), length(nTwo)))
    expect_identical(dfRows$counts[iPair], length(nOne) + length(nTwo))
  }
  expect_identical(dfRows$p_unadjusted, nRaw)
  expect_identical(dfRows$p_value, stats::p.adjust(nRaw, method = "holm"))
  expect_identical(dfRows$adjustment, rep("holm", 6))
  expect_identical(dfRows$status, rep("ok", 6))

  # R's own pairwise function agrees, to testthat's default tolerance.
  mPairwise <- stats::pairwise.t.test(dfComplete$Change, dfComplete$Group, pool.sd = FALSE)$p.value
  expect_equal(dfRows$p_value, mPairwise[cbind(dfRows$group_2, dfRows$group_1)])
})

test_that("Kruskal-Wallis equals kruskal.test(), and its pairwise comparisons equal wilcox.test() and p.adjust() (#3)", {
  lResult <- Analyze_GroupDifference(dfFrame, "Change", "ARM_SEX", strMethod = "kruskal", strPAdjust = "BH")
  lBase <- stats::kruskal.test(Change ~ Group, data = dfComplete)

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$method, unname(lBase$method))
  expect_identical(
    lResult$statistic,
    data.frame(
      name = c("Kruskal-Wallis chi-squared", "df"),
      value = c(unname(lBase$statistic), unname(lBase$parameter))
    )
  )
  expect_identical(lResult$p_value, lBase$p.value)

  dfRows <- lResult$rows
  nRaw <- numeric(6)
  chrAll <- character(0)
  for (iPair in seq_len(6)) {
    nOne <- dfComplete$Change[dfComplete$ARM_SEX == dfRows$group_1[iPair]]
    nTwo <- dfComplete$Change[dfComplete$ARM_SEX == dfRows$group_2[iPair]]
    lPair <- lWithWarnings(function() stats::wilcox.test(nOne, nTwo))
    nRaw[iPair] <- lPair$value$p.value
    chrAll <- c(chrAll, unlist(lPair$warnings))
    expect_identical(dfRows$statistic[iPair], unname(lPair$value$statistic))
    expect_identical(dfRows$method[iPair], unname(lPair$value$method))
    # Each pair's warning stays with its row.
    expect_identical(
      dfRows$warning[iPair],
      if (length(lPair$warnings) > 0L) paste(unlist(lPair$warnings), collapse = "; ") else NA_character_
    )
  }
  expect_identical(dfRows$p_unadjusted, nRaw)
  expect_identical(dfRows$p_value, stats::p.adjust(nRaw, method = "BH"))
  expect_identical(dfRows$adjustment, rep("BH", 6))
  # Whatever this version of R warns about, ties for one, is kept; newer
  # versions compute the exact p-value with ties and have nothing to say.
  expect_identical(lResult$warnings, as.list(unique(chrAll)))

  mPairwise <- suppressWarnings(
    stats::pairwise.wilcox.test(dfComplete$Change, dfComplete$Group, p.adjust.method = "BH")$p.value
  )
  expect_equal(dfRows$p_value, mPairwise[cbind(dfRows$group_2, dfRows$group_1)])

  # Pairwise comparisons can be switched off.
  lPlain <- Analyze_GroupDifference(dfFrame, "Change", "ARM_SEX", strMethod = "kruskal", bPairwise = FALSE)
  expect_identical(nrow(lPlain$rows), 0L)
  expect_identical(lPlain$p_value, lBase$p.value)
})

test_that("a group below the minimum size gets a reason and no numbers (#3)", {
  # Four Treatment participants with a value: below the default minimum of 5.
  dfSmall <- rbind(dfComplete[dfComplete$ARM == "Placebo", ], head(dfComplete[dfComplete$ARM == "Treatment", ], 4))
  for (strMethod in c("t", "wilcoxon", "anova", "kruskal")) {
    lResult <- Analyze_GroupDifference(dfSmall, "Change", "ARM", strMethod = strMethod)
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "too_small")
    expect_identical(lResult$reason, "Not computed: Treatment has 4. The minimum group size is 5.")
    expect_identical(lResult$counts, list(Placebo = sum(dfComplete$ARM == "Placebo"), Treatment = 4L))
    expect_true(is.na(lResult$p_value))
    expect_true(is.na(lResult$method))
  }

  # The minimum is the caller's to set, and the default is the file's constant.
  expect_identical(formals(Analyze_GroupDifference)$nMinGroup, quote(nMinGroupDefault))
  expect_identical(Analyze_GroupDifference(dfSmall, "Change", "ARM", nMinGroup = 4)$status, "ok")
  expect_identical(Analyze_GroupDifference(dfFrame, "Change", "ARM", nMinGroup = 96)$status, "too_small")
})

test_that("an R error in the wrapped call becomes a status with R's message (#3)", {
  dfConstant <- data.frame(Value = rep(c(1, 2), each = 6), Group = rep(c("A", "B"), each = 6))
  strMessage <- tryCatch(stats::t.test(rep(1, 6), rep(2, 6)), error = conditionMessage)

  expect_silent(lResult <- Analyze_GroupDifference(dfConstant, "Value", "Group"))
  ExpectResultShape(lResult)
  expect_identical(lResult$status, "error")
  expect_identical(lResult$reason, strMessage)
  expect_identical(lResult$counts, list(A = 6L, B = 6L))
})

test_that("a request that cannot be met is an error status, never an R error (#3)", {
  lCases <- list(
    list(dfData = dfFrame, strValueCol = "Nope", strGroupCol = "ARM"),
    list(dfData = dfFrame, strValueCol = "ARM", strGroupCol = "ARM"),
    list(dfData = dfFrame, strValueCol = "Change", strGroupCol = "ARM", strMethod = "median"),
    list(dfData = dfFrame, strValueCol = "Change", strGroupCol = "ARM", strPAdjust = "sidak"),
    list(dfData = dfFrame, strValueCol = "Change", strGroupCol = "ARM", nConfLevel = 95),
    list(dfData = dfFrame, strValueCol = "Change", strGroupCol = "ARM", bPairwise = "yes"),
    list(dfData = dfFrame, strValueCol = "Change", strGroupCol = "ARM", chrGroups = c("Placebo", "Placebo")),
    list(dfData = as.list(dfFrame), strValueCol = "Change", strGroupCol = "ARM"),
    # A two-group test asked of four groups, and any test asked of one.
    list(dfData = dfFrame, strValueCol = "Change", strGroupCol = "ARM_SEX", strMethod = "t"),
    list(dfData = dfFrame, strValueCol = "Change", strGroupCol = "ARM", strMethod = "anova", chrGroups = "Placebo")
  )
  for (lArgs in lCases) {
    expect_silent(lResult <- do.call(Analyze_GroupDifference, lArgs))
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "error")
  }
  expect_match(
    Analyze_GroupDifference(dfFrame, "Change", "ARM_SEX", strMethod = "t")$reason,
    "'t' compares exactly two groups and 4 were found",
    fixed = TRUE
  )
})

test_that("inputs as JSON delivers them are handled deliberately, and what is dropped is counted (#3)", {
  lReference <- Analyze_GroupDifference(dfFrame, "Change", "ARM", chrGroups = chrArms)

  # A vector-valued argument as an unnamed list of single values.
  expect_identical(Analyze_GroupDifference(dfFrame, "Change", "ARM", chrGroups = as.list(chrArms)), lReference)
  # A factor for the group and whole numbers for the value.
  dfTyped <- dfFrame
  dfTyped$ARM <- factor(dfTyped$ARM)
  expect_identical(Analyze_GroupDifference(dfTyped, "Change", "ARM", chrGroups = chrArms), lReference)
  lWhole <- Analyze_GroupDifference(dfFrame, "AGE", "ARM")
  expect_identical(lWhole$p_value, stats::t.test(AGE ~ ARM, data = dfFrame)$p.value)

  # A missing group, an empty group, a group not asked for and a value that is
  # not finite are each dropped and counted.
  dfMessy <- dfFrame
  dfMessy$ARM[1:3] <- NA
  dfMessy$ARM[4:5] <- ""
  dfMessy$ARM[6:9] <- "Other"
  dfMessy$Change[10] <- Inf
  lMessy <- Analyze_GroupDifference(dfMessy, "Change", "ARM", chrGroups = chrArms)
  bKept <- dfMessy$ARM %in% chrArms
  ExpectResultShape(lMessy)
  expect_identical(
    lMessy$dropped,
    data.frame(
      reason = c("Missing group", "Group not selected", "Missing value"),
      n = c(5L, 4L, sum(bKept & !is.finite(dfMessy$Change)))
    )
  )
  expect_identical(sum(unlist(lMessy$counts)) + sum(lMessy$dropped$n), nrow(dfMessy))

  # A column with nothing in it at all arrives from JSON as logical.
  dfEmpty <- dfFrame
  dfEmpty$Change <- NA
  lEmpty <- Analyze_GroupDifference(dfEmpty, "Change", "ARM")
  expect_identical(lEmpty$status, "too_small")
  expect_identical(lEmpty$counts, list(Placebo = 0L, Treatment = 0L))
})
