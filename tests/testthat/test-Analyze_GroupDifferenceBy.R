# Analyze_GroupDifferenceBy (#52): the group test within each level of a
# column, one row per level, in one call. Every row is
# Analyze_GroupDifference()'s own answer on that level's rows, so every
# comparison with it below is exact (expect_identical), and the adjusted
# p-values are p.adjust()'s across the levels that have one.

dfLong <- dfSyntheticLong()
chrVisits <- chrSyntheticVisits()
lTruth <- Synthetic_Truth
chrArms <- lTruth$GroupDifference$Groups
strBiomarker <- lTruth$GroupDifference$Biomarker
# One biomarker across the visits, every row with a value.
dfOne <- dfLong[dfLong$TEST == strBiomarker & !is.na(dfLong$STRESN), ]

# The columns of a row: the level, the groups and their counts, then what a
# screen's row holds of a test.
chrByRowCols <- function(nGroups) {
  c(
    "by", paste0("group_", seq_len(nGroups)), paste0("n_", seq_len(nGroups)), "counts", "dropped",
    "estimate", "lower", "upper", "level", "method", "statistic", "p_unadjusted", "p_value",
    "adjustment", "adjusted_over", "status", "reason", "warning"
  )
}

# One level's rows, as a separate call is given them.
dfAtVisit <- function(dfData, strVisit) {
  dfData[dfData$VISIT == strVisit, ]
}

test_that("each level's row is Analyze_GroupDifference()'s answer on that level's rows, for every test (#52)", {
  lCases <- list(
    t = list(method = "t", group = "ARM"), wilcoxon = list(method = "wilcoxon", group = "ARM"),
    anova = list(method = "anova", group = "ARM_SEX"), kruskal = list(method = "kruskal", group = "ARM_SEX")
  )
  for (lCase in lCases) {
    lResult <- Analyze_GroupDifferenceBy(dfOne, "STRESN", lCase$group, "VISIT", strMethod = lCase$method, chrBy = chrVisits)
    chrGroups <- sort(unique(dfOne[[lCase$group]]), method = "radix")
    nGroups <- length(chrGroups)

    ExpectResultShape(lResult)
    expect_identical(lResult$status, "ok")
    expect_identical(lResult$test, lCase$method)
    expect_true(is.na(lResult$p_value))
    expect_identical(nrow(lResult$estimates), 0L)
    expect_named(lResult$rows, chrByRowCols(nGroups))
    # One row per level, in the order the levels were asked for.
    expect_identical(lResult$rows$by, chrVisits)

    for (iRow in seq_along(chrVisits)) {
      # The separate call, on the rows of that visit and nothing else said.
      lSingle <- Analyze_GroupDifference(
        dfAtVisit(dfOne, chrVisits[iRow]), "STRESN", lCase$group,
        strMethod = lCase$method, bPairwise = FALSE
      )
      dfRow <- lResult$rows[iRow, ]
      expect_identical(lSingle$status, "ok")
      expect_identical(dfRow$status, lSingle$status, label = paste(lCase$method, chrVisits[iRow], "status"))
      expect_identical(dfRow$p_unadjusted, lSingle$p_value, label = paste(lCase$method, chrVisits[iRow], "p_unadjusted"))
      expect_identical(dfRow$method, lSingle$method)
      expect_identical(dfRow$statistic, lSingle$statistic$value[1])
      expect_identical(unlist(dfRow[paste0("group_", seq_len(nGroups))], use.names = FALSE), names(lSingle$counts))
      expect_identical(unlist(dfRow[paste0("n_", seq_len(nGroups))], use.names = FALSE), unname(unlist(lSingle$counts)))
      expect_identical(dfRow$counts, sum(unlist(lSingle$counts)))
      expect_identical(dfRow$dropped, sum(lSingle$dropped$n))
      expect_true(is.na(dfRow$reason))
      # With two groups the estimate is the difference in means, first minus
      # second, with its interval; with more there is none.
      dfDifference <- lSingle$estimates[lSingle$estimates$name == "Difference in means", ]
      if (nGroups == 2L) {
        expect_identical(c(dfRow$estimate, dfRow$lower, dfRow$upper, dfRow$level), c(dfDifference$estimate, dfDifference$lower, dfDifference$upper, dfDifference$level))
      } else {
        expect_identical(nrow(dfDifference), 0L)
        expect_true(all(is.na(c(dfRow$estimate, dfRow$lower, dfRow$upper, dfRow$level))))
      }
      # The warnings R raised at that level are on its row.
      expect_identical(
        dfRow$warning,
        if (length(lSingle$warnings) > 0L) paste(unlist(lSingle$warnings), collapse = "; ") else NA_character_
      )
    }
    # And against base R itself, for the two-group tests.
    if (lCase$method == "t") {
      for (iRow in seq_along(chrVisits)) {
        dfVisit <- dfAtVisit(dfOne, chrVisits[iRow])
        lBase <- stats::t.test(dfVisit$STRESN[dfVisit$ARM == chrGroups[1]], dfVisit$STRESN[dfVisit$ARM == chrGroups[2]])
        expect_identical(lResult$rows$p_unadjusted[iRow], lBase$p.value)
        expect_identical(lResult$rows$statistic[iRow], unname(lBase$statistic))
      }
    }
    expect_identical(lResult$counts, stats::setNames(as.list(lResult$rows$counts), chrVisits))
    expect_identical(lResult$method, lResult$rows$method[1])
  }
})

test_that("the adjusted p-values equal p.adjust() over the separate calls, for Holm and Benjamini-Hochberg, and none is the default (#52)", {
  # The separate calls: one per visit.
  nSeparate <- vapply(chrVisits, function(strVisit) {
    Analyze_GroupDifference(dfAtVisit(dfOne, strVisit), "STRESN", "ARM", strMethod = "t")$p_value
  }, numeric(1), USE.NAMES = FALSE)
  expect_true(all(is.finite(nSeparate)))
  # The planted effect makes them differ, so an adjustment changes them.
  expect_false(identical(stats::p.adjust(nSeparate, method = "holm"), nSeparate))
  expect_false(identical(stats::p.adjust(nSeparate, method = "holm"), stats::p.adjust(nSeparate, method = "BH")))

  # Unadjusted unless the caller names a method.
  expect_identical(formals(Analyze_GroupDifferenceBy)$strPAdjust, "none")
  lDefault <- Analyze_GroupDifferenceBy(dfOne, "STRESN", "ARM", "VISIT", chrBy = chrVisits)
  expect_identical(lDefault$rows$p_unadjusted, nSeparate)
  expect_identical(lDefault$rows$p_value, nSeparate)
  expect_identical(lDefault$rows$adjustment, rep("none", 5))
  expect_identical(lDefault$adjustment, "none")
  expect_match(lDefault$notes[[2]], "p_value is not adjusted across the levels", fixed = TRUE)

  for (strPAdjust in c("holm", "BH", "bonferroni")) {
    lAdjusted <- Analyze_GroupDifferenceBy(dfOne, "STRESN", "ARM", "VISIT", chrBy = chrVisits, strPAdjust = strPAdjust)
    ExpectResultShape(lAdjusted)
    # Each level's unadjusted p-value is kept beside the adjusted one.
    expect_identical(lAdjusted$rows$p_unadjusted, nSeparate, label = paste(strPAdjust, "p_unadjusted"))
    expect_identical(lAdjusted$rows$p_value, stats::p.adjust(nSeparate, method = strPAdjust), label = paste(strPAdjust, "p_value"))
    expect_identical(lAdjusted$rows$adjustment, rep(strPAdjust, 5))
    expect_identical(lAdjusted$rows$adjusted_over, rep(5L, 5))
    # The result's own p_value is NA, so its own adjustment is none.
    expect_identical(lAdjusted$adjustment, "none")
    expect_match(
      lAdjusted$notes[[2]],
      sprintf("adjusted across the 5 levels that have a p-value by p.adjust(method = '%s')", strPAdjust),
      fixed = TRUE
    )
    # Everything but the adjustment is the unadjusted answer's.
    chrSame <- setdiff(names(lDefault$rows), c("p_value", "adjustment"))
    expect_identical(lAdjusted$rows[chrSame], lDefault$rows[chrSame])
  }

  # The adjustment does not depend on the order the levels are asked in.
  lReversed <- Analyze_GroupDifferenceBy(dfOne, "STRESN", "ARM", "VISIT", chrBy = rev(chrVisits), strPAdjust = "holm")
  expect_identical(lReversed$rows$by, rev(chrVisits))
  expect_identical(lReversed$rows$p_value, rev(stats::p.adjust(nSeparate, method = "holm")))
  # A method p.adjust() does not have is refused, as the single function refuses it.
  lBad <- Analyze_GroupDifferenceBy(dfOne, "STRESN", "ARM", "VISIT", strPAdjust = "sidak")
  ExpectResultShape(lBad)
  expect_identical(lBad$status, "error")
  expect_match(lBad$reason, "strPAdjust must be one of", fixed = TRUE)
})

test_that("a level with a group below the minimum size is reported with its reason and left out of the adjustment (#52)", {
  # Week 8 keeps three participants of the second arm; the other visits are whole.
  strSmall <- "Week 8"
  bSecond <- dfOne$VISIT == strSmall & dfOne$ARM == chrArms[2]
  dfSmall <- dfOne[!bSecond | cumsum(bSecond) <= 3L, ]
  chrOther <- setdiff(chrVisits, strSmall)

  lResult <- Analyze_GroupDifferenceBy(dfSmall, "STRESN", "ARM", "VISIT", chrGroups = chrArms, chrBy = chrVisits, strPAdjust = "holm")
  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  lSingle <- Analyze_GroupDifference(dfAtVisit(dfSmall, strSmall), "STRESN", "ARM", chrGroups = chrArms)
  expect_identical(lSingle$status, "too_small")

  dfRow <- lResult$rows[lResult$rows$by == strSmall, ]
  expect_identical(dfRow$status, "too_small")
  # The reason is the single function's, word for word, and names the group.
  expect_identical(dfRow$reason, lSingle$reason)
  expect_match(dfRow$reason, sprintf("%s has 3. The minimum group size is 5.", chrArms[2]), fixed = TRUE)
  # Its counts are still given, because they are the explanation.
  expect_identical(c(dfRow$n_1, dfRow$n_2), unname(unlist(lSingle$counts)))
  expect_identical(dfRow$n_2, 3L)
  # No number: no method, statistic, estimate or p-value, adjusted or not.
  expect_true(all(is.na(dfRow[c("estimate", "lower", "upper", "level", "method", "statistic", "p_unadjusted", "p_value", "adjusted_over")])))

  # The other four are adjusted across the four of them, not across five.
  dfTested <- lResult$rows[lResult$rows$by != strSmall, ]
  nSeparate <- vapply(chrOther, function(strVisit) {
    Analyze_GroupDifference(dfAtVisit(dfSmall, strVisit), "STRESN", "ARM", chrGroups = chrArms)$p_value
  }, numeric(1), USE.NAMES = FALSE)
  expect_identical(dfTested$by, chrOther)
  expect_identical(dfTested$p_unadjusted, nSeparate)
  expect_identical(dfTested$p_value, stats::p.adjust(nSeparate, method = "holm"))
  expect_false(identical(dfTested$p_value, stats::p.adjust(c(nSeparate, 0.5), method = "holm")[1:4]))
  expect_identical(dfTested$adjusted_over, rep(4L, 4))
  expect_match(
    lResult$notes[[2]],
    "adjusted across the 4 levels that have a p-value by p.adjust(method = 'holm'); 1 of the 5 levels have none and are left out of the adjustment.",
    fixed = TRUE
  )

  # A group with nobody at a level is too small there: the groups are the same
  # at every level, so the level is not quietly compared without it.
  dfNone <- dfOne[!bSecond, ]
  lNone <- Analyze_GroupDifferenceBy(dfNone, "STRESN", "ARM", "VISIT", chrBy = chrVisits)
  dfRow <- lNone$rows[lNone$rows$by == strSmall, ]
  expect_identical(dfRow$status, "too_small")
  expect_identical(c(dfRow$group_1, dfRow$group_2), sort(chrArms, method = "radix"))
  expect_identical(dfRow[[paste0("n_", match(chrArms[2], sort(chrArms, method = "radix")))]], 0L)
  expect_match(dfRow$reason, sprintf("%s has 0", chrArms[2]), fixed = TRUE)

  # Every level too small: the result says so, and every row has its reason.
  lAll <- Analyze_GroupDifferenceBy(dfOne, "STRESN", "ARM", "VISIT", nMinGroup = 150, strPAdjust = "BH")
  ExpectResultShape(lAll)
  expect_identical(lAll$status, "too_small")
  expect_match(lAll$reason, "every level has a group below the minimum size", fixed = TRUE)
  expect_identical(lAll$rows$status, rep("too_small", 5))
  expect_true(all(is.na(lAll$rows$p_value)) && all(is.na(lAll$rows$adjusted_over)))
  expect_true(all(grepl("The minimum group size is 150.", lAll$rows$reason, fixed = TRUE)))
})

test_that("a level whose values do not vary is reported with R's reason and left out of the adjustment (#52)", {
  # The change from Baseline is zero for everyone at Baseline: the chart's own case.
  dfChange <- dfOne
  nBaseline <- dfOne$STRESN[dfOne$VISIT == "Baseline"][match(dfOne$USUBJID, dfOne$USUBJID[dfOne$VISIT == "Baseline"])]
  dfChange$CHANGE <- dfOne$STRESN - nBaseline
  expect_true(all(dfChange$CHANGE[dfChange$VISIT == "Baseline"] == 0))
  chrOther <- setdiff(chrVisits, "Baseline")

  for (strMethod in c("t", "wilcoxon", "anova", "kruskal")) {
    strGroup <- if (strMethod %in% c("t", "wilcoxon")) "ARM" else "ARM_SEX"
    lResult <- Analyze_GroupDifferenceBy(dfChange, "CHANGE", strGroup, "VISIT", strMethod = strMethod, chrBy = chrVisits, strPAdjust = "BH")
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "ok", label = paste(strMethod, "status"))

    lSingle <- Analyze_GroupDifference(dfAtVisit(dfChange, "Baseline"), "CHANGE", strGroup, strMethod = strMethod, bPairwise = FALSE)
    dfRow <- lResult$rows[1, ]
    expect_identical(dfRow$by, "Baseline")
    expect_identical(dfRow$status, "error", label = paste(strMethod, "the constant level's status"))
    expect_identical(dfRow$reason, lSingle$reason, label = paste(strMethod, "the constant level's reason"))
    # With every value zero t.test() and wilcox.test() return no p-value, and
    # the tests of several groups refuse values that do not vary.
    expect_match(dfRow$reason, "gave no p-value|constant|nothing to rank", label = paste(strMethod, "the constant level's reason"))
    expect_true(all(is.na(dfRow[c("estimate", "method", "statistic", "p_unadjusted", "p_value", "adjusted_over")])))
    # The participants it would have used are still counted.
    expect_identical(dfRow$counts, sum(unlist(lSingle$counts)))

    nSeparate <- vapply(chrOther, function(strVisit) {
      Analyze_GroupDifference(dfAtVisit(dfChange, strVisit), "CHANGE", strGroup, strMethod = strMethod, bPairwise = FALSE)$p_value
    }, numeric(1), USE.NAMES = FALSE)
    expect_identical(lResult$rows$p_unadjusted[-1], nSeparate, label = paste(strMethod, "p_unadjusted"))
    expect_identical(lResult$rows$p_value[-1], stats::p.adjust(nSeparate, method = "BH"), label = paste(strMethod, "p_value"))
    expect_identical(lResult$rows$adjusted_over[-1], rep(4L, 4))
  }

  # Every level constant: nothing was computed, and the result is not "ok".
  dfFlat <- dfOne
  dfFlat$STRESN <- 1
  lFlat <- Analyze_GroupDifferenceBy(dfFlat, "STRESN", "ARM", "VISIT")
  ExpectResultShape(lFlat)
  expect_identical(lFlat$status, "error")
  expect_match(lFlat$reason, "No level could be computed", fixed = TRUE)
  expect_identical(lFlat$rows$status, rep("error", 5))
  # A constant that is not zero is refused by t.test() itself, in its words.
  expect_identical(lFlat$rows$reason, rep("data are essentially constant", 5))
})

test_that("the levels and the groups are the ones named, in the order named, and what is left out is counted (#52)", {
  # Every level present, sorted the same way in every locale, when none is named.
  lSorted <- Analyze_GroupDifferenceBy(dfOne, "STRESN", "ARM", "VISIT")
  expect_identical(lSorted$rows$by, sort(chrVisits, method = "radix"))
  expect_identical(nrow(lSorted$dropped), 0L)

  # Two levels named, as JSON delivers several values, and the groups the other way round.
  lNamed <- Analyze_GroupDifferenceBy(
    dfOne, "STRESN", "ARM", "VISIT",
    chrGroups = as.list(rev(chrArms)), chrBy = list("Week 12", "Week 2")
  )
  ExpectResultShape(lNamed)
  expect_identical(lNamed$rows$by, c("Week 12", "Week 2"))
  expect_identical(names(lNamed$counts), c("Week 12", "Week 2"))
  expect_identical(lNamed$rows$group_1, rep(chrArms[2], 2))
  expect_identical(lNamed$rows$group_2, rep(chrArms[1], 2))
  lForward <- Analyze_GroupDifferenceBy(dfOne, "STRESN", "ARM", "VISIT", chrGroups = chrArms, chrBy = c("Week 12", "Week 2"))
  expect_identical(lNamed$rows$estimate, -lForward$rows$estimate)
  expect_identical(lNamed$rows$p_unadjusted, lForward$rows$p_unadjusted)
  expect_identical(lNamed$rows$n_1, lForward$rows$n_2)
  # The rows of the other three visits are left out, and counted.
  expect_identical(lNamed$dropped, data.frame(
    reason = "Level not selected", n = sum(!dfOne$VISIT %in% c("Week 12", "Week 2")), stringsAsFactors = FALSE
  ))

  # Rows with no level, no group, another group or no value are each counted,
  # in that order, and every row is used or counted.
  chrKept <- c("Placebo F", "Treatment F")
  dfGaps <- dfOne
  iKept <- which(dfGaps$ARM_SEX %in% chrKept)
  # A row short of two things is counted once, under the first.
  dfGaps$VISIT[1:3] <- NA
  dfGaps$VISIT[4] <- ""
  dfGaps$ARM_SEX[c(2, 11:14)] <- NA
  dfGaps$STRESN[c(3, 12, iKept[21:26])] <- NA
  lGaps <- Analyze_GroupDifferenceBy(dfGaps, "STRESN", "ARM_SEX", "VISIT", chrGroups = chrKept)
  ExpectResultShape(lGaps)
  bLevel <- !is.na(dfGaps$VISIT) & nzchar(dfGaps$VISIT)
  bGroup <- bLevel & !is.na(dfGaps$ARM_SEX)
  bKept <- bGroup & dfGaps$ARM_SEX %in% chrKept
  expect_identical(lGaps$dropped, data.frame(
    reason = c("Missing level", "Missing group", "Group not selected", "Missing value"),
    n = c(sum(!bLevel), sum(bLevel & !bGroup), sum(bGroup & !bKept), sum(bKept & is.na(dfGaps$STRESN))),
    stringsAsFactors = FALSE
  ))
  expect_true(all(lGaps$dropped$n > 0L))
  expect_identical(sum(lGaps$dropped$n) + sum(lGaps$rows$counts), nrow(dfGaps))
  # Each row's own count of dropped rows is the single function's for that level.
  expect_identical(sum(lGaps$rows$dropped), sum(lGaps$dropped$n[-1]))

  # A level named that the data do not hold has nobody in either group.
  lAbsent <- Analyze_GroupDifferenceBy(dfOne, "STRESN", "ARM", "VISIT", chrBy = c("Week 4", "Week 99"))
  expect_identical(lAbsent$rows$status, c("ok", "too_small"))
  expect_identical(c(lAbsent$rows$n_1[2], lAbsent$rows$n_2[2]), c(0L, 0L))

  # The value and the level may be whole numbers and the group a factor.
  dfTyped <- dfOne
  dfTyped$WEEK <- as.integer(Synthetic_Results$VISITNUM[match(dfOne$VISIT, Synthetic_Results$VISIT)])
  dfTyped$ARM <- factor(dfTyped$ARM)
  lTyped <- Analyze_GroupDifferenceBy(dfTyped, "STRESN", "ARM", "WEEK")
  expect_identical(lTyped$rows$by, c("0", "12", "2", "4", "8"))
  expect_identical(lTyped$rows$p_unadjusted, lSorted$rows$p_unadjusted[match(c("Baseline", "Week 12", "Week 2", "Week 4", "Week 8"), lSorted$rows$by)])
})

test_that("a request that cannot be met is an error result with a reason, and nothing is raised (#52)", {
  lBad <- list(
    not_a_frame = list("not a data frame", "STRESN", "ARM", "VISIT"),
    no_value = list(dfOne, "Nope", "ARM", "VISIT"),
    no_group = list(dfOne, "STRESN", "Nope", "VISIT"),
    no_by = list(dfOne, "STRESN", "ARM", "Nope"),
    text_value = list(dfOne, "ARM", "ARM", "VISIT"),
    method = list(dfOne, "STRESN", "ARM", "VISIT", strMethod = "median"),
    level = list(dfOne, "STRESN", "ARM", "VISIT", nConfLevel = 1),
    minimum = list(dfOne, "STRESN", "ARM", "VISIT", nMinGroup = 0),
    repeated_levels = list(dfOne, "STRESN", "ARM", "VISIT", chrBy = c("Week 4", "Week 4")),
    no_rows = list(dfOne[0, ], "STRESN", "ARM", "VISIT"),
    no_groups = list(transform(dfOne, ARM = NA_character_), "STRESN", "ARM", "VISIT")
  )
  for (strCase in names(lBad)) {
    expect_no_condition(lResult <- do.call(Analyze_GroupDifferenceBy, lBad[[strCase]]))
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "error", label = paste(strCase, "status"))
    expect_identical(nrow(lResult$rows), 0L, label = paste(strCase, "rows"))
  }
  expect_match(do.call(Analyze_GroupDifferenceBy, lBad$no_by)$reason, "Column 'Nope' (strByCol) is not in the data.", fixed = TRUE)
  expect_match(do.call(Analyze_GroupDifferenceBy, lBad$no_rows)$reason, "has no level to answer", fixed = TRUE)
  expect_match(do.call(Analyze_GroupDifferenceBy, lBad$no_groups)$reason, "has no group", fixed = TRUE)

  # A two-group test of four groups is each level's own error, as the single
  # function gives it, and the result's.
  expect_no_condition(lFour <- Analyze_GroupDifferenceBy(dfOne, "STRESN", "ARM_SEX", "VISIT", strMethod = "t"))
  ExpectResultShape(lFour)
  expect_identical(lFour$status, "error")
  expect_identical(lFour$rows$status, rep("error", 5))
  expect_identical(
    lFour$rows$reason[1],
    Analyze_GroupDifference(dfAtVisit(dfOne, "Baseline"), "STRESN", "ARM_SEX", strMethod = "t")$reason
  )
  expect_named(lFour$rows, chrByRowCols(4L))

  # A warning R raises inside a level is in the result, never raised: ranks
  # with ties, in every version of R that warns of them.
  dfTied <- dfOne
  dfTied$STRESN <- round(dfTied$STRESN)
  expect_no_condition(lTied <- Analyze_GroupDifferenceBy(dfTied, "STRESN", "ARM", "VISIT", strMethod = "wilcoxon", nMinGroup = 2))
  ExpectResultShape(lTied)
  lSingle <- Analyze_GroupDifference(dfAtVisit(dfTied, "Baseline"), "STRESN", "ARM", strMethod = "wilcoxon", nMinGroup = 2)
  expect_identical(lTied$rows$p_unadjusted[1], lSingle$p_value)
  expect_identical(
    lTied$rows$warning[1],
    if (length(lSingle$warnings) > 0L) paste(unlist(lSingle$warnings), collapse = "; ") else NA_character_
  )
  # The note about where the difference in means comes from is the single function's.
  expect_true(all(unlist(lSingle$notes) %in% unlist(lTied$notes)))
})

test_that("the answer crosses into JSON as the other answers do: rows as objects, a missing number as null (#52)", {
  lResult <- Analyze_GroupDifferenceBy(dfOne[dfOne$VISIT != "Week 8" | dfOne$ARM == chrArms[1], ], "STRESN", "ARM", "VISIT", chrBy = chrVisits, strPAdjust = "holm")
  lJson <- jsonlite::fromJSON(
    jsonlite::toJSON(lResult, auto_unbox = TRUE, digits = NA, na = "null", dataframe = "rows"),
    simplifyVector = FALSE
  )
  expect_identical(names(lJson), chrResultMembers)
  expect_identical(length(lJson$rows), 5L)
  expect_identical(names(lJson$counts), chrVisits)
  # A row a chart's formatter reads as it reads a comparison of two groups.
  lRow <- lJson$rows[[1]]
  expect_true(all(c("by", "group_1", "group_2", "n_1", "n_2", "method", "p_value", "p_unadjusted", "adjustment", "status") %in% names(lRow)))
  expect_identical(lRow$adjustment, "holm")
  expect_true(is.numeric(lRow$p_value) && lRow$p_value >= lRow$p_unadjusted)
})
