# Analyze_DifferenceGrid (#52): the standardised difference between two groups
# for every biomarker at every level of a column, one row per cell, in one
# call. Every cell is Analyze_Screen()'s own difference row for that biomarker
# on that level's rows, so every comparison with it below is exact
# (expect_identical). The data are long: one row per participant, biomarker and
# level, where the screen takes one column per biomarker.

dfLong <- dfSyntheticLong()
chrVisits <- chrSyntheticVisits()
chrBiomarkers <- chrSyntheticBiomarkers()
chrArms <- Synthetic_Truth$GroupDifference$Groups
chrGridRowCols <- c(
  "biomarker", "by", "counts", "n_1", "n_2", "dropped", "estimate", "lower", "upper", "level",
  "status", "reason", "warning"
)
# The columns a cell shares with a row of the screen.
chrSharedCols <- setdiff(chrGridRowCols, "by")

# One visit as the screen takes it: one row per participant, one column per
# biomarker, a participant with no result at the visit a gap.
dfWideAtVisit <- function(strVisit) {
  dfWide <- Synthetic_Participants[c("USUBJID", "ARM")]
  for (strBiomarker in chrBiomarkers) {
    dfWide[[strBiomarker]] <- nResultAt(strBiomarker, strVisit)
  }
  dfWide
}

# The same study in long form with those gaps as rows: every participant at
# every visit for every biomarker, the value missing where there is no result.
dfLongWithGaps <- function() {
  do.call(rbind, lapply(chrVisits, function(strVisit) {
    dfWide <- dfWideAtVisit(strVisit)
    do.call(rbind, lapply(chrBiomarkers, function(strBiomarker) {
      data.frame(
        USUBJID = dfWide$USUBJID, TEST = strBiomarker, VISIT = strVisit, STRESN = dfWide[[strBiomarker]],
        ARM = dfWide$ARM, stringsAsFactors = FALSE
      )
    }))
  }))
}

test_that("each cell of the grid is Analyze_Screen()'s standardised difference run on that visit's rows (#52)", {
  dfGaps <- dfLongWithGaps()
  lResult <- Analyze_DifferenceGrid(
    dfGaps, "STRESN", "ARM", "TEST", "VISIT",
    chrGroups = chrArms, chrBiomarkers = chrBiomarkers, chrBy = chrVisits
  )

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$test, "difference")
  expect_identical(lResult$method, "Standardised difference (Hedges' g)")
  # No p-value anywhere: the grid is estimates and their intervals.
  expect_true(is.na(lResult$p_value))
  expect_identical(lResult$adjustment, "none")
  expect_identical(nrow(lResult$estimates), 0L)
  expect_identical(nrow(lResult$statistic), 0L)
  expect_named(lResult$rows, chrGridRowCols)
  expect_false(any(grepl("^p_|^adjust|^method$|^statistic$", names(lResult$rows))))
  expect_true(any(grepl("^No p-values", unlist(lResult$notes))))
  expect_identical(
    lResult$notes[[1]],
    sprintf("Each row's estimate: Standardised difference (Hedges' g), %s - %s.", chrArms[1], chrArms[2])
  )

  # One row per biomarker and visit, the biomarkers in the order asked for and
  # within each the visits in the order asked for.
  expect_identical(lResult$rows$biomarker, rep(chrBiomarkers, each = length(chrVisits)))
  expect_identical(lResult$rows$by, rep(chrVisits, times = length(chrBiomarkers)))

  for (strVisit in chrVisits) {
    lScreen <- Analyze_Screen(dfWideAtVisit(strVisit), chrBiomarkers, "difference", strGroupCol = "ARM", chrGroups = chrArms)
    dfMine <- lResult$rows[lResult$rows$by == strVisit, chrSharedCols]
    dfTheirs <- lScreen$rows[chrSharedCols]
    rownames(dfMine) <- NULL
    # Estimate, interval, level, the counts, what was dropped, status, reason
    # and warning: identical, to the last bit.
    expect_identical(dfMine, dfTheirs, label = paste("the grid's cells at", strVisit))
    expect_true(all(dfMine$status == "ok") && all(is.finite(dfMine$estimate)), label = paste("cells computed at", strVisit))
  }
  # The planted difference is in the grid, at its visit, and the screen has it.
  lPlanted <- Synthetic_Truth$GroupDifference
  dfCell <- lResult$rows[lResult$rows$biomarker == lPlanted$Biomarker & lResult$rows$by == lPlanted$Visit, ]
  expect_identical(nrow(dfCell), 1L)
  expect_true(dfCell$lower > 0 || dfCell$upper < 0)

  # The rows used across the cells, as one whole number.
  expect_identical(lResult$counts, sum(lResult$rows$counts))
  expect_identical(lResult$counts, sum(!is.na(dfGaps$STRESN)))
  expect_identical(lResult$dropped, data.frame(reason = "Missing value", n = sum(is.na(dfGaps$STRESN)), stringsAsFactors = FALSE))

  # A participant with no row at a visit, in place of a row with no value, is
  # the same cell: only what was dropped differs, because nothing was.
  dfPlain <- dfGaps[!is.na(dfGaps$STRESN), ]
  lPlain <- Analyze_DifferenceGrid(
    dfPlain, "STRESN", "ARM", "TEST", "VISIT",
    chrGroups = chrArms, chrBiomarkers = chrBiomarkers, chrBy = chrVisits
  )
  ExpectResultShape(lPlain)
  chrNumbers <- setdiff(chrGridRowCols, "dropped")
  expect_identical(lPlain$rows[chrNumbers], lResult$rows[chrNumbers])
  expect_identical(lPlain$rows$dropped, rep(0L, nrow(lPlain$rows)))
  expect_identical(nrow(lPlain$dropped), 0L)
  # And so is the study's own results table, in the order it comes.
  lStudy <- Analyze_DifferenceGrid(
    dfLong, "STRESN", "ARM", "TEST", "VISIT",
    chrGroups = chrArms, chrBiomarkers = chrBiomarkers, chrBy = chrVisits
  )
  expect_identical(lStudy$rows[chrNumbers], lResult$rows[chrNumbers])
})

test_that("the grid's estimate is the screen's own arithmetic: the same function, and the other way round its negative (#52)", {
  # The cell and Stat_StandardisedDifference() on the two groups' values.
  dfCell <- dfLong[dfLong$TEST == "CRP" & dfLong$VISIT == "Week 4" & !is.na(dfLong$STRESN), ]
  lDirect <- gsm.bio:::Stat_StandardisedDifference(
    dfCell$STRESN[dfCell$ARM == chrArms[1]], dfCell$STRESN[dfCell$ARM == chrArms[2]], 0.9
  )
  lResult <- Analyze_DifferenceGrid(
    dfLong, "STRESN", "ARM", "TEST", "VISIT",
    chrGroups = chrArms, chrBiomarkers = "CRP", chrBy = "Week 4", nConfLevel = 0.9
  )
  expect_identical(nrow(lResult$rows), 1L)
  expect_identical(c(lResult$rows$estimate, lResult$rows$lower, lResult$rows$upper), c(lDirect$estimate, lDirect$lower, lDirect$upper))
  expect_identical(lResult$rows$level, 0.9)

  # The groups the other way round: the difference is first minus second.
  lReversed <- Analyze_DifferenceGrid(
    dfLong, "STRESN", "ARM", "TEST", "VISIT",
    chrGroups = as.list(rev(chrArms)), chrBiomarkers = list("CRP"), chrBy = list("Week 4"), nConfLevel = 0.9
  )
  expect_equal(lReversed$rows$estimate, -lResult$rows$estimate, tolerance = 1e-12)
  expect_equal(c(lReversed$rows$lower, lReversed$rows$upper), -c(lResult$rows$upper, lResult$rows$lower), tolerance = 1e-8)
  expect_identical(c(lReversed$rows$n_1, lReversed$rows$n_2), c(lResult$rows$n_2, lResult$rows$n_1))

  # Every biomarker and every level present, sorted the same way in every
  # locale, when none is named; and the two groups present.
  lAll <- Analyze_DifferenceGrid(dfLong, "STRESN", "ARM", "TEST", "VISIT")
  expect_identical(unique(lAll$rows$biomarker), sort(chrBiomarkers, method = "radix"))
  expect_identical(lAll$rows$by[1:5], sort(chrVisits, method = "radix"))
  expect_identical(nrow(lAll$rows), 60L)
  expect_match(lAll$notes[[1]], paste(sort(chrArms, method = "radix"), collapse = " - "), fixed = TRUE)
})

test_that("a cell with a group below the minimum size, or with no rows at all, is reported with its reason (#52)", {
  # CRP at Week 8 keeps three participants of the second arm, and IL-6 has no
  # row at Week 12.
  bSecond <- dfLong$TEST == "CRP" & dfLong$VISIT == "Week 8" & dfLong$ARM == chrArms[2]
  bGone <- dfLong$TEST == "IL-6" & dfLong$VISIT == "Week 12"
  dfSmall <- dfLong[(!bSecond | cumsum(bSecond) <= 3L) & !bGone, ]
  lResult <- Analyze_DifferenceGrid(
    dfSmall, "STRESN", "ARM", "TEST", "VISIT",
    chrGroups = chrArms, chrBiomarkers = c("CRP", "IL-6"), chrBy = chrVisits
  )
  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(nrow(lResult$rows), 10L)

  dfCell <- lResult$rows[lResult$rows$biomarker == "CRP" & lResult$rows$by == "Week 8", ]
  dfWide <- dfWideAtVisit("Week 8")
  iKept <- which(dfWide$ARM == chrArms[2] & !is.na(dfWide$CRP))[1:3]
  dfWide$CRP[dfWide$ARM == chrArms[2] & !seq_len(nrow(dfWide)) %in% iKept] <- NA
  dfScreen <- Analyze_Screen(dfWide, "CRP", "difference", strGroupCol = "ARM", chrGroups = chrArms)$rows
  expect_identical(dfCell$status, "too_small")
  expect_identical(dfCell$reason, dfScreen$reason)
  expect_match(dfCell$reason, sprintf("%s has 3. The minimum group size is 5.", chrArms[2]), fixed = TRUE)
  expect_identical(c(dfCell$n_1, dfCell$n_2, dfCell$counts), c(dfScreen$n_1, dfScreen$n_2, dfScreen$counts))
  expect_identical(dfCell$n_2, 3L)
  expect_true(all(is.na(dfCell[c("estimate", "lower", "upper", "level")])))

  # A cell with no rows is still a row of the grid: nobody in either group.
  dfCell <- lResult$rows[lResult$rows$biomarker == "IL-6" & lResult$rows$by == "Week 12", ]
  expect_identical(dfCell$status, "too_small")
  expect_identical(c(dfCell$n_1, dfCell$n_2, dfCell$counts, dfCell$dropped), c(0L, 0L, 0L, 0L))
  expect_match(dfCell$reason, sprintf("%s has 0; %s has 0", chrArms[1], chrArms[2]), fixed = TRUE)

  # The other eight cells are computed, and are what they are without the gaps.
  lWhole <- Analyze_DifferenceGrid(
    dfLong, "STRESN", "ARM", "TEST", "VISIT",
    chrGroups = chrArms, chrBiomarkers = c("CRP", "IL-6"), chrBy = chrVisits
  )
  bOther <- lResult$rows$status == "ok"
  expect_identical(sum(bOther), 8L)
  expect_identical(lResult$rows[bOther, ], lWhole$rows[bOther, ])

  # Every cell too small: the result says so, and every row has its reason.
  lAll <- Analyze_DifferenceGrid(dfLong, "STRESN", "ARM", "TEST", "VISIT", nMinGroup = 150)
  ExpectResultShape(lAll)
  expect_identical(lAll$status, "too_small")
  expect_match(lAll$reason, "every cell has a group below the minimum size", fixed = TRUE)
  expect_true(is.na(lAll$method))
  expect_identical(nrow(lAll$rows), 60L)
  expect_true(all(lAll$rows$status == "too_small") && all(is.na(lAll$rows$estimate)))
})

test_that("a cell whose values do not vary is reported with R's reason, as the screen reports it (#52)", {
  # The change from Baseline is zero for everyone at Baseline, for every biomarker.
  dfChange <- dfLong[dfLong$TEST %in% c("CRP", "IL-6"), ]
  dfBaseline <- dfChange[dfChange$VISIT == "Baseline", ]
  dfChange$CHANGE <- dfChange$STRESN -
    dfBaseline$STRESN[match(paste(dfChange$USUBJID, dfChange$TEST), paste(dfBaseline$USUBJID, dfBaseline$TEST))]
  lResult <- Analyze_DifferenceGrid(
    dfChange, "CHANGE", "ARM", "TEST", "VISIT",
    chrGroups = chrArms, chrBiomarkers = c("CRP", "IL-6"), chrBy = chrVisits
  )
  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")

  dfConstant <- lResult$rows[lResult$rows$by == "Baseline", ]
  expect_identical(dfConstant$status, c("error", "error"))
  expect_true(all(is.na(dfConstant[c("estimate", "lower", "upper", "level")])))
  # The reason is the screen's for the same rows: R's own, from t.test().
  dfWide <- data.frame(ARM = Synthetic_Participants$ARM, CRP = 0, `IL-6` = 0, check.names = FALSE)
  dfScreen <- Analyze_Screen(dfWide, c("CRP", "IL-6"), "difference", strGroupCol = "ARM", chrGroups = chrArms)$rows
  expect_identical(dfConstant$reason, dfScreen$reason)
  # With every value zero t.test() returns no p-value, and the answer says so.
  expect_match(dfConstant$reason[1], "gave no p-value", fixed = TRUE)
  expect_identical(dfConstant$counts, dfScreen$counts)
  # The other visits are computed.
  expect_true(all(lResult$rows$status[lResult$rows$by != "Baseline"] == "ok"))

  # Every cell constant: nothing was computed, and the result is not "ok". A
  # constant that is not zero is refused by t.test() itself, in its words.
  dfFlat <- dfChange
  dfFlat$CHANGE <- 2
  lFlat <- Analyze_DifferenceGrid(dfFlat, "CHANGE", "ARM", "TEST", "VISIT")
  ExpectResultShape(lFlat)
  expect_identical(lFlat$status, "error")
  expect_match(lFlat$reason, "No cell could be computed", fixed = TRUE)
  expect_true(all(lFlat$rows$status == "error"))
  expect_true(all(lFlat$rows$reason == "data are essentially constant"))
  expect_identical(
    lFlat$rows$reason[1],
    Analyze_Screen(transform(dfWide, CRP = 2), "CRP", "difference", strGroupCol = "ARM")$rows$reason
  )
})

test_that("what the grid leaves out is counted, and a request it cannot meet is an error result with a reason (#52)", {
  # Rows with no biomarker, another biomarker, no level, another level, no
  # group, another group or no value are each counted, in that order.
  chrKeptBiomarkers <- c("IL-6", "CRP", "VEGF")
  chrKeptVisits <- c("Week 2", "Week 4")
  chrKeptGroups <- c("Placebo F", "Treatment F")
  dfGaps <- dfLong[!is.na(dfLong$STRESN), ]
  iCells <- which(dfGaps$TEST %in% chrKeptBiomarkers & dfGaps$VISIT %in% chrKeptVisits)
  iGroups <- intersect(iCells, which(dfGaps$ARM_SEX %in% chrKeptGroups))
  # A row short of two things is counted once, under the first.
  dfGaps$TEST[iCells[1:2]] <- NA
  dfGaps$VISIT[iCells[2:5]] <- NA
  dfGaps$ARM_SEX[iCells[c(5, 11:16)]] <- NA
  dfGaps$STRESN[c(iCells[c(1, 11)], iGroups[21:29])] <- NA
  lResult <- Analyze_DifferenceGrid(
    dfGaps, "STRESN", "ARM_SEX", "TEST", "VISIT",
    chrGroups = chrKeptGroups, chrBiomarkers = chrKeptBiomarkers, chrBy = chrKeptVisits
  )
  ExpectResultShape(lResult)
  bBiomarker <- !is.na(dfGaps$TEST)
  bKeptBiomarker <- bBiomarker & dfGaps$TEST %in% chrKeptBiomarkers
  bLevel <- bKeptBiomarker & !is.na(dfGaps$VISIT)
  bKeptLevel <- bLevel & dfGaps$VISIT %in% chrKeptVisits
  bGroup <- bKeptLevel & !is.na(dfGaps$ARM_SEX)
  bKeptGroup <- bGroup & dfGaps$ARM_SEX %in% chrKeptGroups
  expect_identical(lResult$dropped, data.frame(
    reason = c(
      "Missing biomarker", "Biomarker not selected", "Missing level", "Level not selected",
      "Missing group", "Group not selected", "Missing value"
    ),
    n = c(
      sum(!bBiomarker), sum(bBiomarker & !bKeptBiomarker), sum(bKeptBiomarker & !bLevel), sum(bLevel & !bKeptLevel),
      sum(bKeptLevel & !bGroup), sum(bGroup & !bKeptGroup), sum(bKeptGroup & is.na(dfGaps$STRESN))
    ),
    stringsAsFactors = FALSE
  ))
  expect_true(all(lResult$dropped$n > 0L))
  expect_identical(sum(lResult$dropped$n) + lResult$counts, nrow(dfGaps))
  # Each cell's own count of dropped rows is the screen's for that cell.
  expect_identical(sum(lResult$rows$dropped), sum(lResult$dropped$n[5:7]))
  expect_identical(lResult$rows$biomarker, rep(chrKeptBiomarkers, each = 2))
  expect_identical(lResult$rows$by, rep(chrKeptVisits, times = 3))

  lBad <- list(
    not_a_frame = list("not a data frame", "STRESN", "ARM", "TEST", "VISIT"),
    no_value = list(dfLong, "Nope", "ARM", "TEST", "VISIT"),
    no_group = list(dfLong, "STRESN", "Nope", "TEST", "VISIT"),
    no_biomarker = list(dfLong, "STRESN", "ARM", "Nope", "VISIT"),
    no_by = list(dfLong, "STRESN", "ARM", "TEST", "Nope"),
    text_value = list(dfLong, "ARM", "ARM", "TEST", "VISIT"),
    four_groups = list(dfLong, "STRESN", "ARM_SEX", "TEST", "VISIT"),
    one_group = list(dfLong, "STRESN", "ARM", "TEST", "VISIT", chrGroups = "Placebo"),
    level = list(dfLong, "STRESN", "ARM", "TEST", "VISIT", nConfLevel = 0),
    minimum = list(dfLong, "STRESN", "ARM", "TEST", "VISIT", nMinGroup = -1),
    repeated_biomarkers = list(dfLong, "STRESN", "ARM", "TEST", "VISIT", chrBiomarkers = c("CRP", "CRP")),
    no_rows = list(dfLong[0, ], "STRESN", "ARM", "TEST", "VISIT")
  )
  for (strCase in names(lBad)) {
    expect_no_condition(lError <- do.call(Analyze_DifferenceGrid, lBad[[strCase]]))
    ExpectResultShape(lError)
    expect_identical(lError$status, "error", label = paste(strCase, "status"))
    expect_identical(lError$test, "difference", label = paste(strCase, "test"))
    expect_identical(nrow(lError$rows), 0L, label = paste(strCase, "rows"))
  }
  # Two groups exactly, in the screen's own words.
  expect_identical(
    do.call(Analyze_DifferenceGrid, lBad$four_groups)$reason,
    "A standardised difference compares exactly two groups and 4 were found. Name two in chrGroups."
  )
  expect_identical(
    do.call(Analyze_DifferenceGrid, lBad$four_groups)$reason,
    Analyze_Screen(dfSyntheticFrame(), "Change", "difference", strGroupCol = "ARM_SEX")$reason
  )
  expect_match(do.call(Analyze_DifferenceGrid, lBad$no_biomarker)$reason, "Column 'Nope' (strBiomarkerCol) is not in the data.", fixed = TRUE)
  expect_match(do.call(Analyze_DifferenceGrid, lBad$no_rows)$reason, "has no level to answer", fixed = TRUE)
})
