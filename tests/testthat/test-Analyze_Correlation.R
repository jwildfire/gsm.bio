# Analyze_Correlation and Analyze_CorrelationMatrix (#3): cor.test() for
# Pearson and Spearman, for one pair overall and per group, and pairwise on
# complete pairs. Every comparison with base R below is exact
# (expect_identical).

dfFrame <- dfSyntheticFrame()
lTruth <- Synthetic_Truth$Correlation
strX <- paste(lTruth$Biomarkers[1], "@", lTruth$Visit)
strY <- paste(lTruth$Biomarkers[2], "@", lTruth$Visit)
# Week 4 has missed visits and missing results, so the pairs are not complete.
strXLater <- paste(lTruth$Biomarkers[1], "@ Week 4")
strYLater <- paste(lTruth$Biomarkers[2], "@ Week 4")

test_that("Pearson correlation equals cor.test() on the synthetic study (#3)", {
  lResult <- Analyze_Correlation(dfFrame, strX, strY, strMethod = "pearson")
  lBase <- stats::cor.test(dfFrame[[strX]], dfFrame[[strY]])

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$method, "Pearson's product-moment correlation")
  expect_identical(lResult$method, unname(lBase$method))
  expect_identical(
    lResult$estimates,
    data.frame(
      name = "cor", group = NA_character_, estimate = unname(lBase$estimate),
      lower = lBase$conf.int[1], upper = lBase$conf.int[2], level = 0.95
    )
  )
  expect_identical(
    lResult$statistic,
    data.frame(name = c("t", "df"), value = c(unname(lBase$statistic), unname(lBase$parameter)))
  )
  expect_identical(lResult$p_value, lBase$p.value)
  expect_identical(lResult$counts, 200L)
  expect_identical(nrow(lResult$dropped), 0L)
  expect_identical(lResult$notes, list())

  # Incomplete pairs are dropped and counted, and the level is the caller's.
  lLater <- Analyze_Correlation(dfFrame, strXLater, strYLater, nConfLevel = 0.9)
  lBaseLater <- stats::cor.test(dfFrame[[strXLater]], dfFrame[[strYLater]], conf.level = 0.9)
  nComplete <- sum(stats::complete.cases(dfFrame[c(strXLater, strYLater)]))
  expect_lt(nComplete, 200L)
  expect_identical(lLater$counts, nComplete)
  expect_identical(lLater$dropped, data.frame(reason = "Incomplete pair", n = 200L - nComplete))
  expect_identical(lLater$estimates$estimate, unname(lBaseLater$estimate))
  expect_identical(c(lLater$estimates$lower, lLater$estimates$upper), as.numeric(lBaseLater$conf.int))
  expect_identical(lLater$estimates$level, 0.9)
})

test_that("the planted correlation falls inside its interval (#3)", {
  lResult <- Analyze_Correlation(dfFrame, strX, strY, strMethod = lTruth$Method)

  expect_gt(lTruth$Value, lResult$estimates$lower)
  expect_lt(lTruth$Value, lResult$estimates$upper)
  expect_lt(lResult$p_value, 0.001)
})

test_that("Spearman correlation equals cor.test(), and says it has no interval (#3)", {
  lResult <- Analyze_Correlation(dfFrame, strXLater, strYLater, strMethod = "spearman")
  lBase <- lWithWarnings(function() {
    stats::cor.test(dfFrame[[strXLater]], dfFrame[[strYLater]], method = "spearman")
  })

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$method, unname(lBase$value$method))
  expect_identical(lResult$statistic, data.frame(name = "S", value = unname(lBase$value$statistic)))
  expect_identical(lResult$p_value, lBase$value$p.value)
  expect_identical(lResult$warnings, lBase$warnings)
  # cor.test() gives no interval for rho, so none is invented.
  expect_null(lBase$value$conf.int)
  expect_identical(
    lResult$estimates,
    data.frame(
      name = "rho", group = NA_character_, estimate = unname(lBase$value$estimate),
      lower = NA_real_, upper = NA_real_, level = NA_real_
    )
  )
  expect_match(lResult$notes[[1]], "no confidence interval for Spearman's rho", fixed = TRUE)
})

test_that("per-group correlations equal cor.test() within each group (#3)", {
  for (strMethod in c("pearson", "spearman")) {
    lResult <- Analyze_Correlation(dfFrame, strXLater, strYLater, strMethod = strMethod, strGroupCol = "ARM")
    ExpectResultShape(lResult)
    expect_named(lResult$rows, c(
      "group", "counts", "estimate", "lower", "upper", "level", "method", "statistic",
      "p_value", "adjustment", "status", "reason", "warning"
    ))
    expect_identical(lResult$rows$group, c("Placebo", "Treatment"))

    for (iGroup in 1:2) {
      dfGroup <- dfFrame[dfFrame$ARM == lResult$rows$group[iGroup], ]
      lBase <- lWithWarnings(function() {
        stats::cor.test(dfGroup[[strXLater]], dfGroup[[strYLater]], method = strMethod)
      })
      expect_identical(lResult$rows$counts[iGroup], sum(stats::complete.cases(dfGroup[c(strXLater, strYLater)])))
      expect_identical(lResult$rows$estimate[iGroup], unname(lBase$value$estimate))
      expect_identical(lResult$rows$statistic[iGroup], unname(lBase$value$statistic))
      expect_identical(lResult$rows$p_value[iGroup], lBase$value$p.value)
      expect_identical(lResult$rows$method[iGroup], unname(lBase$value$method))
      if (strMethod == "pearson") {
        expect_identical(c(lResult$rows$lower[iGroup], lResult$rows$upper[iGroup]), as.numeric(lBase$value$conf.int))
      } else {
        expect_true(is.na(lResult$rows$lower[iGroup]) && is.na(lResult$rows$upper[iGroup]))
      }
    }
    expect_identical(lResult$rows$adjustment, c("none", "none"))

    # The overall answer is the same with or without the per-group rows.
    lOverall <- Analyze_Correlation(dfFrame, strXLater, strYLater, strMethod = strMethod)
    expect_identical(lResult[c("estimates", "statistic", "p_value", "counts")], lOverall[c("estimates", "statistic", "p_value", "counts")])
  }

  # The groups can be named, as a vector or as JSON's list of single values.
  lNamed <- Analyze_Correlation(dfFrame, strX, strY, strGroupCol = "ARM_SEX", chrGroups = list("Treatment M", "Placebo F"))
  expect_identical(lNamed$rows$group, c("Treatment M", "Placebo F"))
  expect_identical(
    lNamed$dropped,
    data.frame(
      reason = "Group not selected (left out of the per-group rows)",
      n = sum(!dfFrame$ARM_SEX %in% c("Treatment M", "Placebo F"))
    )
  )
})

test_that("too few complete pairs get a reason and no numbers, overall and per group (#3)", {
  dfSmall <- dfFrame[1:4, ]
  for (strMethod in c("pearson", "spearman")) {
    lResult <- Analyze_Correlation(dfSmall, strX, strY, strMethod = strMethod)
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "too_small")
    expect_identical(lResult$reason, "Not computed: 4 complete pairs. The minimum is 5.")
    expect_identical(lResult$counts, 4L)
  }

  # One group too small: its row says so, and the rest are computed.
  dfUneven <- dfFrame
  dfUneven$Site <- rep(c("Large", "Small"), times = c(197, 3))
  lUneven <- Analyze_Correlation(dfUneven, strX, strY, strGroupCol = "Site")
  ExpectResultShape(lUneven)
  expect_identical(lUneven$status, "ok")
  expect_identical(lUneven$rows$status, c("ok", "too_small"))
  expect_identical(lUneven$rows$reason, c(NA, "Not computed: 3 complete pairs. The minimum is 5."))
  expect_identical(lUneven$rows$counts, c(197L, 3L))
  expect_true(is.na(lUneven$rows$estimate[2]) && is.na(lUneven$rows$p_value[2]))

  expect_identical(Analyze_Correlation(dfSmall, strX, strY, nMinGroup = 4)$status, "ok")
  expect_identical(formals(Analyze_Correlation)$nMinGroup, quote(nMinGroupDefault))
})

test_that("a correlation request that cannot be met is an error status, never an R error (#3)", {
  lCases <- list(
    list(dfData = dfFrame, strXCol = "Nope", strYCol = strY),
    list(dfData = dfFrame, strXCol = strX, strYCol = "ARM"),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, strMethod = "kendall"),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, strGroupCol = "Nope"),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, nMinGroup = "five")
  )
  for (lArgs in lCases) {
    expect_silent(lResult <- do.call(Analyze_Correlation, lArgs))
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "error")
  }

  # R's own warning about a constant column is captured, not printed.
  dfFlat <- dfFrame
  dfFlat$Flat <- 1
  expect_silent(lFlat <- Analyze_Correlation(dfFlat, strX, "Flat"))
  ExpectResultShape(lFlat)
  expect_identical(lFlat$warnings, list("the standard deviation is zero"))
})

chrMatrixCols <- c(strXLater, strYLater, "IL-6 @ Week 4", "CRP @ Week 4", "Change")

test_that("the correlation matrix equals cor.test() on each pair's complete rows, with the pair count (#3)", {
  for (strMethod in c("pearson", "spearman")) {
    lResult <- Analyze_CorrelationMatrix(dfFrame, chrMatrixCols, strMethod = strMethod)
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "ok")
    expect_named(lResult$rows, c(
      "x", "y", "counts", "estimate", "lower", "upper", "level", "status", "reason", "warning"
    ))
    # Long form: ten pairs of five columns, each pair once.
    expect_identical(nrow(lResult$rows), 10L)
    expect_identical(anyDuplicated(lResult$rows[c("x", "y")]), 0L)
    expect_true(all(lResult$rows$x != lResult$rows$y))

    nCounts <- integer(0)
    for (iPair in seq_len(10)) {
      nX <- dfFrame[[lResult$rows$x[iPair]]]
      nY <- dfFrame[[lResult$rows$y[iPair]]]
      lBase <- lWithWarnings(function() stats::cor.test(nX, nY, method = strMethod))
      nCounts[iPair] <- sum(!is.na(nX) & !is.na(nY))
      expect_identical(lResult$rows$counts[iPair], nCounts[iPair])
      expect_identical(lResult$rows$estimate[iPair], unname(lBase$value$estimate))
      if (strMethod == "pearson") {
        expect_identical(c(lResult$rows$lower[iPair], lResult$rows$upper[iPair]), as.numeric(lBase$value$conf.int))
        expect_identical(lResult$rows$level[iPair], 0.95)
      } else {
        expect_true(is.na(lResult$rows$lower[iPair]) && is.na(lResult$rows$level[iPair]))
      }
    }
    # The pair counts differ from cell to cell, because the missing rows do.
    expect_gt(length(unique(nCounts)), 1)
    expect_identical(
      lResult$counts,
      stats::setNames(lapply(chrMatrixCols, function(strCol) sum(!is.na(dfFrame[[strCol]]))), chrMatrixCols)
    )
    lNamed <- lWithWarnings(function() stats::cor.test(dfFrame[[strX]], dfFrame[[strY]], method = strMethod))
    expect_identical(lResult$method, unname(lNamed$value$method))
  }
})

test_that("the correlation matrix returns no p-values (#3)", {
  lResult <- Analyze_CorrelationMatrix(dfFrame, chrMatrixCols)

  expect_true(is.na(lResult$p_value))
  expect_false(any(grepl("^p_", names(lResult$rows))))
  expect_identical(nrow(lResult$statistic), 0L)
  expect_identical(nrow(lResult$estimates), 0L)
  expect_match(lResult$notes[[1]], "No p-values", fixed = TRUE)
})

test_that("the correlation matrix honours the minimum number of pairs (#3)", {
  dfSparse <- dfFrame
  dfSparse$Sparse <- NA_real_
  dfSparse$Sparse[1:4] <- c(1.5, 2.5, 2, 4)
  lResult <- Analyze_CorrelationMatrix(dfSparse, c(strX, strY, "Sparse"))

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$rows$y, c(strY, "Sparse", "Sparse"))
  expect_identical(lResult$rows$counts, c(200L, 4L, 4L))
  expect_identical(lResult$rows$status, c("ok", "too_small", "too_small"))
  expect_identical(lResult$rows$reason[2], "Not computed: 4 complete pairs. The minimum is 5.")
  expect_true(all(is.na(lResult$rows$estimate[2:3])))

  # A lower minimum computes them; a higher one leaves nothing.
  lLower <- Analyze_CorrelationMatrix(dfSparse, c(strX, strY, "Sparse"), nMinPairs = 4)
  expect_identical(lLower$rows$status, rep("ok", 3))
  expect_identical(lLower$rows$estimate[2], unname(stats::cor.test(dfSparse[[strX]], dfSparse$Sparse)$estimate))
  lNone <- Analyze_CorrelationMatrix(dfSparse, c(strX, strY, "Sparse"), nMinPairs = 201)
  ExpectResultShape(lNone)
  expect_identical(lNone$status, "too_small")
  expect_identical(lNone$reason, "Not computed: no pair of columns has 201 complete pairs.")
  expect_identical(formals(Analyze_CorrelationMatrix)$nMinPairs, quote(nMinGroupDefault))
})

test_that("the correlation matrix takes its columns as a vector or a list, and refuses what it cannot use (#3)", {
  expect_identical(
    Analyze_CorrelationMatrix(dfFrame, as.list(chrMatrixCols)),
    Analyze_CorrelationMatrix(dfFrame, chrMatrixCols)
  )
  # Two columns are still one row of a data frame, never a bare vector.
  lTwo <- Analyze_CorrelationMatrix(dfFrame, c(strX, strY))
  ExpectResultShape(lTwo)
  expect_identical(nrow(lTwo$rows), 1L)

  lCases <- list(
    list(dfData = dfFrame, chrCols = strX),
    list(dfData = dfFrame, chrCols = c(strX, strX)),
    list(dfData = dfFrame, chrCols = c(strX, "Nope")),
    list(dfData = dfFrame, chrCols = c(strX, "ARM")),
    list(dfData = dfFrame, chrCols = list(c(strX, strY)))
  )
  for (lArgs in lCases) {
    expect_silent(lResult <- do.call(Analyze_CorrelationMatrix, lArgs))
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "error")
  }
})
