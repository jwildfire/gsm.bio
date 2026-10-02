# Analyze_Contingency (#3): chisq.test(), with small expected counts flagged
# from the expected counts it returns, and fisher.test(). Every comparison with
# base R below is exact (expect_identical).

dfFrame <- dfSyntheticFrame()
chrArms <- c("Placebo", "Treatment")
chrResponses <- c("Non-responder", "Responder")
chrFour <- sort(unique(dfFrame$ARM_SEX), method = "radix")

test_that("the chi-squared test equals chisq.test() on a two-by-two table, continuity correction and all (#3)", {
  lResult <- Analyze_Contingency(dfFrame, "ARM", "RESPONSE", strMethod = "chisq")
  lBase <- stats::chisq.test(table(dfFrame$ARM, dfFrame$RESPONSE))

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$method, "Pearson's Chi-squared test with Yates' continuity correction")
  expect_identical(lResult$method, unname(lBase$method))
  expect_identical(
    lResult$statistic,
    data.frame(name = c("X-squared", "df"), value = c(unname(lBase$statistic), unname(lBase$parameter)))
  )
  expect_identical(lResult$p_value, lBase$p.value)
  expect_identical(lResult$counts, 200L)
  expect_identical(nrow(lResult$estimates), 0L)
  expect_identical(lResult$warnings, list())
  expect_identical(lResult$notes, list())

  # The table in long form, one row per cell, with chisq.test()'s expected counts.
  expect_identical(
    lResult$rows,
    data.frame(
      row = rep(chrArms, times = 2),
      col = rep(chrResponses, each = 2),
      n = as.integer(lBase$observed),
      expected = as.numeric(lBase$expected),
      small_expected = rep(FALSE, 4)
    )
  )
})

test_that("the chi-squared test equals chisq.test() on a larger table (#3)", {
  lResult <- Analyze_Contingency(dfFrame, "ARM_SEX", "RESPONSE")
  lBase <- stats::chisq.test(table(dfFrame$ARM_SEX, dfFrame$RESPONSE))

  ExpectResultShape(lResult)
  expect_identical(lResult$method, "Pearson's Chi-squared test")
  expect_identical(lResult$statistic$value, c(unname(lBase$statistic), unname(lBase$parameter)))
  expect_identical(lResult$p_value, lBase$p.value)
  expect_identical(lResult$rows$row, rep(chrFour, times = 2))
  expect_identical(lResult$rows$n, as.integer(lBase$observed))
  expect_identical(lResult$rows$expected, as.numeric(lBase$expected))
})

test_that("small expected counts are flagged from chisq.test()'s own expected counts, and R's warning is kept (#3)", {
  # Age by decade against response: the youngest and oldest decades are sparse.
  expect_silent(lResult <- Analyze_Contingency(dfFrame, "AGE_DECADE", "RESPONSE", nMinGroup = 1))
  lBase <- lWithWarnings(function() stats::chisq.test(table(dfFrame$AGE_DECADE, dfFrame$RESPONSE)))

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$p_value, lBase$value$p.value)
  expect_identical(lResult$rows$expected, as.numeric(lBase$value$expected))
  expect_identical(lResult$rows$small_expected, as.logical(lBase$value$expected < 5))
  expect_gt(sum(lResult$rows$small_expected), 0)
  expect_lt(sum(lResult$rows$small_expected), nrow(lResult$rows))
  expect_identical(lResult$warnings, list("Chi-squared approximation may be incorrect"))
  expect_identical(lResult$warnings, lBase$warnings)
  expect_identical(
    lResult$notes[[1]],
    sprintf(
      "%d of %d expected counts are below 5, so the chi-squared approximation may be poor. Fisher's exact test does not rely on it.",
      sum(lBase$value$expected < 5), length(lBase$value$expected)
    )
  )

  # With the default minimum the sparse decades stop the test instead.
  lDefault <- Analyze_Contingency(dfFrame, "AGE_DECADE", "RESPONSE")
  expect_identical(lDefault$status, "too_small")
})

test_that("Fisher's exact test equals fisher.test() on a two-by-two table, with the odds ratio and its interval (#3)", {
  lResult <- Analyze_Contingency(dfFrame, "ARM", "RESPONSE", strMethod = "fisher")
  lBase <- stats::fisher.test(table(dfFrame$ARM, dfFrame$RESPONSE))

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$method, "Fisher's Exact Test for Count Data")
  expect_identical(lResult$method, unname(lBase$method))
  expect_identical(lResult$p_value, lBase$p.value)
  expect_identical(
    lResult$estimates,
    data.frame(
      name = "odds ratio", group = NA_character_, estimate = unname(lBase$estimate),
      lower = lBase$conf.int[1], upper = lBase$conf.int[2], level = 0.95
    )
  )
  expect_identical(nrow(lResult$statistic), 0L)
  expect_identical(lResult$rows$n, as.integer(table(dfFrame$ARM, dfFrame$RESPONSE)))
  expect_true(all(is.na(lResult$rows$expected)) && all(is.na(lResult$rows$small_expected)))

  # The order of the categories sets which way round the odds ratio is.
  lFlipped <- Analyze_Contingency(
    dfFrame, "ARM", "RESPONSE",
    strMethod = "fisher", chrRowGroups = rev(chrArms), nConfLevel = 0.9
  )
  lBaseFlipped <- stats::fisher.test(
    table(factor(dfFrame$ARM, levels = rev(chrArms)), dfFrame$RESPONSE),
    conf.level = 0.9
  )
  expect_identical(lFlipped$estimates$estimate, unname(lBaseFlipped$estimate))
  expect_identical(c(lFlipped$estimates$lower, lFlipped$estimates$upper), as.numeric(lBaseFlipped$conf.int))
  expect_identical(lFlipped$estimates$level, 0.9)
})

test_that("Fisher's exact test equals fisher.test() on a larger table, where R estimates no odds ratio (#3)", {
  lResult <- Analyze_Contingency(dfFrame, "ARM_SEX", "RESPONSE", strMethod = "fisher")
  lBase <- stats::fisher.test(table(dfFrame$ARM_SEX, dfFrame$RESPONSE))

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$p_value, lBase$p.value)
  expect_null(lBase$estimate)
  expect_identical(nrow(lResult$estimates), 0L)
  expect_identical(nrow(lResult$rows), 8L)
})

test_that("a category below the minimum size gets a reason and no numbers (#3)", {
  dfSmall <- dfFrame
  dfSmall$RESPONSE[dfSmall$RESPONSE == "Responder"][-(1:4)] <- "Non-responder"
  for (strMethod in c("chisq", "fisher")) {
    lResult <- Analyze_Contingency(dfSmall, "ARM", "RESPONSE", strMethod = strMethod)
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "too_small")
    expect_identical(lResult$reason, "Not computed: RESPONSE = Responder has 4. The minimum group size is 5.")
    expect_identical(lResult$counts, 200L)
    # The table is still returned, because it is the explanation.
    expect_identical(sum(lResult$rows$n), 200L)
  }
  expect_identical(Analyze_Contingency(dfSmall, "ARM", "RESPONSE", strMethod = "fisher", nMinGroup = 4)$status, "ok")
  expect_identical(formals(Analyze_Contingency)$nMinGroup, quote(nMinGroupDefault))
})

test_that("missing and unselected categories are dropped and counted (#3)", {
  dfMessy <- dfFrame
  dfMessy$ARM[1:3] <- NA
  dfMessy$RESPONSE[4:5] <- ""
  dfMessy$ARM <- factor(dfMessy$ARM)
  lResult <- Analyze_Contingency(
    dfMessy, "ARM_SEX", "RESPONSE",
    chrRowGroups = as.list(chrFour[1:3]), chrColGroups = chrResponses
  )
  bMissing <- !nzchar(dfMessy$RESPONSE)
  bOther <- !bMissing & dfMessy$ARM_SEX == chrFour[4]

  ExpectResultShape(lResult)
  expect_identical(
    lResult$dropped,
    data.frame(reason = c("Missing category", "Category not selected"), n = c(sum(bMissing), sum(bOther)))
  )
  expect_identical(lResult$counts, 200L - sum(bMissing) - sum(bOther))
  lBase <- stats::chisq.test(table(
    factor(dfMessy$ARM_SEX[!bMissing & !bOther], levels = chrFour[1:3]),
    dfMessy$RESPONSE[!bMissing & !bOther]
  ))
  expect_identical(lResult$p_value, lBase$p.value)

  # A factor column gives its labels, and its missing values are dropped.
  lFactor <- Analyze_Contingency(dfMessy, "ARM", "RESPONSE")
  expect_identical(lFactor$dropped, data.frame(reason = "Missing category", n = 5L))
  expect_identical(lFactor$rows$row, rep(chrArms, times = 2))
})

test_that("a contingency request that cannot be met is an error status, never an R error (#3)", {
  dfOne <- dfFrame
  dfOne$Same <- "A"
  lCases <- list(
    list(dfData = dfFrame, strRowCol = "Nope", strColCol = "RESPONSE"),
    list(dfData = dfFrame, strRowCol = "ARM", strColCol = "RESPONSE", strMethod = "g"),
    list(dfData = dfFrame, strRowCol = "ARM", strColCol = "RESPONSE", chrRowGroups = character(0)),
    list(dfData = dfOne, strRowCol = "Same", strColCol = "RESPONSE")
  )
  for (lArgs in lCases) {
    expect_silent(lResult <- do.call(Analyze_Contingency, lArgs))
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "error")
  }
  expect_identical(
    Analyze_Contingency(dfOne, "Same", "RESPONSE")$reason,
    "A two-way table needs two or more categories each way; 'Same' has 1 and 'RESPONSE' has 2."
  )
})
