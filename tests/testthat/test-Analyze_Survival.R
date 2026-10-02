# Analyze_Survival (#4): survdiff() for the log-rank test, survfit() for median
# survival with its log-log interval, and coxph() for the hazard ratio between
# two groups. Every comparison with the survival package below is exact
# (expect_identical).

dfFrame <- dfSyntheticFrame()
lTruth <- Synthetic_Truth$Survival
chrLevels <- lTruth$Groups
dfModel <- data.frame(
  AVAL = dfFrame$AVAL,
  Event = dfFrame$CNSR == 0,
  Group = factor(dfFrame$CRP_LEVEL, levels = chrLevels)
)

test_that("the log-rank test equals survdiff() on the synthetic study (#4)", {
  lResult <- Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = chrLevels)
  lBase <- survival::survdiff(survival::Surv(AVAL, Event) ~ Group, data = dfModel)

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$test, "logrank")
  expect_identical(lResult$method, "Log-rank test")
  expect_identical(lResult$statistic, data.frame(name = c("Chisq", "df"), value = c(lBase$chisq, 1)))
  expect_identical(lResult$p_value, stats::pchisq(lBase$chisq, 1, lower.tail = FALSE))
  if (!is.null(lBase$pvalue)) {
    expect_identical(lResult$p_value, lBase$pvalue)
  }
  expect_identical(lResult$adjustment, "none")
  # Participants and events per group, as survdiff() counted them.
  expect_identical(lResult$counts, stats::setNames(as.list(as.integer(lBase$n)), chrLevels))
  expect_identical(lResult$rows$group, chrLevels)
  expect_identical(lResult$rows$n, as.integer(lBase$n))
  expect_identical(lResult$rows$events, as.integer(lBase$obs))
  expect_identical(nrow(lResult$dropped), 0L)
  expect_identical(lResult$warnings, list())
})

test_that("median survival and its interval equal survfit() with the log-log interval (#4)", {
  lResult <- Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = chrLevels)
  mBase <- summary(survival::survfit(survival::Surv(AVAL, Event) ~ Group, data = dfModel, conf.type = "log-log"))$table

  expect_identical(lResult$rows$median, unname(mBase[, "median"]))
  expect_identical(lResult$rows$lower, unname(mBase[, "0.95LCL"]))
  expect_identical(lResult$rows$upper, unname(mBase[, "0.95UCL"]))
  expect_identical(lResult$rows$level, c(0.95, 0.95))
  dfMedians <- lResult$estimates[lResult$estimates$name == "Median", ]
  expect_identical(dfMedians$group, chrLevels)
  expect_identical(dfMedians$estimate, lResult$rows$median)
  expect_identical(dfMedians$lower, lResult$rows$lower)
  expect_identical(dfMedians$upper, lResult$rows$upper)

  # It is the log-log interval and not survfit()'s default, which differs.
  mDefault <- summary(survival::survfit(survival::Surv(AVAL, Event) ~ Group, data = dfModel))$table
  expect_false(identical(unname(mDefault[, "0.95LCL"]), lResult$rows$lower))
  expect_match(lResult$notes[[2]], "conf.type = 'log-log'", fixed = TRUE)

  # A bound the band never reaches is missing, and a note says why.
  expect_true(anyNA(lResult$rows$upper))
  expect_true(any(grepl("was not reached", unlist(lResult$notes), fixed = TRUE)))

  # The level is the caller's.
  lNinety <- Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = chrLevels, nConfLevel = 0.9)
  mNinety <- summary(survival::survfit(
    survival::Surv(AVAL, Event) ~ Group,
    data = dfModel, conf.type = "log-log", conf.int = 0.9
  ))$table
  expect_identical(lNinety$rows$lower, unname(mNinety[, "0.9LCL"]))
  expect_identical(lNinety$rows$level, c(0.9, 0.9))
})

test_that("the hazard ratio equals coxph(), the first group over the second (#4)", {
  lResult <- Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = chrLevels)
  # The second group is the reference, so the coefficient is the first group's.
  dfModel$Against <- factor(dfFrame$CRP_LEVEL, levels = rev(chrLevels))
  lBase <- summary(survival::coxph(survival::Surv(AVAL, Event) ~ Against, data = dfModel))

  dfRatio <- lResult$estimates[lResult$estimates$name == "Hazard ratio", ]
  expect_identical(dfRatio$group, paste(chrLevels[1], "/", chrLevels[2]))
  expect_identical(dfRatio$estimate, unname(lBase$conf.int[1, "exp(coef)"]))
  expect_identical(dfRatio$lower, unname(lBase$conf.int[1, "lower .95"]))
  expect_identical(dfRatio$upper, unname(lBase$conf.int[1, "upper .95"]))
  expect_identical(dfRatio$level, 0.95)
  expect_gt(dfRatio$estimate, 1)

  # The Cox model's own p-value is the Wald test's, kept apart from the
  # log-rank p-value and labelled.
  expect_identical(lResult$rows$hazard_ratio, c(dfRatio$estimate, NA))
  expect_identical(lResult$rows$hr_lower, c(dfRatio$lower, NA))
  expect_identical(lResult$rows$hr_upper, c(dfRatio$upper, NA))
  expect_identical(lResult$rows$hr_p_value, c(unname(lBase$coefficients[1, "Pr(>|z|)"]), NA))
  expect_identical(lResult$rows$hr_test, c("Wald", NA))
  expect_false(identical(lResult$rows$hr_p_value[1], lResult$p_value))
  expect_match(lResult$notes[[4]], "p_value is the log-rank test's", fixed = TRUE)
  expect_match(lResult$notes[[4]], "Wald test's, a different test", fixed = TRUE)

  # The other order gives the other ratio: coxph() with the other reference.
  lReversed <- Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = rev(chrLevels))
  lBaseReversed <- summary(survival::coxph(survival::Surv(AVAL, Event) ~ Group, data = dfModel))
  dfReversed <- lReversed$estimates[lReversed$estimates$name == "Hazard ratio", ]
  expect_identical(dfReversed$group, paste(chrLevels[2], "/", chrLevels[1]))
  expect_identical(dfReversed$estimate, unname(lBaseReversed$conf.int[1, "exp(coef)"]))
  expect_lt(dfReversed$estimate, 1)
  # The log-rank test does not have a direction: the same p-value either way,
  # to the rounding of the arithmetic.
  expect_equal(lReversed$p_value, lResult$p_value)
})

test_that("the planted survival effect falls inside its interval (#4)", {
  lResult <- Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = lTruth$Groups)
  dfRatio <- lResult$estimates[lResult$estimates$name == "Hazard ratio", ]

  expect_identical(dfRatio$group, paste(lTruth$Groups[1], "/", lTruth$Groups[2]))
  expect_gt(lTruth$Value, dfRatio$lower)
  expect_lt(lTruth$Value, dfRatio$upper)
  expect_lt(lResult$p_value, 0.001)
})

test_that("more than two groups get the log-rank test and medians, and no hazard ratio (#4)", {
  chrFour <- sort(unique(dfFrame$ARM_SEX), method = "radix")
  lResult <- Analyze_Survival(dfFrame, "AVAL", "ARM_SEX", strCensorCol = "CNSR")
  dfFour <- data.frame(AVAL = dfFrame$AVAL, Event = dfFrame$CNSR == 0, Group = factor(dfFrame$ARM_SEX, levels = chrFour))
  lBase <- survival::survdiff(survival::Surv(AVAL, Event) ~ Group, data = dfFour)
  mBase <- summary(survival::survfit(survival::Surv(AVAL, Event) ~ Group, data = dfFour, conf.type = "log-log"))$table

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$statistic$value, c(lBase$chisq, 3))
  expect_identical(lResult$p_value, stats::pchisq(lBase$chisq, 3, lower.tail = FALSE))
  expect_identical(lResult$rows$group, chrFour)
  expect_identical(lResult$rows$events, as.integer(lBase$obs))
  expect_identical(lResult$rows$median, unname(mBase[, "median"]))
  expect_identical(lResult$estimates$name, rep("Median", 4))
  expect_true(all(is.na(lResult$rows$hazard_ratio)) && all(is.na(lResult$rows$hr_test)))
})

test_that("the censor column and the event column are two named conventions, and a wrong column is refused (#4)", {
  lCensor <- Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = chrLevels)
  expect_identical(lCensor$notes[[1]], "An event is a row where CNSR is 0; the others are censored.")

  # The same data the other way round, as whole numbers and as true and false.
  dfFlags <- dfFrame
  dfFlags$EVENT <- 1L - dfFlags$CNSR
  dfFlags$HAD_EVENT <- dfFlags$CNSR == 0
  lEvent <- Analyze_Survival(dfFlags, "AVAL", "CRP_LEVEL", strEventCol = "EVENT", chrGroups = chrLevels)
  lLogical <- Analyze_Survival(dfFlags, "AVAL", "CRP_LEVEL", strEventCol = "HAD_EVENT", chrGroups = chrLevels)
  expect_identical(lEvent$notes[[1]], "An event is a row where EVENT is 1; the others are censored.")
  chrSame <- c("status", "estimates", "statistic", "p_value", "counts", "rows")
  expect_identical(lEvent[chrSame], lCensor[chrSame])
  expect_identical(lLogical[chrSame], lCensor[chrSame])

  # Passing the censor flag as if it were the event flag is not an error R can
  # see, but it shows: the events per group are the censored counts.
  lWrong <- Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strEventCol = "CNSR", chrGroups = chrLevels)
  expect_identical(lWrong$rows$events, lCensor$rows$n - lCensor$rows$events)
  expect_identical(lWrong$notes[[1]], "An event is a row where CNSR is 1; the others are censored.")

  # Neither, both, or a column that is not a flag at all: refused.
  lCases <- list(
    list(dfData = dfFrame, strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL"),
    list(dfData = dfFlags, strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL", strCensorCol = "CNSR", strEventCol = "EVENT"),
    list(dfData = dfFrame, strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL", strCensorCol = "AVAL"),
    list(dfData = dfFrame, strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL", strCensorCol = "ARM"),
    list(dfData = dfFrame, strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL", strCensorCol = "Nope"),
    list(dfData = dfFrame, strTimeCol = "ARM", strGroupCol = "CRP_LEVEL", strCensorCol = "CNSR"),
    list(dfData = dfFrame, strTimeCol = "AVAL", strGroupCol = "Nope", strCensorCol = "CNSR"),
    list(dfData = dfFrame, strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL", strCensorCol = "CNSR", nConfLevel = 1),
    list(dfData = dfFrame, strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = "High")
  )
  for (lArgs in lCases) {
    expect_silent(lResult <- do.call(Analyze_Survival, lArgs))
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "error")
  }
  expect_identical(
    Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL")$reason,
    "Name exactly one of strCensorCol (1 = censored, 0 = event, as ADaM's CNSR) and strEventCol (1 = event, 0 = censored)."
  )
  expect_identical(
    Analyze_Survival(dfFrame, "AVAL", "CRP_LEVEL", strCensorCol = "AVAL")$reason,
    "Column 'AVAL' (strCensorCol) must hold only 0 and 1."
  )
})

test_that("a group below the minimum size gets a reason and no numbers (#4)", {
  dfSmall <- rbind(dfFrame[dfFrame$CRP_LEVEL == "Low", ], head(dfFrame[dfFrame$CRP_LEVEL == "High", ], 4))
  lResult <- Analyze_Survival(dfSmall, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR")

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "too_small")
  expect_identical(lResult$reason, "Not computed: High has 4. The minimum group size is 5.")
  expect_identical(lResult$counts, list(High = 4L, Low = 100L))
  # Participants and events per group are still given; nothing else is.
  expect_identical(lResult$rows$n, c(4L, 100L))
  expect_identical(lResult$rows$events, c(sum(dfSmall$CNSR[dfSmall$CRP_LEVEL == "High"] == 0), sum(dfSmall$CNSR[dfSmall$CRP_LEVEL == "Low"] == 0)))
  expect_true(all(is.na(lResult$rows$median)) && all(is.na(lResult$rows$hazard_ratio)))

  expect_identical(Analyze_Survival(dfSmall, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", nMinGroup = 4)$status, "ok")
  expect_identical(formals(Analyze_Survival)$nMinGroup, quote(nMinGroupDefault))
})

test_that("rows that cannot be used are dropped and counted (#4)", {
  dfMessy <- dfFrame
  dfMessy$CRP_LEVEL[1:2] <- NA
  dfMessy$AVAL[3:5] <- NA
  dfMessy$CNSR[6] <- NA
  dfMessy$AVAL[7] <- -1
  dfMessy$CRP_LEVEL[8:9] <- "Middle"
  lResult <- Analyze_Survival(dfMessy, "AVAL", "CRP_LEVEL", strCensorCol = "CNSR", chrGroups = as.list(chrLevels))

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(
    lResult$dropped,
    data.frame(
      reason = c("Missing group", "Group not selected", "Missing time or event flag", "Negative time"),
      n = c(2L, 2L, 4L, 1L)
    )
  )
  expect_identical(sum(unlist(lResult$counts)), 200L - 9L)
  dfKept <- dfMessy[-(1:9), ]
  lBase <- survival::survdiff(
    survival::Surv(AVAL, CNSR == 0) ~ factor(CRP_LEVEL, levels = chrLevels),
    data = dfKept
  )
  expect_identical(lResult$statistic$value[1], lBase$chisq)
})
