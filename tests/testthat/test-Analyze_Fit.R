# Analyze_Fit (#12): lm() for a line and loess() for a smooth, for one pair of
# numeric variables overall and per group, with the fitted values and their
# band at equally spaced x values. Every comparison with base R below is exact
# (expect_identical): the same call on the same data in the same session.

dfFrame <- dfSyntheticFrame()
lTruth <- Synthetic_Truth$Correlation
# The planted pair: TNF-alpha with IL-10 at Baseline. y is fitted to x.
strX <- paste(lTruth$Biomarkers[1], "@", lTruth$Visit)
strY <- paste(lTruth$Biomarkers[2], "@", lTruth$Visit)
# Week 4 has missed visits and missing results, so the pairs are not complete.
strXLater <- paste(lTruth$Biomarkers[1], "@ Week 4")
strYLater <- paste(lTruth$Biomarkers[2], "@ Week 4")

chrFitRowColumns <- c(
  "group", "x", "fit", "lower", "upper", "level", "counts", "method", "statistic", "df", "r_squared",
  "p_value", "adjustment", "status", "reason", "warning"
)

# The complete pairs of two columns, as the data frame both fits are made on.
dfPairsOf <- function(dfData, strXCol, strYCol) {
  bPair <- !is.na(dfData[[strXCol]]) & !is.na(dfData[[strYCol]])
  data.frame(x = dfData[[strXCol]][bPair], y = dfData[[strYCol]][bPair])
}

# The x values a line is given at: equally spaced from the least to the greatest.
dfGridOf <- function(dfPairs, nPoints) {
  data.frame(x = seq(min(dfPairs$x), max(dfPairs$x), length.out = nPoints))
}

# Holds one fit's coefficients, test and line in a result to lm() on the pairs.
ExpectLinearFit <- function(dfEstimates, dfLine, dfPairs, nPoints, nConfLevel, strLabel) {
  lmBase <- stats::lm(y ~ x, data = dfPairs)
  lSummary <- summary(lmBase)
  mConfint <- stats::confint(lmBase, level = nConfLevel)
  mBand <- stats::predict(lmBase, newdata = dfGridOf(dfPairs, nPoints), interval = "confidence", level = nConfLevel)

  expect_identical(dfEstimates$name, c("Intercept", "Slope"), label = paste(strLabel, "estimate names"))
  expect_identical(dfEstimates$estimate, unname(stats::coef(lmBase)), label = paste(strLabel, "coef()"))
  expect_identical(dfEstimates$lower, unname(mConfint[, 1]), label = paste(strLabel, "confint() lower"))
  expect_identical(dfEstimates$upper, unname(mConfint[, 2]), label = paste(strLabel, "confint() upper"))
  expect_identical(dfEstimates$level, rep(nConfLevel, 2), label = paste(strLabel, "level"))

  expect_identical(nrow(dfLine), as.integer(nPoints), label = paste(strLabel, "points"))
  expect_identical(dfLine$x, dfGridOf(dfPairs, nPoints)$x, label = paste(strLabel, "grid"))
  expect_identical(dfLine$fit, unname(mBand[, "fit"]), label = paste(strLabel, "predict() fit"))
  expect_identical(dfLine$lower, unname(mBand[, "lwr"]), label = paste(strLabel, "predict() lwr"))
  expect_identical(dfLine$upper, unname(mBand[, "upr"]), label = paste(strLabel, "predict() upr"))
  expect_identical(unique(dfLine$level), nConfLevel, label = paste(strLabel, "band level"))
  # The fit's own answer, on each row of its line: the slope's row of
  # summary(fit)$coefficients, R-squared and the residual degrees of freedom.
  expect_identical(unique(dfLine$statistic), lSummary$coefficients["x", "t value"], label = paste(strLabel, "t value"))
  expect_identical(unique(dfLine$p_value), lSummary$coefficients["x", "Pr(>|t|)"], label = paste(strLabel, "Pr(>|t|)"))
  expect_identical(unique(dfLine$r_squared), lSummary$r.squared, label = paste(strLabel, "r.squared"))
  expect_identical(unique(dfLine$df), as.numeric(stats::df.residual(lmBase)), label = paste(strLabel, "residual df"))
  expect_identical(unique(dfLine$counts), nrow(dfPairs), label = paste(strLabel, "counts"))
  expect_identical(unique(dfLine$status), "ok", label = paste(strLabel, "status"))
  invisible(lSummary)
}

# Holds one smooth's curve and band in a result to loess() on the pairs.
ExpectSmoothFit <- function(dfLine, dfPairs, nPoints, nConfLevel, strLabel) {
  loBase <- stats::loess(y ~ x, data = dfPairs)
  lBand <- stats::predict(loBase, newdata = dfGridOf(dfPairs, nPoints), se = TRUE)
  nHalfWidth <- stats::qt((1 + nConfLevel) / 2, lBand$df) * unname(lBand$se.fit)

  expect_identical(nrow(dfLine), as.integer(nPoints), label = paste(strLabel, "points"))
  expect_identical(dfLine$x, dfGridOf(dfPairs, nPoints)$x, label = paste(strLabel, "grid"))
  expect_identical(dfLine$fit, unname(lBand$fit), label = paste(strLabel, "predict() fit"))
  expect_identical(dfLine$lower, unname(lBand$fit) - nHalfWidth, label = paste(strLabel, "band lower"))
  expect_identical(dfLine$upper, unname(lBand$fit) + nHalfWidth, label = paste(strLabel, "band upper"))
  expect_identical(unique(dfLine$df), lBand$df, label = paste(strLabel, "df"))
  expect_identical(unique(dfLine$counts), nrow(dfPairs), label = paste(strLabel, "counts"))
  # A smooth has no slope to test.
  expect_true(all(is.na(dfLine$statistic)) && all(is.na(dfLine$p_value)) && all(is.na(dfLine$r_squared)), label = paste(strLabel, "no test"))
  invisible(list(model = loBase, band = lBand))
}

test_that("the linear fit's intercept, slope, intervals, test and R-squared equal lm() on the planted pair (#12)", {
  lResult <- Analyze_Fit(dfFrame, strX, strY, strMethod = "linear")
  dfPairs <- dfPairsOf(dfFrame, strX, strY)
  lmBase <- stats::lm(y ~ x, data = dfPairs)
  lSummary <- summary(lmBase)

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$test, "linear")
  expect_identical(lResult$method, "Linear regression")
  expect_identical(
    lResult$estimates,
    data.frame(
      name = c("Intercept", "Slope"), group = NA_character_, estimate = unname(stats::coef(lmBase)),
      lower = unname(stats::confint(lmBase)[, 1]), upper = unname(stats::confint(lmBase)[, 2]), level = 0.95
    )
  )
  # The slope's t value, the residual degrees of freedom and R-squared.
  expect_identical(
    lResult$statistic,
    data.frame(
      name = c("t", "df", "r.squared"),
      value = c(lSummary$coefficients["x", "t value"], lSummary$df[2], lSummary$r.squared)
    )
  )
  # The top-level p-value is the test of a zero slope, and the result says so.
  expect_identical(lResult$p_value, lSummary$coefficients["x", "Pr(>|t|)"])
  expect_match(lResult$notes[[1]], "p_value is the t-test that the slope is zero, from summary(lm())", fixed = TRUE)
  expect_identical(lResult$adjustment, "none")
  expect_identical(lResult$counts, 200L)
  expect_identical(nrow(lResult$dropped), 0L)
  expect_identical(lResult$warnings, list())

  # With one explanatory variable the slope's test is the test of the Pearson
  # correlation, so the two functions agree on the planted pair.
  lCorrelation <- Analyze_Correlation(dfFrame, strX, strY)
  expect_equal(lResult$p_value, lCorrelation$p_value, tolerance = 1e-10)
  expect_equal(lResult$statistic$value[3], lCorrelation$estimates$estimate^2, tolerance = 1e-12)
  expect_gt(lResult$estimates$lower[2], 0)
})

test_that("the linear fit's line and band equal predict(interval = 'confidence') at an equally spaced grid (#12)", {
  lResult <- Analyze_Fit(dfFrame, strX, strY)
  dfPairs <- dfPairsOf(dfFrame, strX, strY)

  # Fifty points when the caller does not say, from the least x to the greatest.
  expect_identical(formals(Analyze_Fit)$nPoints, 50L)
  expect_named(lResult$rows, chrFitRowColumns)
  expect_identical(nrow(lResult$rows), 50L)
  expect_true(all(is.na(lResult$rows$group)))
  expect_identical(range(lResult$rows$x), range(dfPairs$x))
  expect_identical(length(unique(round(diff(lResult$rows$x), 10))), 1L)
  ExpectLinearFit(lResult$estimates, lResult$rows, dfPairs, 50L, 0.95, "overall")
  expect_true(all(lResult$rows$lower < lResult$rows$fit & lResult$rows$fit < lResult$rows$upper))
  expect_match(lResult$notes[[2]], "confidence band of the fitted mean, not a prediction band", fixed = TRUE)

  # Incomplete pairs are dropped and counted; the level and the number of
  # points are the caller's.
  lLater <- Analyze_Fit(dfFrame, strXLater, strYLater, nConfLevel = 0.9, nPoints = 7)
  dfLater <- dfPairsOf(dfFrame, strXLater, strYLater)
  ExpectResultShape(lLater)
  expect_lt(nrow(dfLater), 200L)
  expect_identical(lLater$counts, nrow(dfLater))
  expect_identical(lLater$dropped, data.frame(reason = "Incomplete pair", n = 200L - nrow(dfLater)))
  ExpectLinearFit(lLater$estimates, lLater$rows, dfLater, 7, 0.9, "Week 4 at 90%")
  # Two points are a line; the number arrives from JSON as a number or a whole one.
  expect_identical(nrow(Analyze_Fit(dfFrame, strX, strY, nPoints = 2)$rows), 2L)
  expect_identical(Analyze_Fit(dfFrame, strX, strY, nPoints = 7L), Analyze_Fit(dfFrame, strX, strY, nPoints = 7))
})

test_that("the smooth's curve and band equal loess() and predict(se = TRUE) (#12)", {
  lResult <- Analyze_Fit(dfFrame, strX, strY, strMethod = "smooth")
  dfPairs <- dfPairsOf(dfFrame, strX, strY)

  ExpectResultShape(lResult)
  expect_identical(lResult$status, "ok")
  expect_identical(lResult$test, "smooth")
  expect_identical(lResult$method, "Local polynomial regression (loess)")
  expect_named(lResult$rows, chrFitRowColumns)
  lBase <- ExpectSmoothFit(lResult$rows, dfPairs, 50L, 0.95, "overall")
  # What loess() and predict() report of the fit, by their own names.
  expect_identical(
    lResult$statistic,
    data.frame(
      name = c("enp", "df", "residual.scale"),
      value = c(lBase$model$enp, lBase$band$df, lBase$band$residual.scale)
    )
  )
  # No slope, no intercept and no test: none is invented, and the result says
  # so, and says how the band is formed.
  expect_identical(nrow(lResult$estimates), 0L)
  expect_true(is.na(lResult$p_value))
  expect_match(lResult$notes[[1]], "A smooth has no slope, no intercept and no test", fixed = TRUE)
  expect_match(lResult$notes[[2]], "qt((1 + level) / 2, df) times its standard error", fixed = TRUE)
  expect_match(lResult$notes[[2]], "conventional pointwise band, computed in R", fixed = TRUE)
  expect_identical(lResult$counts, 200L)

  lLater <- Analyze_Fit(dfFrame, strXLater, strYLater, strMethod = "smooth", nConfLevel = 0.8, nPoints = 11)
  ExpectResultShape(lLater)
  ExpectSmoothFit(lLater$rows, dfPairsOf(dfFrame, strXLater, strYLater), 11, 0.8, "Week 4 at 80%")
  expect_identical(lLater$dropped$reason, "Incomplete pair")
})

test_that("per-group fits equal the base R calls within each group, for the line and for the smooth (#12)", {
  lLinear <- Analyze_Fit(dfFrame, strXLater, strYLater, strGroupCol = "ARM", nPoints = 12)
  ExpectResultShape(lLinear)
  # The overall fit first, with no group, and then each group's: two rows of
  # estimates and one line each.
  expect_identical(lLinear$estimates$group, c(NA, NA, "Placebo", "Placebo", "Treatment", "Treatment"))
  expect_identical(lLinear$estimates$name, rep(c("Intercept", "Slope"), 3))
  expect_identical(lLinear$rows$group, rep(c(NA, "Placebo", "Treatment"), each = 12))
  ExpectLinearFit(
    lLinear$estimates[1:2, ], lLinear$rows[is.na(lLinear$rows$group), ],
    dfPairsOf(dfFrame, strXLater, strYLater), 12, 0.95, "overall"
  )
  for (strArm in c("Placebo", "Treatment")) {
    dfArm <- dfPairsOf(dfFrame[dfFrame$ARM == strArm, ], strXLater, strYLater)
    ExpectLinearFit(
      lLinear$estimates[lLinear$estimates$group %in% strArm, ], lLinear$rows[lLinear$rows$group %in% strArm, ],
      dfArm, 12, 0.95, strArm
    )
    # Each group's line spans that group's own values.
    expect_identical(range(lLinear$rows$x[lLinear$rows$group %in% strArm]), range(dfArm$x))
  }
  # The overall answer is the same with or without the per-group fits.
  lOverall <- Analyze_Fit(dfFrame, strXLater, strYLater, nPoints = 12)
  expect_identical(lLinear[c("statistic", "p_value", "counts")], lOverall[c("statistic", "p_value", "counts")])
  expect_identical(lLinear$estimates[1:2, ], lOverall$estimates)
  expect_identical(lLinear$rows[1:12, ], lOverall$rows)

  lSmooth <- Analyze_Fit(dfFrame, strXLater, strYLater, strMethod = "smooth", strGroupCol = "ARM", nPoints = 12)
  ExpectResultShape(lSmooth)
  expect_identical(nrow(lSmooth$estimates), 0L)
  expect_identical(lSmooth$rows$group, rep(c(NA, "Placebo", "Treatment"), each = 12))
  ExpectSmoothFit(lSmooth$rows[is.na(lSmooth$rows$group), ], dfPairsOf(dfFrame, strXLater, strYLater), 12, 0.95, "overall")
  for (strArm in c("Placebo", "Treatment")) {
    ExpectSmoothFit(
      lSmooth$rows[lSmooth$rows$group %in% strArm, ],
      dfPairsOf(dfFrame[dfFrame$ARM == strArm, ], strXLater, strYLater), 12, 0.95, strArm
    )
  }

  # The groups can be named, as a vector or as JSON's list of single values,
  # and a pair whose group is not one of them is counted.
  lNamed <- Analyze_Fit(dfFrame, strX, strY, strGroupCol = "ARM_SEX", chrGroups = list("Treatment M", "Placebo F"), nPoints = 3)
  expect_identical(unique(lNamed$rows$group), c(NA, "Treatment M", "Placebo F"))
  expect_identical(unique(lNamed$estimates$group), c(NA, "Treatment M", "Placebo F"))
  expect_identical(
    lNamed$dropped,
    data.frame(
      reason = "Group not selected (left out of the per-group rows)",
      n = sum(!dfFrame$ARM_SEX %in% c("Treatment M", "Placebo F"))
    )
  )
  expect_identical(lNamed, Analyze_Fit(dfFrame, strX, strY, strGroupCol = "ARM_SEX", chrGroups = c("Treatment M", "Placebo F"), nPoints = 3))
})

test_that("too few complete pairs get a reason and no numbers, overall and per group (#12)", {
  dfSmall <- dfFrame[1:4, ]
  for (strMethod in c("linear", "smooth")) {
    lResult <- Analyze_Fit(dfSmall, strX, strY, strMethod = strMethod)
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "too_small")
    expect_identical(lResult$reason, "Not computed: 4 complete pairs. The minimum is 5.")
    expect_identical(lResult$counts, 4L)
    # One row, with the reason and no point.
    expect_identical(nrow(lResult$rows), 1L)
    expect_identical(lResult$rows$status, "too_small")
    expect_identical(lResult$rows$reason, lResult$reason)
    expect_true(is.na(lResult$rows$x) && is.na(lResult$rows$fit) && is.na(lResult$rows$lower) && is.na(lResult$rows$p_value))

    # One group too small: its one row says so, and the rest are fitted.
    dfUneven <- dfFrame
    dfUneven$Site <- rep(c("Large", "Small"), times = c(197, 3))
    lUneven <- Analyze_Fit(dfUneven, strX, strY, strMethod = strMethod, strGroupCol = "Site", nPoints = 6)
    ExpectResultShape(lUneven)
    expect_identical(lUneven$status, "ok")
    expect_identical(lUneven$rows$group, c(rep(NA, 6), rep("Large", 6), "Small"))
    dfSmallRow <- lUneven$rows[lUneven$rows$group %in% "Small", ]
    expect_identical(dfSmallRow$status, "too_small")
    expect_identical(dfSmallRow$reason, "Not computed: 3 complete pairs. The minimum is 5.")
    expect_identical(dfSmallRow$counts, 3L)
    expect_true(is.na(dfSmallRow$x) && is.na(dfSmallRow$fit) && is.na(dfSmallRow$level))
    expect_identical(unique(lUneven$rows$status[!lUneven$rows$group %in% "Small"]), "ok")
    # No coefficients are reported for the group that was not fitted.
    expect_false("Small" %in% lUneven$estimates$group)
    if (strMethod == "linear") {
      expect_identical(lUneven$estimates$group, c(NA, NA, "Large", "Large"))
    }
  }

  expect_identical(Analyze_Fit(dfSmall, strX, strY, nMinGroup = 4)$status, "ok")
  expect_identical(formals(Analyze_Fit)$nMinGroup, quote(nMinGroupDefault))
})

test_that("degenerate data is answered, never thrown: one x value, one y value, a smooth R cannot fit (#12, #24)", {
  # Every x the same: there is no line to fit, for either method.
  dfFlat <- dfFrame
  dfFlat$FlatX <- 3
  for (strMethod in c("linear", "smooth")) {
    expect_silent(lFlatX <- Analyze_Fit(dfFlat, "FlatX", strY, strMethod = strMethod, strGroupCol = "ARM"))
    ExpectResultShape(lFlatX)
    expect_identical(lFlatX$status, "error")
    expect_identical(lFlatX$reason, "Not computed: every x value is 3, so there is no line to fit.")
    expect_identical(lFlatX$counts, 200L)
    expect_identical(lFlatX$rows$status, rep("error", 3))
    expect_true(all(is.na(lFlatX$rows$x)))
  }

  # Every y the same: there is no line to fit, as t.test() refuses data that
  # are essentially constant, for a line and for a smooth (#24).
  dfFlat$FlatY <- 2
  for (strMethod in c("linear", "smooth")) {
    expect_silent(lFlatY <- Analyze_Fit(dfFlat, strX, "FlatY", strMethod = strMethod, nPoints = 4))
    ExpectResultShape(lFlatY)
    expect_identical(lFlatY$status, "error")
    expect_identical(lFlatY$reason, "Not computed: every y value is 2, so there is nothing to fit.")
    expect_identical(lFlatY$warnings, list())
  }

  # A smooth on a handful of points: below seven complete pairs it is too
  # small, whatever the minimum group size (#24).
  dfTiny <- data.frame(x = c(1, 1, 2, 2, 3), y = c(1.2, 0.7, 2.9, 2.1, 3.3), g = c("a", "a", "a", "b", "b"))
  expect_silent(lTiny <- Analyze_Fit(dfTiny, "x", "y", strMethod = "smooth", nPoints = 3))
  ExpectResultShape(lTiny)
  expect_identical(lTiny$status, "too_small")
  expect_identical(lTiny$reason, "Not computed: 5 complete pairs, and a smooth needs at least 7.")
  # The same points in two groups, each below the minimum: reasons, no numbers.
  expect_silent(lTinyGroups <- Analyze_Fit(dfTiny, "x", "y", strMethod = "smooth", strGroupCol = "g", nPoints = 3))
  ExpectResultShape(lTinyGroups)
  expect_identical(lTinyGroups$rows$status[lTinyGroups$rows$group %in% c("a", "b")], c("too_small", "too_small"))
  # With the minimum group size lowered, each group is still below a smooth's own.
  expect_silent(lTinyFitted <- Analyze_Fit(dfTiny, "x", "y", strMethod = "smooth", strGroupCol = "g", nMinGroup = 2, nPoints = 3))
  ExpectResultShape(lTinyFitted)
  expect_true(all(lTinyFitted$rows$status == "too_small"))
  expect_true(all(grepl("a smooth needs at least 7", lTinyFitted$rows$reason[!is.na(lTinyFitted$rows$group)], fixed = TRUE)))
})

test_that("a fit request that cannot be met is an error status, never an R error (#12)", {
  lCases <- list(
    list(dfData = dfFrame, strXCol = "Nope", strYCol = strY),
    list(dfData = dfFrame, strXCol = strX, strYCol = "ARM"),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, strMethod = "identity"),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, strGroupCol = "Nope"),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, nMinGroup = "five"),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, nConfLevel = 1),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, nPoints = 1),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, nPoints = 2.5),
    list(dfData = dfFrame, strXCol = strX, strYCol = strY, nPoints = 5000),
    list(dfData = "not a table", strXCol = strX, strYCol = strY)
  )
  for (lArgs in lCases) {
    expect_silent(lResult <- do.call(Analyze_Fit, lArgs))
    ExpectResultShape(lResult)
    expect_identical(lResult$status, "error")
    expect_identical(nrow(lResult$rows), 0L)
  }
  expect_identical(
    Analyze_Fit(dfFrame, strX, "ARM")$reason,
    "Column 'ARM' (strYCol) is not numeric."
  )
  # The identity line needs no statistics, and is not a method here.
  expect_match(Analyze_Fit(dfFrame, strX, strY, strMethod = "identity")$reason, "strMethod must be one of: linear, smooth", fixed = TRUE)
  # The arguments are Analyze_Correlation's, with the number of points after them.
  expect_identical(
    names(formals(Analyze_Fit)),
    c(names(formals(Analyze_Correlation)), "nPoints")
  )
  expect_identical(formals(Analyze_Fit)$strMethod, "linear")
})
