# gsm.bio statistics
#
# The one definition of every statistics function in gsm.bio. The package's
# exported functions are built from this file when the package is installed,
# and the same file is handed to R in the browser, or to a server, as it is.
# So it stands alone: it calls base R and the stats package and nothing else,
# it never attaches or loads a package, and it never evaluates text.
#
# Every Analyze_* function takes a data frame with one row per participant
# first, and after it only named arguments that JSON can carry: strings,
# numbers, booleans and vectors of those. An argument that takes several
# values accepts a vector or an unnamed list of single values. No argument is
# a formula, a function or an expression.
#
# Each statistic is the base R function the design names, called with R's own
# defaults. Nothing is reimplemented. The wrappers fix the inputs, drop and
# count what cannot be used, and put the answer in one shape (Stat_Result,
# below). They never raise an error and never emit a warning: both become part
# of the answer.

# The smallest group a statistic is computed for, when the caller does not say.
# A default, not an agreed or validated threshold.
nMinGroupDefault <- 5L

# The expected count below which chisq.test() itself warns that its
# approximation may be incorrect.
nSmallExpectedCount <- 5

# ---- The result shape -------------------------------------------------------
#
# Every function returns a plain named list with these members, always all of
# them and always in this order. Only base R is needed to turn it into JSON:
# there is no factor, no matrix and no classed object other than a plain data
# frame, a single value is an unnamed vector of length one, and anything that
# is a collection is a data frame or an unnamed list, never a bare vector.
#
#   status      "ok", "too_small" or "error"
#   reason      why there are no numbers; NA when status is "ok"
#   test        the method asked for, as the caller named it
#   method      the method's name as R reports it
#   estimates   data frame: name, group, estimate, lower, upper, level
#   statistic   data frame: name, value (the statistic, then its parameters)
#   p_value     one number, or NA
#   adjustment  how p_value was adjusted; "none" when it was not
#   counts      the participants used: one whole number, or a named list of
#               group to whole number
#   dropped     data frame: reason, n (the rows left out, and why)
#   warnings    unnamed list of the warnings R raised, as text
#   notes       unnamed list of remarks of our own, as text
#   rows        data frame of the function's many-row results; no rows if none

Stat_Estimates <- function(chrName = character(0), chrGroup = NA_character_, nEstimate = numeric(0),
                           nLower = NA_real_, nUpper = NA_real_, nLevel = NA_real_) {
  nRows <- length(chrName)
  data.frame(
    name = chrName,
    group = rep_len(as.character(chrGroup), nRows),
    estimate = as.numeric(nEstimate),
    lower = rep_len(as.numeric(nLower), nRows),
    upper = rep_len(as.numeric(nUpper), nRows),
    level = rep_len(as.numeric(nLevel), nRows),
    stringsAsFactors = FALSE
  )
}

Stat_Statistic <- function(chrName = character(0), nValue = numeric(0)) {
  data.frame(name = as.character(chrName), value = as.numeric(nValue), stringsAsFactors = FALSE)
}

# Only the reasons that dropped something are listed.
Stat_Dropped <- function(chrReason = character(0), nRows = integer(0)) {
  bAny <- nRows > 0
  data.frame(reason = as.character(chrReason[bAny]), n = as.integer(nRows[bAny]), stringsAsFactors = FALSE)
}

Stat_Result <- function(strTest = NA_character_, strStatus = "ok", strReason = NA_character_,
                        strMethod = NA_character_, dfEstimates = Stat_Estimates(),
                        dfStatistic = Stat_Statistic(), nPValue = NA_real_, strAdjustment = "none",
                        xCounts = NA_integer_, dfDropped = Stat_Dropped(), chrWarnings = character(0),
                        chrNotes = character(0), dfRows = data.frame()) {
  list(
    status = strStatus,
    reason = strReason,
    test = strTest,
    method = strMethod,
    estimates = dfEstimates,
    statistic = dfStatistic,
    p_value = as.numeric(nPValue),
    adjustment = strAdjustment,
    counts = xCounts,
    dropped = dfDropped,
    warnings = as.list(unique(as.character(chrWarnings))),
    notes = as.list(as.character(chrNotes)),
    rows = dfRows
  )
}

# One whole number per group, as a named list.
Stat_GroupCounts <- function(chrGroups, nCounts) {
  stats::setNames(as.list(as.integer(nCounts)), chrGroups)
}

# ---- Running R without letting an error or a warning escape -----------------

# Call fnCall and return its value with the warnings it raised and the message
# of the error that stopped it, if one did.
Stat_Capture <- function(fnCall) {
  chrWarnings <- character(0)
  strError <- NA_character_
  xValue <- withCallingHandlers(
    tryCatch(
      fnCall(),
      error = function(cndError) {
        strError <<- conditionMessage(cndError)
        NULL
      }
    ),
    warning = function(cndWarning) {
      chrWarnings <<- c(chrWarnings, conditionMessage(cndWarning))
      invokeRestart("muffleWarning")
    }
  )
  list(value = xValue, warnings = chrWarnings, error = strError)
}

# Run a whole function body. Anything it stops on, a bad argument or a column
# that is not there, comes back as an "error" result carrying the message.
Stat_Run <- function(strTest, fnBody) {
  lRun <- Stat_Capture(fnBody)
  if (is.na(lRun$error)) {
    return(lRun$value)
  }
  bLabel <- is.character(strTest) && length(strTest) == 1L
  Stat_Result(
    strTest = if (bLabel) strTest else NA_character_,
    strStatus = "error",
    strReason = lRun$error,
    chrWarnings = lRun$warnings
  )
}

# The parts of an htest that every wrapper reports.
Stat_FromTest <- function(lTest) {
  list(
    method = unname(lTest$method),
    statistic = Stat_Statistic(
      c(names(lTest$statistic), names(lTest$parameter)),
      c(unname(lTest$statistic), unname(lTest$parameter))
    ),
    p_value = unname(lTest$p.value)
  )
}

# ---- Reading the arguments and the columns ----------------------------------

Stat_CheckData <- function(dfData) {
  if (!is.data.frame(dfData)) {
    stop("dfData must be a data frame with one row per participant.", call. = FALSE)
  }
  invisible(dfData)
}

Stat_CheckString <- function(strValue, strArg) {
  if (!is.character(strValue) || length(strValue) != 1L || is.na(strValue) || !nzchar(strValue)) {
    stop(sprintf("%s must be one string.", strArg), call. = FALSE)
  }
  strValue
}

Stat_CheckChoice <- function(strValue, chrChoices, strArg) {
  Stat_CheckString(strValue, strArg)
  if (!strValue %in% chrChoices) {
    stop(
      sprintf("%s must be one of: %s. It was '%s'.", strArg, paste(chrChoices, collapse = ", "), strValue),
      call. = FALSE
    )
  }
  strValue
}

Stat_CheckFlag <- function(bValue, strArg) {
  if (!is.logical(bValue) || length(bValue) != 1L || is.na(bValue)) {
    stop(sprintf("%s must be TRUE or FALSE.", strArg), call. = FALSE)
  }
  bValue
}

Stat_CheckNumber <- function(nValue, strArg, nAbove = -Inf, nBelow = Inf) {
  if (!is.numeric(nValue) || length(nValue) != 1L || is.na(nValue) || nValue <= nAbove || nValue >= nBelow) {
    stop(sprintf("%s must be one number above %s and below %s.", strArg, nAbove, nBelow), call. = FALSE)
  }
  as.numeric(nValue)
}

# An argument that takes several values arrives as a vector or, from JSON, as
# an unnamed list of single values. NULL means the caller left it out.
Stat_Vector <- function(xValue, strArg) {
  if (is.null(xValue)) {
    return(NULL)
  }
  if (is.list(xValue)) {
    bSingle <- vapply(xValue, function(xItem) is.atomic(xItem) && length(xItem) == 1L, logical(1))
    if (!all(bSingle)) {
      stop(sprintf("%s must be a vector, or a list of single values.", strArg), call. = FALSE)
    }
    xValue <- unlist(xValue, use.names = FALSE)
  }
  chrValue <- as.character(xValue)
  if (length(chrValue) == 0L || anyNA(chrValue) || anyDuplicated(chrValue) > 0L) {
    stop(sprintf("%s must name one or more different values, none missing.", strArg), call. = FALSE)
  }
  chrValue
}

Stat_Column <- function(dfData, strCol, strArg) {
  Stat_CheckString(strCol, strArg)
  if (!strCol %in% names(dfData)) {
    stop(sprintf("Column '%s' (%s) is not in the data.", strCol, strArg), call. = FALSE)
  }
  xCol <- dfData[[strCol]]
  if (is.list(xCol) || !is.null(dim(xCol))) {
    stop(sprintf("Column '%s' (%s) does not hold one value per row.", strCol, strArg), call. = FALSE)
  }
  xCol
}

# A number per row. Whole numbers and decimals are both numbers; a value that
# is missing or not finite is NA; a column with nothing in it at all, which
# JSON delivers as logical, is all NA. Anything else is refused.
Stat_Numeric <- function(dfData, strCol, strArg) {
  xCol <- Stat_Column(dfData, strCol, strArg)
  if (is.logical(xCol) && all(is.na(xCol))) {
    return(rep(NA_real_, length(xCol)))
  }
  if (!is.numeric(xCol)) {
    stop(sprintf("Column '%s' (%s) is not numeric.", strCol, strArg), call. = FALSE)
  }
  nCol <- as.numeric(xCol)
  nCol[!is.finite(nCol)] <- NA_real_
  nCol
}

# A category per row, as text. A factor gives its labels; a missing value and
# an empty string are both NA.
Stat_Category <- function(dfData, strCol, strArg) {
  chrCol <- as.character(Stat_Column(dfData, strCol, strArg))
  chrCol[!is.na(chrCol) & !nzchar(chrCol)] <- NA_character_
  chrCol
}

# The groups to use, in order: the ones the caller named, or every one present,
# sorted the same way in every locale.
Stat_Levels <- function(chrCategory, xGroups, strArg) {
  chrGroups <- Stat_Vector(xGroups, strArg)
  if (is.null(chrGroups)) {
    chrGroups <- sort(unique(chrCategory[!is.na(chrCategory)]), method = "radix")
  }
  chrGroups
}

Stat_TooSmallReason <- function(chrGroups, nCounts, nMinGroup) {
  bSmall <- nCounts < nMinGroup
  sprintf(
    "Not computed: %s. The minimum group size is %s.",
    paste(sprintf("%s has %d", chrGroups[bSmall], as.integer(nCounts[bSmall])), collapse = "; "),
    format(nMinGroup)
  )
}

# ---- Group comparison -------------------------------------------------------

# Welch's t-test between two groups, for the difference in means and its
# interval: the first group's mean minus the second's.
Stat_Welch <- function(nFirst, nSecond, nConfLevel) {
  lRun <- Stat_Capture(function() stats::t.test(nFirst, nSecond, conf.level = nConfLevel))
  if (!is.na(lRun$error)) {
    return(lRun)
  }
  lRun$difference <- unname(lRun$value$estimate[1] - lRun$value$estimate[2])
  lRun$lower <- lRun$value$conf.int[1]
  lRun$upper <- lRun$value$conf.int[2]
  lRun
}

Analyze_GroupDifference <- function(dfData, strValueCol, strGroupCol, strMethod = "t", chrGroups = NULL,
                                    bPairwise = TRUE, strPAdjust = "holm", nConfLevel = 0.95,
                                    nMinGroup = nMinGroupDefault) {
  Stat_Run(strMethod, function() {
    Stat_CheckData(dfData)
    Stat_CheckChoice(strMethod, c("t", "wilcoxon", "anova", "kruskal"), "strMethod")
    Stat_CheckFlag(bPairwise, "bPairwise")
    Stat_CheckChoice(strPAdjust, stats::p.adjust.methods, "strPAdjust")
    Stat_CheckNumber(nConfLevel, "nConfLevel", 0, 1)
    Stat_CheckNumber(nMinGroup, "nMinGroup", 0)
    nValue <- Stat_Numeric(dfData, strValueCol, "strValueCol")
    chrGroup <- Stat_Category(dfData, strGroupCol, "strGroupCol")
    chrLevels <- Stat_Levels(chrGroup, chrGroups, "chrGroups")

    # Drop, and count, what cannot be used.
    bNoGroup <- is.na(chrGroup)
    bOtherGroup <- !bNoGroup & !chrGroup %in% chrLevels
    bNoValue <- !bNoGroup & !bOtherGroup & is.na(nValue)
    bUsed <- !bNoGroup & !bOtherGroup & !bNoValue
    dfDropped <- Stat_Dropped(
      c("Missing group", "Group not selected", "Missing value"),
      c(sum(bNoGroup), sum(bOtherGroup), sum(bNoValue))
    )
    lValues <- lapply(chrLevels, function(strLevel) nValue[bUsed & chrGroup == strLevel])
    nCounts <- vapply(lValues, length, integer(1))
    lCounts <- Stat_GroupCounts(chrLevels, nCounts)
    nGroups <- length(chrLevels)
    bTwoGroupTest <- strMethod %in% c("t", "wilcoxon")

    if (nGroups < 2L || (bTwoGroupTest && nGroups != 2L)) {
      return(Stat_Result(
        strTest = strMethod, strStatus = "error", xCounts = lCounts, dfDropped = dfDropped,
        strReason = if (bTwoGroupTest) {
          sprintf(
            "'%s' compares exactly two groups and %d were found. Name two in chrGroups, or use 'anova' or 'kruskal'.",
            strMethod, nGroups
          )
        } else {
          sprintf("'%s' compares two or more groups and %d was found.", strMethod, nGroups)
        }
      ))
    }
    if (any(nCounts < nMinGroup)) {
      return(Stat_Result(
        strTest = strMethod, strStatus = "too_small", xCounts = lCounts, dfDropped = dfDropped,
        strReason = Stat_TooSmallReason(chrLevels, nCounts, nMinGroup)
      ))
    }

    dfEstimates <- Stat_Estimates(rep("Mean", nGroups), chrLevels, vapply(lValues, mean, numeric(1)))
    chrWarnings <- character(0)
    chrNotes <- character(0)

    # With two groups, the difference in means and its interval come from
    # t.test(), whichever test was asked for.
    lWelch <- NULL
    if (nGroups == 2L) {
      lWelch <- Stat_Welch(lValues[[1]], lValues[[2]], nConfLevel)
      if (!is.na(lWelch$error)) {
        return(Stat_Result(
          strTest = strMethod, strStatus = "error", strReason = lWelch$error, xCounts = lCounts,
          dfDropped = dfDropped, chrWarnings = lWelch$warnings
        ))
      }
      chrWarnings <- c(chrWarnings, lWelch$warnings)
      dfEstimates <- rbind(dfEstimates, Stat_Estimates(
        "Difference in means", paste(chrLevels[1], "-", chrLevels[2]),
        lWelch$difference, lWelch$lower, lWelch$upper, nConfLevel
      ))
      if (strMethod != "t") {
        chrNotes <- c(chrNotes, "The difference in means and its interval are from t.test() (Welch), whatever the test.")
      }
    }

    # The rows used, in the order they came, for the tests that take them all.
    dfModel <- data.frame(Value = nValue[bUsed], Group = factor(chrGroup[bUsed], levels = chrLevels))
    lTest <- if (strMethod == "t") {
      lWelch
    } else if (strMethod == "wilcoxon") {
      Stat_Capture(function() stats::wilcox.test(lValues[[1]], lValues[[2]]))
    } else if (strMethod == "anova") {
      Stat_Capture(function() summary(stats::aov(Value ~ Group, data = dfModel))[[1]])
    } else {
      Stat_Capture(function() stats::kruskal.test(dfModel$Value, dfModel$Group))
    }
    if (!is.na(lTest$error)) {
      return(Stat_Result(
        strTest = strMethod, strStatus = "error", strReason = lTest$error, xCounts = lCounts,
        dfDropped = dfDropped, chrWarnings = c(chrWarnings, lTest$warnings)
      ))
    }
    if (strMethod != "t") {
      chrWarnings <- c(chrWarnings, lTest$warnings)
    }
    lParts <- if (strMethod == "anova") {
      # aov() has no name for itself; this one is ours.
      list(
        method = "One-way analysis of variance",
        statistic = Stat_Statistic(c("F", "num df", "denom df"), c(lTest$value[1, "F value"], lTest$value[, "Df"])),
        p_value = lTest$value[1, "Pr(>F)"]
      )
    } else {
      Stat_FromTest(lTest$value)
    }

    # With more than two groups, each pair is compared with the two-group test
    # of the same family and the p-values are adjusted across the pairs.
    dfRows <- data.frame()
    if (nGroups > 2L && bPairwise) {
      nPairs <- nGroups * (nGroups - 1L) / 2L
      iFirst <- rep(seq_len(nGroups - 1L), times = rev(seq_len(nGroups - 1L)))
      iSecond <- unlist(lapply(seq_len(nGroups - 1L), function(iGroup) seq(iGroup + 1L, nGroups)))
      dfRows <- data.frame(
        group_1 = chrLevels[iFirst], group_2 = chrLevels[iSecond],
        n_1 = nCounts[iFirst], n_2 = nCounts[iSecond], counts = nCounts[iFirst] + nCounts[iSecond],
        estimate = NA_real_, lower = NA_real_, upper = NA_real_, level = nConfLevel,
        method = NA_character_, statistic = NA_real_, p_unadjusted = NA_real_, p_value = NA_real_,
        adjustment = strPAdjust, status = "ok", reason = NA_character_, warning = NA_character_,
        stringsAsFactors = FALSE
      )
      for (iPair in seq_len(nPairs)) {
        nFirst <- lValues[[iFirst[iPair]]]
        nSecond <- lValues[[iSecond[iPair]]]
        lPairWelch <- Stat_Welch(nFirst, nSecond, nConfLevel)
        lPairTest <- if (strMethod == "anova") {
          lPairWelch
        } else {
          Stat_Capture(function() stats::wilcox.test(nFirst, nSecond))
        }
        chrPairWarnings <- unique(c(lPairWelch$warnings, lPairTest$warnings))
        chrPairErrors <- unique(c(lPairWelch$error, lPairTest$error))
        chrPairErrors <- chrPairErrors[!is.na(chrPairErrors)]
        if (length(chrPairWarnings) > 0L) {
          dfRows$warning[iPair] <- paste(chrPairWarnings, collapse = "; ")
          chrWarnings <- c(chrWarnings, chrPairWarnings)
        }
        if (length(chrPairErrors) > 0L) {
          dfRows$status[iPair] <- "error"
          dfRows$reason[iPair] <- paste(chrPairErrors, collapse = "; ")
        } else {
          lPairParts <- Stat_FromTest(lPairTest$value)
          dfRows$estimate[iPair] <- lPairWelch$difference
          dfRows$lower[iPair] <- lPairWelch$lower
          dfRows$upper[iPair] <- lPairWelch$upper
          dfRows$method[iPair] <- lPairParts$method
          dfRows$statistic[iPair] <- lPairParts$statistic$value[1]
          dfRows$p_unadjusted[iPair] <- lPairParts$p_value
        }
      }
      dfRows$p_value <- stats::p.adjust(dfRows$p_unadjusted, method = strPAdjust)
      chrNotes <- c(chrNotes, sprintf(
        "Pairwise: each pair is compared with %s; p_value is adjusted across the pairs by p.adjust(method = '%s'); the intervals are not adjusted.",
        if (strMethod == "anova") "t.test() (Welch)" else "wilcox.test()", strPAdjust
      ))
    }

    Stat_Result(
      strTest = strMethod, strMethod = lParts$method, dfEstimates = dfEstimates,
      dfStatistic = lParts$statistic, nPValue = lParts$p_value, xCounts = lCounts,
      dfDropped = dfDropped, chrWarnings = chrWarnings, chrNotes = chrNotes, dfRows = dfRows
    )
  })
}

# ---- Correlation ------------------------------------------------------------

# cor.test() on the complete pairs of two numeric vectors. Returns plain
# pieces, for one overall answer, one group's row or one cell of a matrix.
Stat_CorrelationPair <- function(nX, nY, strMethod, nConfLevel, nMinGroup) {
  bPair <- !is.na(nX) & !is.na(nY)
  lPair <- list(
    status = "ok", reason = NA_character_, counts = sum(bPair), method = NA_character_,
    name = NA_character_, estimate = NA_real_, lower = NA_real_, upper = NA_real_, level = NA_real_,
    statistic = Stat_Statistic(), p_value = NA_real_, warnings = character(0)
  )
  if (lPair$counts < nMinGroup) {
    lPair$status <- "too_small"
    lPair$reason <- sprintf(
      "Not computed: %d complete pairs. The minimum is %s.", lPair$counts, format(nMinGroup)
    )
    return(lPair)
  }
  lRun <- Stat_Capture(function() {
    stats::cor.test(nX[bPair], nY[bPair], method = strMethod, conf.level = nConfLevel)
  })
  lPair$warnings <- lRun$warnings
  if (!is.na(lRun$error)) {
    lPair$status <- "error"
    lPair$reason <- lRun$error
    return(lPair)
  }
  lParts <- Stat_FromTest(lRun$value)
  lPair$method <- lParts$method
  lPair$statistic <- lParts$statistic
  lPair$p_value <- lParts$p_value
  lPair$name <- names(lRun$value$estimate)
  lPair$estimate <- unname(lRun$value$estimate)
  # cor.test() gives an interval for Pearson only, and only from four pairs.
  if (!is.null(lRun$value$conf.int)) {
    lPair$lower <- lRun$value$conf.int[1]
    lPair$upper <- lRun$value$conf.int[2]
    lPair$level <- nConfLevel
  }
  lPair
}

Stat_NoIntervalNote <- function(strMethod) {
  if (strMethod == "spearman") {
    "cor.test() gives no confidence interval for Spearman's rho, so none is reported."
  } else {
    character(0)
  }
}

Analyze_Correlation <- function(dfData, strXCol, strYCol, strMethod = "pearson", strGroupCol = NULL,
                                chrGroups = NULL, nConfLevel = 0.95, nMinGroup = nMinGroupDefault) {
  Stat_Run(strMethod, function() {
    Stat_CheckData(dfData)
    Stat_CheckChoice(strMethod, c("pearson", "spearman"), "strMethod")
    Stat_CheckNumber(nConfLevel, "nConfLevel", 0, 1)
    Stat_CheckNumber(nMinGroup, "nMinGroup", 0)
    nX <- Stat_Numeric(dfData, strXCol, "strXCol")
    nY <- Stat_Numeric(dfData, strYCol, "strYCol")
    bPair <- !is.na(nX) & !is.na(nY)
    chrReason <- "Incomplete pair"
    nDropped <- sum(!bPair)

    # Per group, when a group column is named. The overall answer uses every
    # complete pair, with or without a group.
    dfRows <- data.frame()
    chrWarnings <- character(0)
    if (!is.null(strGroupCol)) {
      chrGroup <- Stat_Category(dfData, strGroupCol, "strGroupCol")
      chrLevels <- Stat_Levels(chrGroup, chrGroups, "chrGroups")
      chrReason <- c(chrReason, "Missing group (left out of the per-group rows)", "Group not selected (left out of the per-group rows)")
      nDropped <- c(nDropped, sum(bPair & is.na(chrGroup)), sum(bPair & !is.na(chrGroup) & !chrGroup %in% chrLevels))
      nGroups <- length(chrLevels)
      dfRows <- data.frame(
        group = chrLevels, counts = NA_integer_, estimate = NA_real_, lower = NA_real_, upper = NA_real_,
        level = NA_real_, method = NA_character_, statistic = NA_real_, p_value = NA_real_,
        adjustment = "none", status = "ok", reason = NA_character_, warning = NA_character_,
        stringsAsFactors = FALSE
      )
      for (iGroup in seq_len(nGroups)) {
        bGroup <- !is.na(chrGroup) & chrGroup == chrLevels[iGroup]
        lGroup <- Stat_CorrelationPair(nX[bGroup], nY[bGroup], strMethod, nConfLevel, nMinGroup)
        dfRows$counts[iGroup] <- lGroup$counts
        dfRows$estimate[iGroup] <- lGroup$estimate
        dfRows$lower[iGroup] <- lGroup$lower
        dfRows$upper[iGroup] <- lGroup$upper
        dfRows$level[iGroup] <- lGroup$level
        dfRows$method[iGroup] <- lGroup$method
        dfRows$statistic[iGroup] <- if (nrow(lGroup$statistic) > 0L) lGroup$statistic$value[1] else NA_real_
        dfRows$p_value[iGroup] <- lGroup$p_value
        dfRows$status[iGroup] <- lGroup$status
        dfRows$reason[iGroup] <- lGroup$reason
        if (length(lGroup$warnings) > 0L) {
          dfRows$warning[iGroup] <- paste(unique(lGroup$warnings), collapse = "; ")
          chrWarnings <- c(chrWarnings, lGroup$warnings)
        }
      }
    }
    dfDropped <- Stat_Dropped(chrReason, nDropped)

    lAll <- Stat_CorrelationPair(nX, nY, strMethod, nConfLevel, nMinGroup)
    if (lAll$status != "ok") {
      return(Stat_Result(
        strTest = strMethod, strStatus = lAll$status, strReason = lAll$reason, xCounts = lAll$counts,
        dfDropped = dfDropped, chrWarnings = c(lAll$warnings, chrWarnings), dfRows = dfRows
      ))
    }
    Stat_Result(
      strTest = strMethod, strMethod = lAll$method,
      dfEstimates = Stat_Estimates(lAll$name, NA_character_, lAll$estimate, lAll$lower, lAll$upper, lAll$level),
      dfStatistic = lAll$statistic, nPValue = lAll$p_value, xCounts = lAll$counts, dfDropped = dfDropped,
      chrWarnings = c(lAll$warnings, chrWarnings), chrNotes = Stat_NoIntervalNote(strMethod), dfRows = dfRows
    )
  })
}

Analyze_CorrelationMatrix <- function(dfData, chrCols, strMethod = "pearson", nConfLevel = 0.95,
                                      nMinPairs = nMinGroupDefault) {
  Stat_Run(strMethod, function() {
    Stat_CheckData(dfData)
    Stat_CheckChoice(strMethod, c("pearson", "spearman"), "strMethod")
    Stat_CheckNumber(nConfLevel, "nConfLevel", 0, 1)
    Stat_CheckNumber(nMinPairs, "nMinPairs", 0)
    chrCols <- Stat_Vector(chrCols, "chrCols")
    nCols <- length(chrCols)
    if (nCols < 2L) {
      stop("chrCols must name two or more columns.", call. = FALSE)
    }
    lValues <- lapply(chrCols, function(strCol) Stat_Numeric(dfData, strCol, "chrCols"))
    lCounts <- Stat_GroupCounts(chrCols, vapply(lValues, function(nCol) sum(!is.na(nCol)), integer(1)))

    # One row per pair of columns, each pair once, on that pair's complete rows.
    nPairs <- nCols * (nCols - 1L) / 2L
    iFirst <- rep(seq_len(nCols - 1L), times = rev(seq_len(nCols - 1L)))
    iSecond <- unlist(lapply(seq_len(nCols - 1L), function(iCol) seq(iCol + 1L, nCols)))
    dfRows <- data.frame(
      x = chrCols[iFirst], y = chrCols[iSecond], counts = NA_integer_, estimate = NA_real_,
      lower = NA_real_, upper = NA_real_, level = NA_real_, status = "ok", reason = NA_character_,
      warning = NA_character_, stringsAsFactors = FALSE
    )
    chrWarnings <- character(0)
    strMethodName <- NA_character_
    for (iPair in seq_len(nPairs)) {
      lPair <- Stat_CorrelationPair(
        lValues[[iFirst[iPair]]], lValues[[iSecond[iPair]]], strMethod, nConfLevel, nMinPairs
      )
      dfRows$counts[iPair] <- lPair$counts
      dfRows$estimate[iPair] <- lPair$estimate
      dfRows$lower[iPair] <- lPair$lower
      dfRows$upper[iPair] <- lPair$upper
      dfRows$level[iPair] <- lPair$level
      dfRows$status[iPair] <- lPair$status
      dfRows$reason[iPair] <- lPair$reason
      if (length(lPair$warnings) > 0L) {
        dfRows$warning[iPair] <- paste(unique(lPair$warnings), collapse = "; ")
        chrWarnings <- c(chrWarnings, lPair$warnings)
      }
      if (!is.na(lPair$method)) {
        strMethodName <- lPair$method
      }
    }

    bNone <- all(dfRows$status == "too_small")
    Stat_Result(
      strTest = strMethod,
      strStatus = if (bNone) "too_small" else "ok",
      strReason = if (bNone) {
        sprintf("Not computed: no pair of columns has %s complete pairs.", format(nMinPairs))
      } else {
        NA_character_
      },
      strMethod = strMethodName, xCounts = lCounts,
      chrWarnings = chrWarnings,
      chrNotes = c(
        "No p-values: a matrix reports each coefficient, its interval and its pair count. Use Analyze_Correlation() to test one pair.",
        Stat_NoIntervalNote(strMethod)
      ),
      dfRows = dfRows
    )
  })
}

# ---- Contingency ------------------------------------------------------------

Analyze_Contingency <- function(dfData, strRowCol, strColCol, strMethod = "chisq", chrRowGroups = NULL,
                                chrColGroups = NULL, nConfLevel = 0.95, nMinGroup = nMinGroupDefault) {
  Stat_Run(strMethod, function() {
    Stat_CheckData(dfData)
    Stat_CheckChoice(strMethod, c("chisq", "fisher"), "strMethod")
    Stat_CheckNumber(nConfLevel, "nConfLevel", 0, 1)
    Stat_CheckNumber(nMinGroup, "nMinGroup", 0)
    chrRow <- Stat_Category(dfData, strRowCol, "strRowCol")
    chrCol <- Stat_Category(dfData, strColCol, "strColCol")
    chrRowLevels <- Stat_Levels(chrRow, chrRowGroups, "chrRowGroups")
    chrColLevels <- Stat_Levels(chrCol, chrColGroups, "chrColGroups")

    bMissing <- is.na(chrRow) | is.na(chrCol)
    bOther <- !bMissing & (!chrRow %in% chrRowLevels | !chrCol %in% chrColLevels)
    bUsed <- !bMissing & !bOther
    dfDropped <- Stat_Dropped(c("Missing category", "Category not selected"), c(sum(bMissing), sum(bOther)))

    # The two-way table of counts, as a plain matrix that never leaves here.
    mTable <- unclass(table(
      factor(chrRow[bUsed], levels = chrRowLevels),
      factor(chrCol[bUsed], levels = chrColLevels)
    ))
    dimnames(mTable) <- list(chrRowLevels, chrColLevels)
    nRowTotals <- rowSums(mTable)
    nColTotals <- colSums(mTable)
    nUsed <- as.integer(sum(mTable))
    dfRows <- data.frame(
      row = rep(chrRowLevels, times = length(chrColLevels)),
      col = rep(chrColLevels, each = length(chrRowLevels)),
      n = as.integer(mTable), expected = NA_real_, small_expected = NA,
      stringsAsFactors = FALSE
    )

    if (length(chrRowLevels) < 2L || length(chrColLevels) < 2L) {
      return(Stat_Result(
        strTest = strMethod, strStatus = "error", xCounts = nUsed, dfDropped = dfDropped, dfRows = dfRows,
        strReason = sprintf(
          "A two-way table needs two or more categories each way; '%s' has %d and '%s' has %d.",
          strRowCol, length(chrRowLevels), strColCol, length(chrColLevels)
        )
      ))
    }
    chrMargin <- c(paste0(strRowCol, " = ", chrRowLevels), paste0(strColCol, " = ", chrColLevels))
    nMargin <- c(nRowTotals, nColTotals)
    if (any(nMargin < nMinGroup)) {
      return(Stat_Result(
        strTest = strMethod, strStatus = "too_small", xCounts = nUsed, dfDropped = dfDropped, dfRows = dfRows,
        strReason = Stat_TooSmallReason(chrMargin, nMargin, nMinGroup)
      ))
    }

    lRun <- if (strMethod == "chisq") {
      Stat_Capture(function() stats::chisq.test(mTable))
    } else {
      Stat_Capture(function() stats::fisher.test(mTable, conf.level = nConfLevel))
    }
    if (!is.na(lRun$error)) {
      return(Stat_Result(
        strTest = strMethod, strStatus = "error", strReason = lRun$error, xCounts = nUsed,
        dfDropped = dfDropped, chrWarnings = lRun$warnings, dfRows = dfRows
      ))
    }
    lParts <- Stat_FromTest(lRun$value)
    dfEstimates <- Stat_Estimates()
    chrNotes <- character(0)
    if (strMethod == "chisq") {
      # The flag is read from the expected counts chisq.test() itself returns.
      dfRows$expected <- as.numeric(lRun$value$expected)
      dfRows$small_expected <- dfRows$expected < nSmallExpectedCount
      if (any(dfRows$small_expected)) {
        chrNotes <- sprintf(
          "%d of %d expected counts are below %s, so the chi-squared approximation may be poor. Fisher's exact test does not rely on it.",
          sum(dfRows$small_expected), nrow(dfRows), format(nSmallExpectedCount)
        )
      }
    } else if (!is.null(lRun$value$estimate)) {
      # fisher.test() estimates an odds ratio for a two-by-two table only.
      dfEstimates <- Stat_Estimates(
        names(lRun$value$estimate), NA_character_, unname(lRun$value$estimate),
        lRun$value$conf.int[1], lRun$value$conf.int[2], nConfLevel
      )
    }

    Stat_Result(
      strTest = strMethod, strMethod = lParts$method, dfEstimates = dfEstimates,
      dfStatistic = lParts$statistic, nPValue = lParts$p_value, xCounts = nUsed, dfDropped = dfDropped,
      chrWarnings = lRun$warnings, chrNotes = chrNotes, dfRows = dfRows
    )
  })
}
