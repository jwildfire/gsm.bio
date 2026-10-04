# A figure's title, subtitle and footnotes, the footnote written for it, and
# the words of its statistics line: bio.viz's rules for getting results out,
# written in R (bio.viz, src/shared/titles.js and src/r/formatStatistic.js;
# docs/output.md). A static figure from Visualize_*() says what its chart
# says, by the same rules: a placeholder is a name in braces replaced by text,
# a name the figure does not have is left as written, and one footnote is
# always last, saying when and by what the figure was drawn and what stands
# behind each statistic it printed.
#
# The one difference is who drew it: the chart's footnote names bio.viz and
# says a stored result was "stored with the page"; a figure is drawn here, by
# gsm.bio, from R's answers computed in the same session, and its footnote
# says so. Nothing here computes a statistic, and nothing is exported.

# The settings every chart has for its title, subtitle and footnotes.
lOutputTitleDefaults <- list(title = NULL, subtitle = NULL, footnotes = NULL)

strOutputPlaceholder <- "\\{([A-Za-z_][A-Za-z0-9_]*)\\}"

# The template with every placeholder whose name is in `lValues` replaced by
# the text of its value, once, left to right; every other left as written. A
# value that is NULL or NA is written as nothing, a number as it reads. Nothing
# in the template or in a value is evaluated, and a value is never read for
# placeholders of its own.
Output_FillText <- function(strTemplate, lValues = list()) {
  strTemplate <- as.character(strTemplate)
  lMatch <- gregexpr(strOutputPlaceholder, strTemplate, perl = TRUE)[[1]]
  if (lMatch[1] == -1L) {
    return(strTemplate)
  }
  nStarts <- as.integer(lMatch)
  nLengths <- attr(lMatch, "match.length")
  chrPieces <- character(0)
  iFrom <- 1L
  for (iMatch in seq_along(nStarts)) {
    strWritten <- substr(strTemplate, nStarts[iMatch], nStarts[iMatch] + nLengths[iMatch] - 1L)
    strName <- substr(strWritten, 2L, nchar(strWritten) - 1L)
    strText <- if (strName %in% names(lValues)) {
      xValue <- lValues[[strName]]
      if (is.null(xValue) || length(xValue) == 0L || is.na(xValue[[1]])) "" else Core_Text(xValue[[1]])
    } else {
      strWritten
    }
    chrPieces <- c(chrPieces, substr(strTemplate, iFrom, nStarts[iMatch] - 1L), strText)
    iFrom <- nStarts[iMatch] + nLengths[iMatch]
  }
  paste0(c(chrPieces, substr(strTemplate, iFrom, nchar(strTemplate))), collapse = "")
}

# The placeholders a template names, each once, in the order written.
Output_PlaceholdersIn <- function(strTemplate) {
  chrFound <- regmatches(strTemplate, gregexpr(strOutputPlaceholder, strTemplate, perl = TRUE))[[1]]
  unique(substr(chrFound, 2L, nchar(chrFound) - 1L))
}

# `title`, `subtitle` and `footnotes` checked as the chart checks them, with
# `footnotes` as text, the empty ones dropped.
Output_CheckTitles <- function(lConfig) {
  for (strKey in c("title", "subtitle")) {
    xValue <- lConfig[[strKey]]
    if (!is.null(xValue) && !(is.character(xValue) && length(xValue) == 1L && !is.na(xValue))) {
      Core_Stop("Setting '", strKey, "' must be text, which may hold placeholders such as {n}, or NULL for none")
    }
  }
  xFootnotes <- lConfig$footnotes
  if (!is.null(xFootnotes)) {
    lList <- if (is.list(xFootnotes)) xFootnotes else as.list(xFootnotes)
    if (!all(vapply(lList, function(xOne) is.character(xOne) && length(xOne) == 1L && !is.na(xOne), logical(1)))) {
      Core_Stop("Setting 'footnotes' must be text, or a list of texts, or NULL for none")
    }
    chrFootnotes <- unlist(lList, use.names = FALSE)
    lConfig["footnotes"] <- list(chrFootnotes[!Core_IsBlank(chrFootnotes)])
  }
  lConfig
}

# R's counts as the footnote writes them: `n = 200` for one; `Placebo n = 95,
# Treatment n = 91` for up to four groups; more as the least and the most with
# how many there are, `n = 179 to 186 across 12 biomarkers`. NULL with none.
Output_CountsText <- function(xCounts, strOf = "groups") {
  if (is.numeric(xCounts) && length(xCounts) == 1L && is.null(names(xCounts)) && is.finite(xCounts)) {
    return(paste0("n = ", Core_Text(xCounts)))
  }
  if (is.null(xCounts) || length(xCounts) == 0L || is.null(names(xCounts))) {
    return(NULL)
  }
  lCounts <- as.list(xCounts)
  bNamed <- vapply(lCounts, function(xCount) is.numeric(xCount) && length(xCount) == 1L && is.finite(xCount), logical(1))
  lCounts <- lCounts[bNamed]
  if (length(lCounts) == 0L) {
    return(NULL)
  }
  if (length(lCounts) <= 4L) {
    return(paste(paste0(names(lCounts), " n = ", vapply(lCounts, Core_Text, character(1))), collapse = ", "))
  }
  nAll <- unlist(lCounts)
  strRange <- if (min(nAll) == max(nAll)) Core_Text(min(nAll)) else paste(Core_Text(min(nAll)), "to", Core_Text(max(nAll)))
  paste0("n = ", strRange, " across ", length(lCounts), " ", strOf)
}

# Which R computed a figure's statistics: the session that drew it.
Output_ComputedBy <- function() {
  paste0("computed by R ", paste(R.version$major, R.version$minor, sep = "."), " with gsm.bio ", Output_Version())
}

Output_Version <- function() {
  unname(as.character(getNamespaceVersion("gsm.bio")))
}

# The date a figure was drawn, as the footnote writes it: ISO 8601, in UTC.
Output_DateDrawn <- function(tmWhen = Sys.time()) {
  format(tmWhen, "%Y-%m-%d", tz = "UTC")
}

# The footnote a figure writes last: when and by what it was drawn, and, for
# every statistic it printed, R's method and the counts R used, with the R and
# gsm.bio versions that computed them. `lAnswers` are R's answers, as the
# Analyze_*() functions returned them; `strOf` is what R's counts are of when
# there are more than four.
Output_AutomaticFootnote <- function(lAnswers, strOf = "groups", strDate = Output_DateDrawn(), strVersion = Output_Version()) {
  strDrawn <- paste0("Drawn on ", strDate, " by gsm.bio ", strVersion, ".")
  if (length(lAnswers) == 0L) {
    return(paste(strDrawn, "No statistic was asked of R."))
  }
  chrSaid <- vapply(lAnswers, function(lAnswer) {
    strMethod <- if (is.character(lAnswer$method) && length(lAnswer$method) == 1L && !Core_IsBlank(lAnswer$method)) {
      lAnswer$method
    } else {
      "no statistic"
    }
    strCounts <- Output_CountsText(lAnswer$counts, strOf)
    if (is.null(strCounts)) strMethod else paste0(strMethod, " (", strCounts, ")")
  }, character(1))
  paste0(strDrawn, " Statistics: ", paste(chrSaid, collapse = "; "), "; ", Output_ComputedBy(), ".")
}

# The title, subtitle and footnotes as they read for one figure: the settings'
# templates filled from `lValues`, with the footnote the figure writes last.
Output_Titles <- function(lConfig, lValues, lAnswers, strOf = "groups") {
  Filled <- function(strTemplate) if (is.null(strTemplate)) NULL else Output_FillText(strTemplate, lValues)
  list(
    title = Filled(lConfig$title),
    subtitle = Filled(lConfig$subtitle),
    footnotes = c(
      vapply(as.list(lConfig$footnotes), Filled, character(1)),
      Output_AutomaticFootnote(lAnswers, strOf, strDate = lValues$date)
    )
  )
}

# The filters in force, in words, as every chart writes them: `Sex is F; Arm is
# Placebo or Treatment`, by each filter's label, or `none`.
Output_FiltersText <- function(lFilters, lSpecs) {
  lInForce <- Chart_FiltersInForce(lFilters)
  if (length(lInForce) == 0L) {
    return("none")
  }
  chrSaid <- vapply(names(lInForce), function(strColumn) {
    lSpec <- Filter(function(lOne) identical(lOne$value_col, strColumn), lSpecs)
    strLabel <- if (length(lSpec) > 0L && !is.null(lSpec[[1]]$label)) lSpec[[1]]$label else strColumn
    paste(strLabel, "is", paste(Core_Text(lFilters[[strColumn]]), collapse = " or "))
  }, character(1))
  paste(chrSaid, collapse = "; ")
}

# What every figure's placeholders hold, beside its own.
Output_SharedPlaceholders <- function(lFilters, lSpecs) {
  list(date = Output_DateDrawn(), version = Output_Version(), filters = Output_FiltersText(lFilters, lSpecs))
}

# ---- Words for variables ---------------------------------------------------------

chrOutputValueLabels <- c(
  raw = "Result", baseline = "Baseline", change = "Change from baseline",
  fold_change = "Fold change from baseline", percent_change = "Percent change from baseline"
)

chrOutputValueWords <- c(
  change = "change from baseline", fold_change = "fold change from baseline",
  percent_change = "percent change from baseline"
)

# A number as the chart writes an estimate: four significant digits, as it
# reads (`String(Number(x.toPrecision(4)))`).
Output_Figure <- function(nValue) {
  Core_Text(signif(nValue, 4))
}

# A coefficient as the grid writes it in a cell: two decimals, with a minus
# sign, and no negative zero.
Output_Coefficient <- function(nEstimate) {
  chrFixed <- formatC(nEstimate, format = "f", digits = 2)
  chrFixed[chrFixed == "-0.00"] <- "0.00"
  chrFixed <- sub("-", "\u2212", chrFixed, fixed = TRUE)
  chrFixed[is.na(nEstimate)] <- NA_character_
  enc2utf8(chrFixed)
}

# A variable in words (bio.viz, src/core/variable.js, `label`): a column by its
# name; a biomarker at its visit, with what is drawn of its value; and a cut,
# where it is cut.
Output_Label <- function(lSpec) {
  strWords <- if (!is.null(lSpec$col)) {
    lSpec$col
  } else if (identical(lSpec$value, "baseline")) {
    paste(lSpec$measure, "at baseline")
  } else {
    strAt <- paste(lSpec$measure, "at", lSpec$visit)
    if (is.null(lSpec$value) || lSpec$value == "raw") strAt else paste0(strAt, ", ", chrOutputValueWords[[lSpec$value]])
  }
  if (is.null(lSpec$cut)) {
    return(strWords)
  }
  strCut <- if (is.character(lSpec$cut)) {
    paste("cut at the", lSpec$cut)
  } else {
    chrPoints <- Core_CutBound(unlist(lSpec$cut))
    paste("cut at", if (length(chrPoints) < 2L) chrPoints else paste(paste(chrPoints[-length(chrPoints)], collapse = ", "), "and", chrPoints[length(chrPoints)]))
  }
  paste0(strWords, ", ", strCut)
}

# A biomarker's unit, when its rows have exactly one.
Output_Unit <- function(dfResults, lConfig, strMeasure) {
  if (is.null(lConfig$unit_col) || !lConfig$unit_col %in% names(dfResults)) {
    return(NULL)
  }
  chrUnits <- Core_Levels(dfResults[[lConfig$unit_col]][Core_Text(dfResults[[lConfig$measure_col]]) == strMeasure])
  if (length(chrUnits) == 1L) chrUnits else NULL
}

# An axis as its title reads (bio.viz, `axisTitle`): a number by its label; a
# biomarker in words with its unit, or `(%)` for a percent change.
Output_AxisTitle <- function(dfResults, lConfig, lAxis, lNumbers = NULL) {
  if (!is.null(lAxis$col)) {
    lFound <- Filter(function(lSpec) identical(lSpec$value_col, lAxis$col), lNumbers)
    return(if (length(lFound) > 0L) lFound[[1]]$label else lAxis$col)
  }
  strWords <- Output_Label(lAxis)
  if (identical(lAxis$value, "fold_change")) {
    return(strWords)
  }
  if (identical(lAxis$value, "percent_change")) {
    return(paste0(strWords, " (%)"))
  }
  strUnit <- Output_Unit(dfResults, lConfig, lAxis$measure)
  if (is.null(strUnit)) strWords else paste0(strWords, " (", strUnit, ")")
}

# The vertical axis of a group comparison (bio.viz, `yTitle`): the biomarker
# at its one visit, or with what is drawn of it across several, with its unit.
Output_YTitle <- function(dfResults, lConfig, strMeasure, strValueType, chrVisits) {
  strWords <- if (strValueType == "baseline") {
    Output_Label(list(measure = strMeasure, value = "baseline"))
  } else if (length(chrVisits) == 1L) {
    Output_Label(list(measure = strMeasure, visit = chrVisits, value = strValueType))
  } else if (strValueType == "raw") {
    strMeasure
  } else {
    paste0(strMeasure, ", ", chrOutputValueWords[[strValueType]])
  }
  if (strValueType == "fold_change") {
    return(strWords)
  }
  if (strValueType == "percent_change") {
    return(paste0(strWords, " (%)"))
  }
  strUnit <- Output_Unit(dfResults, lConfig, strMeasure)
  if (is.null(strUnit)) strWords else paste0(strWords, " (", strUnit, ")")
}

# What a control that makes groups calls them: a column by the label `groups`
# gives it, or its name; a cut variable in words.
Output_GroupLabel <- function(xBy, lConfig) {
  if (is.null(xBy)) {
    return("")
  }
  if (Chart_IsCut(xBy)) {
    return(Output_Label(xBy))
  }
  lFound <- Filter(function(lSpec) identical(lSpec$value_col, xBy), lConfig$groups)
  if (length(lFound) > 0L) lFound[[1]]$label else xBy
}

# ---- The statistics line ---------------------------------------------------------
#
# What a chart prints under a figure for one of R's answers (bio.viz,
# src/r/formatStatistic.js): the method, the p-value and the counts, or R's
# reason for not computing it; and each estimate, to four significant digits.

Output_P <- function(nP) {
  strRounded <- formatC(nP, format = "f", digits = 3)
  if (nP < 1e-3 || strRounded == "0.000") {
    return("p < 0.001")
  }
  if (strRounded == "1.000") {
    return("p > 0.999")
  }
  paste("p =", strRounded)
}

chrOutputAdjustments <- c(
  holm = "Holm", hochberg = "Hochberg", hommel = "Hommel", bonferroni = "Bonferroni",
  BH = "Benjamini-Hochberg", fdr = "Benjamini-Hochberg", BY = "Benjamini-Yekutieli"
)

# Counts as the statistics line writes them: whole numbers only.
Output_LineCounts <- function(xCounts) {
  IsCount <- function(xOne) is.numeric(xOne) && length(xOne) == 1L && !is.na(xOne) && xOne >= 0 && xOne == round(xOne)
  if (IsCount(xCounts) && is.null(names(xCounts))) {
    return(paste("n =", Core_Text(xCounts)))
  }
  if (is.null(xCounts) || length(xCounts) == 0L || is.null(names(xCounts))) {
    return(NULL)
  }
  lCounts <- as.list(xCounts)
  if (!all(vapply(lCounts, IsCount, logical(1)))) {
    return(NULL)
  }
  paste(ifelse(names(lCounts) == "n", paste("n =", unlist(lCounts)), paste0(names(lCounts), " n = ", unlist(lCounts))), collapse = ", ")
}

# The sentence a chart prints for one of R's answers.
Output_StatisticText <- function(lValue) {
  Text <- function(xValue) if (is.character(xValue) && length(xValue) == 1L && !Core_IsBlank(xValue)) trimws(xValue) else NULL
  strMethod <- Text(lValue$method)
  strCounts <- Output_LineCounts(lValue$counts)
  strReason <- Text(lValue$reason)
  WithCounts <- function(strLead) {
    if (grepl("[.!?]$", strLead)) {
      return(if (is.null(strCounts)) strLead else paste0(strLead, " Counts: ", strCounts, "."))
    }
    if (is.null(strCounts)) paste0(strLead, ".") else paste0(strLead, " (", strCounts, ").")
  }
  if (identical(lValue$status, "error")) {
    return(WithCounts(paste0("R reported an error: ", if (is.null(strReason)) "no message" else strReason)))
  }
  if (!is.null(strReason)) {
    strLead <- if (grepl("^not computed\\b", strReason, ignore.case = TRUE, perl = TRUE)) {
      strReason
    } else if (!is.null(strMethod)) {
      paste0(strMethod, ": not computed, ", strReason)
    } else {
      paste0("Not computed, ", strReason)
    }
    return(WithCounts(strLead))
  }
  nP <- lValue$p_value
  if (!(is.numeric(nP) && length(nP) == 1L && !is.na(nP) && nP >= 0 && nP <= 1)) {
    return("p-value not shown: the result has no p-value between 0 and 1.")
  }
  if (is.null(strMethod)) {
    return("p-value not shown: the result does not name its method.")
  }
  if (is.null(strCounts)) {
    return("p-value not shown: the result does not give the counts it used.")
  }
  strAdjustment <- Text(lValue$adjustment)
  strAdjustment <- if (is.null(strAdjustment) || tolower(strAdjustment) == "none") {
    NULL
  } else if (strAdjustment %in% names(chrOutputAdjustments)) {
    chrOutputAdjustments[[strAdjustment]]
  } else {
    strAdjustment
  }
  strLabel <- if (is.null(strAdjustment)) "Exploratory, unadjusted." else paste0("Exploratory, adjusted (", strAdjustment, ").")
  paste0(strMethod, ": ", Output_P(nP), " (", strCounts, "). ", strLabel)
}

# One estimate as the line writes it: its name and group, the estimate, and
# its interval. A median that was not reached says so.
Output_EstimateText <- function(lRow) {
  Present <- function(xValue) !is.null(xValue) && length(xValue) == 1L && !is.na(xValue)
  strGroup <- if (Present(lRow$group) && !Core_IsBlank(lRow$group)) paste0(" (", lRow$group, ")") else ""
  strPercent <- if (Present(lRow$level)) Core_Text(signif(lRow$level * 100, 12)) else NULL
  if (identical(lRow$name, "Median")) {
    Said <- function(xValue) if (Present(xValue)) Output_Figure(xValue) else "not reached"
    strInterval <- if (!Present(lRow$lower) && !Present(lRow$upper)) "not reached" else paste(Said(lRow$lower), "to", Said(lRow$upper))
    return(paste0(lRow$name, strGroup, ": ", Said(lRow$estimate), ", ", strPercent, "% confidence interval ", strInterval, "."))
  }
  strLead <- paste0(lRow$name, strGroup, ": ", Output_Figure(lRow$estimate))
  if (!Present(lRow$lower) && !Present(lRow$upper) && !Present(lRow$level)) {
    return(paste0(strLead, "."))
  }
  paste0(strLead, ", ", strPercent, "% confidence interval ", Output_Figure(lRow$lower), " to ", Output_Figure(lRow$upper), ".")
}
