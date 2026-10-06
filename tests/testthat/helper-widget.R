# What the widget tests share. Nothing here is exported.

# A JSON file, as nested lists: an array is an unnamed list whatever its
# length, an object a named list, and null is NULL.
# A JSON file, read as the UTF-8 it is whatever the session's locale.
lReadJson <- function(...) {
  jsonlite::fromJSON(paste(readLines(file.path(...), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), simplifyVector = FALSE)
}

# The copied bio.viz script-tag bundle, as its record names it: its folder
# carries bio.viz's version, which a copy of a new release changes.
strBioVizBundleFile <- function() {
  lRecord <- lReadJson(system.file("htmlwidgets", "lib", "SOURCE.json", package = "gsm.bio"))
  strFile <- Filter(function(lFile) lFile$library == "bio.viz", lRecord$files)[[1]]$file
  system.file("htmlwidgets", "lib", strFile, package = "gsm.bio")
}

strSha256 <- function(strFile) {
  digest::digest(file = strFile, algo = "sha256")
}

# A list of records, as a JSON file holds a table, as a data frame.
dfFromRecords <- function(lRecords) {
  chrColumns <- unique(unlist(lapply(lRecords, names)))
  dfTable <- data.frame(row.names = seq_along(lRecords))
  for (strColumn in chrColumns) {
    dfTable[[strColumn]] <- unlist(lapply(lRecords, function(lRecord) {
      if (is.null(lRecord[[strColumn]])) NA else lRecord[[strColumn]]
    }))
  }
  rownames(dfTable) <- NULL
  dfTable
}

# The synthetic study as its CSV files hold it, every value text: what a page
# that reads the files has, and what bio.viz's fixtures were written from.
lStudyAsText <- function() {
  ReadText <- function(strFile) {
    utils::read.csv(
      system.file("extdata", strFile, package = "gsm.bio"),
      colClasses = "character", na.strings = character(0), check.names = FALSE
    )
  }
  list(results = ReadText("synthetic_results.csv"), participants = ReadText("synthetic_participants.csv"))
}

# The tables and the settings of bio.viz's demo page, which its fixtures were
# written from: the study, with arm and sex side by side as one more category.
lDemo <- function() {
  dfParticipants <- Synthetic_Participants
  dfParticipants$ARM_SEX <- paste(dfParticipants$ARM, dfParticipants$SEX)
  lLabelled <- list(
    list(value_col = "ARM", label = "Arm"),
    list(value_col = "SEX", label = "Sex"),
    list(value_col = "RESPONSE", label = "Response")
  )
  list(
    results = Synthetic_Results,
    participants = dfParticipants,
    settings = list(
      start_value = "IL-6",
      visits = "Week 4",
      value_type = "change",
      baseline_visits = "Baseline",
      group_by = "ARM",
      groups = c(lLabelled, list(list(value_col = "ARM_SEX", label = "Arm and sex"))),
      filters = lLabelled
    )
  )
}

# The settings most widget tests open on: IL-6 open, change from Baseline, by
# arm, with no visit named, so every visit is chosen.
lWidgetSettings <- function() {
  list(start_value = "IL-6", value_type = "change", baseline_visits = "Baseline", group_by = "ARM")
}

# htmlwidgets needs pandoc to save a page as one self-contained file.
bPandoc <- function() {
  rmarkdown::pandoc_available()
}

# Saves a widget and returns the page's text. Without pandoc the page is saved
# beside its scripts instead of holding them; the payload is written the same
# way in both.
strSavedPage <- function(lWidget, bSelfContained = bPandoc()) {
  strDir <- tempfile("Widget_GroupComparison")
  dir.create(strDir)
  strFile <- file.path(strDir, "group-comparison.html")
  # The page is UTF-8, so it is read back as UTF-8 whatever the session's
  # locale; read in the locale's own encoding, a cut group's sign \u2264 would
  # not read back as itself.
  htmlwidgets::saveWidget(lWidget, file = strFile, selfcontained = bSelfContained)
  paste(readLines(strFile, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# The payload a saved page carries for its widget, read back out of the page.
lPagePayload <- function(strPage) {
  strPattern <- "<script type=\"application/json\" data-for=\"[^\"]+\">(.*?)</script>"
  chrFound <- regmatches(strPage, regexpr(strPattern, strPage, perl = TRUE))
  expect_identical(length(chrFound), 1L, label = "the page holds one widget payload")
  jsonlite::fromJSON(sub(strPattern, "\\1", chrFound, perl = TRUE), simplifyVector = FALSE)$x
}

# Where a value read out of a page differs from the R value it was written
# from, member by member, by the rules bio.viz's connection reads a result by:
# a data frame is an array of row objects, a named list an object, an unnamed
# list an array, a single value a single value, and NA is null. No difference
# is `character(0)`.
chrPageDifferences <- function(xPage, xValue, strLabel) {
  if (is.data.frame(xValue)) {
    if (!is.list(xPage) || !is.null(names(xPage)) || length(xPage) != nrow(xValue)) {
      return(paste(strLabel, "is not an array of", nrow(xValue), "rows"))
    }
    return(unlist(lapply(seq_len(nrow(xValue)), function(iRow) {
      if (!identical(names(xPage[[iRow]]), names(xValue))) {
        return(paste(strLabel, "row", iRow, "does not have the columns", paste(names(xValue), collapse = ", ")))
      }
      unlist(lapply(names(xValue), function(strColumn) {
        chrPageDifferences(xPage[[iRow]][[strColumn]], xValue[[strColumn]][[iRow]], paste(strLabel, "row", iRow, strColumn))
      }))
    })))
  }
  if (is.list(xValue)) {
    if (!is.list(xPage) || length(xPage) != length(xValue) || !identical(names(xPage), names(xValue))) {
      return(paste(strLabel, "is not a collection of the same members"))
    }
    return(unlist(lapply(seq_along(xValue), function(iMember) {
      chrPageDifferences(xPage[[iMember]], xValue[[iMember]], paste(strLabel, if (is.null(names(xValue))) iMember else names(xValue)[iMember]))
    })))
  }
  if (length(xValue) != 1L) {
    return(paste(strLabel, "is not a single value in R"))
  }
  bSame <- if (is.numeric(xValue) && (is.nan(xValue) || is.infinite(xValue))) {
    # R's non-finite numbers, as bio.viz's connection reads them back.
    identical(xPage, if (is.nan(xValue)) "NaN" else if (xValue > 0) "Inf" else "-Inf")
  } else if (is.na(xValue)) {
    is.null(xPage)
  } else if (is.numeric(xValue)) {
    is.numeric(xPage) && length(xPage) == 1L && isTRUE(all.equal(as.numeric(xPage), as.numeric(xValue), tolerance = 1e-12))
  } else {
    identical(xPage, xValue)
  }
  if (bSame) character(0) else paste(strLabel, "differs")
}

ExpectInPage <- function(xPage, xValue, strLabel) {
  expect_identical(as.character(chrPageDifferences(xPage, xValue, strLabel)), character(0), label = paste("differences in", strLabel))
}

# The scripts a widget's page runs of the package's own: its binding, and the
# script every binding is made with.
strWidgetScripts <- function(strWidget) {
  chrFiles <- c(
    system.file("htmlwidgets", paste0(strWidget, ".js"), package = "gsm.bio"),
    system.file("htmlwidgets", "shared", "gsm.bio.widget.js", package = "gsm.bio")
  )
  expect_true(all(nzchar(chrFiles)), label = paste("the scripts of", strWidget, "exist"))
  paste(unlist(lapply(chrFiles, readLines, warn = FALSE)), collapse = "\n")
}

# The value of every setting a chart of the bundle defaults, read from the
# bundle's own text: one `name: value` per line of its DEFAULT_SETTINGS. The
# chart is the one that has the setting named.
lBundleDefaults <- function(strSetting) {
  chrLines <- readLines(strBioVizBundleFile(), warn = FALSE)
  # A block of plain values: each setting's value as the JSON it is written in.
  Block <- function(iStart) {
    iEnd <- iStart + match("  });", chrLines[-seq_len(iStart)])
    chrBlock <- grep("^\\s*//", chrLines[(iStart + 1L):(iEnd - 1L)], value = TRUE, invert = TRUE)
    stats::setNames(sub(",$", "", sub("^\\s*[a-z_]+: ", "", chrBlock)), sub("^\\s*([a-z_]+): .*$", "\\1", chrBlock))
  }
  # The outcome settings two charts share are written once, in OUTCOME_DEFAULTS
  # (bio.viz, src/shared/outcomes.js), and each chart's defaults name them
  # there: OUTCOME_DEFAULTS.time_col.
  iOutcomes <- grep("^  var OUTCOME_DEFAULTS = Object\\.freeze\\(\\{$", chrLines)
  chrOutcomes <- if (length(iOutcomes) == 1L) Block(iOutcomes) else character(0)
  # Settings every chart has (TITLE_DEFAULTS, DOWNLOAD_DEFAULTS in bio.viz's
  # src/shared/) are written once and spread into each chart's defaults:
  # ...TITLE_DEFAULTS. Most are written on one line; the unscheduled-visit
  # rule's (UNSCHEDULED_DEFAULTS, bio.viz's src/core/unscheduled.js) are a
  # block, a setting to a line.
  Spread <- function(strName) {
    iBlock <- grep(paste0("^  var ", strName, " = Object\\.freeze\\(\\{$"), chrLines)
    if (length(iBlock) == 1L) {
      return(Block(iBlock))
    }
    strLine <- grep(paste0("^  var ", strName, " = Object\\.freeze\\(\\{.*\\}\\);$"), chrLines, value = TRUE)
    if (length(strLine) != 1L) stop("the bundle spreads ", strName, ", which it does not define once")
    chrPairs <- strsplit(sub("^.*\\{ *(.*?) *\\}\\);$", "\\1", strLine, perl = TRUE), ", *")[[1]]
    stats::setNames(sub("^[a-z_]+: ", "", chrPairs), sub(":.*$", "", chrPairs))
  }
  for (iStart in grep("^  var DEFAULT_SETTINGS[0-9]* = Object\\.freeze\\(\\{$", chrLines)) {
    chrValues <- Block(iStart)
    bSpread <- grepl("^\\s*\\.\\.\\.[A-Z_]+,?$", names(chrValues))
    if (any(bSpread)) {
      chrSpread <- sub("^\\s*\\.\\.\\.([A-Z_]+),?$", "\\1", names(chrValues)[bSpread])
      chrValues <- c(chrValues[!bSpread], unlist(lapply(chrSpread, Spread)))
    }
    if (strSetting %in% names(chrValues)) {
      bShared <- grepl("^OUTCOME_DEFAULTS\\.[a-z_]+$", chrValues)
      chrValues[bShared] <- chrOutcomes[sub("^OUTCOME_DEFAULTS\\.", "", chrValues[bShared])]
      return(lapply(chrValues, function(strValue) jsonlite::fromJSON(strValue)))
    }
  }
  NULL
}

# A value as it reads back from the JSON a page is written in: a typed cut
# point is a number, whole or not, as the chart reads it.
lJsonRoundTrip <- function(xValue) {
  jsonlite::fromJSON(jsonlite::toJSON(xValue, auto_unbox = TRUE, digits = NA), simplifyVector = FALSE)
}
