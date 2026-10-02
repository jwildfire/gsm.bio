# What the widget tests share. Nothing here is exported.

# A JSON file, as nested lists: an array is an unnamed list whatever its
# length, an object a named list, and null is NULL.
lReadJson <- function(...) {
  jsonlite::fromJSON(paste(readLines(file.path(...), warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
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

# The settings most widget tests open on: change from Baseline, by arm.
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
  htmlwidgets::saveWidget(lWidget, file = strFile, selfcontained = bSelfContained)
  paste(readLines(strFile, warn = FALSE), collapse = "\n")
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
  bSame <- if (is.na(xValue)) {
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
