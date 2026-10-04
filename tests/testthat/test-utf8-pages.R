# A widget's page carries its text as UTF-8 whatever the session's locale
# (#22). A table read in a session whose locale is not UTF-8 holds text that is
# not ASCII unmarked, as its bytes; jsonlite writes such text into the page as
# escapes ("<c3><96>dem" for "Ödem"), so the chart's categories, and the keys it
# asks R by, would not be R's.

# Runs `fnBody` with LC_CTYPE set to C, as a session started without a UTF-8
# locale is.
WithCLocale <- function(fnBody) {
  strCtype <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", strCtype))
  Sys.setlocale("LC_CTYPE", "C")
  fnBody()
}

# The bytes of a page, and whether they hold a byte string.
bPageHas <- function(strFile, rawText) {
  rawPage <- readBin(strFile, "raw", file.size(strFile))
  nAt <- grepRaw(rawText, rawPage, fixed = TRUE, all = TRUE)
  length(nAt) > 0L
}

# A participant table whose category is text that is not ASCII, in each form a
# session can hold it: unmarked bytes (read in a C locale), marked UTF-8, and
# Latin-1.
dfStages <- function() {
  strUnmarked <- rawToChar(as.raw(c(0xc3, 0x96, 0x64, 0x65, 0x6d)))
  strMarked <- rawToChar(as.raw(c(0xc3, 0x89, 0x69, 0x72, 0x65)))
  Encoding(strMarked) <- "UTF-8"
  strLatin1 <- iconv(rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xc3, 0xa9))), "UTF-8", "latin1")
  dfParticipants <- Synthetic_Participants
  dfParticipants$STAGE <- c(strUnmarked, strMarked, strLatin1, "Week 1")[(seq_len(nrow(dfParticipants)) - 1L) %% 4L + 1L]
  dfParticipants
}

chrStagesUtf8 <- function() {
  chrStages <- vapply(
    list(c(0xc3, 0x96, 0x64, 0x65, 0x6d), c(0xc3, 0x89, 0x69, 0x72, 0x65), c(0x63, 0x61, 0x66, 0xc3, 0xa9)),
    function(nBytes) rawToChar(as.raw(nBytes)), character(1)
  )
  Encoding(chrStages) <- "UTF-8"
  c(chrStages, "Week 1")
}

test_that("a page saved in a session whose locale is not UTF-8 carries text that is not ASCII as UTF-8 (#22)", {
  WithCLocale(function() {
    expect_false(l10n_info()[["UTF-8"]])
    lWidget <- Widget_GroupComparison(Synthetic_Results, dfStages(), lSettings = list(
      start_value = "IL-6", visits = "Week 4", value_type = "change", baseline_visits = "Baseline", group_by = "STAGE"
    ))
    strFile <- file.path(tempfile("utf8"), "page.html")
    dir.create(dirname(strFile))
    htmlwidgets::saveWidget(lWidget, file = strFile, selfcontained = bPandoc())
    # Every form is written as its UTF-8 bytes, and none as an escape.
    for (strStage in chrStagesUtf8()) {
      expect_true(bPageHas(strFile, charToRaw(strStage)), label = paste("the page holds", strStage))
    }
    for (strEscape in c("<c3>", "<U+00", "<e9>")) {
      expect_false(bPageHas(strFile, charToRaw(strEscape)), label = paste("the page holds", strEscape))
    }
    # Read back as UTF-8, the page's tables and its stored keys name the
    # categories as they are.
    lPage <- jsonlite::fromJSON(
      sub(".*<script type=\"application/json\" data-for=\"[^\"]+\">(.*?)</script>.*", "\\1",
        paste(readLines(strFile, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      ),
      simplifyVector = FALSE
    )$x
    expect_setequal(intersect(unlist(lPage$dfParticipants$STAGE), chrStagesUtf8()), chrStagesUtf8())
    lStored <- Filter(function(lResult) identical(lResult$dataId$measure, "IL-6"), lPage$lStatistics$results)
    expect_length(lStored, 1L)
    expect_setequal(unlist(lStored[[1]]$dataId$groups), chrStagesUtf8())
    expect_identical(unlist(lStored[[1]]$dataId$groups), Core_SortText(chrStagesUtf8()))
  })
})

test_that("text that is not ASCII is marked UTF-8 for the page and otherwise left as it is (#22)", {
  WithCLocale(function() {
    dfParticipants <- dfStages()
    lMarked <- Widget_Utf8(list(df = dfParticipants, values = dfParticipants$STAGE, nested = list(f = factor(dfParticipants$STAGE))))
    expect_identical(unique(Encoding(lMarked$values)), c("UTF-8", "unknown"))
    expect_identical(unique(lMarked$values), chrStagesUtf8())
    expect_identical(lMarked$df$STAGE, lMarked$values)
    expect_setequal(levels(lMarked$nested$f), chrStagesUtf8())
    # Numbers, logicals and ASCII text are as they were.
    expect_identical(lMarked$df[setdiff(names(dfParticipants), "STAGE")], dfParticipants[setdiff(names(dfParticipants), "STAGE")])
    expect_identical(Widget_Utf8(Synthetic_Participants), Synthetic_Participants)
  })
})
