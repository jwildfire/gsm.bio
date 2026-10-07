# The released statistics answer as they did (#52). The by-level answers were
# added to the one statistics file, so every answer the file gave before has to
# be the answer it gives now. bio.viz recorded those answers from desktop R:
# each fixture under fixtures/bio.viz/ holds the rows a chart hands R, the
# function and arguments it asks with, and the whole of what R returned. This
# file asks again, with the rows and the arguments as recorded, and compares
# the whole answer, member by member.
#
# An answer is the answer of the R that computed it, and each fixture records
# the R that made it. Under that R every member is compared. Under another R
# the comparison is of what the charts' own tests hold across versions: the
# status, the counts and the p-value.

strRecordedFixture <- function(...) {
  testthat::test_path("fixtures", "bio.viz", ...)
}

# The arguments that name a numeric column. Every other column is text.
chrNumericArguments <- c(
  "strValueCol", "strXCol", "strYCol", "chrCols", "strWithCol", "strTimeCol", "strCensorCol", "strEventCol"
)

# The rows of one recorded request, as bio.viz's scripts read them: every value
# text and a gap missing, then the columns the request reads as numbers made
# numbers, on a logarithmic axis their base-10 logarithm.
dfRecordedRows <- function(strFolder, lRequest) {
  dfRows <- utils::read.csv(
    strRecordedFixture(strFolder, lRequest$file),
    colClasses = "character", na.strings = "", check.names = FALSE
  )
  chrNumeric <- unlist(lRequest$args[intersect(chrNumericArguments, names(lRequest$args))])
  for (strColumn in chrNumeric) {
    dfRows[[strColumn]] <- as.numeric(dfRows[[strColumn]])
  }
  for (strAxis in c("x", "y")) {
    if (identical(lRequest$dataId[[paste0(strAxis, "_scale")]], "log") && lRequest$name != "Analyze_GroupDifference") {
      dfRows[[strAxis]] <- log10(dfRows[[strAxis]])
    }
  }
  dfRows
}

# One value per participant from a fixture that holds its rows inline, a gap
# as NA.
xInline <- function(lValues, xType) {
  vapply(lValues, function(xValue) if (is.null(xValue)) methods::as(NA, class(xType)) else xValue, xType)
}

# Every recorded request of every fixture, with its rows: the case's name, the
# function, the arguments, the rows and what R returned.
lRecordedAnswers <- function() {
  lAll <- list()
  Add <- function(strFixture, lRequest, dfRows, strRecordedR) {
    lAll[[length(lAll) + 1L]] <<- list(
      case = paste(strFixture, lRequest$case), name = lRequest$name, args = lRequest$args,
      data = dfRows, rows = lRequest$rows, value = lRequest$value, r_version = strRecordedR
    )
  }
  for (strChart in c("group", "association", "matrix", "screen")) {
    strFixture <- paste0(strChart, "-statistics")
    lFixture <- lReadJson(strRecordedFixture(paste0(strFixture, "-r.json")))
    # A recipe is asked again where the fixture holds both its rows and its
    # answer; some hold only the key a stored result is written under.
    for (lRequest in c(lFixture$results, lFixture$recipes)) {
      if (!is.null(lRequest$value) && !is.null(lRequest$file)) {
        Add(strFixture, lRequest, dfRecordedRows(strFixture, lRequest), lFixture$made_by$r_version)
      }
    }
  }
  lCrossTab <- lReadJson(strRecordedFixture("cross-tab-r.json"))
  for (lCase in lCrossTab$cases) {
    dfRows <- data.frame(row = xInline(lCase$row_of, character(1)), col = xInline(lCase$col_of, character(1)), stringsAsFactors = FALSE)
    Add("cross-tab", lCase, dfRows, lCrossTab$made_by$r_version)
  }
  lSurvival <- lReadJson(strRecordedFixture("stratified-survival-r.json"))
  for (lCase in lSurvival$cases) {
    # The flag is handed over the way round the request names it: a censor
    # flag, or an event flag.
    iEvent <- as.integer(xInline(lCase$event_of, logical(1)))
    dfRows <- data.frame(
      group = xInline(lCase$group_of, character(1)), time = xInline(lCase$time_of, numeric(1)),
      stringsAsFactors = FALSE
    )
    if (is.null(lCase$args$strEventCol)) {
      dfRows[[lCase$args$strCensorCol]] <- 1L - iEvent
    } else {
      dfRows[[lCase$args$strEventCol]] <- iEvent
    }
    Add("stratified-survival", lCase, dfRows, lSurvival$made_by$r_version)
  }
  lAll
}

test_that("every answer bio.viz recorded from the statistics file is still its answer, member by member (#52)", {
  lAnswers <- lRecordedAnswers()
  # Every function that was released is asked, with answers that have numbers
  # and answers that have a reason in their place.
  expect_setequal(
    unique(vapply(lAnswers, function(lAnswer) lAnswer$name, character(1))),
    c(
      "Analyze_GroupDifference", "Analyze_Correlation", "Analyze_CorrelationMatrix", "Analyze_Fit",
      "Analyze_Contingency", "Analyze_Survival", "Analyze_Screen"
    )
  )
  expect_setequal(
    unique(vapply(lAnswers, function(lAnswer) lAnswer$value$status, character(1))),
    c("ok", "too_small")
  )
  expect_gte(length(lAnswers), 91L)

  nWhole <- 0L
  for (lAnswer in lAnswers) {
    # The rows are the rows the answer was recorded on.
    expect_identical(nrow(lAnswer$data), lAnswer$rows, label = paste(lAnswer$case, "rows"))
    lMine <- do.call(get(lAnswer$name, envir = asNamespace("gsm.bio")), c(list(lAnswer$data), lAnswer$args))
    ExpectResultShape(lMine)
    expect_identical(lMine$status, lAnswer$value$status, label = paste(lAnswer$case, "status"))
    expect_identical(
      as.character(chrPageDifferences(lAnswer$value$counts, lMine$counts, "counts")), character(0),
      label = paste(lAnswer$case, "differences in counts")
    )
    if (lMine$status == "ok" && !is.na(lMine$p_value)) {
      expect_equal(lMine$p_value, lAnswer$value$p_value, tolerance = 1e-8, label = paste(lAnswer$case, "p_value"))
    }
    if (identical(as.character(getRversion()), lAnswer$r_version)) {
      # The whole answer: every member, every row and every column, to 1 part
      # in 10^12, and every word.
      expect_identical(
        as.character(chrPageDifferences(lAnswer$value, lMine, lAnswer$case)), character(0),
        label = paste("differences in", lAnswer$case)
      )
      nWhole <- nWhole + 1L
    }
  }
  # Under the R that recorded them, none is left out of the whole comparison.
  if (all(vapply(lAnswers, function(lAnswer) identical(as.character(getRversion()), lAnswer$r_version), logical(1)))) {
    expect_identical(nWhole, length(lAnswers))
  }

  # The comparison can fail: an answer on other rows is not the recorded one.
  lFirst <- lAnswers[[1]]
  lOther <- do.call(Analyze_GroupDifference, c(list(lFirst$data[-1, ]), lFirst$args))
  expect_gt(length(chrPageDifferences(lFirst$value, lOther, "other rows")), 0)
})

test_that("each released function takes the arguments it took, with the defaults it had (#52)", {
  # As the statistics file had them before the by-level answers were added.
  lReleased <- list(
    Analyze_GroupDifference = "dfData, strValueCol, strGroupCol, strMethod = \"t\", chrGroups = NULL, bPairwise = TRUE, strPAdjust = \"holm\", nConfLevel = 0.95, nMinGroup = nMinGroupDefault",
    Analyze_Correlation = "dfData, strXCol, strYCol, strMethod = \"pearson\", strGroupCol = NULL, chrGroups = NULL, nConfLevel = 0.95, nMinGroup = nMinGroupDefault",
    Analyze_CorrelationMatrix = "dfData, chrCols, strMethod = \"pearson\", nConfLevel = 0.95, nMinPairs = nMinGroupDefault",
    Analyze_Fit = "dfData, strXCol, strYCol, strMethod = \"linear\", strGroupCol = NULL, chrGroups = NULL, nConfLevel = 0.95, nMinGroup = nMinGroupDefault, nPoints = 50L",
    Analyze_Contingency = "dfData, strRowCol, strColCol, strMethod = \"chisq\", chrRowGroups = NULL, chrColGroups = NULL, nConfLevel = 0.95, nMinGroup = nMinGroupDefault",
    Analyze_Survival = "dfData, strTimeCol, strGroupCol, strCensorCol = NULL, strEventCol = NULL, chrGroups = NULL, nConfLevel = 0.95, nMinGroup = nMinGroupDefault",
    Analyze_Screen = "dfData, chrCols, strComparison = \"difference\", strGroupCol = NULL, chrGroups = NULL, strWithCol = NULL, strCorMethod = \"pearson\", strTimeCol = NULL, strCensorCol = NULL, strEventCol = NULL, strPAdjust = \"BH\", nConfLevel = 0.95, nMinGroup = nMinGroupDefault"
  )
  for (strName in names(lReleased)) {
    lFormals <- formals(get(strName, envir = asNamespace("gsm.bio")))
    strNow <- paste(
      ifelse(
        vapply(lFormals, function(xDefault) is.symbol(xDefault) && !nzchar(as.character(xDefault)), logical(1)),
        names(lFormals),
        paste(names(lFormals), "=", vapply(lFormals, function(xDefault) paste(deparse(xDefault), collapse = ""), character(1)))
      ),
      collapse = ", "
    )
    expect_identical(strNow, lReleased[[strName]], label = paste(strName, "arguments and defaults"))
  }
})
