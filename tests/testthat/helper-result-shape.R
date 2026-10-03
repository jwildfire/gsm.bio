# The fixed result shape, checked the same way for every statistics function.

chrResultMembers <- c(
  "status", "reason", "test", "method", "estimates", "statistic", "p_value",
  "adjustment", "counts", "dropped", "warnings", "notes", "rows"
)

bSnakeCase <- function(chrNames) {
  all(grepl("^[a-z][a-z0-9_]*$", chrNames))
}

# One unnamed value with nothing attached: what becomes a single JSON value.
ExpectSingleValue <- function(xValue, strType, strLabel) {
  expect_true(is.atomic(xValue) && length(xValue) == 1L, label = paste(strLabel, "is one value"))
  expect_null(attributes(xValue), label = paste("attributes of", strLabel))
  expect_identical(typeof(xValue), strType, label = paste("type of", strLabel))
}

# A plain data frame: no class but data.frame, no attribute but the three a
# data frame must have, and every column a bare vector that is not a factor.
ExpectPlainFrame <- function(dfValue, strLabel) {
  expect_identical(class(dfValue), "data.frame", label = paste("class of", strLabel))
  expect_setequal(names(attributes(dfValue)), c("names", "class", "row.names"))
  expect_true(bSnakeCase(names(dfValue)), label = paste("snake case columns of", strLabel))
  for (strCol in names(dfValue)) {
    expect_true(is.atomic(dfValue[[strCol]]), label = paste(strLabel, strCol, "is atomic"))
    expect_null(attributes(dfValue[[strCol]]), label = paste("attributes of", strLabel, strCol))
  }
}

# An unnamed list of single strings: what becomes a JSON array of text.
ExpectTextList <- function(lValue, strLabel) {
  expect_identical(class(lValue), "list", label = paste("class of", strLabel))
  expect_null(names(lValue), label = paste("names of", strLabel))
  for (xItem in lValue) {
    ExpectSingleValue(xItem, "character", paste("an item of", strLabel))
  }
}

ExpectResultShape <- function(lResult) {
  expect_identical(class(lResult), "list")
  expect_identical(names(lResult), chrResultMembers)
  expect_setequal(names(attributes(lResult)), "names")

  for (strMember in c("status", "reason", "test", "method", "adjustment")) {
    ExpectSingleValue(lResult[[strMember]], "character", strMember)
  }
  ExpectSingleValue(lResult$p_value, "double", "p_value")
  expect_true(lResult$status %in% c("ok", "too_small", "error"))
  expect_false(is.na(lResult$adjustment))

  # counts: one whole number, or a named list of group to whole number.
  if (is.list(lResult$counts)) {
    expect_identical(class(lResult$counts), "list")
    expect_true(length(lResult$counts) > 0L && !is.null(names(lResult$counts)))
    expect_true(all(nzchar(names(lResult$counts))) && anyDuplicated(names(lResult$counts)) == 0L)
    for (xCount in lResult$counts) {
      ExpectSingleValue(xCount, "integer", "a count")
    }
  } else {
    ExpectSingleValue(lResult$counts, "integer", "counts")
  }

  for (strMember in c("estimates", "statistic", "dropped", "rows")) {
    ExpectPlainFrame(lResult[[strMember]], strMember)
  }
  expect_named(lResult$estimates, c("name", "group", "estimate", "lower", "upper", "level"))
  expect_named(lResult$statistic, c("name", "value"))
  expect_named(lResult$dropped, c("reason", "n"))
  ExpectTextList(lResult$warnings, "warnings")
  ExpectTextList(lResult$notes, "notes")

  # A reason stands in place of the numbers, and only then.
  if (lResult$status == "ok") {
    expect_true(is.na(lResult$reason))
  } else {
    expect_false(is.na(lResult$reason))
    expect_true(is.na(lResult$p_value))
    expect_identical(nrow(lResult$estimates), 0L)
    expect_identical(nrow(lResult$statistic), 0L)
  }
  invisible(lResult)
}
