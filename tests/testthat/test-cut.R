# The shared cut rule (#18): a number cut into groups at its median, tertiles,
# quartiles or typed points, written once more in R in R/core.R with
# quantile() and cut() themselves. It is held to bio.viz's recorded cases,
# fixtures/bio.viz/cut-r.json, written by bio.viz's tools/r-cut.R from desktop
# R: the points, the groups each participant falls in, and their labels.

lCutCases <- function() {
  lReadJson(testthat::test_path("fixtures", "bio.viz"), "cut-r.json")$cases
}

# A list of JSON numbers or nulls, as a vector with NA.
nFromJson <- function(lValues) {
  vapply(lValues, function(xValue) if (is.null(xValue)) NA_real_ else as.numeric(xValue), numeric(1))
}

chrFromJson <- function(lValues) {
  vapply(lValues, function(xValue) if (is.null(xValue)) NA_character_ else xValue, character(1))
}

# A cut as R reads it: a name, or typed points as numbers.
xCutOf <- function(xCut) if (is.list(xCut)) nFromJson(xCut) else xCut

test_that("the cut points, the groups and their labels are bio.viz's recorded answers from desktop R, case by case (#18)", {
  lCases <- lCutCases()
  expect_gte(length(lCases), 15L)
  for (lCase in lCases) {
    nX <- nFromJson(lCase$values)
    lCut <- Core_CutPoints(nX, xCutOf(lCase$cut))
    strLabel <- lCase$name
    expect_identical(lCut$n, as.integer(lCase$n), label = paste(strLabel, "n"))
    expect_identical(lCut$asked, nFromJson(lCase$asked), label = paste(strLabel, "asked"))
    expect_identical(lCut$points, nFromJson(lCase$points), label = paste(strLabel, "points"))
    expect_identical(lCut$repeated, lCase$repeated, label = paste(strLabel, "repeated"))
    expect_identical(lCut$merged, lCase$merged, label = paste(strLabel, "merged"))
    expect_identical(lCut$labels, chrFromJson(lCase$labels), label = paste(strLabel, "labels"))
    chrGroups <- Core_CutGroups(nX, lCut$points)
    expect_identical(chrGroups, chrFromJson(lCase$groups), label = paste(strLabel, "groups"))
    expect_identical(
      vapply(lCut$labels, function(strGroup) sum(chrGroups == strGroup, na.rm = TRUE), integer(1), USE.NAMES = FALSE),
      as.integer(unlist(lCase$counts)),
      label = paste(strLabel, "counts")
    )
  }
  # The cases reach the three named cuts and typed points, a value on a point,
  # ties that collapse points, missing values, labels that merge, and bounds
  # written in full at every size.
  chrCuts <- vapply(lCases, function(lCase) if (is.list(lCase$cut)) "typed" else lCase$cut, character(1))
  expect_setequal(chrCuts, c("median", "tertiles", "quartiles", "typed"))
  expect_true(any(vapply(lCases, function(lCase) lCase$repeated, logical(1))))
  expect_true(any(vapply(lCases, function(lCase) lCase$merged, logical(1))))
})

test_that("a cut variable's values are the frame's, on the participants the filters keep, as bio.viz cut them (#18)", {
  lCases <- Filter(function(lCase) !is.null(lCase$variable), lCutCases())
  expect_gte(length(lCases), 6L)
  for (lCase in lCases) {
    lConfig <- GroupComparison_Settings(list(baseline_visits = "Baseline"))
    lFilters <- lapply(lCase$filters, function(lValues) chrFromJson(lValues))
    lCut <- Chart_Cut(Synthetic_Results, Synthetic_Participants, lConfig, lFilters, Core_Variable(lCase$variable))
    expect_identical(lCut$n, as.integer(lCase$n), label = paste(lCase$name, "n"))
    expect_identical(lCut$points, nFromJson(lCase$points), label = paste(lCase$name, "points"))
    expect_identical(lCut$labels, chrFromJson(lCase$labels), label = paste(lCase$name, "labels"))
    # Each participant's value, by id.
    nMine <- lCut$values[match(chrFromJson(lCase$ids), lCut$ids)]
    expect_equal(nMine, nFromJson(lCase$values), tolerance = 1e-12, label = paste(lCase$name, "values"))
    # Drawn by a biomarker: the count in each group of those with a value of it.
    if (!is.null(lCase$drawn)) {
      dfDrawn <- Core_Frame(
        Synthetic_Results, Synthetic_Participants, list(y = lCase$drawn$y),
        c(Chart_CoreSettings(lConfig), list(required = character(0)))
      )$data
      chrIds <- Core_Text(dfDrawn[[lConfig$id_col]])[!is.na(dfDrawn$y)]
      chrGroups <- Core_CutGroups(lCut$values[lCut$ids %in% chrIds], lCut$points)
      expect_identical(
        vapply(lCut$labels, function(strGroup) sum(chrGroups == strGroup, na.rm = TRUE), integer(1), USE.NAMES = FALSE),
        as.integer(unlist(lCase$drawn$counts)),
        label = paste(lCase$name, "drawn counts")
      )
    }
  }
})

test_that("a variable takes a cut, checked as bio.viz checks it, and is written as the settings write it (#18)", {
  expect_identical(Core_Variable(list(measure = "CRP", visit = "Baseline", cut = "median"))$cut, "median")
  expect_identical(Core_Variable(list(col = "AGE", type = "number", cut = list(40, 60)))$cut, c(40, 60))
  expect_identical(
    Core_WrittenCut(Core_Variable(list(measure = "CRP", visit = "Baseline", cut = "median"))),
    list(measure = "CRP", visit = "Baseline", value = "raw", cut = "median")
  )
  expect_identical(
    Core_WrittenCut(Core_Variable(list(measure = "CRP", value = "baseline", cut = 8))),
    list(measure = "CRP", value = "baseline", cut = list(8))
  )
  expect_identical(
    Core_WrittenCut(Core_Variable(list(col = "AGE", type = "number", cut = c(40, 60)))),
    list(col = "AGE", type = "number", cut = list(40, 60))
  )
  expect_error(Core_Variable(list(measure = "CRP", visit = "Baseline", cut = "deciles")), "cut")
  expect_error(Core_Variable(list(measure = "CRP", visit = "Baseline", cut = list())), "empty")
  expect_error(Core_Variable(list(measure = "CRP", visit = "Baseline", cut = c(5, 2))), "ascending")
  expect_error(Core_Variable(list(measure = "CRP", visit = "Baseline", cut = c(1, Inf))), "finite")
  expect_error(Core_Variable(list(measure = "CRP", visit = "Baseline", cut = c(2.0001, 2.00012))), "four significant digits")
  expect_error(Core_Variable(list(col = "AGE", cut = "median")), "number")
  # A setting read as a number takes no cut.
  expect_error(
    AssociationScatter_Settings(list(x = list(measure = "CRP", visit = "Baseline", cut = "median"))),
    "takes no `cut`"
  )
  # A bound is written to four significant digits, in full.
  expect_identical(Core_CutBound(c(2.0625, 123456.7, 1.2345e-5, 3.382e21)), c("2.062", "123500", "0.00001234", "3382000000000000000000"))
})
