# What every chart shares, held to bio.viz's own code (#19). The filters a chart
# opens on decide which participants R computes on, so Chart_Filters() is held
# to the safety.viz kit the widgets ship: fixtures/filter-states/states.json
# holds what the kit opened each case in cases.json on, written by
# data-raw/filter-states.mjs from the vendored bundle.

lFilterCases <- function() {
  lReadJson(testthat::test_path("fixtures", "filter-states"), "cases.json")$cases
}

lFilterStates <- function() {
  lReadJson(testthat::test_path("fixtures", "filter-states"), "states.json")
}

test_that("the filters open on what the kit opens them on: a start the data lacks is All, and all = FALSE is the first value (#19)", {
  lCases <- lFilterCases()
  lStates <- lFilterStates()$states
  expect_gte(length(lCases), 12L)
  expect_identical(
    vapply(lStates, function(lState) lState$case, character(1)),
    vapply(lCases, function(lCase) lCase$case, character(1))
  )
  for (iCase in seq_along(lCases)) {
    lCase <- lCases[[iCase]]
    lConfig <- GroupComparison_Settings(list(filters = lCase$filters))
    dfCategories <- Chart_Categories(Synthetic_Results, Synthetic_Participants, lConfig)
    lMine <- Chart_Filters(Synthetic_Participants, lConfig, dfCategories)
    lTheirs <- lapply(lStates[[iCase]]$state, function(xValue) if (is.null(xValue)) NULL else as.character(unlist(xValue)))
    expect_identical(names(lMine), names(lTheirs), label = paste(lCase$case, "filters"))
    for (strColumn in names(lTheirs)) {
      expect_identical(lMine[[strColumn]], lTheirs[[strColumn]], label = paste(lCase$case, strColumn))
    }
  }
  # The cases reach every turn of the rule: a start kept and one dropped, the
  # first value in force, several values kept in part and dropped whole, and
  # the participant's id, which is no filter.
  chrWarnings <- unlist(lapply(lStates, function(lState) lState$warnings))
  expect_true(any(grepl("opens on All", chrWarnings, fixed = TRUE)))
  expect_true(any(grepl("opens on [ Placebo ]", chrWarnings, fixed = TRUE)))
  expect_true(any(grepl("opens without it", chrWarnings, fixed = TRUE)))
})

test_that("the filter states were written by the vendored kit, from the study the package ships (#19)", {
  lStates <- lFilterStates()
  lRecord <- lReadJson(system.file("htmlwidgets", "lib", package = "gsm.bio"), "SOURCE.json")
  lBundle <- Filter(function(lFile) lFile$library == "safety.viz", lRecord$files)[[1]]
  expect_identical(lStates$written_by, "data-raw/filter-states.mjs")
  expect_identical(lStates$bundle$sha256, lBundle$sha256)
  expect_identical(lStates$bundle$bio_viz_commit, lRecord$commit)
  expect_identical(
    strSha256(system.file("extdata", "synthetic_participants.csv", package = "gsm.bio")),
    lStates$study[[1]]$sha256
  )
})
