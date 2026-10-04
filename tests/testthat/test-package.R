# The package itself (#1): it loads, it is the version the milestone ships, and
# its dependencies are the ones the design allows. The statistics functions run
# unchanged in a desktop session, in the browser and on a server, so they use
# stats and survival and nothing else (test-statistics-file.R holds the file to
# that). The widgets (#9) add htmlwidgets, which only a desktop session or a
# server loads.

chrDependencies <- function(strField) {
  strValue <- utils::packageDescription("gsm.bio")[[strField]]
  if (is.null(strValue)) {
    return(character(0))
  }
  chrEntries <- trimws(strsplit(strValue, ",", fixed = TRUE)[[1]])
  sort(sub("\\s*\\(.*\\)$", "", chrEntries))
}

test_that("the package loads and reports its version: a development version after the v0.1.0 release (#1, #18)", {
  expect_true(isNamespaceLoaded("gsm.bio"))
  expect_identical(utils::packageDescription("gsm.bio")$Package, "gsm.bio")
  # Between releases dev carries a development version, the last release with
  # .9000; the release of v0.2.0 sets 0.2.0.
  expect_identical(as.character(utils::packageVersion("gsm.bio")), "0.1.0.9000")
})

test_that("the package imports stats and survival for the statistics, and htmlwidgets for the widgets (#1, #9)", {
  expect_identical(chrDependencies("Imports"), c("htmlwidgets", "stats", "survival"))
  expect_identical(chrDependencies("Depends"), "R")
  expect_identical(chrDependencies("Remotes"), character(0))
})

test_that("what only a test needs is suggested, not imported (#1, #9)", {
  # effectsize checks the standardised difference; digest, jsonlite and
  # rmarkdown check the vendored files and read a saved page back.
  expect_identical(chrDependencies("Suggests"), c("digest", "effectsize", "jsonlite", "rmarkdown", "testthat"))
  expect_identical(utils::packageDescription("gsm.bio")[["Config/testthat/edition"]], "3")
})

test_that("the package exports the seven statistics functions and the widgets, and ships the synthetic study as its only data (#1, #2, #3, #4, #9, #12, #13, #16, #18, #35)", {
  expect_setequal(
    getNamespaceExports("gsm.bio"),
    c(
      "Analyze_GroupDifference", "Analyze_Correlation", "Analyze_CorrelationMatrix", "Analyze_Fit",
      "Analyze_Contingency", "Analyze_Survival", "Analyze_Screen", "Widget_GroupComparison",
      "Widget_AssociationScatter", "Widget_CorrelationMatrix", "Widget_BiomarkerScreen",
      "Widget_CrossTab", "Widget_StratifiedSurvival"
    )
  )
  expect_setequal(
    utils::data(package = "gsm.bio")$results[, "Item"],
    c("Synthetic_Results", "Synthetic_Participants", "Synthetic_Outcomes", "Synthetic_Truth")
  )
})

test_that("the package-level help page exists (#1)", {
  # Installed, the help index lists it; loaded from source, the Rd file does.
  strAliases <- system.file("help", "aliases.rds", package = "gsm.bio")
  if (nzchar(strAliases)) {
    expect_true("gsm.bio-package" %in% names(readRDS(strAliases)))
  } else {
    expect_true(file.exists(testthat::test_path("..", "..", "man", "gsm.bio-package.Rd")))
  }
})
