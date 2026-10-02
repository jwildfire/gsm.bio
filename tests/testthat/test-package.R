# The package itself (#1): it loads, it is the version the milestone ships, and
# its dependencies are the two the design allows. The statistics functions run
# unchanged in a desktop session, in the browser and on a server, so anything
# beyond stats and survival under Imports would break one of the three.

chrDependencies <- function(strField) {
  strValue <- utils::packageDescription("gsm.bio")[[strField]]
  if (is.null(strValue)) {
    return(character(0))
  }
  chrEntries <- trimws(strsplit(strValue, ",", fixed = TRUE)[[1]])
  sort(sub("\\s*\\(.*\\)$", "", chrEntries))
}

test_that("the package loads and reports the version its milestone ships (#1)", {
  expect_true(isNamespaceLoaded("gsm.bio"))
  expect_identical(utils::packageDescription("gsm.bio")$Package, "gsm.bio")
  expect_identical(as.character(utils::packageVersion("gsm.bio")), "0.1.0")
})

test_that("the package imports only stats and survival (#1)", {
  expect_identical(chrDependencies("Imports"), c("stats", "survival"))
  expect_identical(chrDependencies("Depends"), "R")
  expect_identical(chrDependencies("Remotes"), character(0))
})

test_that("testthat and the standardised-difference comparison package are suggested, not imported (#1)", {
  expect_identical(chrDependencies("Suggests"), c("effectsize", "testthat"))
  expect_identical(utils::packageDescription("gsm.bio")[["Config/testthat/edition"]], "3")
})

test_that("the package exports no function yet and ships the synthetic study as its only data (#1, #2)", {
  expect_identical(getNamespaceExports("gsm.bio"), character(0))
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
