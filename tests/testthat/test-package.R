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

test_that("the package loads and reports its version: a development version after the v0.4.0 release (#1, #18, #44, #50, #59, #69, #79, #105)", {
  expect_true(isNamespaceLoaded("gsm.bio"))
  expect_identical(utils::packageDescription("gsm.bio")$Package, "gsm.bio")
  # Between releases dev carries a development version, the last release with
  # .9000; a release sets its own version, as the next one will.
  expect_identical(as.character(utils::packageVersion("gsm.bio")), "0.4.0.9000")
})

test_that("the package imports stats and survival for the statistics, htmlwidgets for the widgets, grDevices for the RTF writer and utils for the app's .csv files (#1, #9, #38, #73)", {
  # grDevices ships with R: Write_RTF() opens a device of its own for r2rtf to
  # measure text on, so it leaves no Rplots.pdf and the caller's device as it was.
  # utils ships with R too: the app reads a reader's .csv file with read.csv() (#73).
  expect_identical(chrDependencies("Imports"), c("grDevices", "htmlwidgets", "stats", "survival", "utils"))
  expect_identical(chrDependencies("Depends"), "R")
  expect_identical(chrDependencies("Remotes"), character(0))
})

test_that("what only a test, a static figure, the RTF writer or a Shiny page needs is suggested, not imported (#1, #9, #37, #38, #53, #71, #73)", {
  # effectsize checks the standardised difference; digest, jsonlite and
  # rmarkdown check the vendored files and read a saved page back; chromote
  # opens a saved page in a headless browser, for the tests that drive the
  # chart itself; ggplot2 draws the static figures and r2rtf writes a table to
  # RTF, and each says so when it is not installed; svglite writes a batch
  # run's SVG figures; shiny draws the widgets in a Shiny page, whose session
  # answers their statistics (#71), and says so when it is not installed;
  # haven reads a reader's .xpt or .sas7bdat file in the app (#73), which says
  # so when it is not installed.
  expect_identical(
    chrDependencies("Suggests"),
    c("chromote", "digest", "effectsize", "ggplot2", "haven", "jsonlite", "r2rtf", "rmarkdown", "shiny", "svglite", "testthat")
  )
  expect_identical(utils::packageDescription("gsm.bio")[["Config/testthat/edition"]], "3")
})

test_that("the package exports the nine statistics functions, the widgets with their Shiny output and render functions and the app they make, the figures and the tables, and ships the synthetic study as its only data (#1, #2, #3, #4, #9, #12, #13, #16, #18, #35, #37, #38, #39, #52, #71, #72)", {
  expect_setequal(
    getNamespaceExports("gsm.bio"),
    c(
      "Analyze_GroupDifference", "Analyze_GroupDifferenceBy", "Analyze_Correlation", "Analyze_CorrelationMatrix",
      "Analyze_Fit", "Analyze_Contingency", "Analyze_Survival", "Analyze_Screen", "Analyze_DifferenceGrid",
      "Widget_GroupComparison",
      "Widget_AssociationScatter", "Widget_CorrelationMatrix", "Widget_BiomarkerScreen",
      "Widget_CrossTab", "Widget_StratifiedSurvival",
      "Widget_GroupComparisonOutput", "Widget_AssociationScatterOutput", "Widget_CorrelationMatrixOutput",
      "Widget_BiomarkerScreenOutput", "Widget_CrossTabOutput", "Widget_StratifiedSurvivalOutput",
      "renderWidget_GroupComparison", "renderWidget_AssociationScatter", "renderWidget_CorrelationMatrix",
      "renderWidget_BiomarkerScreen", "renderWidget_CrossTab", "renderWidget_StratifiedSurvival",
      "Serve_Statistics", "RunApp",
      "Visualize_GroupComparison", "Visualize_AssociationScatter", "Visualize_CorrelationMatrix",
      "Visualize_BiomarkerScreen", "Visualize_CrossTab", "Visualize_StratifiedSurvival",
      "Table_GroupComparison", "Table_AssociationScatter", "Table_CorrelationMatrix",
      "Table_BiomarkerScreen", "Table_CrossTab", "Table_StratifiedSurvival", "Write_RTF", "Run_Specifications"
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

test_that("every function v0.1.0 exported takes v0.1.0's arguments in v0.1.0's places, so a v0.1.0 call by position works unchanged (#48)", {
  # The arguments of each v0.1.0 export, in order, as the v0.1.0 tag has them.
  lV010 <- list(
    Analyze_Contingency = c("dfData", "strRowCol", "strColCol", "strMethod", "chrRowGroups", "chrColGroups", "nConfLevel", "nMinGroup"),
    Analyze_Correlation = c("dfData", "strXCol", "strYCol", "strMethod", "strGroupCol", "chrGroups", "nConfLevel", "nMinGroup"),
    Analyze_CorrelationMatrix = c("dfData", "chrCols", "strMethod", "nConfLevel", "nMinPairs"),
    Analyze_Fit = c("dfData", "strXCol", "strYCol", "strMethod", "strGroupCol", "chrGroups", "nConfLevel", "nMinGroup", "nPoints"),
    Analyze_GroupDifference = c("dfData", "strValueCol", "strGroupCol", "strMethod", "chrGroups", "bPairwise", "strPAdjust", "nConfLevel", "nMinGroup"),
    Analyze_Screen = c(
      "dfData", "chrCols", "strComparison", "strGroupCol", "chrGroups", "strWithCol", "strCorMethod", "strTimeCol",
      "strCensorCol", "strEventCol", "strPAdjust", "nConfLevel", "nMinGroup"
    ),
    Analyze_Survival = c("dfData", "strTimeCol", "strGroupCol", "strCensorCol", "strEventCol", "chrGroups", "nConfLevel", "nMinGroup"),
    Widget_AssociationScatter = c("dfResults", "dfParticipants", "lSettings", "width", "height", "elementId", "bDebug"),
    Widget_BiomarkerScreen = c("dfResults", "dfParticipants", "lSettings", "width", "height", "elementId", "bDebug"),
    Widget_CorrelationMatrix = c("dfResults", "dfParticipants", "lSettings", "width", "height", "elementId", "bDebug"),
    Widget_GroupComparison = c("dfResults", "dfParticipants", "lSettings", "width", "height", "elementId", "bDebug")
  )
  for (strName in names(lV010)) {
    chrNow <- names(formals(get(strName, envir = asNamespace("gsm.bio"))))
    expect_identical(chrNow[seq_along(lV010[[strName]])], lV010[[strName]], label = paste(strName, "arguments"))
  }
  # Each v0.1.0 widget, called by position as v0.1.0 took it.
  for (strWidget in grep("^Widget_", names(lV010), value = TRUE)) {
    lWidget <- get(strWidget)(Synthetic_Results, Synthetic_Participants, list(), "100%", "500px", "by-position", FALSE)
    expect_identical(lWidget[c("width", "height", "elementId")], list(width = "100%", height = "500px", elementId = "by-position"), label = strWidget)
  }
})
