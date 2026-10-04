# What gsm.bio takes from bio.viz (#9): the two bundles the widget loads and
# the fixtures the tests hold R to. Each was copied by
# data-raw/vendor-bio-viz.R, which wrote a record beside it: the bio.viz commit,
# and a checksum and a size per file. These tests fail when a copied file and
# its record disagree, or when the records name different bundles.

strLibDir <- function() {
  system.file("htmlwidgets", "lib", package = "gsm.bio")
}

test_that("the vendored bundles match the checksums recorded beside them (#9)", {
  expect_true(nzchar(strLibDir()))
  lRecord <- lReadJson(strLibDir(), "SOURCE.json")

  expect_identical(lRecord$copied_by, "data-raw/vendor-bio-viz.R")
  expect_identical(lRecord$repository, "https://github.com/jwildfire/bio.viz")
  expect_match(lRecord$commit, "^[0-9a-f]{40}$")
  expect_identical(
    vapply(lRecord$files, function(lFile) lFile$library, character(1)),
    c("bio.viz", "safety.viz")
  )
  for (lFile in lRecord$files) {
    strPath <- file.path(strLibDir(), lFile$file)
    expect_true(file.exists(strPath), label = paste(lFile$file, "exists"))
    expect_identical(strSha256(strPath), lFile$sha256, label = paste("the checksum of", lFile$file))
    expect_identical(as.numeric(file.size(strPath)), as.numeric(lFile$bytes), label = paste("the size of", lFile$file))
    expect_identical(lFile$file, sprintf("%s-%s/%s.js", lFile$library, lFile$version, lFile$library))
  }
  # The comparison can fail: a file is not its neighbour's checksum.
  expect_false(identical(strSha256(file.path(strLibDir(), lRecord$files[[1]]$file)), lRecord$files[[2]]$sha256))

  # Nothing is in the folder but the record and the files it lists.
  expect_setequal(
    list.files(strLibDir(), recursive = TRUE),
    c("SOURCE.json", vapply(lRecord$files, function(lFile) lFile$file, character(1)))
  )
})

test_that("the record says the safety.viz copy is a stand-in, and carries where bio.viz took it from: safety.viz's dev (#9, #18, #19)", {
  lRecord <- lReadJson(strLibDir(), "SOURCE.json")
  lKit <- lRecord$safety_viz

  expect_true(lKit$stand_in)
  expect_match(lKit$note, "stand-in", fixed = TRUE)
  expect_match(lKit$note, "Until gsm.safety carries a safety.viz bundle with the kit", fixed = TRUE)
  # bio.viz's own record of its copy, whole: taken from safety.viz's dev
  # branch, at a recorded commit.
  lTheirs <- lKit$bio_viz_record
  expect_identical(lTheirs$repository, "https://github.com/jwildfire/safety.viz")
  expect_identical(lTheirs$ref, "dev")
  expect_match(lTheirs$commit, "^[0-9a-f]{40}$")
  expect_true(lTheirs$merged_to_dev)
  # And ours: bio.viz's dev branch, at a recorded commit. A copy from a branch
  # not merged to dev yet says so, with a note to copy again, and is allowed
  # only while the package is at a development version: a release never
  # carries one (#18).
  expect_match(lRecord$commit, "^[0-9a-f]{40}$")
  expect_true(is.logical(lRecord$merged_to_dev))
  strVersion <- as.character(utils::packageVersion("gsm.bio"))
  bDevelopment <- length(unclass(package_version(strVersion))[[1]]) > 3L
  if (isTRUE(lRecord$merged_to_dev)) {
    expect_identical(lRecord$ref, "dev")
    expect_null(lRecord$note)
  } else {
    expect_true(bDevelopment, label = sprintf("version %s is a development version, so it may carry an unmerged copy", strVersion))
    expect_false(identical(lRecord$ref, "dev"))
    expect_match(lRecord$note, "not merged to bio.viz's dev branch", fixed = TRUE)
    expect_match(lRecord$note, "copy again from dev", fixed = TRUE)
  }
  lFixtureRecord <- lReadJson(testthat::test_path("fixtures", "bio.viz"), "SOURCE.json")
  expect_identical(lFixtureRecord[c("ref", "commit", "merged_to_dev")], lRecord[c("ref", "commit", "merged_to_dev")])
  lCopy <- Filter(function(lFile) lFile$library == "safety.viz", lRecord$files)[[1]]
  expect_identical(lTheirs$files[[1]]$sha256, lCopy$sha256)
  expect_identical(lTheirs$version, lCopy$version)
})

test_that("every widget's dependency file loads the two recorded bundles, safety.viz first, and then the package's own script (#9, #13)", {
  lRecord <- lReadJson(strLibDir(), "SOURCE.json")
  Versions <- function(strLibrary) {
    Filter(function(lFile) lFile$library == strLibrary, lRecord$files)[[1]]$version
  }
  chrWidgets <- grep("^Widget_", getNamespaceExports("gsm.bio"), value = TRUE)
  expect_gt(length(chrWidgets), 1)

  for (strWidget in chrWidgets) {
    chrYaml <- readLines(system.file("htmlwidgets", paste0(strWidget, ".yaml"), package = "gsm.bio"), warn = FALSE)
    chrYaml <- grep("^\\s*#", chrYaml, value = TRUE, invert = TRUE)
    expect_identical(chrYaml, c(
      "dependencies:",
      "  - name: safety-viz",
      paste0("    version: ", Versions("safety.viz")),
      paste0("    src: 'htmlwidgets/lib/safety.viz-", Versions("safety.viz"), "'"),
      "    script: 'safety.viz.js'",
      "  - name: bio-viz",
      paste0("    version: ", Versions("bio.viz")),
      paste0("    src: 'htmlwidgets/lib/bio.viz-", Versions("bio.viz"), "'"),
      "    script: 'bio.viz.js'",
      "  - name: gsm-bio-widget",
      paste0("    version: ", utils::packageVersion("gsm.bio")),
      "    src: 'htmlwidgets/shared'",
      "    script: 'gsm.bio.widget.js'"
    ), label = paste("the dependencies of", strWidget))
    expect_true(file.exists(system.file("htmlwidgets", paste0(strWidget, ".js"), package = "gsm.bio")), label = paste(strWidget, "has a binding"))
  }
  # The script every binding is made with is the package's own, not a copy.
  expect_true(file.exists(system.file("htmlwidgets", "shared", "gsm.bio.widget.js", package = "gsm.bio")))
  expect_identical(list.files(system.file("htmlwidgets", "shared", package = "gsm.bio")), "gsm.bio.widget.js")
})

test_that("the fixtures copied from bio.viz match their record, at the bundles' commit (#9)", {
  strFixtures <- testthat::test_path("fixtures", "bio.viz")
  lRecord <- lReadJson(strFixtures, "SOURCE.json")

  expect_identical(lRecord$copied_by, "data-raw/vendor-bio-viz.R")
  expect_identical(lRecord$commit, lReadJson(strLibDir(), "SOURCE.json")$commit)
  expect_gt(length(lRecord$files), 0)
  for (lFile in lRecord$files) {
    strPath <- file.path(strFixtures, lFile$file)
    expect_true(file.exists(strPath), label = paste(lFile$file, "exists"))
    expect_identical(strSha256(strPath), lFile$sha256, label = paste("the checksum of", lFile$file))
  }
  expect_setequal(
    list.files(strFixtures, recursive = TRUE),
    c("SOURCE.json", vapply(lRecord$files, function(lFile) lFile$file, character(1)))
  )

  # bio.viz wrote its rows from the synthetic study as gsm.bio ships it: the
  # files it records are the package's own, byte for byte.
  lTheirs <- lReadJson(strFixtures, "group-statistics", "SOURCE.json")
  for (strFile in c("synthetic_results.csv", "synthetic_participants.csv")) {
    lFrom <- Filter(function(lFile) basename(lFile$file) == strFile, lTheirs$derived_from)
    expect_identical(length(lFrom), 1L, label = paste("bio.viz's record of", strFile))
    expect_identical(strSha256(system.file("extdata", strFile, package = "gsm.bio")), lFrom[[1]]$sha256, label = strFile)
  }
})

test_that("the core's frames were written by the vendored bundle, from the study the package ships (#9)", {
  lFrames <- lReadJson(testthat::test_path("fixtures", "core-frames"), "frames.json")
  lRecord <- lReadJson(strLibDir(), "SOURCE.json")
  lBundle <- Filter(function(lFile) lFile$library == "bio.viz", lRecord$files)[[1]]

  expect_identical(lFrames$written_by, "data-raw/core-frames.mjs")
  expect_identical(lFrames$bundle$sha256, lBundle$sha256)
  expect_identical(lFrames$bundle$bio_viz_commit, lRecord$commit)
  for (lFile in lFrames$study) {
    expect_identical(
      strSha256(system.file("extdata", basename(lFile$file), package = "gsm.bio")), lFile$sha256,
      label = basename(lFile$file)
    )
  }
  # One frame per case, in the cases' order.
  lCases <- lReadJson(testthat::test_path("fixtures", "core-frames"), "cases.json")$cases
  expect_identical(
    vapply(lFrames$frames, function(lFrame) lFrame$case, character(1)),
    vapply(lCases, function(lCase) lCase$case, character(1))
  )
})

test_that("the specification schema matches the checksum recorded beside it, from the same bio.viz commit as the bundles (#39)", {
  strDir <- system.file("specification", package = "gsm.bio")
  lRecord <- lReadJson(strDir, "SOURCE.json")
  expect_identical(lRecord$copied_by, "data-raw/vendor-bio-viz.R")
  expect_identical(lRecord$commit, lReadJson(strLibDir(), "SOURCE.json")$commit)
  expect_length(lRecord$files, 1L)
  lFile <- lRecord$files[[1]]
  expect_identical(lFile$file, "specification.schema.json")
  expect_identical(lFile$source, "src/data/specification.schema.json")
  expect_identical(strSha256(file.path(strDir, lFile$file)), lFile$sha256)
  expect_setequal(list.files(strDir), c("SOURCE.json", "specification.schema.json"))
})
