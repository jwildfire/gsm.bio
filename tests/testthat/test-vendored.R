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
  # The widgets, not the Shiny output function each has beside it (#71).
  chrWidgets <- grep("Output$", grep("^Widget_", getNamespaceExports("gsm.bio"), value = TRUE), value = TRUE, invert = TRUE)
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

test_that("the three records name the bio.viz release the copy is byte for byte the same as, or none, and say the same of where it was copied from (#48, #53, #59)", {
  lRecords <- list(
    lReadJson(system.file("htmlwidgets", "lib", "SOURCE.json", package = "gsm.bio")),
    lReadJson(system.file("specification", "SOURCE.json", package = "gsm.bio")),
    lReadJson(testthat::test_path("fixtures", "bio.viz"), "SOURCE.json")
  )
  strBioViz <- Filter(function(lFile) lFile$library == "bio.viz", lRecords[[1]]$files)[[1]]$version
  # The three were written by one run of the copy: they say the same.
  for (lRecord in lRecords) {
    expect_identical(lRecord$release, lRecords[[1]]$release)
    expect_identical(lRecord[c("ref", "commit", "merged_to_dev")], lRecords[[1]][c("ref", "commit", "merged_to_dev")])
  }
  # The release beside the commit copied from, which stays dev's. Between
  # bio.viz's releases, and while a release of both is prepared and bio.viz's
  # tag is not made yet, the records name none: what a release of gsm.bio may
  # then carry is the next test's.
  lRelease <- lRecords[[1]]$release
  if (!is.null(lRelease)) {
    expect_identical(lRelease$tag, paste0("v", strBioViz))
    expect_match(lRelease$commit, "^[0-9a-f]{40}$")
    expect_match(lRelease$note, "byte for byte the same", fixed = TRUE)
    expect_identical(lRecords[[1]]$ref, "dev")
  }
})

test_that("a release of gsm.bio carries a release build of bio.viz from bio.viz's dev branch, and a development build is refused (#59)", {
  lRecord <- lReadJson(system.file("htmlwidgets", "lib", "SOURCE.json", package = "gsm.bio"))
  strRecorded <- Filter(function(lFile) lFile$library == "bio.viz", lRecord$files)[[1]]$version
  chrBundle <- chrBioVizBundle()
  lSays <- lBundleSays(chrBundle)
  strVersion <- as.character(utils::packageVersion("gsm.bio"))

  # The bundle says the version its record and its folder name.
  expect_identical(lSays$version, strRecorded)
  # The copy this version carries is one it may carry.
  expect_identical(chrReleaseRefusals(strVersion, lRecord, lSays), character(0))
  if (bReleaseVersion(strVersion)) {
    # A release: the footnote of every chart it draws names bio.viz's release.
    expect_false(lSays$development)
    expect_identical(lSays$said, strRecorded)
    expect_true(bReleaseVersion(strRecorded), label = paste(strRecorded, "is a release version"))
    expect_true(lRecord$merged_to_dev)
  }

  # The rule can refuse. The copied bundle built as a development build, with
  # nothing else changed, says so in its footnote, and no release may carry it.
  lDevelopment <- lBundleSays(chrBundleBuiltAs(chrBundle, TRUE))
  expect_true(lDevelopment$development)
  expect_identical(lDevelopment$said, paste(strRecorded, "with development changes"))
  lMerged <- utils::modifyList(lRecord, list(ref = "dev", merged_to_dev = TRUE))
  expect_identical(
    chrReleaseRefusals("0.3.0", lMerged, lDevelopment),
    sprintf(
      "gsm.bio 0.3.0 is a release, and the bio.viz it carries is a development build: its footnote reads \"bio.viz %s with development changes\"",
      strRecorded
    )
  )
  # The same build is what dev carries between releases.
  expect_identical(chrReleaseRefusals("0.3.0.9000", lMerged, lDevelopment), character(0))
  # A release build is carried, and only from a commit on bio.viz's dev branch.
  lRelease <- lBundleSays(chrBundleBuiltAs(chrBundle, FALSE))
  expect_false(lRelease$development)
  expect_identical(lRelease$said, strRecorded)
  expect_identical(chrReleaseRefusals("0.3.0", lMerged, lRelease), character(0))
  expect_identical(
    chrReleaseRefusals("0.3.0", utils::modifyList(lMerged, list(ref = "108-release-prep", merged_to_dev = FALSE)), lRelease),
    "gsm.bio 0.3.0 is a release, and the bio.viz it carries was not copied from a commit on bio.viz's dev branch"
  )
  # A version that is not a release's is refused whatever its flag says.
  expect_identical(
    chrReleaseRefusals("0.3.0", lMerged, utils::modifyList(lRelease, list(version = "0.3.0-rc.1", said = "0.3.0-rc.1"))),
    "gsm.bio 0.3.0 is a release, and the bio.viz it carries has the version 0.3.0-rc.1, which is not a release version"
  )
  # A bundle the test cannot read is never taken for a release.
  expect_error(lBundleSays(grep("var DEVELOPMENT", chrBundle, value = TRUE, invert = TRUE)), "does not fix its development once")
})

test_that("once bio.viz's tag for the copied version exists, every copied file is byte for byte the tag's (#59)", {
  skip_if_not(bSourceTree(), "bio.viz's tag is asked of GitHub, from the source tree only")
  lRecord <- lReadJson(system.file("htmlwidgets", "lib", "SOURCE.json", package = "gsm.bio"))
  strBioViz <- Filter(function(lFile) lFile$library == "bio.viz", lRecord$files)[[1]]$version
  strVersion <- as.character(utils::packageVersion("gsm.bio"))
  lCopied <- lCopiedFiles()
  expect_gt(length(lCopied), 10L)

  # The comparison itself, on a tag that exists and that today's copy has moved
  # on from: bio.viz v0.2.0. It finds the files that differ, and nothing else.
  lEarlier <- lBioVizTag("v0.2.0")
  expect_identical(lEarlier$commit, "4440a4377935b3cc7263a07cb64db760b2d4a1a5")
  expect_gt(length(lEarlier$files), 100L)
  strSchema <- file.path(system.file("specification", package = "gsm.bio"), "specification.schema.json")
  lSchema <- list(list(file = basename(strSchema), root = dirname(strSchema), source = "src/data/specification.schema.json"))
  bSchemaSame <- identical(unname(lEarlier$files["src/data/specification.schema.json"]), strGitName(strSchema))
  expect_identical(chrDiffersFromTag(lEarlier, lSchema), if (bSchemaSame) character(0) else "src/data/specification.schema.json")
  lElsewhere <- list(utils::modifyList(lSchema[[1]], list(source = "package.json")))
  expect_identical(chrDiffersFromTag(lEarlier, lElsewhere), "package.json")
  lMissing <- list(utils::modifyList(lSchema[[1]], list(source = "no/such/file.json")))
  expect_identical(chrDiffersFromTag(lEarlier, lMissing), "no/such/file.json")
  expect_null(lBioVizTag("v0.0.0-no-such-tag"))

  # The tag of the version copied. Between bio.viz's releases dev has moved on
  # from it, and a development version of gsm.bio carries dev. A release of
  # gsm.bio is the tag's bytes from the day the tag is made; so is any copy
  # whose records name the release.
  lTag <- lBioVizTag(paste0("v", strBioViz))
  bHeld <- bReleaseVersion(strVersion) || !is.null(lRecord$release)
  if (is.null(lTag)) {
    expect_null(lRecord$release, label = "the release the records name, with no such tag on GitHub")
  } else if (bHeld) {
    expect_identical(chrDiffersFromTag(lTag, lCopied), character(0), label = paste0("copied files that differ from bio.viz's tag v", strBioViz))
    if (!is.null(lRecord$release)) expect_identical(lRecord$release$commit, lTag$commit)
  } else {
    succeed("a development version carries bio.viz's dev, which has moved on from the tag")
  }
})

test_that("a tag question nothing answers is an error with what git said, never read as no such tag, and a tag that does not exist is no lines and no error (#67)", {
  skip_if_not(bSourceTree(), "a tag is asked of GitHub, from the source tree only")
  # An address nothing answers at: the name is reserved, so it resolves
  # nowhere and git stops at once. Asked once, so the test does not wait
  # between tries. The error carries what git said, which names the address.
  strNowhere <- "https://nohost.invalid/jwildfire/gsm.bio.git"
  cndError <- tryCatch(chrRemoteTag(strNowhere, "v0.1.0", nTries = 1L), error = function(cndError) cndError)
  expect_s3_class(cndError, "error")
  expect_false(is.character(cndError), label = "an answer where git could not ask")
  expect_match(conditionMessage(cndError), "git ls-remote --tags did not run: ", fixed = TRUE)
  expect_match(conditionMessage(cndError), "nohost.invalid", fixed = TRUE)
  # A repository that answers: a tag it does not have is no lines, with no
  # error, and a tag it has is one line, the tag's.
  strHere <- "https://github.com/jwildfire/gsm.bio.git"
  expect_identical(chrRemoteTag(strHere, "v0.0.0-no-such-tag"), character(0))
  chrReleased <- chrRemoteTag(strHere, "v0.1.0")
  expect_length(chrReleased, 1L)
  expect_match(chrReleased, "^[0-9a-f]{40}\\trefs/tags/v0\\.1\\.0$")
})
