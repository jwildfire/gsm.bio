# Guards for the repository files a session and a reader meet first (#1):
# CLAUDE.md, README.md, NEWS.md and the two workflows.
#
# None of these but NEWS.md is in the built package. In the source tree, where
# devtools::test() runs, a missing file is a failure; under R CMD check, where
# the tests run against the installed package, there is no source tree and the
# tests skip (bSourceTree() is in helper-source-tree.R). A file that is merely
# absent must never read as a pass.

chrRepositoryFile <- function(...) {
  skip_if_not(bSourceTree(), paste(file.path(...), "is not part of the built package"))
  strPath <- testthat::test_path("..", "..", ...)
  expect_true(file.exists(strPath), label = paste(file.path(...), "exists"))
  if (!file.exists(strPath)) {
    return(character(0))
  }
  readLines(strPath, warn = FALSE)
}

test_that("CLAUDE.md carries the Standards block and the commands a session runs (#1)", {
  chrLines <- chrRepositoryFile("CLAUDE.md")
  strText <- paste(chrLines, collapse = "\n")

  expect_true(any(grepl("^# Standards$", chrLines)))
  expect_match(strText, "The obot program's standards are mandatory here", fixed = TRUE)
  expect_match(strText, "jwildfire/obot.roadmap `docs/`", fixed = TRUE)
  expect_match(strText, "/requirement-session", fixed = TRUE)
  for (strCommand in c("devtools::document()", "devtools::test()", "devtools::check()", "pkgdown::build_site()")) {
    expect_match(strText, strCommand, fixed = TRUE)
  }
})

# The versions NEWS.md heads its sections with, newest first, and whether each
# is still marked Upcoming.
dfNewsVersions <- function(chrLines) {
  chrHeadings <- grep("^# gsm\\.bio v", chrLines, value = TRUE)
  data.frame(
    version = sub("^# gsm\\.bio v([0-9.]+).*$", "\\1", chrHeadings),
    upcoming = grepl(" (Upcoming)", chrHeadings, fixed = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("README.md gives the install lines for the newest release, or the release this tree prepares, and for the integration branch (#1, #27, #29, #48)", {
  strText <- paste(chrRepositoryFile("README.md"), collapse = "\n")
  dfVersions <- dfNewsVersions(chrRepositoryFile("NEWS.md"))
  strPackage <- as.character(utils::packageVersion("gsm.bio"))
  # The release this tree prepares: NEWS's newest section, still Upcoming, is
  # the package's own version. Its tag is made from main when it ships, so the
  # README tagged with it already installs it. Otherwise the release line is
  # the newest released version, whose tag is on GitHub.
  bPreparing <- isTRUE(dfVersions$upcoming[1]) && identical(dfVersions$version[1], strPackage)
  strRelease <- if (bPreparing) strPackage else dfVersions$version[!dfVersions$upcoming][1]
  expect_false(is.na(strRelease), label = "NEWS.md has a released version")
  expect_match(strText, sprintf('remotes::install_github("jwildfire/gsm.bio@v%s")', strRelease), fixed = TRUE)
  expect_match(strText, 'remotes::install_github("jwildfire/gsm.bio@dev")', fixed = TRUE)
  expect_false(grepl("is tagged", strText, fixed = TRUE), label = "a sentence that waits for the tag")
  chrTags <- suppressWarnings(system2(
    "git", c("ls-remote", "--tags", "https://github.com/jwildfire/gsm.bio.git", sprintf("refs/tags/v%s", strRelease)),
    stdout = TRUE, stderr = FALSE
  ))
  if (bPreparing) {
    expect_lte(length(chrTags), 1L, label = sprintf("tag v%s on GitHub, once at most", strRelease))
  } else {
    expect_identical(length(chrTags), 1L, label = sprintf("tag v%s on GitHub", strRelease))
  }
})

test_that("NEWS.md opens with the v0.2.0 section, Upcoming until its tag, above the v0.1.0 release (#1, #29, #44)", {
  strPath <- if (bSourceTree()) {
    testthat::test_path("..", "..", "NEWS.md")
  } else {
    system.file("NEWS.md", package = "gsm.bio")
  }
  expect_true(nzchar(strPath) && file.exists(strPath), label = "NEWS.md exists")
  skip_if_not(nzchar(strPath) && file.exists(strPath), "NEWS.md is missing")

  chrHeadings <- grep("^# ", readLines(strPath, warn = FALSE), value = TRUE)
  # The release step drops "(Upcoming)" when it publishes the tag.
  expect_true(chrHeadings[1] %in% c("# gsm.bio v0.2.0 (Upcoming)", "# gsm.bio v0.2.0"), label = chrHeadings[1])
  expect_identical(chrHeadings[2], "# gsm.bio v0.1.0")
  # Only the newest section is ever Upcoming.
  expect_false(any(grepl("(Upcoming)", chrHeadings[-1], fixed = TRUE)))
})

test_that("the R CMD check workflow has one job named R-CMD-check that fails on notes (#1)", {
  chrLines <- chrRepositoryFile(".github", "workflows", "R-CMD-check.yaml")

  # The dev ruleset requires a check called exactly "R-CMD-check": one job, no
  # matrix, so nothing is appended to the name.
  chrYaml <- grep("^\\s*#", chrLines, value = TRUE, invert = TRUE)
  chrJobs <- chrYaml[seq_along(chrYaml) > match("jobs:", chrYaml)]
  expect_identical(grep("^  [A-Za-z0-9_-]+:$", chrJobs, value = TRUE), "  R-CMD-check:")
  expect_true(any(grepl("^    name: R-CMD-check$", chrJobs)))
  expect_false(any(grepl("strategy:|matrix", chrJobs)))
  expect_true(any(grepl("^    runs-on: ubuntu-latest$", chrLines)))
  expect_true(any(grepl("error-on: '\"note\"'", chrLines, fixed = TRUE)))
})

test_that("the R CMD check job also runs the suite from the source tree, where nothing may skip (#2)", {
  strText <- paste(chrRepositoryFile(".github", "workflows", "R-CMD-check.yaml"), collapse = "\n")

  # R CMD check cannot see data-raw/, so the test that reruns the data script
  # would never run on CI's R version without this step.
  expect_match(strText, "testthat::test_local(stop_on_failure = TRUE)", fixed = TRUE)
  expect_match(strText, "if (any(dfTests$skipped))", fixed = TRUE)
})

test_that("both workflows run again when a draft pull request is marked ready (#1)", {
  # Auto-merge can only be switched on while a required check is pending, so
  # marking a draft ready has to start the checks again.
  for (strWorkflow in c("R-CMD-check.yaml", "pkgdown.yaml")) {
    strText <- paste(chrRepositoryFile(".github", "workflows", strWorkflow), collapse = "\n")
    expect_match(
      strText,
      "  pull_request:\n    branches: [dev]\n",
      fixed = TRUE, label = paste(strWorkflow, "pull_request trigger on dev")
    )
    expect_match(
      strText,
      "    types: [opened, synchronize, reopened, ready_for_review]\n",
      fixed = TRUE, label = paste(strWorkflow, "pull_request types")
    )
  }
})

test_that("the pkgdown workflow may write, and deploys to gh-pages on pushes only (#1)", {
  chrLines <- chrRepositoryFile(".github", "workflows", "pkgdown.yaml")
  strText <- paste(chrLines, collapse = "\n")

  expect_match(strText, "permissions:\n  contents: write", fixed = TRUE)
  expect_match(strText, "if: github.event_name != 'pull_request'", fixed = TRUE)
  expect_match(strText, "branch: gh-pages", fixed = TRUE)
  expect_match(strText, "build_site_github_pages", fixed = TRUE)
})

test_that("the check job has pandoc, which the proof of the saved page needs (#9)", {
  chrLines <- chrRepositoryFile(".github", "workflows", "R-CMD-check.yaml")
  # Before R is set up, so both the check and the source-tree step have it.
  iPandoc <- grep("uses: r-lib/actions/setup-pandoc@v2", chrLines, fixed = TRUE)
  iCheck <- grep("uses: r-lib/actions/check-r-package@v2", chrLines, fixed = TRUE)
  expect_identical(length(iPandoc), 1L)
  expect_true(length(iCheck) == 1L && iPandoc < iCheck)
})

test_that("CLAUDE.md gives the one command that copies bio.viz's files again, and the script is there (#9, #19)", {
  strText <- paste(chrRepositoryFile("CLAUDE.md"), collapse = "\n")
  expect_match(strText, "Rscript data-raw/vendor-bio-viz.R", fixed = TRUE)
  for (strScript in c("vendor-bio-viz.R", "core-frames.mjs", "filter-states.mjs")) {
    chrScript <- chrRepositoryFile("data-raw", strScript)
    expect_gt(length(chrScript), 0)
  }
  # The script writes the three things the tests read.
  strScript <- paste(chrRepositoryFile("data-raw", "vendor-bio-viz.R"), collapse = "\n")
  for (strWritten in c('file.path("inst", "htmlwidgets", "lib")', 'file.path("tests", "testthat", "fixtures", "bio.viz")', "core-frames.mjs", "filter-states.mjs")) {
    expect_match(strScript, strWritten, fixed = TRUE)
  }
})
