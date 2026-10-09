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

test_that("README.md gives the install lines for the newest release, or the release this tree prepares, and for the integration branch (#1, #27, #29, #48, #62)", {
  strText <- paste(chrRepositoryFile("README.md"), collapse = "\n")
  dfVersions <- dfNewsVersions(chrRepositoryFile("NEWS.md"))
  strPackage <- as.character(utils::packageVersion("gsm.bio"))
  # The release this tree prepares: NEWS's newest section, still Upcoming, is
  # the package's own version. Its tag is made from main when it ships, so the
  # README tagged with it already installs it. Otherwise the release line is
  # the newest released version, whose tag is on GitHub.
  bPreparing <- isTRUE(dfVersions$upcoming[1]) && identical(dfVersions$version[1], strPackage)
  # A development version prepares no release: after a tag, the release line
  # is the release, and its tag must be there (#50).
  if (grepl("\\.9000$", strPackage)) expect_false(bPreparing, label = "a development version preparing a release")
  strRelease <- if (bPreparing) strPackage else dfVersions$version[!dfVersions$upcoming][1]
  expect_false(is.na(strRelease), label = "NEWS.md has a released version")
  expect_match(strText, sprintf('remotes::install_github("jwildfire/gsm.bio@v%s")', strRelease), fixed = TRUE)
  expect_match(strText, 'remotes::install_github("jwildfire/gsm.bio@dev")', fixed = TRUE)
  expect_false(grepl("is tagged", strText, fixed = TRUE), label = "a sentence that waits for the tag")
  # The tag, asked of GitHub with git. When GitHub cannot be asked the test
  # stops with what git said: no answer is not "no tag", whichever is
  # expected (#62).
  chrTags <- chrRemoteTag("https://github.com/jwildfire/gsm.bio.git", sprintf("v%s", strRelease))
  if (bPreparing) {
    expect_lte(length(chrTags), 1L, label = sprintf("tag v%s on GitHub, once at most", strRelease))
  } else {
    expect_identical(length(chrTags), 1L, label = sprintf("tag v%s on GitHub", strRelease))
  }
})

test_that("NEWS.md opens with the v0.4.0 section, Upcoming until its tag, above the v0.3.0, v0.2.0 and v0.1.0 releases (#1, #29, #44, #50, #59, #69, #79)", {
  strPath <- if (bSourceTree()) {
    testthat::test_path("..", "..", "NEWS.md")
  } else {
    system.file("NEWS.md", package = "gsm.bio")
  }
  expect_true(nzchar(strPath) && file.exists(strPath), label = "NEWS.md exists")
  skip_if_not(nzchar(strPath) && file.exists(strPath), "NEWS.md is missing")

  chrHeadings <- grep("^# ", readLines(strPath, warn = FALSE), value = TRUE)
  # The release step drops "(Upcoming)" when it publishes the tag.
  expect_true(chrHeadings[1] %in% c("# gsm.bio v0.4.0 (Upcoming)", "# gsm.bio v0.4.0"), label = chrHeadings[1])
  expect_identical(chrHeadings[2:4], c("# gsm.bio v0.3.0", "# gsm.bio v0.2.0", "# gsm.bio v0.1.0"))
  # Only the newest section is ever Upcoming.
  expect_false(any(grepl("(Upcoming)", chrHeadings[-1], fixed = TRUE)))
})

test_that("the newest release's notes in NEWS.md, and the section collecting the next release, keep the release notes' shape and length (#69)", {
  strPath <- if (bSourceTree()) {
    testthat::test_path("..", "..", "NEWS.md")
  } else {
    system.file("NEWS.md", package = "gsm.bio")
  }
  expect_true(nzchar(strPath) && file.exists(strPath), label = "NEWS.md exists")
  skip_if_not(nzchar(strPath) && file.exists(strPath), "NEWS.md is missing")
  lSections <- lNewsSections(strPath)
  bUpcoming <- vapply(lSections, function(chrLines) grepl(" (Upcoming)", chrLines[1], fixed = TRUE), logical(1))

  # The newest released section: the whole shape, and every limit.
  iReleased <- which(!bUpcoming)[1]
  lReleased <- lNewsCheck(lSections[[iReleased]])
  expect_identical(lReleased$problems, character(0), label = paste0("what is wrong with the v", names(lSections)[iReleased], " notes"))
  expect_lte(lReleased$total, lNewsLimits$section)
  expect_gt(lReleased$total, 0)
  # The section collecting the next release: the headings and the lengths as
  # it grows, and the whole shape once it is the release this tree prepares.
  for (iAt in which(bUpcoming)) {
    bPreparing <- identical(names(lSections)[iAt], as.character(utils::packageVersion("gsm.bio")))
    expect_identical(
      lNewsCheck(lSections[[iAt]], bReleased = bPreparing)$problems, character(0),
      label = paste0("what is wrong with the v", names(lSections)[iAt], " notes")
    )
  }

  # Words are counted as a reader meets them. A link counts as its text.
  expect_identical(nNewsWords("the [annotated demo](https://example.org/a/long/address) has it"), 5L)
  # The issue and pull-request links that close a bullet are not counted;
  # the same link inside a sentence is read, and so counted.
  expect_identical(nNewsWords("**A claim.** Two more. [obot.roadmap#367](https://e.org/367), [#53](https://e.org/53), PR [#57](https://e.org/57)"), 4L)
  expect_identical(nNewsWords("See [#64](https://e.org/64) for more."), 4L)
  # Marks and a dash alone are not words, and code counts as it reads.
  expect_identical(nNewsWords("**Bold**, `code_name()` \u2014 and _more_"), 4L)

  # The rules can fail. A release's notes written to the template pass, and
  # each limit refuses one thing more.
  strWords <- function(nWords) paste(rep("word", nWords), collapse = " ")
  chrNotes <- function(chrNew = "- **A claim.** More.", strIntro = "What the release is.", chrRest = character(0)) {
    c(
      "# gsm.bio v9.9.9", "", "**See it move:** the [demo](https://example.org/demo) has the detail.", "", strIntro, "",
      "## What's new", "", chrNew, chrRest, "", "## Tests and provenance", "", "Ten tests pass."
    )
  }
  Problems <- function(...) lNewsCheck(chrNotes(...))$problems
  expect_identical(Problems(), character(0))
  expect_identical(Problems(chrNew = paste0("- **A claim.** ", strWords(68L), " [#1](https://e.org/1)")), character(0))
  expect_match(Problems(chrNew = paste0("- **A claim.** ", strWords(69L), " [#1](https://e.org/1)")), "^71 words, limit 70, under \"What's new\"")
  expect_match(Problems(chrNew = rep("- **A claim.** More.", 7L)), "\"What's new\" has 7 bullets; the limit is 6.", fixed = TRUE)
  expect_match(Problems(strIntro = strWords(81L)), "The introduction is 81 words; the limit is 80.", fixed = TRUE)
  expect_match(Problems(chrRest = c("", "## Not in this release", "", "- **Something.** Later.")), "\"## Not in this release\" is not one of the headings", fixed = TRUE)
  expect_match(Problems(chrRest = c("", "## Also in this release", "", paste0("- **A fix.** ", strWords(59L)))), "^61 words, limit 60, under \"Also in this release\"")
  expect_match(Problems(chrNew = "- A claim with no bold."), "does not open with its claim in bold", fixed = TRUE)
  chrLong <- Problems(chrNew = rep(paste0("- **A claim.** ", strWords(60L)), 6L), chrRest = c("", "## Also in this release", "", rep(paste0("- **A fix.** ", strWords(50L)), 5L)))
  expect_match(chrLong, "^The section is [0-9]+ words; the limit is 600\\.")
  # A release's notes open on the line to the demo page; a section still
  # collecting work need not yet.
  chrNoDemo <- chrNotes()[-(3:4)]
  expect_match(lNewsCheck(chrNoDemo)$problems[1], "does not open with a \"**See it move:**\" line", fixed = TRUE)
  expect_identical(lNewsCheck(c("# gsm.bio v9.9.9 (Upcoming)", "", "The next version."), bReleased = FALSE)$problems, character(0))
  expect_identical(lNewsCheck(chrNoDemo[-(3:4)], bReleased = FALSE)$problems, character(0))
  # The sections released before the limits are left as they were published,
  # and are over them: the check finds that in real notes.
  expect_gt(length(lNewsCheck(lSections[["0.2.0"]])$problems), 0L)
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
