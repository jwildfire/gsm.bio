# Guards for the repository files a session and a reader meet first (#1):
# CLAUDE.md, README.md, NEWS.md and the two workflows.
#
# None of these but NEWS.md is in the built package. In the source tree, where
# devtools::test() runs, a missing file is a failure; under R CMD check, where
# the tests run against the installed package, there is no source tree and the
# tests skip. A file that is merely absent must never read as a pass.

bSourceTree <- function() {
  file.exists(testthat::test_path("..", "..", "DESCRIPTION"))
}

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

test_that("README.md gives the install line for the integration branch (#1)", {
  strText <- paste(chrRepositoryFile("README.md"), collapse = "\n")
  expect_match(strText, 'remotes::install_github("jwildfire/gsm.bio@dev")', fixed = TRUE)
})

test_that("NEWS.md opens with the upcoming v0.1.0 section (#1)", {
  strPath <- if (bSourceTree()) {
    testthat::test_path("..", "..", "NEWS.md")
  } else {
    system.file("NEWS.md", package = "gsm.bio")
  }
  expect_true(nzchar(strPath) && file.exists(strPath), label = "NEWS.md exists")
  skip_if_not(nzchar(strPath) && file.exists(strPath), "NEWS.md is missing")

  chrHeadings <- grep("^# ", readLines(strPath, warn = FALSE), value = TRUE)
  expect_identical(chrHeadings[1], "# gsm.bio v0.1.0 (Upcoming)")
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

test_that("the pkgdown workflow may write, and deploys to gh-pages on pushes only (#1)", {
  chrLines <- chrRepositoryFile(".github", "workflows", "pkgdown.yaml")
  strText <- paste(chrLines, collapse = "\n")

  expect_match(strText, "permissions:\n  contents: write", fixed = TRUE)
  expect_match(strText, "if: github.event_name != 'pull_request'", fixed = TRUE)
  expect_match(strText, "branch: gh-pages", fixed = TRUE)
  expect_match(strText, "build_site_github_pages", fixed = TRUE)
})
