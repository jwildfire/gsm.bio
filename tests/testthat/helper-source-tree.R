# Where the suite is running. devtools::test() and CI's source-tree step run it
# in the repository, where data-raw/ and the repository files exist. R CMD check
# runs it against the installed package, where they do not.

bSourceTree <- function() {
  file.exists(testthat::test_path("..", "..", "DESCRIPTION"))
}

strSourceRoot <- function() {
  normalizePath(testthat::test_path("..", ".."))
}
