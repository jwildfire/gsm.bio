# Writes what bio.viz's own reader makes of the specifications the tests read,
# for a session with no node:
#
# - tests/testthat/fixtures/specifications/bioviz-reader.json, its reads of
#   lSpecCases(), and of lSpecDepthCases() handed to it as objects, each in
#   full;
# - tests/testthat/fixtures/specifications/bioviz-fuzz.json, its reads of
#   lFuzzCases(), every setting of every chart given each of a fixed list of
#   values, in brief.
#
#   Rscript data-raw/specifications/record-readers.R
#
# Run from the repository root, with node, after the bundle is copied again
# (data-raw/vendor-bio-viz.R). The reader is run from the copied bundle by
# tests/testthat/fixtures/specifications/bioviz-reader.mjs, and the cases are
# the tests' own (tests/testthat/helper-specifications.R). The tests compare
# both recordings with the reader run live wherever node is installed.
if (!nzchar(Sys.which("node"))) stop("record-readers.R runs bio.viz's reader in node, which is not installed")
suppressMessages(devtools::load_all(quiet = TRUE))
library(testthat)
for (strHelper in list.files("tests/testthat", "^helper", full.names = TRUE)) sys.source(strHelper, envir = globalenv())
strWas <- setwd("tests/testthat")
Sys.setenv(TESTTHAT = "true")
lRead <- lBioVizAllLive()
lCases <- lFuzzCases()
lFuzz <- lFuzzRecord(lCases, lFuzzTheirs(lCases, bNode = TRUE))
setwd(strWas)
strFolder <- file.path("tests", "testthat", "fixtures", "specifications")
writeLines(
  as.character(jsonlite::toJSON(unname(lRead), auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)),
  file.path(strFolder, "bioviz-reader.json"),
  useBytes = TRUE
)
# One line for each read, so the file stays small and a change shows as the
# lines it changed.
chrHead <- vapply(c("what", "written_by", "cases"), function(strKey) {
  paste0("  ", jsonlite::toJSON(strKey, auto_unbox = TRUE), ": ", jsonlite::toJSON(lFuzz[[strKey]], auto_unbox = TRUE), ",")
}, character(1))
writeLines(c(
  "{",
  chrHead,
  paste0("  \"sentences\": ", jsonlite::toJSON(lFuzz$sentences), ","),
  "  \"reads\": [",
  paste0("    ", vapply(lFuzz$reads, function(iRead) as.character(jsonlite::toJSON(iRead)), character(1)), c(rep(",", length(lFuzz$reads) - 1L), "")),
  "  ]",
  "}"
), file.path(strFolder, "bioviz-fuzz.json"), useBytes = TRUE)
cat(length(lRead), "reads and", length(lFuzz$reads), "fuzz reads recorded\n")
