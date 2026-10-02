#!/usr/bin/env Rscript
# Copies what gsm.bio takes from bio.viz, byte for byte, and records where each
# file came from: the bio.viz commit, and a checksum and a size per file.
#
#   Rscript data-raw/vendor-bio-viz.R               the head of bio.viz's `dev` branch
#   Rscript data-raw/vendor-bio-viz.R --ref <ref>   another branch, or one commit
#
# Run from the repository root, by hand, when bio.viz's `dev` moves. It needs
# git, a network connection and node; the output is committed. Three things are
# copied or written:
#
# 1. The two bundles Widget_GroupComparison() loads, into inst/htmlwidgets/lib/,
#    with their record, SOURCE.json, beside them and the widget's dependency
#    file, inst/htmlwidgets/Widget_GroupComparison.yaml, written to match:
#
#      bio.viz's script-tag bundle       dist/bio.viz-<version>/bio.viz.js
#      the copy of safety.viz's bundle   site/vendor/safety.viz/safety.viz.js
#      bio.viz itself uses
#
#    The safety.viz copy is a stand-in. The design has gsm.bio take safety.viz's
#    bundle from gsm.safety; until gsm.safety carries a safety.viz bundle with
#    the kit, the widget carries the copy bio.viz builds its chart from. bio.viz
#    keeps its own record of where that copy came from, and that record is
#    carried into ours whole.
#
# 2. The fixtures the tests hold R to, into tests/testthat/fixtures/bio.viz/,
#    with their record: the rows bio.viz's own core wrote for thirteen panels of
#    its demo chart, and the requests the chart makes for them with what desktop
#    R answered.
#
# 3. tests/testthat/fixtures/core-frames/frames.json, written by running the
#    core of the bundle just copied on the cases in cases.json beside it
#    (data-raw/core-frames.mjs): the value types and the tables bio.viz's own
#    fixtures do not cover.
#
# tests/testthat/test-vendored.R fails the suite when a copied file and its
# record disagree, or when the three records name different bundles.

chrArgs <- commandArgs(trailingOnly = TRUE)
strRef <- "dev"
if ("--ref" %in% chrArgs) {
  strRef <- chrArgs[match("--ref", chrArgs) + 1L]
  if (is.na(strRef) || !nzchar(strRef)) stop("--ref needs a branch or a commit")
}
if (!file.exists("DESCRIPTION") || !dir.exists("data-raw")) stop("run from the repository root")
for (strPackage in c("digest", "jsonlite")) {
  if (!requireNamespace(strPackage, quietly = TRUE)) stop("this script needs the ", strPackage, " package")
}

strRepository <- "https://github.com/jwildfire/bio.viz"

# ---- The commit ----------------------------------------------------------------

strCommit <- if (grepl("^[0-9a-f]{40}$", strRef)) {
  strRef
} else {
  chrRemote <- system2("git", c("ls-remote", paste0(strRepository, ".git"), paste0("refs/heads/", strRef)), stdout = TRUE)
  if (length(chrRemote) != 1L) stop("bio.viz has no branch named ", strRef)
  sub("\\s.*$", "", chrRemote)
}
if (!grepl("^[0-9a-f]{40}$", strCommit)) stop("could not resolve ", strRef, " to a commit")

# One file of bio.viz at the commit, as bytes.
ReadAt <- function(strPath) {
  strUrl <- sprintf("https://raw.githubusercontent.com/jwildfire/bio.viz/%s/%s", strCommit, strPath)
  strFile <- tempfile()
  on.exit(unlink(strFile))
  utils::download.file(strUrl, strFile, mode = "wb", quiet = TRUE)
  readBin(strFile, "raw", n = file.size(strFile))
}

ReadJsonAt <- function(strPath) {
  jsonlite::fromJSON(rawToChar(ReadAt(strPath)), simplifyVector = FALSE)
}

Sha256 <- function(rawBytes) digest::digest(rawBytes, algo = "sha256", serialize = FALSE)

# Writes the bytes and returns the file's line of a record.
Place <- function(rawBytes, strRoot, strFile, strSource, ...) {
  strPath <- file.path(strRoot, strFile)
  dir.create(dirname(strPath), recursive = TRUE, showWarnings = FALSE)
  writeBin(rawBytes, strPath)
  c(list(file = strFile, source = strSource), list(...), list(sha256 = Sha256(rawBytes), bytes = length(rawBytes)))
}

WriteJson <- function(lValue, strPath) {
  writeLines(jsonlite::toJSON(lValue, auto_unbox = TRUE, pretty = TRUE, null = "null"), strPath, useBytes = TRUE)
}

# ---- 1. The bundles ------------------------------------------------------------

strBioVizVersion <- ReadJsonAt("package.json")$version
lKit <- ReadJsonAt("site/vendor/safety.viz/SOURCE.json")
strSafetyVizVersion <- lKit$version

strLib <- file.path("inst", "htmlwidgets", "lib")
# The folder holds what this run writes and nothing left over from another.
unlink(strLib, recursive = TRUE)

strBioVizSource <- sprintf("dist/bio.viz-%s/bio.viz.js", strBioVizVersion)
rawBioViz <- ReadAt(strBioVizSource)
rawSafetyViz <- ReadAt("site/vendor/safety.viz/safety.viz.js")
# bio.viz's own record of its copy must describe the bytes it holds.
if (!identical(Sha256(rawSafetyViz), lKit$files[[1]]$sha256)) {
  stop("bio.viz's copy of safety.viz's bundle does not match bio.viz's own record of it")
}

lBundles <- list(
  Place(
    rawBioViz, strLib, sprintf("bio.viz-%s/bio.viz.js", strBioVizVersion), strBioVizSource,
    library = "bio.viz", version = strBioVizVersion
  ),
  Place(
    rawSafetyViz, strLib, sprintf("safety.viz-%s/safety.viz.js", strSafetyVizVersion),
    "site/vendor/safety.viz/safety.viz.js",
    library = "safety.viz", version = strSafetyVizVersion
  )
)

WriteJson(list(
  what = "The JavaScript bundles Widget_GroupComparison() loads, copied from bio.viz byte for byte.",
  copied_by = "data-raw/vendor-bio-viz.R",
  repository = strRepository,
  ref = strRef,
  commit = strCommit,
  files = lBundles,
  safety_viz = list(
    stand_in = TRUE,
    note = paste(
      "The copy of safety.viz's bundle is a stand-in, to be replaced. The design has gsm.bio take",
      "safety.viz's bundle from gsm.safety. Until gsm.safety carries a safety.viz bundle with the kit,",
      "the widget carries the copy bio.viz itself builds its chart from, taken from bio.viz at the",
      "commit above. Where bio.viz took it from is bio.viz's own record, carried here whole as",
      "`bio_viz_record`: read `merged_to_dev` and `note` there."
    ),
    bio_viz_record_file = "site/vendor/safety.viz/SOURCE.json",
    bio_viz_record = lKit
  )
), file.path(strLib, "SOURCE.json"))

# The widget's dependencies, by the versions just copied. safety.viz first: the
# chart is built from its kit. Its dependency is named as gsm.safety names its
# own, so a page holding widgets of both packages loads one copy of safety.viz.
writeLines(c(
  "# Written by data-raw/vendor-bio-viz.R: do not edit by hand.",
  "dependencies:",
  "  - name: safety-viz",
  sprintf("    version: %s", strSafetyVizVersion),
  sprintf("    src: 'htmlwidgets/lib/safety.viz-%s'", strSafetyVizVersion),
  "    script: 'safety.viz.js'",
  "  - name: bio-viz",
  sprintf("    version: %s", strBioVizVersion),
  sprintf("    src: 'htmlwidgets/lib/bio.viz-%s'", strBioVizVersion),
  "    script: 'bio.viz.js'"
), file.path("inst", "htmlwidgets", "Widget_GroupComparison.yaml"))

# ---- 2. The fixtures -----------------------------------------------------------

strFixtures <- file.path("tests", "testthat", "fixtures", "bio.viz")
unlink(strFixtures, recursive = TRUE)

strFrames <- "tests/fixtures/group-statistics"
lFramesRecord <- ReadJsonAt(file.path(strFrames, "SOURCE.json"))
chrFrameFiles <- c("SOURCE.json", vapply(lFramesRecord$files, function(lFile) lFile$file, character(1)))
lFixtures <- lapply(chrFrameFiles, function(strFile) {
  strSource <- paste(strFrames, strFile, sep = "/")
  Place(ReadAt(strSource), strFixtures, file.path("group-statistics", strFile), strSource)
})
lFixtures <- c(lFixtures, list(Place(
  ReadAt("tests/fixtures/group-statistics-r.json"), strFixtures,
  "group-statistics-r.json", "tests/fixtures/group-statistics-r.json"
)))

WriteJson(list(
  what = paste(
    "Fixtures copied from bio.viz byte for byte: the rows bio.viz's own core wrote for thirteen panels",
    "of its demo chart (group-statistics/), and the request the chart makes for each with what",
    "desktop R answered (group-statistics-r.json)."
  ),
  copied_by = "data-raw/vendor-bio-viz.R",
  repository = strRepository,
  ref = strRef,
  commit = strCommit,
  files = lFixtures
), file.path(strFixtures, "SOURCE.json"))

# ---- 3. The frames the copied bundle's own core writes ---------------------------

iStatus <- system2("node", c(file.path("data-raw", "core-frames.mjs")))
if (!identical(iStatus, 0L)) {
  stop("data-raw/core-frames.mjs did not run: the bundles were copied, and frames.json is not theirs yet")
}

cat(sprintf(
  "Copied bio.viz %s and safety.viz %s from bio.viz at %s, with %d fixture files\n",
  strBioVizVersion, strSafetyVizVersion, substr(strCommit, 1, 7), length(lFixtures)
))
