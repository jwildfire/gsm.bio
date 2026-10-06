#!/usr/bin/env Rscript
# Copies what gsm.bio takes from bio.viz, byte for byte, and records where each
# file came from: the bio.viz commit, and a checksum and a size per file.
#
#   Rscript data-raw/vendor-bio-viz.R               the head of bio.viz's `dev` branch
#   Rscript data-raw/vendor-bio-viz.R --ref <ref>   another branch, or one commit
#
# Run from the repository root, by hand, when bio.viz's `dev` moves. It needs
# git, a network connection and node; the output is committed. Four things are
# copied or written:
#
# 1. The two bundles the widgets load, into inst/htmlwidgets/lib/, with their
#    record, SOURCE.json, beside them and each widget's dependency file,
#    inst/htmlwidgets/Widget_<Chart>.yaml, written to match:
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
# 1b. The JSON Schema of bio.viz's chart specifications, into
#    inst/specification/, with its record: Run_Specifications() reads each
#    chart's settings by name from it.
#
# 2. The fixtures the tests hold R to, into tests/testthat/fixtures/bio.viz/,
#    with their record, a set per chart: the rows bio.viz's own core wrote for
#    panels of the chart's demo, and the requests the chart makes for them with
#    what desktop R answered.
#
# 3. tests/testthat/fixtures/core-frames/frames.json, written by running the
#    core of the bundle just copied on the cases in cases.json beside it
#    (data-raw/core-frames.mjs): the value types and the tables bio.viz's own
#    fixtures do not cover.
#
# 4. tests/testthat/fixtures/filter-states/states.json, written by running the
#    safety.viz kit of the bundle just copied on the filter settings in
#    cases.json beside it (data-raw/filter-states.mjs): what each filter opens
#    on, which R's Chart_Filters() is held to.
# 5. The release: when bio.viz's tag for the version copied, v<version>, holds
#    every copied file byte for byte, the three records name it as `release`
#    beside the commit copied from.
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

# Whether the commit is on bio.viz's dev branch: dev is it, or has it among its
# ancestors. A copy from a branch that is not merged yet says so in its record,
# and is copied again from dev once the branch lands.
bMergedToDev <- local({
  strFile <- tempfile()
  on.exit(unlink(strFile))
  utils::download.file(
    sprintf("https://api.github.com/repos/jwildfire/bio.viz/compare/dev...%s", strCommit), strFile,
    quiet = TRUE
  )
  jsonlite::fromJSON(strFile)$status %in% c("identical", "behind")
})
strUnmergedNote <- if (bMergedToDev) {
  NULL
} else {
  paste0(
    "Copied from bio.viz's branch ", strRef, ", which is not merged to bio.viz's dev branch at this commit. ",
    "When it lands there, copy again from dev (Rscript data-raw/vendor-bio-viz.R) and rerun the tests."
  )
}

# One file of bio.viz at the commit, or at another, as bytes.
ReadAt <- function(strPath, strAt = strCommit) {
  strUrl <- sprintf("https://raw.githubusercontent.com/jwildfire/bio.viz/%s/%s", strAt, strPath)
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
  # A member that is not set is left out.
  lValue <- Filter(Negate(is.null), lValue)
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
  what = "The JavaScript bundles the widgets load, copied from bio.viz byte for byte.",
  copied_by = "data-raw/vendor-bio-viz.R",
  repository = strRepository,
  ref = strRef,
  commit = strCommit,
  merged_to_dev = bMergedToDev,
  note = strUnmergedNote,
  files = lBundles,
  safety_viz = list(
    stand_in = TRUE,
    note = paste(
      "The copy of safety.viz's bundle is a stand-in, to be replaced. The design has gsm.bio take",
      "safety.viz's bundle from gsm.safety. Until gsm.safety carries a safety.viz bundle with the kit,",
      "the widget carries the copy bio.viz itself builds its chart from, taken from bio.viz at the",
      "commit above. Where bio.viz took it from is bio.viz's own record, carried here whole as",
      "`bio_viz_record`: read its `ref`, `commit` and `merged_to_dev`."
    ),
    bio_viz_record_file = "site/vendor/safety.viz/SOURCE.json",
    bio_viz_record = lKit
  )
), file.path(strLib, "SOURCE.json"))

# Every widget's dependencies, by the versions just copied. safety.viz first:
# a chart is built from its kit. Its dependency is named as gsm.safety names its
# own, so a page holding widgets of both packages loads one copy of safety.viz.
# Last, the package's own script, which every binding is made with
# (inst/htmlwidgets/shared/, not copied from anywhere).
chrWidgets <- c(
  "Widget_GroupComparison", "Widget_AssociationScatter", "Widget_CorrelationMatrix", "Widget_BiomarkerScreen",
  "Widget_CrossTab", "Widget_StratifiedSurvival"
)
strPackageVersion <- read.dcf("DESCRIPTION", fields = "Version")[[1]]
for (strWidget in chrWidgets) {
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
    "    script: 'bio.viz.js'",
    "  - name: gsm-bio-widget",
    sprintf("    version: %s", strPackageVersion),
    "    src: 'htmlwidgets/shared'",
    "    script: 'gsm.bio.widget.js'"
  ), file.path("inst", "htmlwidgets", paste0(strWidget, ".yaml")))
}

# ---- 1b. The specification schema ----------------------------------------------

# The format bio.viz writes a chart's specification in (bio.viz#68), with each
# chart's settings by name and default: Run_Specifications() reads a
# specification by it.
strSchemaDir <- file.path("inst", "specification")
strSchemaSource <- "src/data/specification.schema.json"
lSchema <- Place(ReadAt(strSchemaSource), strSchemaDir, "specification.schema.json", strSchemaSource)
WriteJson(list(
  what = "The JSON Schema of bio.viz's chart specifications, copied from bio.viz byte for byte.",
  copied_by = "data-raw/vendor-bio-viz.R",
  repository = strRepository,
  ref = strRef,
  commit = strCommit,
  merged_to_dev = bMergedToDev,
  note = strUnmergedNote,
  files = list(lSchema)
), file.path(strSchemaDir, "SOURCE.json"))

# ---- 2. The fixtures -----------------------------------------------------------

strFixtures <- file.path("tests", "testthat", "fixtures", "bio.viz")
unlink(strFixtures, recursive = TRUE)

# One set per chart: the rows the chart's own code wrote for panels of its demo
# (a folder, with its own record), and beside it the request the chart makes for
# each with what desktop R answered.
chrFixtureSets <- c("group-statistics", "association-statistics", "matrix-statistics", "screen-statistics")
lFixtures <- list()
for (strSet in chrFixtureSets) {
  strFrames <- paste0("tests/fixtures/", strSet)
  lFramesRecord <- ReadJsonAt(paste0(strFrames, "/SOURCE.json"))
  chrFrameFiles <- c("SOURCE.json", vapply(lFramesRecord$files, function(lFile) lFile$file, character(1)))
  lFixtures <- c(lFixtures, lapply(chrFrameFiles, function(strFile) {
    strSource <- paste(strFrames, strFile, sep = "/")
    Place(ReadAt(strSource), strFixtures, file.path(strSet, strFile), strSource)
  }))
  strAnswers <- paste0(strSet, "-r.json")
  lFixtures <- c(lFixtures, list(Place(
    ReadAt(paste0("tests/fixtures/", strAnswers)), strFixtures, strAnswers, paste0("tests/fixtures/", strAnswers)
  )))
}

# And single files, with no rows of their own: what desktop R makes of the
# shared cut rule (cut-r.json), and the cross-tabulation's tables with R's
# answers (cross-tab-r.json), and the stratified survival chart's curves with R's
# answers (stratified-survival-r.json), each worked out from the study by
# bio.viz's tools.
for (strFile in c("cut-r.json", "cross-tab-r.json", "stratified-survival-r.json")) {
  strSource <- paste0("tests/fixtures/", strFile)
  lFixtures <- c(lFixtures, list(Place(ReadAt(strSource), strFixtures, strFile, strSource)))
}

WriteJson(list(
  what = paste(
    "Fixtures copied from bio.viz byte for byte, a set per chart: the rows bio.viz's own core wrote",
    "for panels of the chart's demo (a folder), and the request the chart makes for each with what",
    "desktop R answered (the JSON file beside it)."
  ),
  copied_by = "data-raw/vendor-bio-viz.R",
  repository = strRepository,
  ref = strRef,
  commit = strCommit,
  merged_to_dev = bMergedToDev,
  note = strUnmergedNote,
  files = lFixtures
), file.path(strFixtures, "SOURCE.json"))

# ---- 3. The frames the copied bundle's own core writes ---------------------------

iStatus <- system2("node", c(file.path("data-raw", "core-frames.mjs")))
if (!identical(iStatus, 0L)) {
  stop("data-raw/core-frames.mjs did not run: the bundles were copied, and frames.json is not theirs yet")
}

# ---- 4. The filter states the copied bundle's own kit opens on -----------------

iStatus <- system2("node", c(file.path("data-raw", "filter-states.mjs")))
if (!identical(iStatus, 0L)) {
  stop("data-raw/filter-states.mjs did not run: the bundles were copied, and states.json is not theirs yet")
}

# ---- 5. The release the copy is the same as -----------------------------------

# bio.viz's tag for the version copied, v<version>, when it exists and every
# file copied is byte for byte the same at it: the three records then name the
# release beside the commit they were copied from, which stays dev's.
strTag <- paste0("v", strBioVizVersion)
chrTagged <- system2(
  "git", c("ls-remote", "--tags", paste0(strRepository, ".git"), paste0("refs/tags/", strTag), paste0("refs/tags/", strTag, "^{}")),
  stdout = TRUE
)
lRelease <- NULL
if (length(chrTagged) > 0L) {
  # An annotated tag's commit is the line marked ^{}; a light one's is its own.
  strPeeled <- grep("\\^\\{\\}$", chrTagged, value = TRUE)
  strTagCommit <- sub("\\s.*$", "", if (length(strPeeled) > 0L) strPeeled[1] else chrTagged[1])
  lCopied <- c(lBundles, list(lSchema), lFixtures)
  # A file the tag does not have is a file that differs: dev has moved on.
  bSame <- all(vapply(lCopied, function(lFile) {
    rawTagged <- tryCatch(suppressWarnings(ReadAt(lFile$source, strTagCommit)), error = function(e) NULL)
    !is.null(rawTagged) && identical(Sha256(rawTagged), lFile$sha256)
  }, logical(1)))
  if (bSame) {
    lRelease <- list(
      tag = strTag, commit = strTagCommit,
      note = paste0("Every file copied is byte for byte the same at bio.viz's release tag ", strTag, ".")
    )
  } else {
    message("bio.viz's tag ", strTag, " differs from the copy in at least one file, so the records name no release")
  }
}
if (!is.null(lRelease)) {
  for (strRecord in c(file.path(strLib, "SOURCE.json"), file.path(strSchemaDir, "SOURCE.json"), file.path(strFixtures, "SOURCE.json"))) {
    lRecord <- jsonlite::read_json(strRecord, simplifyVector = FALSE)
    iAfter <- match("merged_to_dev", names(lRecord))
    lRecord <- c(lRecord[seq_len(iAfter)], list(release = lRelease), lRecord[-seq_len(iAfter)])
    WriteJson(lRecord, strRecord)
  }
}

cat(sprintf(
  "Copied bio.viz %s and safety.viz %s from bio.viz at %s, with %d fixture files\n",
  strBioVizVersion, strSafetyVizVersion, substr(strCommit, 1, 7), length(lFixtures)
))
