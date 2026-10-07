# What a release of gsm.bio may carry of bio.viz (#59), for
# tests/testthat/test-vendored.R.
#
# The two packages are released together, so gsm.bio's release is prepared
# before bio.viz's tag exists. The rule is in two parts:
#
# - What the copy has to be, read from the copied files alone
#   (chrReleaseRefusals): a release build of bio.viz, copied from a commit on
#   bio.viz's dev branch. A development build, whose footnote reads "bio.viz
#   <version> with development changes", is refused.
# - What it has to equal once bio.viz's tag for that version exists
#   (lBioVizTag, chrDiffersFromTag): every copied file, byte for byte. The tag
#   is asked of GitHub with git, in the source tree only.

strBioVizRepository <- "https://github.com/jwildfire/bio.viz.git"

# A release's version: three numbers. A development version has a fourth
# (0.3.0.9000), and a build of bio.viz off a release says so in words.
bReleaseVersion <- function(strVersion) {
  length(strVersion) == 1L && grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", strVersion)
}

# The lines of the copied bio.viz script-tag bundle.
chrBioVizBundle <- function() {
  readLines(strBioVizBundleFile(), warn = FALSE)
}

# The three lines bio.viz's build fixes what a bundle says of itself on
# (bio.viz, src/shared/titles.js, with scripts/build-lib.mjs putting
# package.json's `version` and `bioviz.development` in their places):
#
#   var VERSION = true ? "0.3.0" : "unbuilt";
#   var DEVELOPMENT = true ? false : true;
#   var VERSION_SAID = DEVELOPMENT ? `${VERSION} with development changes` : VERSION;
#
# A bundle that does not have each of them once, as written here, is not read:
# the helper stops, so a build it does not understand never passes as a release.
lBundleLines <- list(
  version = '^  var VERSION = true \\? "([^"]*)" : "unbuilt";$',
  development = "^  var DEVELOPMENT = true \\? (true|false) : true;$",
  said = "^  var VERSION_SAID = DEVELOPMENT \\? `\\$\\{VERSION\\} with development changes` : VERSION;$"
)

# What a bundle says of itself: its version, whether it is a development build,
# and the version as its footnote says it, after "bio.viz ".
lBundleSays <- function(chrLines) {
  Only <- function(strLine) {
    iFound <- grep(lBundleLines[[strLine]], chrLines)
    if (length(iFound) != 1L) {
      stop("the bio.viz bundle does not fix its ", strLine, " once, on the line this test reads", call. = FALSE)
    }
    sub(lBundleLines[[strLine]], "\\1", chrLines[iFound])
  }
  strVersion <- Only("version")
  bDevelopment <- identical(Only("development"), "true")
  Only("said")
  list(
    version = strVersion, development = bDevelopment,
    said = if (bDevelopment) paste(strVersion, "with development changes") else strVersion
  )
}

# The same bundle built the other way: its development flag set on or off, and
# nothing else changed. What the rule is shown a development build with.
chrBundleBuiltAs <- function(chrLines, bDevelopment) {
  iFlag <- grep(lBundleLines$development, chrLines)
  if (length(iFlag) != 1L) stop("the bio.viz bundle does not fix its development flag once", call. = FALSE)
  chrLines[iFlag] <- sprintf("  var DEVELOPMENT = true ? %s : true;", if (bDevelopment) "true" else "false")
  chrLines
}

# Why a version of gsm.bio may not carry a copy of bio.viz, a sentence each, or
# none. A development version of gsm.bio carries whatever bio.viz's dev holds.
# A release carries a release build of bio.viz from a commit on bio.viz's dev
# branch: its tag may not exist yet, since the two are released together.
chrReleaseRefusals <- function(strPackageVersion, lRecord, lSays) {
  if (!bReleaseVersion(strPackageVersion)) {
    return(character(0))
  }
  strRelease <- sprintf("gsm.bio %s is a release, and the bio.viz it carries", strPackageVersion)
  as.character(c(
    if (!bReleaseVersion(lSays$version)) {
      sprintf("%s has the version %s, which is not a release version", strRelease, lSays$version)
    },
    if (isTRUE(lSays$development)) {
      sprintf("%s is a development build: its footnote reads \"bio.viz %s\"", strRelease, lSays$said)
    },
    if (!isTRUE(lRecord$merged_to_dev) || !identical(lRecord$ref, "dev")) {
      sprintf("%s was not copied from a commit on bio.viz's dev branch", strRelease)
    }
  ))
}

# git, asked up to three times, a little later each time: GitHub is over a
# network. What it wrote, or an error with what it said the last time. A git
# that could not be asked is never an answer.
chrGit <- function(chrArgs, nTries = 3L) {
  for (iTry in seq_len(nTries)) {
    chrOut <- suppressWarnings(system2("git", chrArgs, stdout = TRUE, stderr = TRUE))
    if (is.null(attr(chrOut, "status"))) {
      return(as.character(chrOut))
    }
    if (iTry < nTries) Sys.sleep(2 * iTry)
  }
  stop("git ", paste(chrArgs[1:2], collapse = " "), " did not run: ", paste(chrOut, collapse = " "), call. = FALSE)
}

# A tag of a repository as GitHub has it: the lines git lists for it, none
# when there is no such tag. An error when GitHub could not be asked, so that
# "could not ask" is never read as "no such tag".
chrRemoteTag <- function(strRepository, strTag, nTries = 3L) {
  chrListed <- chrGit(c("ls-remote", "--tags", strRepository, paste0("refs/tags/", strTag)), nTries = nTries)
  grep("^[0-9a-f]{40}\trefs/tags/", chrListed, value = TRUE)
}

# bio.viz's tag as GitHub has it: NULL when there is no such tag, and otherwise
# its commit with the git name of every file in it, by path. One small fetch
# with no file contents (the tag's commit and its folders only).
lBioVizTag <- function(strTag) {
  strRef <- paste0("refs/tags/", strTag)
  if (length(chrRemoteTag(strBioVizRepository, strTag)) == 0L) {
    return(NULL)
  }
  strStore <- tempfile("bio.viz-tag-")
  on.exit(unlink(strStore, recursive = TRUE), add = TRUE)
  chrGit(c("init", "--quiet", "--bare", shQuote(strStore)), nTries = 1L)
  chrGit(c("-C", shQuote(strStore), "fetch", "--quiet", "--depth", "1", "--filter=blob:none", strBioVizRepository, strRef))
  strCommit <- chrGit(c("-C", shQuote(strStore), "rev-parse", shQuote("FETCH_HEAD^{commit}")), nTries = 1L)
  chrTree <- chrGit(c("-C", shQuote(strStore), "ls-tree", "-r", strCommit), nTries = 1L)
  chrTree <- grep("^[0-9]+ blob [0-9a-f]{40}\t", chrTree, value = TRUE)
  list(
    tag = strTag, commit = strCommit,
    files = stats::setNames(sub("^[0-9]+ blob ([0-9a-f]{40})\t.*$", "\\1", chrTree), sub("^[^\t]*\t", "", chrTree))
  )
}

# The name git gives a file's bytes: what a tag's listing is compared with.
strGitName <- function(strPath) {
  rawBytes <- readBin(strPath, "raw", n = file.size(strPath))
  rawHeader <- c(charToRaw(sprintf("blob %d", length(rawBytes))), as.raw(0L))
  digest::digest(c(rawHeader, rawBytes), algo = "sha1", serialize = FALSE)
}

# The copied files that are not, byte for byte, the tag's: each by the path it
# was copied from. `lCopied` is a record's files with the folder they are in.
chrDiffersFromTag <- function(lTag, lCopied) {
  bSame <- vapply(lCopied, function(lFile) {
    identical(unname(lTag$files[lFile$source]), strGitName(file.path(lFile$root, lFile$file)))
  }, logical(1))
  vapply(lCopied[!bSame], function(lFile) lFile$source, character(1))
}

# Every file the copy command copied, from the three records, each with the
# folder it is in.
lCopiedFiles <- function() {
  lRoots <- list(
    system.file("htmlwidgets", "lib", package = "gsm.bio"),
    system.file("specification", package = "gsm.bio"),
    testthat::test_path("fixtures", "bio.viz")
  )
  unlist(lapply(lRoots, function(strRoot) {
    lapply(lReadJson(strRoot, "SOURCE.json")$files, function(lFile) c(lFile, list(root = strRoot)))
  }), recursive = FALSE)
}
