#!/usr/bin/env Rscript
# Writes the record Posit Connect installs the app from, for real, and prints
# what it lists (#74).
#
#   Rscript data-raw/connect-manifest.R              gsm.bio's `dev` branch
#   Rscript data-raw/connect-manifest.R --ref <ref>  another branch, tag or commit
#
# rsconnect::writeManifest() records where each package the app needs was
# installed from, and refuses a package it cannot trace to a source. A package
# loaded from a checkout has no such record, so this cannot be a test of the
# suite: gsm.bio is installed here from GitHub, into a library of its own that
# is thrown away, and the manifest is written for inst/app/ against it.
#
# Run from the repository root, by hand, with a network connection. It needs
# the rsconnect and remotes packages, and shiny and haven installed. It writes
# nothing into the repository. What it prints is quoted in the article
# vignettes/articles/app.Rmd: run it again when the app's packages change.

chrArgs <- commandArgs(trailingOnly = TRUE)
strRef <- if ("--ref" %in% chrArgs) chrArgs[match("--ref", chrArgs) + 1L] else "dev"
for (strPackage in c("rsconnect", "remotes", "shiny", "haven")) {
  if (!requireNamespace(strPackage, quietly = TRUE)) {
    stop("data-raw/connect-manifest.R needs the ", strPackage, " package", call. = FALSE)
  }
}
if (!file.exists(file.path("inst", "app", "app.R"))) {
  stop("run data-raw/connect-manifest.R from the repository root", call. = FALSE)
}

strLibrary <- tempfile("gsm-bio-library")
dir.create(strLibrary)
strApp <- tempfile("gsm-bio-app")
dir.create(strApp)
invisible(file.copy(file.path("inst", "app", "app.R"), strApp))
on.exit(unlink(c(strLibrary, strApp), recursive = TRUE), add = TRUE)

# gsm.bio alone goes into the library of its own; everything else it needs is
# found where this session finds it.
chrWas <- .libPaths()
remotes::install_github(
  paste0("jwildfire/gsm.bio@", strRef),
  lib = strLibrary, dependencies = FALSE, upgrade = "never", quiet = TRUE
)
.libPaths(c(strLibrary, chrWas))

rsconnect::writeManifest(appDir = strApp, quiet = TRUE)
lManifest <- jsonlite::fromJSON(file.path(strApp, "manifest.json"), simplifyVector = FALSE)
lGsmBio <- lManifest$packages$gsm.bio
cat("rsconnect ", as.character(utils::packageVersion("rsconnect")), ", ", R.version.string, "\n", sep = "")
cat("gsm.bio ", lGsmBio$description$Version, " from ", lGsmBio$Source, ": ",
  lGsmBio$description$RemoteUsername, "/", lGsmBio$description$RemoteRepo, "@", lGsmBio$description$RemoteRef,
  " at ", substr(lGsmBio$description$RemoteSha, 1L, 7L), "\n",
  sep = ""
)
cat("app mode: ", lManifest$metadata$appmode, "; R ", lManifest$platform, "\n", sep = "")
cat("files: ", paste(names(lManifest$files), collapse = ", "), "\n", sep = "")
cat(length(lManifest$packages), " packages: ", paste(sort(names(lManifest$packages)), collapse = ", "), "\n", sep = "")
for (strPackage in c("gsm.bio", "shiny", "haven")) {
  cat(strPackage, if (strPackage %in% names(lManifest$packages)) " is listed" else " IS NOT LISTED", "\n", sep = "")
}
if (!all(c("gsm.bio", "shiny", "haven") %in% names(lManifest$packages))) {
  quit(status = 1L)
}
