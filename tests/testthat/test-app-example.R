# The app.R a server runs, and the article that gives it (#74).
#
# The file ships with the package, so the first test runs wherever the suite
# does. The article is in the repository and not in the built package: the
# second test reads it in the source tree and skips under check.

# The file's code, one expression each, as R reads it.
lExampleApp <- function() {
  strFile <- system.file("app", "app.R", package = "gsm.bio")
  expect_true(nzchar(strFile))
  as.list(parse(strFile, keep.source = FALSE))
}

test_that("the app.R that ships with the package attaches shiny, haven and gsm.bio by name, and its last line returns the app (#74)", {
  skip_if_not_installed("shiny")
  lCode <- lExampleApp()
  # Three attachments and the app, and nothing else is run.
  expect_identical(
    vapply(lCode, function(xCall) paste(deparse(xCall), collapse = ""), character(1)),
    c("library(shiny)", "library(haven)", "library(gsm.bio)", "RunApp()")
  )
  # What a server takes from the file is the value of its last line.
  xApp <- eval(lCode[[length(lCode)]], envir = new.env(parent = asNamespace("gsm.bio")))
  expect_s3_class(xApp, "shiny.appobj")
  # haven is suggested, never imported: the file is what tells a server to
  # install it.
  lDescription <- read.dcf(system.file("DESCRIPTION", package = "gsm.bio"), fields = c("Imports", "Suggests"))
  expect_match(lDescription[, "Suggests"], "\\bhaven\\b")
  expect_match(lDescription[, "Suggests"], "\\bshiny\\b")
  expect_no_match(lDescription[, "Imports"], "\\bhaven\\b|\\bshiny\\b")
})

test_that("the article gives the app.R that ships, the manifest it quotes lists gsm.bio, shiny and haven, and it says no deployment has been tested (#74)", {
  skip_if_not(bSourceTree(), "the article is in the repository, not in the built package")
  strRoot <- strSourceRoot()
  chrArticle <- readLines(file.path(strRoot, "vignettes", "articles", "app.Rmd"), warn = FALSE)
  # The file's code, line for line, is a chunk of the article.
  chrFile <- readLines(file.path(strRoot, "inst", "app", "app.R"), warn = FALSE)
  chrCode <- chrFile[nzchar(chrFile) & !startsWith(chrFile, "#")]
  expect_identical(chrCode, c("library(shiny)", "library(haven)", "library(gsm.bio)", "RunApp()"))
  nStart <- which(chrArticle == "library(shiny)")
  expect_length(nStart, 1L)
  chrChunk <- chrArticle[nStart:(nStart + 4L)]
  expect_identical(chrChunk[nzchar(chrChunk)], chrCode)
  expect_true(any(grepl("system.file(\"app\", \"app.R\", package = \"gsm.bio\")", chrArticle, fixed = TRUE)))

  # What the one real run of rsconnect::writeManifest() printed, kept beside
  # the article that quotes it.
  chrManifest <- readLines(file.path(strRoot, "vignettes", "articles", "app-manifest.txt"), warn = FALSE)
  for (strPackage in c("gsm.bio", "shiny", "haven")) {
    expect_true(paste(strPackage, "is listed") %in% chrManifest, label = paste(strPackage, "is listed in the manifest"))
  }
  expect_true(any(grepl("^gsm.bio .* from github: jwildfire/gsm.bio@", chrManifest)))
  expect_true(any(grepl("app-manifest.txt", chrArticle, fixed = TRUE)))

  # Said plainly.
  expect_true(any(grepl("No one has deployed this app to a Posit Connect server yet.", chrArticle, fixed = TRUE)))
  # The article is in the site's index, and the README links it.
  chrSite <- readLines(file.path(strRoot, "_pkgdown.yml"), warn = FALSE)
  expect_true(any(grepl("articles/app$", chrSite)))
  chrReadme <- readLines(file.path(strRoot, "README.md"), warn = FALSE)
  expect_true(any(grepl("https://jwildfire.github.io/gsm.bio/articles/app.html", chrReadme, fixed = TRUE)))
})
