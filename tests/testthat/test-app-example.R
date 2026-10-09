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

test_that("the article's install line names a release and its times say what they were measured at, and the README says how to stop the app from a terminal and that no one has deployed it (#86)", {
  skip_if_not(bSourceTree(), "the article and the README are in the repository, not in the built package")
  strRoot <- strSourceRoot()
  chrArticle <- readLines(file.path(strRoot, "vignettes", "articles", "app.Rmd"), warn = FALSE)
  # A deployer installs a release by its tag: a line with no tag installs the
  # default branch, which is `dev`.
  chrInstall <- grep("install_github(", chrArticle, fixed = TRUE, value = TRUE)
  expect_gt(length(chrInstall), 0L)
  expect_true(all(grepl("install_github\\(\"jwildfire/gsm\\.bio@v[0-9]+\\.[0-9]+\\.[0-9]+\"\\)", chrInstall)))
  # The times: the R, the version and the commit they were measured at.
  strTimes <- grep("^On the synthetic study, ", chrArticle, value = TRUE)
  expect_length(strTimes, 1L)
  for (strFact in c("R 4.3.3", "gsm.bio 0.3.0.9000", "commit c52f2c7")) {
    expect_match(strTimes, strFact, fixed = TRUE)
  }
  # The footnote it quotes is an example, on the R the times were measured on.
  strQuoted <- grep("on this server\"", chrArticle, value = TRUE)
  expect_length(strQuoted, 1L)
  expect_match(strQuoted, "for example, \"computed by R 4.3.3 with gsm.bio ", fixed = TRUE)

  chrReadme <- readLines(file.path(strRoot, "README.md"), warn = FALSE)
  strStop <- grep("^[0-9]+\\. To stop the app", chrReadme, value = TRUE)
  expect_length(strStop, 1L)
  expect_match(strStop, "Esc", fixed = TRUE)
  expect_match(strStop, "stop button", fixed = TRUE)
  expect_match(strStop, "in R started from a terminal, press Ctrl-C", fixed = TRUE)
  # Nothing waits for the tag or runs ahead of it, and every paragraph that
  # names Posit Connect as where the app goes says no one has put it there.
  expect_false(any(grepl("is released", chrReadme, fixed = TRUE)))
  strStatus <- chrReadme[which(chrReadme == "## Status") + 2L]
  expect_match(strStatus, "^Version [0-9.]+ is the ")
  expect_match(strStatus, "Posit Connect", fixed = TRUE)
  expect_match(strStatus, "no one has deployed it to a Connect server yet", fixed = TRUE)
})

# ---- What the second release review found of the texts (#97) -----------------

test_that("the app.R that ships calls the Data page by the name the app gives it (#97)", {
  chrFile <- readLines(system.file("app", "app.R", package = "gsm.bio"), warn = FALSE)
  expect_false(any(grepl("Data view", chrFile, fixed = TRUE)))
  expect_true(any(grepl("app's Data page", chrFile, fixed = TRUE)))
})

test_that("the README and the article say what the page asks of Google Fonts: two typefaces, by three requests to two hosts from the reader's browser, carrying the app's address and nothing of the study, and the system's fonts when they are blocked; and the article says the server's R needs a UTF-8 locale (#97)", {
  skip_if_not(bSourceTree(), "the article and the README are in the repository, not in the built package")
  strRoot <- strSourceRoot()
  # The host the page's one link names is the one the texts name first.
  expect_match(strAppFonts, "^https://fonts\\.googleapis\\.com/css2\\?family=Instrument\\+Sans[^&]*&family=Instrument\\+Serif&")
  lTexts <- list(
    `the README` = readLines(file.path(strRoot, "README.md"), warn = FALSE),
    `the article` = readLines(file.path(strRoot, "vignettes", "articles", "app.Rmd"), warn = FALSE)
  )
  for (strText in names(lTexts)) {
    # One paragraph says it all, and it is where the text says what a server needs.
    strSays <- grep("fonts.googleapis.com", lTexts[[strText]], fixed = TRUE, value = TRUE)
    expect_length(strSays, 1L)
    for (strFact in c(
      "Google Fonts", "Instrument Sans", "Instrument Serif", "reader's browser", "three requests", "`fonts.googleapis.com` for a style sheet",
      "`fonts.gstatic.com` for two font files", "the app's address", "nothing of the study", "system's fonts"
    )) {
      expect_match(strSays, strFact, fixed = TRUE, label = paste(strText, "on", strFact))
    }
    # A further letter set is a further font file: the texts do not say three is all there can be.
    expect_match(strSays, "one more", fixed = TRUE, label = strText)
    nSays <- grep("fonts.googleapis.com", lTexts[[strText]], fixed = TRUE)
    nNeeds <- grep("what the server needs|What the server needs", lTexts[[strText]])
    expect_length(nNeeds, 1L)
    expect_gte(nSays, nNeeds)
    expect_lte(nSays - nNeeds, 12L)
  }
  # The article: what the server's R must be to read a file with a letter outside ASCII.
  chrArticle <- lTexts$`the article`
  strLocale <- grep("^- A UTF-8 locale", chrArticle, value = TRUE)
  expect_length(strLocale, 1L)
  expect_match(strLocale, "outside ASCII", fixed = TRUE)
  expect_match(strLocale, "refused", fixed = TRUE)
  nNeeds <- which(chrArticle == "What the server needs:")
  expect_gt(grep("^- A UTF-8 locale", chrArticle), nNeeds)
  expect_lt(grep("^- A UTF-8 locale", chrArticle), grep("^Two settings on the content's Runtime tab", chrArticle))
})
