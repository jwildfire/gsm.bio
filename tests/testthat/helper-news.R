# The shape and the length of a release's section of NEWS.md (#69), for
# tests/testthat/test-repository.R.
#
# The rules are obot.agent's release-notes skill's (skills/release-notes/
# SKILL.md, with its checker check-notes.mjs), written a second time so the
# suite holds them with no node and no network. Words are counted as a reader
# meets them: a link counts as its text, not its address, and the issue and
# pull-request links that close a bullet are not counted.

lNewsLimits <- list(
  section = 600L, # every counted word of the section
  intro = 80L, # the paragraphs between "See it move" and the first heading
  whats_new_bullets = 6L,
  whats_new_bullet = 70L,
  notice_bullet = 100L, # Deprecated, Removed: the reader has to act
  also_bullet = 60L, # Also in this release
  tests = 100L # the Tests and provenance paragraph
)

chrNewsHeadings <- c("What's new", "Deprecated", "Removed", "Also in this release", "Tests and provenance")

# The citation that closes a bullet or a paragraph: links to issues and pull
# requests, with "PR" and punctuation between them.
strNewsCitation <- "(?:[\\s,;(]*(?:PR\\s+)?\\[[^\\]]*#\\d+\\]\\([^)]*\\)[\\s,;).]*)+$"

# The words of a line, as a reader meets them.
nNewsWords <- function(strText) {
  strText <- enc2utf8(strText)
  strRead <- sub(strNewsCitation, "", strText, perl = TRUE)
  strRead <- gsub("!\\[[^\\]]*\\]\\([^)]*\\)", " ", strRead, perl = TRUE)
  strRead <- gsub("\\[([^\\]]*)\\]\\([^)]*\\)", "\\1", strRead, perl = TRUE)
  strRead <- gsub("<[^>]+>", " ", strRead, perl = TRUE)
  strRead <- gsub("[*_`]", "", strRead, perl = TRUE)
  chrWords <- strsplit(strRead, "(*UCP)\\s+", perl = TRUE)[[1]]
  sum(grepl("[\\p{L}\\p{N}]", chrWords, perl = TRUE))
}

# NEWS.md's sections, each the lines from its "# " heading to the next, named
# by the version in the heading. A comment is not part of any section.
lNewsSections <- function(strPath) {
  strNews <- paste(readLines(strPath, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  strNews <- gsub("<!--[\\s\\S]*?-->", "", strNews, perl = TRUE)
  chrLines <- strsplit(strNews, "\n", fixed = TRUE)[[1]]
  iStarts <- grep("^# \\S", chrLines)
  iEnds <- c(iStarts[-1] - 1L, length(chrLines))
  lSections <- Map(function(iStart, iEnd) chrLines[iStart:iEnd], iStarts, iEnds)
  stats::setNames(lSections, sub("^# \\S+ v([0-9.]+).*$", "\\1", chrLines[iStarts]))
}

# What is wrong with a section, a sentence each, and the words it counts.
# `bReleased` is whether the section is a release's notes, released or being
# prepared: it then opens with the line to the demo page and an introduction.
# A section still collecting work between releases has neither yet, and is
# held to the headings and the lengths alone.
lNewsCheck <- function(chrLines, bReleased = TRUE) {
  chrProblems <- character(0)
  Fail <- function(...) chrProblems <<- c(chrProblems, paste0(...))
  chrBody <- chrLines[-1]
  strFirst <- c(chrBody[trimws(chrBody) != ""], "")[1]
  bSeeItMove <- grepl("^\\*\\*See it move:\\*\\*.*\\]\\(https?://", strFirst, perl = TRUE)
  if (bReleased && !bSeeItMove) {
    Fail("The section does not open with a \"**See it move:**\" line that links the demo page.")
  }

  # The opening part, and the part under each "##" heading.
  bHeading <- grepl("^## .+", chrBody)
  iPart <- cumsum(bHeading)
  lParts <- lapply(split(seq_along(chrBody), iPart), function(iLines) {
    bIsHeading <- bHeading[iLines[1]]
    list(
      heading = if (bIsHeading) sub("\\s*$", "", sub("^## ", "", chrBody[iLines[1]])) else NULL,
      lines = chrBody[if (bIsHeading) iLines[-1] else iLines]
    )
  })
  if (length(lParts) == 0L || !is.null(lParts[[1]]$heading)) {
    lParts <- c(list(list(heading = NULL, lines = character(0))), lParts)
  }
  Bullets <- function(lPart) grep("^- ", lPart$lines, value = TRUE)
  Prose <- function(lPart) lPart$lines[trimws(lPart$lines) != "" & !grepl("^- ", lPart$lines)]
  Count <- function(chrText) sum(vapply(chrText, nNewsWords, numeric(1)))

  # A release's first line is the line to the demo page, counted apart from
  # the introduction, as the checker counts it whatever the line is.
  lOpening <- lParts[[1]]
  chrIntro <- Prose(lOpening)
  bFirstApart <- bReleased || bSeeItMove
  if (bFirstApart) chrIntro <- chrIntro[chrIntro != strFirst]
  nIntro <- Count(chrIntro)
  nTotal <- nIntro + if (bFirstApart) nNewsWords(strFirst) else 0
  if (nIntro > lNewsLimits$intro) Fail("The introduction is ", nIntro, " words; the limit is ", lNewsLimits$intro, ".")
  if (bReleased && length(chrIntro) == 0L) Fail("The section has no introduction: say in two to four sentences what the release is.")
  if (length(Bullets(lOpening)) > 0L && length(lParts) > 1L) Fail("Bullets come under a heading, not before the first one.")

  lUnder <- lParts[-1]
  iOrder <- vapply(lUnder, function(lPart) match(lPart$heading, chrNewsHeadings, nomatch = 0L), integer(1))
  for (iAt in seq_along(lUnder)) {
    lPart <- lUnder[[iAt]]
    if (iOrder[iAt] == 0L) {
      Fail("\"## ", lPart$heading, "\" is not one of the headings: ", paste(chrNewsHeadings, collapse = ", "), ".")
    } else if (iAt > 1L && iOrder[iAt - 1L] > 0L && iOrder[iAt] < iOrder[iAt - 1L]) {
      Fail("\"## ", lPart$heading, "\" is out of order.")
    }
    chrBullets <- Bullets(lPart)
    bTests <- identical(lPart$heading, "Tests and provenance")
    nLimit <- if (identical(lPart$heading, "What's new")) {
      lNewsLimits$whats_new_bullet
    } else if (lPart$heading %in% c("Deprecated", "Removed")) {
      lNewsLimits$notice_bullet
    } else {
      lNewsLimits$also_bullet
    }
    if (identical(lPart$heading, "What's new") && length(chrBullets) > lNewsLimits$whats_new_bullets) {
      Fail("\"What's new\" has ", length(chrBullets), " bullets; the limit is ", lNewsLimits$whats_new_bullets, ".")
    }
    for (strBullet in chrBullets) {
      nBullet <- nNewsWords(sub("^- ", "", strBullet))
      nTotal <- nTotal + nBullet
      if (!bTests && nBullet > nLimit) {
        Fail(nBullet, " words, limit ", nLimit, ", under \"", lPart$heading, "\": ", substr(strBullet, 3L, 62L))
      }
      if (!bTests && !grepl("^- \\*\\*[^*]+\\*\\*", strBullet)) {
        Fail("A bullet under \"", lPart$heading, "\" does not open with its claim in bold: ", substr(strBullet, 3L, 62L))
      }
    }
    nProse <- Count(Prose(lPart))
    nTotal <- nTotal + nProse
    if (bTests && nProse + Count(chrBullets) > lNewsLimits$tests) {
      Fail("\"Tests and provenance\" is ", nProse + Count(chrBullets), " words; the limit is ", lNewsLimits$tests, ".")
    }
  }
  if (length(lUnder) > 0L && !any(vapply(lUnder, function(lPart) identical(lPart$heading, "What's new"), logical(1)))) {
    Fail("A section with headings has a \"What's new\" heading first.")
  }
  if (nTotal > lNewsLimits$section) {
    Fail("The section is ", nTotal, " words; the limit is ", lNewsLimits$section, ". The detail goes on the demo page.")
  }
  list(total = nTotal, problems = chrProblems)
}
