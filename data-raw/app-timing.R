#!/usr/bin/env Rscript
# Measures what a reader of the app waits for, on the synthetic study, and
# prints it (#74).
#
#   Rscript data-raw/app-timing.R
#
# The app is run by a second R session on this machine and opened in a
# headless browser, as the suite's own tests of it do
# (tests/testthat/helper-browser.R). For each chart it prints the time from
# choosing the chart in the list to R's answer being on the page, the number
# of messages the chart sent the session and the size of the largest. A chart's
# first message asks whether the session answers statistics at all. Then,
# for the group comparison, the time from choosing another test to its answer:
# one request, and nothing drawn again but the line under the chart.
#
# Run from the repository root, by hand. It needs devtools, shiny, chromote
# and a Chrome or Chromium. The times are this machine's, with the browser and
# the server on it: a network adds its own. What it prints is quoted in the
# article vignettes/articles/app.Rmd.

suppressMessages(devtools::load_all(quiet = TRUE))
for (strHelper in c("helper-source-tree.R", "helper-browser.R")) {
  source(file.path("tests", "testthat", strHelper))
}
bSourceTree <- function() TRUE
strSourceRoot <- function() normalizePath(".")

strApp <- "
RunApp(lSettings = list(
  GroupComparison = list(start_value = 'CRP', visits = 'Week 4', group_by = 'ARM'),
  AssociationScatter = list(color_by = 'ARM'),
  BiomarkerScreen = list(group_by = 'ARM'),
  CrossTab = list(row_by = 'ARM', col_by = 'RESPONSE'),
  StratifiedSurvival = list(group_by = 'ARM')
))
"
lApp <- lRunApp(strApp)
lPage <- lOpenPage(NULL, strAddress = lApp$address)

# Every request the page sends is noted with its size, as it is sent.
invisible(lPage$Evaluate("(() => { window.gsmBioSent = []; const send = Shiny.setInputValue; Shiny.setInputValue = function (name, value, options) { if (name === 'gsm_bio_request') window.gsmBioSent.push(value.length); return send.call(this, name, value, options); }; return true; })()"))
Answered <- function(strChart) {
  sprintf("(HTMLWidgets.find('#%s') && HTMLWidgets.find('#%s').chart() && HTMLWidgets.find('#%s').chart().statistics().length > 0 && HTMLWidgets.find('#%s').chart().statistics().every((asked) => asked.answer))", strChart, strChart, strChart, strChart)
}
# The time, in the page, from an action to a condition being true.
Timed <- function(strAction, strCondition) {
  lPage$Evaluate("(() => { window.gsmBioSent.length = 0; window.gsmBioDone = null; return true; })()")
  lPage$Evaluate(sprintf(
    "(() => { const start = performance.now(); %s; const wait = () => { if (%s) { window.gsmBioDone = performance.now() - start; } else { setTimeout(wait, 2); } }; wait(); return true; })()",
    strAction, strCondition
  ))
  if (!bWaitFor(lPage, "window.gsmBioDone !== null", 60)) stop("no answer in a minute: ", strAction, call. = FALSE)
  lPage$Evaluate("({ ms: window.gsmBioDone, sent: window.gsmBioSent.slice() })")
}
Row <- function(strWhat, lTimed, strRows) {
  nSent <- unlist(lTimed$sent)
  cat(sprintf(
    "%-44s %6.0f ms  %d message%s to the session, largest %s kB  (%s)\n", strWhat, lTimed$ms, length(nSent), if (length(nSent) == 1L) "" else "s",
    format(round(max(c(nSent, 0)) / 1000), big.mark = ","), strRows
  ))
}

cat(R.version.string, ", gsm.bio ", as.character(utils::packageVersion("gsm.bio")), ", ", Sys.info()[["sysname"]], " ", Sys.info()[["machine"]], "\n", sep = "")
cat("The synthetic study: ", format(nrow(Synthetic_Results), big.mark = ","), " results rows, ", nrow(Synthetic_Participants), " participants, ", length(unique(Synthetic_Results$TEST)), " biomarkers\n\n", sep = "")
cat("From choosing a chart in the list to R's answer on the page:\n")
chrRows <- c(
  AssociationScatter = "two biomarkers at one visit", CorrelationMatrix = "12 biomarkers at one visit",
  BiomarkerScreen = "12 biomarkers at one visit", CrossTab = "two categories", StratifiedSurvival = "one endpoint"
)
for (strChart in names(chrRows)) {
  lTimed <- Timed(sprintf("document.querySelector('a[data-value=\"%s\"]').click()", strChart), Answered(strChart))
  Row(chrAppCharts[[strChart]], lTimed, chrRows[[strChart]])
}
cat("\nFrom choosing another test to its answer, the chart already drawn:\n")
invisible(lPage$Evaluate("document.querySelector('a[data-value=\"GroupComparison\"]').click()"))
invisible(bWaitFor(lPage, Answered("GroupComparison")))
strChoose <- "(() => { const node = document.querySelector('#GroupComparison select[data-control=\"test\"]'); node.value = '%s'; node.dispatchEvent(new Event('change', { bubbles: true })); })()"
strIs <- "HTMLWidgets.find('#GroupComparison').chart().statistics().length === 1 && HTMLWidgets.find('#GroupComparison').chart().statistics()[0].answer && HTMLWidgets.find('#GroupComparison').chart().statistics()[0].args.strMethod === '%s'"
for (strTest in c("wilcoxon", "t", "wilcoxon")) {
  Row(paste0("Group comparison, one visit, test ", strTest), Timed(sprintf(strChoose, strTest), sprintf(strIs, strTest)), "one biomarker at one visit")
}
lPage$Close()
lApp$Stop()
