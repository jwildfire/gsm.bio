# Saves a widget page per chart, on the synthetic study, whose chart then
# writes the specifications tests/testthat/fixtures/specifications/charts.json
# holds (data-raw/specifications/write-specifications.mjs). Run from the
# repository root after the bundle is copied again:
#
#   Rscript data-raw/specifications/save-pages.R <folder>
#   node data-raw/specifications/write-specifications.mjs <folder> tests/testthat/fixtures/specifications/charts.json
#
# The second needs Playwright's Chromium (in a bio.viz checkout's node_modules).
suppressMessages(devtools::load_all(quiet = TRUE))
strOut <- normalizePath(commandArgs(TRUE)[1])
lColumns <- list(list(value_col = "ARM", label = "Arm"), list(value_col = "SEX", label = "Sex"), list(value_col = "RESPONSE", label = "Response"))
lPages <- list(
  "group-comparison" = Widget_GroupComparison(Synthetic_Results, Synthetic_Participants, lSettings = list(
    start_value = "IL-6", visits = c("Week 4", "Week 8"), value_type = "change", baseline_visits = "Baseline", group_by = "ARM",
    groups = lColumns, filters = lColumns, title = "{measure}: {value} by {group}", subtitle = "{n} participants, at {visits}",
    footnotes = list("Synthetic study from gsm.bio.", "Filters: {filters}.")
  )),
  "association-scatter" = Widget_AssociationScatter(Synthetic_Results, Synthetic_Participants, lSettings = list(
    x = list(measure = "TNF-alpha", visit = "Baseline"), y = list(measure = "IL-10", visit = "Baseline"), fit = "linear",
    filters = lColumns, title = "{y} against {x}", subtitle = "{n} participants"
  )),
  "correlation-matrix" = Widget_CorrelationMatrix(Synthetic_Results, Synthetic_Participants, lSettings = list(
    visit = "Baseline", filters = lColumns, title = "{heading}"
  )),
  "biomarker-screen" = Widget_BiomarkerScreen(Synthetic_Results, Synthetic_Participants, lSettings = list(
    visit = "Week 4", value_type = "change", group_by = "ARM", baseline_visits = "Baseline", groups = lColumns, filters = lColumns,
    title = "{heading}", subtitle = "{biomarkers} biomarkers, {n} participants"
  )),
  "cross-tab" = Widget_CrossTab(Synthetic_Results, Synthetic_Participants, lSettings = list(
    row_by = "RESPONSE", col_by = list(measure = "CRP", visit = "Baseline", cut = "median"), groups = lColumns, filters = lColumns,
    title = "${1 + 1} {rows} by {columns} <script>alert(1)</script>", subtitle = "{n} participants; system('touch PWNED')"
  )),
  "stratified-survival" = Widget_StratifiedSurvival(Synthetic_Results, Synthetic_Participants, lSettings = list(
    endpoint = "EFS", group_by = list(measure = "CRP", visit = "Baseline", cut = "median"), groups = lColumns, filters = lColumns,
    title = "{endpoint} by {group}", subtitle = "{n} participants"
  ), dfOutcomes = Synthetic_Outcomes)
)
for (strName in names(lPages)) htmlwidgets::saveWidget(lPages[[strName]], file.path(strOut, paste0(strName, ".html")), selfcontained = TRUE)
