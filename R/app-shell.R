# The app's shell: the page around the charts (#84).
#
# A header band with the app's name and the mark gsm.bio with its version, a
# row of pills for Data and the six charts, a chip that says what the charts
# are drawn on, and one footer line. The pills are the links of Shiny's own tab
# set, so each is a link a keyboard reaches and the session knows which is
# chosen; the chip and every pill are written by the session, which knows the
# tables there are. Nothing here is a Shiny input, and nothing inside a chart
# is styled.

# What a pill says when it is pointed at: one line of what its page draws.
chrAppWhat <- c(
  Data = "The tables the charts are drawn on, and where a study of your own is loaded.",
  GroupComparison = "One biomarker between groups, over the visits.",
  AssociationScatter = "Two variables, one point per participant.",
  CorrelationMatrix = "Every pair of biomarkers at one visit.",
  BiomarkerScreen = "Every biomarker on one comparison.",
  CrossTab = "Two categories, with a cut on a biomarker.",
  StratifiedSurvival = "High against low, with a Kaplan-Meier curve."
)

# The family's two web fonts, as bio.viz's and safety.viz's sites ask for
# them. The page asks once and does not wait: where the fonts cannot be
# reached, the system's own are behind each in the style below.
strAppFonts <- "https://fonts.googleapis.com/css2?family=Instrument+Sans:wght@400..700&family=Instrument+Serif&display=swap"

# The one thing the page asks of anywhere but its own server. It is asked for
# as a print style sheet and switched on when it arrives, so a page that
# cannot reach the fonts is drawn without waiting for them.
App_Fonts <- function() {
  shiny::tags$link(rel = "stylesheet", href = strAppFonts, media = "print", onload = "this.media='all'")
}

# What the app looks like beyond its charts: the family's type, colours, rules
# and radii, in the page. The name's row is one line high and wraps: where the
# chip leaves no room for the mark beside the name, as on a phone drawn in the
# system's wider fonts, the mark goes to a second line that is not shown, and
# the name and the chip stay whole. Every size is in pixels: Shiny's page sets the root
# font to 10 pixels, a chart's output gives it back to the browser (#80), and
# the shell reads neither.
strAppStyle <- "
.gsm-bio-app { display: flex; flex-direction: column; min-height: 100vh; background: #fafaf8; color: #1f2328; }
.gsm-bio-app-head, .gsm-bio-app-foot, .gsm-bio-app-data, .gsm-bio-app-lacks { font-family: 'Instrument Sans', system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif; }
.gsm-bio-app-unseen { position: absolute; width: 1px; height: 1px; margin: -1px; padding: 0; overflow: hidden; clip: rect(0, 0, 0, 0); white-space: nowrap; border: 0; }

.gsm-bio-app-head { position: relative; display: grid; grid-template-columns: minmax(0, 1fr) auto; grid-template-areas: 'name study' 'pills pills'; align-items: center; gap: 4px 16px; padding: 14px 18px 0; background: #f3f4f1; border-bottom: 1px solid #e4e6e3; }
.gsm-bio-app-head::before { content: ''; position: absolute; left: 0; right: 0; top: 0; height: 4px; background: repeating-linear-gradient(90deg, #f97316 0 10px, #c2410c 10px 20px, #fdba74 20px 30px); }
.gsm-bio-app-head .recalculating { opacity: 1; }
.gsm-bio-app-name { grid-area: name; display: flex; flex-wrap: wrap; align-items: baseline; align-content: flex-start; gap: 12px 9px; height: 28px; min-width: 0; overflow: hidden; white-space: nowrap; }
.gsm-bio-app-head h1 { flex: none; margin: 0; font-family: 'Instrument Serif', Georgia, 'Iowan Old Style', serif; font-size: 22px; font-weight: 400; line-height: 28px; letter-spacing: 0; color: #1f2328; }
.gsm-bio-app-head h1::before { content: ''; display: inline-block; width: 22px; height: 22px; margin: -3px 7px 0 0; vertical-align: middle; background: #f97316; clip-path: polygon(25% 5%, 75% 5%, 100% 50%, 75% 95%, 25% 95%, 0 50%); }
.gsm-bio-app-mark { font: 600 10px/1 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; letter-spacing: 1.4px; text-transform: uppercase; color: #6c3270; }
.gsm-bio-app-dot { flex: none; width: 8px; height: 8px; border-radius: 50%; background: #f97316; }
.gsm-bio-app-meta { font: 500 10px/1 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; letter-spacing: 0.6px; text-transform: uppercase; color: #5c5c5c; }

.gsm-bio-app-study { grid-area: study; justify-self: end; display: inline-flex; align-items: center; min-width: 0; max-width: 440px; padding: 4px 11px; border: 1px solid #ddd3c6; border-radius: 999px; background: #fff; color: #2a211b; font-size: 12px; line-height: 18px; white-space: nowrap; text-decoration: none; }
.gsm-bio-app-study:hover, .gsm-bio-app-study:focus { border-color: #c2410c; color: #2a211b; text-decoration: none; }
.gsm-bio-app-study > .shiny-html-output, .gsm-bio-app-study-said { display: inline-flex; align-items: center; gap: 7px; min-width: 0; }
.gsm-bio-app-study-name { overflow: hidden; text-overflow: ellipsis; }
.gsm-bio-app-study .gsm-bio-app-dot { width: 7px; height: 7px; }
.gsm-bio-app-study-kept .gsm-bio-app-dot { background: #6c3270; }
.gsm-bio-app-study .gsm-bio-app-meta { font-size: 10.5px; letter-spacing: 0.4px; color: #6b5d52; }

.gsm-bio-app-pills { grid-area: pills; min-width: 0; margin: 0 -3px; padding: 3px 0 7px; }
.gsm-bio-app-pills > .nav { display: flex; flex-wrap: wrap; gap: 6px; margin: 0; padding: 3px; list-style: none; }
.gsm-bio-app-pills > .nav::before, .gsm-bio-app-pills > .nav::after { display: none; }
.gsm-bio-app-pills > .nav > li { float: none; margin: 0; }
.gsm-bio-app-pills > .nav > li > a, .gsm-bio-app-pills > .nav > li > a:hover, .gsm-bio-app-pills > .nav > li > a:focus, .gsm-bio-app-pills > .nav > li.active > a, .gsm-bio-app-pills > .nav > li.active > a:hover, .gsm-bio-app-pills > .nav > li.active > a:focus { display: block; padding: 0; border: 0; border-radius: 999px; background: transparent; color: #1f2328; font-size: 13px; line-height: 18px; white-space: nowrap; text-decoration: none; }
.gsm-bio-app-pill { display: inline-flex; align-items: center; gap: 7px; padding: 5px 11px; border: 1px solid transparent; border-radius: 999px; }
.gsm-bio-app-pills > .nav > li > a:hover .gsm-bio-app-pill { background: #e9ebe6; }
.gsm-bio-app-pills > .nav > li.active > a .gsm-bio-app-pill { border-color: #c2410c; background: #ffedd5; }
.gsm-bio-app-pill-data .gsm-bio-app-dot { background: #6c3270; }
.gsm-bio-app-pills > .nav > li.active > a .gsm-bio-app-pill-data { border-color: #6c3270; background: #ebe2ec; }
.gsm-bio-app-pill-dim { color: #666; }
.gsm-bio-app-pill-dim .gsm-bio-app-dot { background: transparent; box-shadow: inset 0 0 0 1.5px #8a8f8a; }
.gsm-bio-app-pills > .nav > li > a:focus, .gsm-bio-app-study:focus { outline: 0; }
.gsm-bio-app-pills > .nav > li > a:focus-visible, .gsm-bio-app-study:focus-visible { outline: 2px solid #1f2328; outline-offset: 1px; }

.gsm-bio-app-main { flex: 1 0 auto; min-width: 0; padding: 18px 20px 10px; }
.gsm-bio-app-card { min-width: 0; padding: 10px; border: 1px solid #e4e6e3; border-radius: 12px; background: #fff; }
.gsm-bio-app-card.gsm-bio-app-data { padding: 4px 20px 20px; }
.gsm-bio-app-foot { margin-top: 8px; padding: 10px 24px; border-top: 1px solid #ece2d7; background: #faf6f1; color: #6b5d52; font-size: 11.5px; line-height: 17px; }
.gsm-bio-app-foot p { margin: 0; }

.gsm-bio-app h2 { font-size: 16px; margin: 20px 0 4px; }
.gsm-bio-app .gsm-bio-app-what { color: #52616f; font-size: 13px; }
.gsm-bio-app .gsm-bio-app-lacks { margin: 14px 12px; font-size: 14px; }
.gsm-bio-app .gsm-bio-app-problem { color: #a4262c; }
.gsm-bio-app .gsm-bio-app-columns { display: flex; flex-wrap: wrap; gap: 0 16px; }
.gsm-bio-app .gsm-bio-app-columns .form-group { min-width: 180px; }
.gsm-bio-app .gsm-bio-app-rows { overflow-x: auto; margin: 4px 0 8px; }
.gsm-bio-app .gsm-bio-app-table { border-collapse: collapse; font-size: 13px; white-space: nowrap; }
.gsm-bio-app .gsm-bio-app-table th, .gsm-bio-app .gsm-bio-app-table td { border-bottom: 1px solid #dde3ea; padding: 3px 10px 3px 0; text-align: left; }
.gsm-bio-app .gsm-bio-app-table th { color: #52616f; font-weight: 600; }
.gsm-bio-app .gsm-bio-app-table .gsm-bio-app-row { color: #7a8794; }
.gsm-bio-app .gsm-bio-app-turn { display: flex; gap: 8px; margin: 0 0 24px; }
.gsm-bio-app .gsm-bio-app-viewer .nav-tabs { margin: 8px 0; }
.gsm-bio-app .gsm-bio-app-viewer .tab-content { display: none; }

@media (max-width: 640px) {
  .gsm-bio-app-head { gap: 4px 12px; padding: 12px 14px 0; }
  .gsm-bio-app-head h1 { font-size: 20px; }
  .gsm-bio-app-study { max-width: 40vw; }
  .gsm-bio-app-pills { margin-right: -14px; overflow-x: auto; scrollbar-width: none; -webkit-mask-image: linear-gradient(90deg, #000 calc(100% - 36px), transparent); mask-image: linear-gradient(90deg, #000 calc(100% - 36px), transparent); }
  .gsm-bio-app-pills::-webkit-scrollbar { display: none; }
  .gsm-bio-app-pills > .nav { flex-wrap: nowrap; width: max-content; padding-right: 40px; }
  .gsm-bio-app-version, .gsm-bio-app-study .gsm-bio-app-meta { display: none; }
  .gsm-bio-app-main { padding: 12px 10px; }
  .gsm-bio-app-card { padding: 4px; }
  .gsm-bio-app-card.gsm-bio-app-data { padding: 2px 12px 16px; }
  .gsm-bio-app-foot { padding: 10px 14px; }
}
@media (min-width: 1880px) {
  .gsm-bio-app-head { grid-template-columns: auto minmax(0, 1fr) auto; grid-template-areas: 'name pills study'; min-height: 56px; padding-top: 4px; }
  .gsm-bio-app-pills { padding: 3px 0; }
}
"

# What the page does beyond Shiny's own script: the chosen pill is brought
# into view where the row of pills scrolls, and the chip opens Data. Shiny's
# tab set does the rest: it opens a pill's page, says which pill is selected
# to a reader who cannot see it, and walks the row with the arrow keys.
strAppScript <- "
(function () {
  var $ = window.jQuery;
  if (!$) return;
  $(document).on('shown.bs.tab', '#gsm_bio_chart a', function () {
    var row = document.querySelector('.gsm-bio-app-pills');
    var pill = this.getBoundingClientRect();
    var within = row.getBoundingClientRect();
    if (pill.left < within.left || pill.right > within.right - 40) {
      row.scrollLeft += pill.left - within.left - (within.width - pill.width) / 2;
    }
  });
  $(document).on('click', '#gsm_bio_study', function (event) {
    event.preventDefault();
    document.querySelector('#gsm_bio_chart a[data-value=\"Data\"]').click();
  });
})();
"

# A count with its noun: one table, three tables.
App_Count <- function(nCount, strNoun) {
  paste0(format(nCount, big.mark = ","), " ", strNoun, if (nCount == 1L) "" else "s")
}

# The source line: what the charts are drawn on, as a sentence.
App_Source <- function(lStudy) {
  paste0("Drawn on ", lStudy$source, ".")
}

# The study chip's words: what the charts are drawn on in a few words, and
# beside them how much of it there is, or that it is held for this session.
App_Chip <- function(lStudy) {
  bSession <- !is.null(lStudy$files)
  strName <- if (bSession) paste(lStudy$files, collapse = ", ") else lStudy$name
  # The dot between the two counts is written as markup, so it is the same in
  # every locale; both counts are numbers with a fixed noun.
  xFacts <- if (bSession) {
    "this session"
  } else {
    shiny::HTML(paste(
      App_Count(length(unique(lStudy$results[[lCoreDefaults$id_col]])), "participant"), "&middot;",
      App_Count(length(App_Loaded(lStudy)), "table")
    ))
  }
  shiny::tags$span(
    class = paste(c("gsm-bio-app-study-said", if (!bSession) "gsm-bio-app-study-kept"), collapse = " "),
    shiny::tags$span(class = "gsm-bio-app-dot", `aria-hidden` = "true"),
    shiny::tags$span(class = "gsm-bio-app-study-name", strName),
    shiny::tags$span(class = "gsm-bio-app-meta", xFacts)
  )
}

# What a pill reads: Data with the tables there are, or a chart's name. A
# chart that cannot be drawn on the tables there are is dimmed and carries the
# reason, as its hover text and in words a screen reader reads.
App_Pill <- function(strPill, lStudy) {
  xDot <- shiny::tags$span(class = "gsm-bio-app-dot", `aria-hidden` = "true")
  if (identical(strPill, "Data")) {
    return(shiny::tags$span(
      class = "gsm-bio-app-pill gsm-bio-app-pill-data",
      xDot,
      shiny::tags$span(class = "gsm-bio-app-pill-name", "Data"),
      shiny::tags$span(class = "gsm-bio-app-meta", App_Count(length(App_Loaded(lStudy)), "table"))
    ))
  }
  strLacks <- App_Lacks(strPill, lStudy)
  shiny::tags$span(
    class = paste(c("gsm-bio-app-pill", if (!is.null(strLacks)) "gsm-bio-app-pill-dim"), collapse = " "),
    title = strLacks,
    xDot,
    shiny::tags$span(class = "gsm-bio-app-pill-name", chrAppCharts[[strPill]]),
    if (!is.null(strLacks)) shiny::tags$span(class = "gsm-bio-app-unseen", strLacks)
  )
}

# The name of the output a pill is written by.
App_PillId <- function(strPill) {
  paste0("gsm_bio_pill_", strPill)
}

# Every link of a tag, changed.
App_Links <- function(xTag, Change) {
  if (inherits(xTag, "shiny.tag")) {
    if (identical(xTag$name, "a")) {
      return(Change(xTag))
    }
    xTag$children <- lapply(xTag$children, App_Links, Change)
  } else if (is.list(xTag)) {
    xTag[] <- lapply(xTag, App_Links, Change)
  }
  xTag
}

# The pills and the pages they open. Both are Shiny's own tab set, taken
# apart: its list of links goes in the header band and its pages under it.
# A link keeps everything Shiny gave it, and gains its line of hover text.
App_Tabs <- function(lStudy, lPages) {
  strOpens <- names(chrAppCharts)[1]
  Page <- function(strPill) {
    shiny::tabPanel(
      # A pill is written by the session, which knows the tables there are; it
      # is written here as well, so the page opens as it will stay.
      shiny::tags$span(id = App_PillId(strPill), class = "shiny-html-output", App_Pill(strPill, lStudy)),
      value = strPill,
      lPages[[strPill]]
    )
  }
  # One page is drawn at a time: Shiny draws an output when it is shown.
  xSet <- shiny::tabsetPanel(
    id = "gsm_bio_chart", type = "pills", selected = strOpens,
    Page("Data"), Page("GroupComparison"), Page("AssociationScatter"), Page("CorrelationMatrix"),
    Page("BiomarkerScreen"), Page("CrossTab"), Page("StratifiedSurvival")
  )
  bList <- vapply(xSet$children, function(xPart) inherits(xPart, "shiny.tag") && identical(xPart$name, "ul"), logical(1))
  if (length(bList) != 2L || sum(bList) != 1L) {
    App_Stop("shiny wrote the app's tab set as something other than a list of links and their pages, which this version of gsm.bio does not know how to lay out.")
  }
  xList <- App_Links(xSet$children[[which(bList)]], function(xLink) {
    shiny::tagAppendAttributes(xLink, title = chrAppWhat[[xLink$attribs[["data-value"]]]])
  })
  list(pills = xList, pages = xSet$children[[which(!bList)]])
}

# The header band: the name and the mark, the study chip, and the pills.
App_Head <- function(lStudy, xPills) {
  shiny::tags$header(
    class = "gsm-bio-app-head",
    shiny::tags$div(
      class = "gsm-bio-app-name",
      shiny::tags$h1("Biomarker charts"),
      shiny::tags$span(
        class = "gsm-bio-app-mark",
        "gsm.bio ",
        shiny::tags$span(class = "gsm-bio-app-version", StoredResultsProvenance()$gsm_bio_version)
      )
    ),
    # The chip is a link, not an input: the page's script opens Data with it.
    shiny::tags$a(
      id = "gsm_bio_study", class = "gsm-bio-app-study", href = "#gsm_bio_chart",
      title = "What the charts are drawn on. Press to open Data.",
      shiny::tags$span(id = "gsm_bio_study_said", class = "shiny-html-output", App_Chip(lStudy)),
      # The source line, a sentence: read by a screen reader, and by the tests.
      shiny::tags$span(id = "gsm_bio_source", class = "shiny-text-output gsm-bio-app-unseen", App_Source(lStudy))
    ),
    shiny::tags$nav(class = "gsm-bio-app-pills", `aria-label` = "Data and the six charts", xPills)
  )
}

# The footer's one line: the two facts that stand whatever is shown.
App_Foot <- function() {
  lBy <- StoredResultsProvenance()
  shiny::tags$footer(
    class = "gsm-bio-app-foot",
    shiny::tags$p(sprintf(
      "Every statistic is computed on request by R %s with gsm.bio %s on this server. Files you load are held in this session's memory and nowhere else.",
      lBy$r_version, lBy$gsm_bio_version
    ))
  )
}

# The shell's side of the session: the source line, the chip and the pills,
# written again whenever a reader draws the charts on other tables.
App_ShellServer <- function(output, rStudy) {
  output$gsm_bio_source <- shiny::renderText(App_Source(rStudy()))
  output$gsm_bio_study_said <- shiny::renderUI(App_Chip(rStudy()))
  for (strEach in names(chrAppWhat)) {
    local({
      strPill <- strEach
      output[[App_PillId(strPill)]] <- shiny::renderUI(App_Pill(strPill, rStudy()))
    })
  }
  invisible(NULL)
}
