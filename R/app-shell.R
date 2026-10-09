# The app's shell: the page around the charts (#84).
#
# A header band with the app's name and the mark gsm.bio with its version, a
# row of pills for Data and the six charts, a chip that says what the charts
# are drawn on, and one footer line. The pills are the links of Shiny's own tab
# set, so each is a link a keyboard reaches and the session knows which is
# chosen; the chip and every pill are written by the session, which knows the
# tables there are. Nothing here is a Shiny input, and nothing inside a chart
# is styled. The style and the script of the Data page (#85) are here as
# well, with the shell's: its rail, its cards and the button's row.

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
.gsm-bio-app-foot { margin-top: 8px; padding: 10px 24px; border-top: 1px solid #ece2d7; background: #faf6f1; color: #6b5d52; font-size: 11.5px; line-height: 17px; }
.gsm-bio-app-foot p { margin: 0; }

.gsm-bio-app .gsm-bio-app-lacks { margin: 14px 12px; font-size: 14px; }

.gsm-bio-app-data { display: grid; grid-template-columns: 270px minmax(0, 1fr); gap: 18px; align-items: start; color: #2a211b; }
.gsm-bio-app-data .gsm-bio-app-card { padding: 14px 16px; border-color: #ece2d7; border-radius: 10px; }
.gsm-bio-app-cards { min-width: 0; }
.gsm-bio-app-cards > * + * { margin-top: 12px; }
.gsm-bio-app-data h2, .gsm-bio-app-data h3, .gsm-bio-app-chosen-name { margin: 0; font: 600 13px/18px ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; color: #2a211b; overflow-wrap: anywhere; }
.gsm-bio-app-data h2 { margin-bottom: 8px; }
.gsm-bio-app-data .gsm-bio-app-what { margin: 0 0 6px; color: #6b5d52; font-size: 12.5px; line-height: 18px; overflow-wrap: anywhere; }
.gsm-bio-app-data .gsm-bio-app-problem { margin: 0 0 6px; padding: 8px 12px; border: 1px solid #fecaca; border-left: 3px solid #b91c1c; border-radius: 6px; background: #fff5f5; color: #7f1d1d; font-size: 12.5px; line-height: 18px; overflow-wrap: anywhere; }
.gsm-bio-app-data .btn { padding: 6px 12px; border: 1px solid #cfc6b9; border-radius: 6px; background: #fff; box-shadow: none; color: #2a211b; font-size: 12.5px; line-height: 18px; white-space: normal; }
.gsm-bio-app-data .btn:hover, .gsm-bio-app-data .btn:focus { border-color: #c2410c; background: #fff; color: #2a211b; }
.gsm-bio-app-data .btn:focus-visible, .gsm-bio-app-data a:focus-visible, .gsm-bio-app-data select:focus-visible { outline: 2px solid #1f2328; outline-offset: 1px; }
.gsm-bio-app-tag { display: inline-block; padding: 1px 6px; border: 1px solid transparent; border-radius: 4px; font: 600 10px/14px ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; letter-spacing: 0.6px; text-transform: uppercase; white-space: nowrap; }
.gsm-bio-app-tag-same { background: #dcfce7; color: #166534; }
.gsm-bio-app-tag-optional, .gsm-bio-app-tag-said { background: #f5efe7; color: #6b5d52; }
.gsm-bio-app-tag-need { border-color: #fed7aa; background: #fff7ed; color: #9a3412; }

.gsm-bio-app-rail { padding: 16px; border: 1px solid #e4e6e3; border-radius: 12px; background: #fff; }
.gsm-bio-app-rail p { margin: 0; }
.gsm-bio-app-rail .gsm-bio-app-kicker { margin-bottom: 8px; font: 600 10.5px/14px ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; letter-spacing: 1.3px; text-transform: uppercase; color: #6b5d52; }
.gsm-bio-app-steps { margin: 0; padding: 0; list-style: none; counter-reset: gsm-bio-step; }
.gsm-bio-app-step { display: flex; align-items: flex-start; gap: 10px; padding: 6px 0 12px; counter-increment: gsm-bio-step; }
.gsm-bio-app-step-mark { flex: none; width: 24px; height: 24px; background: #271810; color: #fff; clip-path: polygon(25% 5%, 75% 5%, 100% 50%, 75% 95%, 25% 95%, 0 50%); font: 600 11px/24px ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; text-align: center; }
.gsm-bio-app-step-mark::before { content: counter(gsm-bio-step); }
.gsm-bio-app-step-done .gsm-bio-app-step-mark::before { content: '\\2713'; }
.gsm-bio-app-step-next .gsm-bio-app-step-mark { background: #6c3270; }
.gsm-bio-app-step-waiting .gsm-bio-app-step-mark { background: #cfc6b9; color: #2a211b; }
.gsm-bio-app-step-body { min-width: 0; }
.gsm-bio-app-rail .gsm-bio-app-step-title { color: #1f2328; font-size: 13.5px; font-weight: 600; line-height: 24px; }
.gsm-bio-app-rail .gsm-bio-app-step-waiting .gsm-bio-app-step-title { color: #6b5d52; }
.gsm-bio-app-step-says { color: #6b5d52; font: 12px/18px ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; overflow-wrap: anywhere; }
.gsm-bio-app-rail .gsm-bio-app-step-note { margin-top: 6px; color: #6b5d52; font-size: 12px; line-height: 18px; overflow-wrap: anywhere; }
.gsm-bio-app-open { display: inline-block; margin-top: 8px; padding: 6px 12px; border: 1px solid #cfc6b9; border-radius: 6px; background: #fff; color: #2a211b; font-size: 12.5px; line-height: 18px; text-decoration: none; }
.gsm-bio-app-open:hover, .gsm-bio-app-open:focus { border-color: #c2410c; color: #2a211b; text-decoration: none; }
.gsm-bio-app-drawn { margin-top: 2px; padding-top: 12px; border-top: 1px solid #ece2d7; }
.gsm-bio-app-drawn ul { margin: 0; padding: 0; list-style: none; }
.gsm-bio-app-drawn li { display: flex; flex-wrap: wrap; align-items: baseline; gap: 0 8px; padding: 3px 0; color: #6b5d52; font-size: 12px; line-height: 18px; overflow-wrap: anywhere; }
.gsm-bio-app-drawn .gsm-bio-app-dot { align-self: center; }
.gsm-bio-app-drawn-name { color: #1f2328; font: 600 12.5px/18px ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
.gsm-bio-app-drawn-none .gsm-bio-app-dot { background: #cfc6b9; }
.gsm-bio-app-drawn-none .gsm-bio-app-drawn-name { color: #6b5d52; }
.gsm-bio-app-rail .gsm-bio-app-session { margin-top: 12px; padding-top: 10px; border-top: 1px solid #ece2d7; }

.gsm-bio-app-viewer .nav-tabs { display: flex; flex-wrap: wrap; gap: 4px; margin: 0 0 8px; border-bottom: 1px solid #ece2d7; }
.gsm-bio-app-viewer .nav-tabs::before, .gsm-bio-app-viewer .nav-tabs::after { display: none; }
.gsm-bio-app-viewer .nav-tabs > li { float: none; margin: 0 0 -1px; }
.gsm-bio-app-viewer .nav-tabs > li > a, .gsm-bio-app-viewer .nav-tabs > li > a:hover, .gsm-bio-app-viewer .nav-tabs > li > a:focus, .gsm-bio-app-viewer .nav-tabs > li.active > a, .gsm-bio-app-viewer .nav-tabs > li.active > a:hover, .gsm-bio-app-viewer .nav-tabs > li.active > a:focus { margin: 0; padding: 6px 10px; border: 0; border-bottom: 2px solid transparent; border-radius: 0; background: transparent; color: #6b5d52; font-size: 12.5px; line-height: 18px; }
.gsm-bio-app-viewer .nav-tabs > li.active > a, .gsm-bio-app-viewer .nav-tabs > li.active > a:hover, .gsm-bio-app-viewer .nav-tabs > li.active > a:focus { border-bottom-color: #f97316; color: #2a211b; font-weight: 600; }
.gsm-bio-app-viewer .tab-content { display: none; }
.gsm-bio-app .gsm-bio-app-rows { overflow-x: auto; margin: 4px 0 8px; }
.gsm-bio-app .gsm-bio-app-table { border-collapse: collapse; color: #2a211b; font-size: 12px; line-height: 18px; white-space: nowrap; }
.gsm-bio-app .gsm-bio-app-table th, .gsm-bio-app .gsm-bio-app-table td { padding: 4px 12px 4px 0; border-bottom: 1px solid #ece2d7; text-align: left; }
.gsm-bio-app .gsm-bio-app-table th { color: #6b5d52; font: 600 10.5px/18px ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; letter-spacing: 0.6px; }
.gsm-bio-app .gsm-bio-app-table .gsm-bio-app-row { color: #7a6d62; }
.gsm-bio-app .gsm-bio-app-turn { display: flex; flex-wrap: wrap; gap: 8px; margin: 10px 0 0; }

.gsm-bio-app-file-head { display: flex; flex-wrap: wrap; align-items: center; gap: 4px 10px; margin-bottom: 10px; }
.gsm-bio-app-file-what { color: #6b5d52; font-size: 12px; line-height: 18px; }
.gsm-bio-app-file .shiny-input-container { margin: 0; }
.gsm-bio-app-file .input-group { display: flex; flex-wrap: wrap; align-items: center; gap: 8px 12px; width: 100%; padding: 12px 14px; border: 1.5px dashed #cfc6b9; border-radius: 10px; background: #fff; }
.gsm-bio-app-file .input-group-btn { display: block; width: auto; }
.gsm-bio-app-file .input-group .btn-file { border-radius: 6px; }
.gsm-bio-app-file .input-group > .form-control { flex: 1 1 140px; float: none; width: auto; min-width: 0; height: auto; padding: 0; border: 0; border-radius: 0; background: transparent; box-shadow: none; color: #6b5d52; font-size: 12.5px; line-height: 18px; }
.gsm-bio-app-file .progress { height: 0; margin: 0; border-radius: 6px; background: #f5efe7; box-shadow: none; }
.gsm-bio-app-file .progress[style*='visible'] { height: 18px; margin-top: 8px; }
.gsm-bio-app-file .progress-bar { background-color: #ece2d7; box-shadow: none; color: #5c4f45; font-size: 11.5px; line-height: 18px; }
.gsm-bio-app-file .progress-bar.progress-bar-danger, .gsm-bio-app-file .progress-bar.bar-danger { background-color: #b91c1c; color: #fff; }
.gsm-bio-app-chosen-head { display: flex; flex-wrap: wrap; align-items: center; gap: 6px 10px; margin: 12px 0 6px; }
.gsm-bio-app-data .gsm-bio-app-remove { margin-left: auto; padding: 3px 10px; font-size: 12px; }
.gsm-bio-app-columns { display: grid; gap: 6px; max-width: 660px; margin: 8px 0 14px; }
.gsm-bio-app-ask { display: grid; grid-template-columns: 170px minmax(0, 1fr) 84px; align-items: center; gap: 4px 12px; font-size: 12.5px; }
.gsm-bio-app-ask .form-group { display: contents; }
.gsm-bio-app-ask label { margin: 0; color: #2a211b; font-weight: 400; line-height: 18px; }
.gsm-bio-app-ask select.form-control { width: 100%; height: 30px; padding: 4px 8px; border: 1px solid #cfc6b9; border-radius: 6px; background-color: #fff; box-shadow: none; color: #2a211b; font-size: 12.5px; }
.gsm-bio-app-ask select:invalid { border-color: #e0a467; background-color: #fff7ed; color: #9a3412; }

.gsm-bio-app-draw { display: flex; flex-wrap: wrap; align-items: flex-start; gap: 12px 14px; margin-top: 14px; }
.gsm-bio-app-draw-button { flex: 0 1 330px; min-width: 0; }
.gsm-bio-app-draw > .shiny-html-output { flex: 1 1 320px; min-width: 0; }
.gsm-bio-app-data .gsm-bio-app-draw .btn-primary, .gsm-bio-app-data .gsm-bio-app-draw .btn-primary:hover, .gsm-bio-app-data .gsm-bio-app-draw .btn-primary:focus { padding: 7px 13px; border-color: #271810; background: #271810; color: #f5ede4; }
.gsm-bio-app-data .gsm-bio-app-draw .btn-primary:hover { background: #3a271c; }
.gsm-bio-app-draw .gsm-bio-app-what { margin: 6px 0 0; }
.gsm-bio-app-done { padding: 8px 12px; border: 1px solid #bbf7d0; border-left: 3px solid #166534; border-radius: 6px; background: #f0fdf4; color: #14532d; font-size: 12.5px; line-height: 18px; }
.gsm-bio-app-done p { margin: 0 0 6px; }
.gsm-bio-app-ready { margin: 0; padding: 0; list-style: none; }
.gsm-bio-app-ready li { display: flex; flex-wrap: wrap; gap: 0 8px; padding: 2px 0; }
.gsm-bio-app-ready a, .gsm-bio-app-ready a:hover, .gsm-bio-app-ready a:focus { color: #14532d; font-weight: 600; text-decoration: underline; }
.gsm-bio-app-ready .gsm-bio-app-ready-lacks, .gsm-bio-app-ready .gsm-bio-app-ready-lacks a { color: #57534e; }

@media (max-width: 900px) {
  .gsm-bio-app-data { grid-template-columns: minmax(0, 1fr); gap: 12px; }
}
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
  .gsm-bio-app-data .gsm-bio-app-card, .gsm-bio-app-rail { padding: 12px; }
  .gsm-bio-app-ask { grid-template-columns: minmax(0, 1fr) auto; }
  .gsm-bio-app-ask label { grid-column: 1 / -1; }
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
# to a reader who cannot see it, and walks the row with the arrow keys. On the
# Data page (#85) a link that names a chart opens it, and the control that
# takes a file away empties the file's own control as well, which the session
# cannot do, so the same file can be chosen again.
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
  $(document).on('click', '[data-gsm-bio-open]', function (event) {
    event.preventDefault();
    var pill = document.querySelector('#gsm_bio_chart a[data-value=\"' + this.getAttribute('data-gsm-bio-open') + '\"]');
    if (pill) {
      pill.click();
      window.scrollTo(0, 0);
    }
  });
  $(document).on('click', '.gsm-bio-app-remove', function () {
    var file = document.getElementById(this.getAttribute('data-file'));
    if (!file) return;
    var place = $(file).closest('.shiny-input-container');
    file.value = '';
    place.find('input[type=\"text\"]').val('');
    place.find('.progress').css('visibility', 'hidden');
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

# Every tag of one name inside a tag, changed: a link, a select, a list.
App_Change <- function(xTag, strName, Change) {
  if (inherits(xTag, "shiny.tag")) {
    if (identical(xTag$name, strName)) {
      return(Change(xTag))
    }
    xTag$children <- lapply(xTag$children, App_Change, strName, Change)
  } else if (is.list(xTag)) {
    xTag[] <- lapply(xTag, App_Change, strName, Change)
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
  xList <- App_Change(xSet$children[[which(bList)]], "a", function(xLink) {
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
