# gsm.bio

The statistics behind the bio.viz biomarker charts: plain R functions
that wrap the stats and survival packages, a seeded synthetic study to
prove them on, and htmlwidgets that draw the charts from R with the
statistics stored in the page. `dev` is the integration branch (merges
on green checks); `main` is the release branch (@jwildfire’s review).

# Standards

The obot program’s standards are mandatory here: the issue contract,
ways of working and developer guidelines in jwildfire/obot.roadmap
`docs/` (on disk at ~/obot.roadmap/docs/ in a cloud environment). Work
runs one requirement per session
(`/requirement-session <hub requirement>`).

# Commands

- `devtools::document()` — regenerate `man/` and `NAMESPACE` after any
  roxygen change; it must leave no diff.
- `devtools::test()` — the testthat suite (edition 3). Tests that read
  files outside the built package (the repository guards, the rerun of
  the data script) run here and skip under check; CI runs the suite a
  second time from the source tree, where a skip fails the run.
- `devtools::check()` — must be clean (no errors, warnings or notes)
  before a PR opens; CI runs the same check as `R-CMD-check` and fails
  on a note. Where R cannot reach its time service the check adds one
  note, `unable to verify current time`, which is about the machine and
  not the package: run it with `_R_CHECK_SYSTEM_CLOCK_=0` set, as CI
  does.
- `Rscript data-raw/synthetic-study.R` — regenerates the synthetic
  study: `data/Synthetic_*.rda` and `inst/extdata/synthetic_*.csv`. It
  is seeded, so it must leave no diff unless the model changed.
- [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
  — builds the reference site into `docs/` (not committed); CI deploys
  it from `dev` to the `gh-pages` branch.
- `Rscript data-raw/vendor-bio-viz.R` — copies what gsm.bio takes from
  bio.viz’s `dev` again, with its records: the two bundles the widgets
  load (`inst/htmlwidgets/lib/`), the fixtures the tests hold R to
  (`tests/testthat/fixtures/bio.viz/`), the frames the copied bundle’s
  own core writes (`tests/testthat/fixtures/core-frames/frames.json`),
  and what its safety.viz kit opens each filter on
  (`tests/testthat/fixtures/filter-states/states.json`). It needs git, a
  network connection and node. Run it when bio.viz’s `dev` moves, then
  run the suite: a test that fails names the rule in `R/core.R`,
  `R/chart.R` or a chart’s own file that bio.viz changed.

# Conventions

- Every `test_that()` name ends with the issue it proves, `(#N)`;
  `tests/testthat/test-qcthat-convention.R` fails the suite otherwise.
- Imports are stats and survival for the statistics, htmlwidgets for the
  widgets, and grDevices (part of R) for the device
  [`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)
  measures text on, and nothing else; `tests/testthat/test-package.R`
  fails the suite otherwise. A package needed only to check a result
  goes under Suggests.
- A statistic is a thin wrapper around the base R function the design
  names. Nothing is reimplemented except the standardised difference.
- The statistics have one definition: `inst/statistics/statistics.R`.
  `R/statistics.R` evaluates that file into the namespace and holds only
  the documentation, so edit the functions in the file under `inst/`,
  never copy them into `R/`. The file must run in a bare session with
  only stats and survival attached: call `stats::` and `survival::` by
  name, attach nothing, evaluate no text.
- A widget computes no statistic of its own: every stored result is an
  `Analyze_*` function’s answer. It is named `Widget_<Chart>()` and
  takes its tables, then `lSettings`, a list under bio.viz’s own setting
  names, then `width`, `height`, `elementId` and `bDebug`, as
  gsm.safety’s widgets do. The outcomes table, which came after v0.1.0,
  is `dfOutcomes`, after `lSettings` and before `width`, in every widget
  that reads one, so a released widget’s positional calls keep working.
- A static figure is `Visualize_<Chart>()`, takes the same arguments as
  its widget less the sizes, and returns a `ggplot`. It draws the view
  its chart opens on from the same rules, prints statistics only from
  `Analyze_*()` on the same rows, and writes its title and footnotes
  with `R/output.R`. ggplot2 is in Suggests: `R/figure.R` says so when
  it is missing, and every ggplot2 call is `ggplot2::`. Its snapshot
  (`tests/testthat/_snaps/Visualize.md`) is of the figure’s own data and
  labels, the same in every locale and ggplot2 version; accept a change
  with `testthat::snapshot_accept("Visualize")` only after looking at
  it.
- A statistics table is `Table_<Chart>()`, takes the same arguments as
  its widget less the sizes, and returns a data frame of text, one row
  per statistic, whose numbers are `Analyze_*()` answers on the request
  the chart makes, written by `R/output.R`’s display rules; its R
  answers are its attribute `results`.
  [`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)
  writes one with r2rtf (Suggests), escaping text as RTF itself so the
  file is the same in every locale.
- A file copied from bio.viz is never edited here: change it in bio.viz
  and copy it again. `tests/testthat/test-vendored.R` fails the suite
  when a copied file and its record disagree.
- `R/core.R`, `R/chart.R` and one file per chart (`R/GroupComparison.R`,
  `R/AssociationScatter.R`, `R/CorrelationMatrix.R`,
  `R/BiomarkerScreen.R`, `R/CrossTab.R`, `R/StratifiedSurvival.R`) and
  `R/output.R` (bio.viz’s rules for titles, footnotes and the statistics
  line) are bio.viz’s rules written a second time, so that R computes on
  the rows the chart draws: which participants are in a panel, and how
  the chart asks for a panel’s result. `R/core.R` is bio.viz’s core,
  `R/chart.R` what every chart shares (bio.viz’s `src/shared/`, the
  outcomes table’s rules among it), and a chart’s own file its own
  rules. They are the only such copies. The tests hold them to frames
  and requests written by bio.viz’s own code, so change them only to
  follow bio.viz.
- A widget’s binding names its chart and nothing more:
  `inst/htmlwidgets/shared/gsm.bio.widget.js` is the script every
  binding is made with, and `R/utils-widget.R` what every `Widget_*()`
  function is made with. A new widget adds its chart’s rules, a few
  lines of binding, and its name to `chrWidgets` in
  `data-raw/vendor-bio-viz.R`, which writes its dependency file.
- The proof that a widget saves as one self-contained file needs pandoc.
  In the source tree the test fails without it; it never skips there.
- Every `Analyze_*` function returns the one result shape documented in
  [`?StatisticsResult`](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md)
  and built by `Stat_Result()`; `tests/testthat/helper-result-shape.R`
  checks it. It never raises an error or a warning: both go into the
  result.
- Argument and variable prefixes follow gsm.core: `df` data frame, `l`
  list, `str` character scalar, `chr` character vector, `n` numeric, `b`
  logical.
- Public or synthetic data only.
- `Synthetic_Truth` is the data generator’s own parameter list. A test
  of a planted effect reads the truth from it and never types the number
  again.
- `NEWS.md` is always current on `dev`: unreleased work goes under the
  `vX.Y.Z (Upcoming)` heading as it lands, one user-facing bullet per
  feature linking its hub requirement and PR.
- One branch per task, `<task-number>-<slug>`, off `dev`; the PR body
  carries `Closes #<task>` and the definition-of-done evidence.
