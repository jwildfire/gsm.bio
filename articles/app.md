# The app, and putting it on Posit Connect

[`RunApp()`](https://jwildfire.github.io/gsm.bio/reference/RunApp.md) is
the six bio.viz charts as one Shiny app. Shiny does two jobs in it and
no more: it holds the tables, and its R session answers every statistic
a chart asks for. The charts are the package’s widgets, with the
controls they already have.

## Run it

``` r

library(gsm.bio)

RunApp()
```

With no table the app opens on the synthetic study that ships with the
package. Give it a study’s own tables and it opens on those:

``` r

RunApp(dfResults, dfParticipants, dfOutcomes)
```

- The results table is the only one needed: one row per participant,
  biomarker and visit, with the columns `USUBJID`, `TEST`, `STRESN`,
  `VISIT` and `VISITNUM`.
- With a participants table (`USUBJID` and whatever describes a
  participant) the charts offer groups and filters. Without one they
  have none.
- With an outcomes table (`USUBJID`, `PARAMCD`, `PARAM`, `AVAL` and
  `CNSR`) the stratified survival chart is drawn. Without one its place
  holds a sentence saying so.

A table that lacks a column is refused with a sentence naming the
column. `lSettings` opens a chart on a view of your choosing, under that
chart’s name and bio.viz’s setting names:

``` r

RunApp(
  dfResults, dfParticipants,
  lSettings = list(GroupComparison = list(start_value = "CRP", group_by = "ARM"))
)
```

## A reader’s own files

The first entry in the app’s list is the Data view. A reader chooses a
results file there, and optionally a participants and an outcomes file,
each a `.csv`, `.xpt` or `.sas7bdat` file.

- R reads the file on the server. `.xpt` and `.sas7bdat` files are read
  with haven.
- The view shows the file’s first rows as R read them, under the file’s
  own column names.
- Under each file the view asks which column is each one the charts
  need. A column that already has gsm.bio’s name for it is filled in.
- On the button the columns are renamed to gsm.bio’s names and the
  charts are drawn on the reader’s tables.
- Nothing is drawn on a table until every column is said. A column left
  unsaid, a column chosen twice, a result that is text and a file R
  cannot read are each answered with a sentence, and the charts stay as
  they were.

Above the files the Data view shows what is loaded: the tables the
charts are drawn on, ten rows at a time, with where each came from.
After the button it shows the reader’s tables.

The file is held in the R session’s memory and nowhere else. Nothing is
written to the server beyond Shiny’s own temporary copy of an upload,
which goes when the session ends, and nothing is kept between sessions:
a reader who comes back loads the file and says its columns again.

The app accepts files up to 100 MB. Shiny’s own limit is 5 MB;
`nMaxUploadMB` sets another.

## Where the statistics come from

Every statistic is computed on request by the R session behind the page,
by gsm.bio’s own `Analyze_*()` functions, and the line under a chart
says so: “computed by R 4.5.1 with gsm.bio 0.4.0 on this server”. A
saved widget page can answer only what was stored when it was made; the
app answers every view its controls reach.

- The rows a chart draws are sent to the session with each request,
  because the chart works them out in the page: a change from baseline,
  the filters and the cuts are applied there, and R tests those rows and
  no others.
- The session runs the nine `Analyze_*()` functions and nothing else.
  Any other name a page sends is answered with a sentence and nothing is
  called. The rows and the arguments are read as data, never as R code.
- A number crosses to the page as JSON at sixteen significant digits, as
  a stored result’s does.

## On Posit Connect

The deployment is one file. It ships with the package, at
`system.file("app", "app.R", package = "gsm.bio")`:

``` r

library(shiny)
library(haven)
library(gsm.bio)

RunApp()
```

The three packages are attached by name so that the record Connect
installs the app from lists each of them. To open on a study’s own
tables, read them in that file and hand them to
[`RunApp()`](https://jwildfire.github.io/gsm.bio/reference/RunApp.md);
the file’s own comments show how.

Put the file in a folder of its own and deploy the folder:

``` r

dir.create("gsm-bio-app")
file.copy(system.file("app", "app.R", package = "gsm.bio"), "gsm-bio-app")

rsconnect::deployApp("gsm-bio-app", appName = "gsm-bio", appTitle = "Biomarker charts")
```

What the server needs:

- gsm.bio installed on your own machine from GitHub, with
  `remotes::install_github("jwildfire/gsm.bio")` or pak. rsconnect
  records where each package was installed from and refuses one it
  cannot trace to a source, so a copy loaded from a checkout with
  `devtools::load_all()` will not deploy.
- A route from the Connect server to GitHub, to install gsm.bio there,
  and to a CRAN repository for shiny, haven and the rest. gsm.bio is not
  on CRAN.
- A version of R on the server near the one you deploy from. R’s own
  answers can differ between versions, which is why the line under each
  chart names the R that computed it.

Two settings on the content’s Runtime tab are worth a look:

- Several readers share one R process by default, and R does one thing
  at a time: while it computes one reader’s statistic, the others wait.
  Most statistics here take a few thousandths of a second (below). The
  known slow one is Fisher’s exact test on a large table. If readers
  wait, lower “Max connections per process” so Connect starts more
  processes.
- A process that is idle is stopped after a while and the next reader
  waits for R to start. “Min processes” of 1 keeps one running.

## What was measured

On the synthetic study, 11,472 results rows for 200 participants and 12
biomarkers, with the browser and the server on one machine (R 4.3.3, an
Apple laptop). A network adds its own time. `data-raw/app-timing.R` in
the repository measures it again.

From choosing a chart in the list to R’s answer on the page, which
includes sending the tables to the page and drawing the chart:

| Chart               | Time   | Largest message to the session |
|---------------------|--------|--------------------------------|
| Association scatter | 0.25 s | 7 kB                           |
| Correlation matrix  | 0.22 s | 18 kB                          |
| Biomarker screen    | 0.25 s | 20 kB                          |
| Cross-tabulation    | 0.19 s | 7 kB                           |
| Stratified survival | 0.23 s | 6 kB                           |

From choosing another test in the group comparison to its answer, with
the chart already drawn: 16 to 18 thousandths of a second, in one
message of 5 kB.

These are one run’s times. Another run on the same machine differs by a
few hundredths of a second for a chart and a few thousandths for a test.

No chart sends the whole results table with a request: it sends the rows
it draws, which here is at most one row per participant with a column
per biomarker. The table that grows with a study is the one sent to the
page when a chart is opened, all of it, each time.

## What has not been tested

No one has deployed this app to a Posit Connect server yet. The sessions
that built it had no server to deploy to. What stands in for that:

- The suite runs the app in a second R session and drives it in a
  headless browser: each chart is opened, a statistic no saved page
  stores is asked for and held to R’s own answer, and a file of each of
  the three types is loaded and drawn.
- The record Connect installs from was written for real, once, by
  `data-raw/connect-manifest.R`: it installs gsm.bio from GitHub into a
  library of its own and runs `rsconnect::writeManifest()` on the
  deployment folder. What it printed is below.

&nbsp;

    rsconnect 1.5.0, R version 4.3.3 (2024-02-29)
    gsm.bio 0.3.0.9000 from github: jwildfire/gsm.bio@dev at c52f2c7
    app mode: shiny; R 4.3.3
    files: app.R
    62 packages: Matrix, R6, Rcpp, base64enc, bit, bit64, bslib, cachem, cli, clipr, commonmark, cpp11, crayon, digest, evaluate, fansi, fastmap, fontawesome, forcats, fs, glue, gsm.bio, haven, highr, hms, htmltools, htmlwidgets, httpuv, jquerylib, jsonlite, knitr, later, lattice, lifecycle, magrittr, memoise, mime, otel, pillar, pkgconfig, prettyunits, progress, promises, rappdirs, readr, rlang, rmarkdown, sass, shiny, sourcetools, survival, tibble, tidyselect, tinytex, tzdb, utf8, vctrs, vroom, withr, xfun, xtable, yaml
    gsm.bio is listed
    shiny is listed
    haven is listed

The first real deployment is the check on the rest: whether the server
can reach GitHub, how long R takes to start there, and how the app
behaves with several readers. What it finds belongs in an issue on
[gsm.bio](https://github.com/jwildfire/gsm.bio/issues).
