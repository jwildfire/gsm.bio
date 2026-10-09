# Run the six charts as one Shiny app

A Shiny app with a list of the six bio.viz charts beside one chart drawn
at a time, on the tables it is given or, given none, on the synthetic
study that ships with the package. The R session behind the page answers
every statistic a chart asks for
([`Serve_Statistics()`](https://jwildfire.github.io/gsm.bio/reference/Serve_Statistics.md)),
so a reader who changes a test, a group or a filter gets R's result for
that view, and the line under the chart says it was computed on this
server and by which R.

## Usage

``` r
RunApp(
  dfResults = NULL,
  dfParticipants = NULL,
  dfOutcomes = NULL,
  lSettings = list(),
  nMaxUploadMB = 100
)
```

## Arguments

- dfResults:

  `data.frame` The results table, or `NULL` for the synthetic study.

- dfParticipants:

  `data.frame` The participants table, or `NULL` for none.

- dfOutcomes:

  `data.frame` The outcomes table, or `NULL` for none.

- lSettings:

  `list` Settings for the charts, a list for each under its chart's
  name: `GroupComparison`, `AssociationScatter`, `CorrelationMatrix`,
  `BiomarkerScreen`, `CrossTab` or `StratifiedSurvival`. Each is what
  that chart's widget takes as `lSettings`, under bio.viz's setting
  names. A chart not named opens on its defaults.

- nMaxUploadMB:

  `numeric` The largest file the app accepts from a reader, in
  megabytes. Shiny's own limit is 5.

## Value

A Shiny app object. Printing it runs the app.

## Details

The charts are the package's widgets with the controls they have. Shiny
holds the tables and answers the statistics, and does nothing else: no
control of a chart is made again as a Shiny input.

## The tables

The app reads its tables under gsm.bio's column names, so a table with
other names is renamed before the call:

- results, needed: `USUBJID`, `TEST`, `STRESN`, `VISIT` and `VISITNUM`,
  one row per participant, biomarker and visit;

- participants, optional: `USUBJID` and whatever columns describe a
  participant. With it a chart offers groups and filters; without it a
  chart has none;

- outcomes, optional: `USUBJID`, `PARAMCD`, `PARAM`, `AVAL` and `CNSR`.
  Without it the stratified survival chart is replaced by a sentence
  saying so.

A table that lacks a column is refused with a sentence naming the
column. Called with no table, the app opens on
[Synthetic_Results](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Results.md),
[Synthetic_Participants](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Participants.md)
and
[Synthetic_Outcomes](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Outcomes.md).

## A reader's own files

The first entry of the list is the Data view. A reader chooses a results
file there, and optionally a participants and an outcomes file, each a
`.csv`, `.xpt` or `.sas7bdat` file. R reads it on the server. Under each
file the view asks which of its columns is each one the charts need,
filled in already where a column has gsm.bio's own name. On the button
the columns are renamed to gsm.bio's names and the charts are drawn on
the reader's tables.

Nothing is drawn on a table until every column is said: a column left
unsaid, a column chosen twice, a result that is text and a file R cannot
read are each answered with a sentence, and the tables already drawn
stay. A column of the file that already had one of gsm.bio's names, and
was not the one chosen for it, is kept with `_original` added to its
name.

A file is held in the session's memory and nowhere else. Nothing is
written to the server beyond Shiny's own temporary copy of an upload,
which goes when the session ends, and nothing is kept between sessions.
A `.xpt` or `.sas7bdat` file is read with haven, which is suggested, not
imported: without it the view says so and reads `.csv` files only.

## On a server

`RunApp()` returns the app and starts nothing itself, so the same call
serves an R session, where printing the app runs it, and the last line
of an `app.R` on a server such as Posit Connect:

    library(shiny)
    library(gsm.bio)
    dfResults <- readRDS("results.rds")
    RunApp(dfResults)

The tables are held in the R session's memory. The rows a chart draws
are sent to the session with each request for a statistic, and the
session runs the nine `Analyze_*` functions and no other.

shiny is suggested, not imported: without it `RunApp()` stops with a
sentence naming the package to install.

## See also

[`Serve_Statistics()`](https://jwildfire.github.io/gsm.bio/reference/Serve_Statistics.md)
and the output and render functions in
[gsm.bio-shiny](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md),
which the app is made of.

Other shiny:
[`Serve_Statistics()`](https://jwildfire.github.io/gsm.bio/reference/Serve_Statistics.md),
[`gsm.bio-shiny`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)

## Examples

``` r
if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
  # The synthetic study.
  RunApp()

  # A study's own tables, with the group comparison opened on one biomarker
  # by arm.
  RunApp(
    Synthetic_Results, Synthetic_Participants,
    lSettings = list(GroupComparison = list(start_value = "CRP", group_by = "ARM"))
  )
}
```
