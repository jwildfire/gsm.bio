# Run the six charts as one Shiny app

A Shiny app of the six bio.viz charts, one drawn at a time and chosen
from a row of pills in the page's header, on the tables it is given or,
given none, on the synthetic study that ships with the package. The R
session behind the page answers every statistic a chart asks for
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
control of a chart is made again as a Shiny input. The Data page's own
controls are Shiny inputs: the three files, a select for each column of
a chosen file, the control that takes a file away, and the button.

## The page

A header band carries the app's name, "Biomarker charts", with the mark
gsm.bio and its version beside it; a row of pills, Data first and then
the six charts; and a chip that says what the charts are drawn on, on
every page, and opens Data when it is pressed. The chart has the page's
width under the band, and one footer line says which R computes the
statistics and that a reader's files are held for the session only.

- A pill is a link of Shiny's own tab set. A keyboard reaches the chosen
  pill with the Tab key and walks the row with the arrow keys, and a
  pill pointed at says in one line what its chart draws.

- A chart that cannot be drawn on the tables there are has its pill
  dimmed, with the reason as its hover text. The pill still opens the
  chart's page, which holds the same sentence.

- On a phone the header is two rows and the pills scroll sideways in
  their own row, with the chosen one brought into view.

- The page asks Google Fonts for the two fonts bio.viz's and
  safety.viz's sites use, Instrument Sans and Instrument Serif, and for
  nothing else outside its own server. It does not wait for them: where
  they cannot be reached, as behind a firewall, the page is drawn in the
  system's fonts.

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
  saying so, and its pill is dimmed.

A table that lacks a column is refused with a sentence naming the
column. Called with no table, the app opens on
[Synthetic_Results](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Results.md),
[Synthetic_Participants](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Participants.md)
and
[Synthetic_Outcomes](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Outcomes.md).

## A reader's own files

The first pill opens the Data page. It has a card for each table:
results, which the charts need, and participants and outcomes, which are
optional. A reader chooses a file in a card, each a `.csv`, `.xpt` or
`.sas7bdat` file, and R reads it on the server. The card then asks which
of the file's columns is each one the charts need. A column that has
gsm.bio's own name is filled in already and tagged "same name"; one the
reader has still to say is amber and tagged "say which". On the button
the columns are renamed to gsm.bio's names and the charts are drawn on
the reader's tables, and the page lists the charts that are ready, each
with what it draws. Each opens from that list, and a chart that lacks a
table says which.

A rail beside the cards, above them on a phone, counts what is left in
three steps: the files chosen and any R could not read, the columns
still to say, and the charts that are ready. Under the steps it says
what the charts are drawn on now.

Nothing is drawn on a table until every column is said: a column left
unsaid, a column chosen twice, a result that is text and a file R cannot
read are each answered with a sentence beside the button, and the tables
already drawn stay. A file R cannot read is also reported in the card it
was chosen in. A column of the file that already had one of gsm.bio's
names, and was not the one chosen for it, is kept with `_original` added
to its name.

A file is taken away with the Remove control in its card. Until it is,
it is one of the files the button draws: the page names them all beside
the button, so a file chosen for an earlier study is seen before it is
drawn with a later one. An optional file R could not read holds the
button until it is removed or another is chosen in its place.

The Data page also shows what is loaded: the tables the charts are drawn
on, ten rows at a time, with where each came from and its rows and
columns. A file just chosen shows its first rows under its own column
names, so a reader can tell which column is which. Values are shown as R
holds them. A number is written in full to the fifteen digits that
identify it, never as `1e+05`, unless it is a thousand million million
or more, or smaller than a part in that many; those are left in R's
scientific form.

A file is held in the session's memory and nowhere else. Nothing is
written to the server beyond Shiny's own temporary copy of an upload,
which goes when the session ends, and nothing is kept between sessions.
A `.xpt` or `.sas7bdat` file is read with haven, which is suggested, not
imported: without it the page says so and reads `.csv` files only.

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
