# Run Chart Specifications to Figures and Tables

Reads a list of bio.viz chart specifications, the format a bio.viz chart
writes with `specification()` (bio.viz, docs/output.md,
"Specifications"), and draws each against the tables given: a static
figure by the chart's `Visualize_*()` function and an RTF table by its
`Table_*()` function and
[`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md),
into a folder, with a manifest of what was written. One specification
can be run across every biomarker, one figure and one table per
biomarker.

## Usage

``` r
Run_Specifications(
  xSpecifications,
  dfResults,
  dfParticipants = NULL,
  dfOutcomes = NULL,
  strFolder,
  bAcrossBiomarkers = FALSE,
  chrFormats = "png",
  bTables = TRUE,
  nWidth = 9,
  nHeight = 6
)
```

## Arguments

- xSpecifications:

  The specifications: a JSON file, JSON text, or the list
  [`jsonlite::read_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html)
  (or `jsonlite::fromJSON(simplifyVector = FALSE)`) reads one as. A JSON
  array of specifications, or one specification. `fromJSON()`'s default
  simplifies them into a data frame or vectors, which are refused with a
  sentence that says so.

- dfResults:

  `data.frame` The results table, one row per participant, biomarker and
  visit.

- dfParticipants:

  `data.frame` One row per participant, or `NULL`. Default: `NULL`.

- dfOutcomes:

  `data.frame` The outcomes table, for the stratified survival chart and
  the screen's hazard ratio, or `NULL`. Default: `NULL`.

- strFolder:

  `character` The folder to write into, made if it is not there. A file
  already there with an output's name is replaced, and any other file
  there is left as it is: a run does not empty the folder. A view that
  fails part way leaves none of its files.

- bAcrossBiomarkers:

  `logical` Whether to run each specification across every biomarker:
  one value for all, or one per specification. Default: `FALSE`.

- chrFormats:

  `character` The figure's formats, any of `"png"`, `"pdf"` and `"svg"`.
  An SVG needs the svglite package. A PDF is drawn by cairo where this R
  can load it; where it cannot (a Mac without XQuartz, for one), by the
  pdf device, which draws a character beyond Latin-1, such as the sign
  of a cut, as a dot or a stand-in such as `<=`, and the row's `reason`
  says so. Default: `"png"`.

- bTables:

  `logical` Whether to write each table to RTF, which needs r2rtf.
  Default: `TRUE`.

- nWidth, nHeight:

  `numeric` A figure's size, in inches. Default: `9` by `6`.

## Value

A `data.frame`, the manifest, one row per output. It has these columns:

- `specification`: the specification's place in the list;

- `chart`;

- `biomarker`: `NA` when the view is not one of several;

- `status`: `"written"`, `"refused"` (the specification could not be
  read), or `"failed"` (it was read and could not be drawn);

- `reason`: why a specification was refused or a view failed; for a view
  written, what was not drawn as asked, a note of explicit cut points, a
  table that could not be made, or a PDF drawn without cairo; `NA` when
  there is nothing to say;

- `participants`: how many participants the view's filters keep;

- `title` and `subtitle`, filled;

- `statistics`: the lines printed under the figure;

- `figure`: the figure's files, separated by `;`;

- `table`: the RTF table's file.

File names are relative to `strFolder`, each output's its own: the
specification's place, the chart, and the biomarker's name as a file
name can hold it (its place among the views when nothing of the name can
be kept), numbered `-2`, `-3` and on when two would be the same. The
manifest is also written there as `manifest.json`.

## A specification is data

A specification is read by jsonlite as data: text, numbers, `true`,
`false`, `null`, lists and objects. Nothing in it is evaluated. A title
or a setting that looks like code is text, filled and drawn as text. No
name in a specification chooses a function: a chart's statistics are
computed by calling its own `Analyze_*()` functions from a fixed list
(the one place the package calls a function it is handed, through
[`do.call()`](https://rdrr.io/r/base/do.call.html)).

Each specification is read as bio.viz reads one, and refused, with a
sentence naming why, as bio.viz refuses it:

- one of another `format` or `format_version`, or with a member the
  format does not have;

- one whose `bio_viz_version` is not text;

- a chart bio.viz does not have, or a setting the chart does not have;

- a value the chart refuses;

- a filter whose operator is not `in`, that names no column or a column
  another filter is on, or that lists a value twice;

- `__proto__`, `constructor` or `prototype` as a column;

- nesting deeper than 64;

- a list or object setting of a shape the chart does not take, in
  bio.viz's words.

A refused specification is a row of the manifest with its reason. The
rest still run. The tests hold the reader to bio.viz's own, run in node
on the same specifications, over every setting of every chart. It
differs on purpose in two places:

- a chart's `statistic`, and the scatter's `fit_statistic`, must be the
  chart's own `Analyze_*()` function or `null`: bio.viz takes the name
  of any R function, and gsm.bio computes only its own;

- a specification given as text is checked for depth, as one given as an
  object is; bio.viz checks only an object (bio.viz#74).

A refusal of a value the chart's own rules check (a choice it does not
have, a number out of range) is in R's words, which are not always
bio.viz's; which values are refused is the same.

The filters in force, `{ column, operator: "in", values }`, are laid
onto the chart's `filters` setting as where each starts, as bio.viz
does. A value is compared as text.

## Across every biomarker

With `bAcrossBiomarkers`, the setting that holds a chart's biomarker
takes each biomarker the chart offers in turn:

- the group comparison's `start_value`;

- the association scatter's `x`, or `y` when `x` is not a biomarker;

- the correlation matrix's `measure`, across visits;

- a cut biomarker in the cross-tabulation's `row_by` or `col_by`;

- a cut biomarker in the stratified survival chart's `group_by`.

A chart that is already of every biomarker, such as the screen or a
matrix across biomarkers, or one with no biomarker to take, is one view.
The scatter's other axis keeps its own biomarker out of the views only
when the two axes are at the same visit. A cut named by its rule (a
median, tertiles) is worked out on each biomarker's values; explicit cut
points are applied to every biomarker unchanged, and each row's `reason`
says so.

## What the tables cannot honour

A view is drawn as its chart draws it from the tables given. Where a
setting names something the tables lack (a column, a biomarker, a
visit), or a filter is on a column that is not a filter or on a value
its column lacks, the chart draws what it falls back to, and the row's
`reason` says what was not drawn as asked, in the words of bio.viz's
notices; its status stays `"written"`. A view whose filters keep no
participant is `"failed"` with "No participant passes the filters.", as
the chart's footnote says.

## See also

[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md)
and
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md)
and the functions beside them, which draw each output.

Other figures:
[`Visualize_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_AssociationScatter.md),
[`Visualize_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_BiomarkerScreen.md),
[`Visualize_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CorrelationMatrix.md),
[`Visualize_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CrossTab.md),
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md),
[`Visualize_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_StratifiedSurvival.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("r2rtf", quietly = TRUE)) {
  strSpecifications <- '[{
    "format": "bio.viz specification", "format_version": 1, "bio_viz_version": "0.3.0",
    "chart": "group-comparison",
    "settings": {
      "start_value": "IL-6", "visits": ["Week 4"], "value_type": "change",
      "baseline_visits": ["Baseline"], "group_by": "ARM", "title": "{measure} by {group}"
    },
    "filters": []
  }]'
  Run_Specifications(
    strSpecifications,
    Synthetic_Results,
    Synthetic_Participants,
    strFolder = tempfile("figures")
  )
}
#>   specification            chart biomarker  status reason participants
#> 1             1 group-comparison      <NA> written   <NA>          200
#>         title subtitle
#> 1 IL-6 by ARM     <NA>
#>                                                                                                                                                                                                                                                                                                                                                 statistics
#> 1 Week 4 Welch Two Sample t-test: p < 0.001 (Placebo n = 95, Treatment n = 91). Exploratory, unadjusted. Difference in means (Placebo - Treatment): 1.235, 95% confidence interval 0.844 to 1.626. | Drawn on 2026-10-07 by gsm.bio 0.3.0. Statistics: Welch Two Sample t-test (Placebo n = 95, Treatment n = 91); computed by R 4.6.1 with gsm.bio 0.3.0.
#>                    figure                   table
#> 1 01-group-comparison.png 01-group-comparison.rtf
```
