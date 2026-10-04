# Correlation Matrix Figure

A static ggplot2 figure of the grid the correlation matrix opens on: one
cell per pair of variables, coloured by R's coefficient and labelled
with it, across biomarkers at one visit or across the visits of one
biomarker. It is the grid
[`Widget_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CorrelationMatrix.md)
draws in the browser, from the same settings, and its coefficients are
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md)'s
on the same frame. A cell R did not compute, too few pairs among them,
is left blank.

## Usage

``` r
Visualize_CorrelationMatrix(
  dfResults,
  dfParticipants = NULL,
  lSettings = list()
)
```

## Arguments

- dfResults:

  `data.frame` Long-format results, one row per participant, biomarker
  and visit. Column names are supplied by `lSettings`; the defaults
  expect `USUBJID`/`TEST`/`STRESN`/`VISIT`/`VISITNUM`/`STRESU`, the
  columns of
  [Synthetic_Results](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Results.md).

- dfParticipants:

  `data.frame` One row per participant, or `NULL`. With it the chart has
  filters; it also says who the participants are. Default: `NULL`.

- lSettings:

  `list` bio.viz correlation matrix settings, as
  [`Widget_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CorrelationMatrix.md)
  takes them, and `title`, `subtitle` and `footnotes`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A `ggplot` object.

## Titles and footnotes

The settings `title`, `subtitle` and `footnotes` are text with named
placeholders, as for
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md).
This figure fills `{heading}` (what the grid is of), `{variables}` (how
many), `{visit}`, `{value}`, `{n}` (the participants in the frame),
`{filters}`, `{date}` and `{version}`.

## See also

[`Widget_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CorrelationMatrix.md),
its interactive twin.

Other figures:
[`Visualize_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_AssociationScatter.md),
[`Visualize_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_BiomarkerScreen.md),
[`Visualize_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CrossTab.md),
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md),
[`Visualize_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_StratifiedSurvival.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  Visualize_CorrelationMatrix(
    Synthetic_Results,
    Synthetic_Participants,
    lSettings = list(visit = "Baseline", title = "{heading}")
  )
}

```
