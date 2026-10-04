# Biomarker Screen Figure

A static ggplot2 figure of the screen the biomarker screen opens on: one
row per biomarker, R's estimate and its interval on one shared axis,
with the estimate, the unadjusted p-value and the adjusted one printed
beside each row's name. The rows run from the largest estimate down, as
the screen sorts them by default. It is the screen
[`Widget_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Widget_BiomarkerScreen.md)
draws in the browser, from the same settings, and its rows are
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md)'s
on the same frame. A hazard ratio is drawn on a logarithmic axis, with 1
marked.

## Usage

``` r
Visualize_BiomarkerScreen(
  dfResults,
  dfParticipants = NULL,
  lSettings = list(),
  dfOutcomes = NULL
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
  filters, and the columns of groups and the numbers it offers are read
  from it. Default: `NULL`.

- lSettings:

  `list` bio.viz biomarker screen settings, as
  [`Widget_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Widget_BiomarkerScreen.md)
  takes them, and `title`, `subtitle` and `footnotes`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

- dfOutcomes:

  `data.frame` An outcomes table, one row per participant and endpoint
  with a time and a flag, as
  [`Widget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Widget_StratifiedSurvival.md)
  takes it, or `NULL`. With it the screen offers a hazard ratio. It
  comes after `lSettings`, so a call written for v0.1.0 works as it did.
  Default: `NULL`.

## Value

A `ggplot` object.

## Titles and footnotes

The settings `title`, `subtitle` and `footnotes` are text with named
placeholders, as for
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md).
This figure fills `{heading}` (what the rows are), `{comparison}`,
`{visit}`, `{endpoint}` (a hazard ratio's endpoint), `{biomarkers}` (how
many are screened), `{n}` (the participants in the frame), `{filters}`,
`{date}` and `{version}`.

## See also

[`Widget_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Widget_BiomarkerScreen.md),
its interactive twin.

Other figures:
[`Run_Specifications()`](https://jwildfire.github.io/gsm.bio/reference/Run_Specifications.md),
[`Visualize_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_AssociationScatter.md),
[`Visualize_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CorrelationMatrix.md),
[`Visualize_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CrossTab.md),
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md),
[`Visualize_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_StratifiedSurvival.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  Visualize_BiomarkerScreen(
    Synthetic_Results,
    Synthetic_Participants,
    lSettings = list(visit = "Week 4", value_type = "change", group_by = "ARM", title = "{heading}")
  )
}

```
