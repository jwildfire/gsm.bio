# Cross-Tabulation Figure

A static ggplot2 figure of the table the cross-tabulation opens on: a
bar per row of the table, stacked by its columns as the chart's bars
are, each segment labelled with its count, and R's test of the table
printed under the figure. With `percent = "row"` (the default) a bar is
the row's percentages; with `"col"` a bar is a column's; with `"none"`
the counts themselves. It is the table
[`Widget_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CrossTab.md)
draws in the browser, from the same settings, and its test is
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md)'s
on the same rows.

## Usage

``` r
Visualize_CrossTab(dfResults, dfParticipants = NULL, lSettings = list())
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
  filters, offers its category columns to the Rows and Columns controls,
  and its numbers can be cut. Default: `NULL`.

- lSettings:

  `list` bio.viz cross-tabulation settings, as
  [`Widget_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CrossTab.md)
  takes them, and `title`, `subtitle` and `footnotes`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A `ggplot` object.

## Titles and footnotes

The settings `title`, `subtitle` and `footnotes` are text with named
placeholders, as for
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md).
This figure fills `{rows}` and `{columns}` (what they are, as the
controls name them), `{n}` (the participants in the table), `{filters}`,
`{date}` and `{version}`.

## See also

[`Widget_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CrossTab.md),
its interactive twin.

Other figures:
[`Visualize_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_AssociationScatter.md),
[`Visualize_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_BiomarkerScreen.md),
[`Visualize_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CorrelationMatrix.md),
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md),
[`Visualize_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_StratifiedSurvival.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  Visualize_CrossTab(
    Synthetic_Results,
    Synthetic_Participants,
    lSettings = list(
      row_by = "RESPONSE",
      col_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
      title = "{rows} by {columns}"
    )
  )
}

```
