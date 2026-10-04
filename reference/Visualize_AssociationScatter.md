# Association Scatter Figure

A static ggplot2 figure of the view the association scatter opens on:
one point per participant with a variable on each axis, a panel per
level of `panel_by`, R's fitted line and its band when `fit` asks for
one, and R's correlation coefficient of each panel printed under the
figure. It is the figure
[`Widget_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Widget_AssociationScatter.md)
draws in the browser, from the same settings; the coefficient is
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md)'s
and the line
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md)'s,
on the same rows.

## Usage

``` r
Visualize_AssociationScatter(
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
  filters, offers its category columns to colour and panel by and its
  numeric columns on either axis; it also says who the participants are.
  Without it a colour or a number comes from a column carried on the
  results rows. Default: `NULL`.

- lSettings:

  `list` bio.viz association scatter settings, as
  [`Widget_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Widget_AssociationScatter.md)
  takes them, and `title`, `subtitle` and `footnotes`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A `ggplot` object.

## Titles and footnotes

The settings `title`, `subtitle` and `footnotes` are text with named
placeholders, as for
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md).
This figure fills `{x}` and `{y}` (the axes, as their titles read),
`{n}` (the participants drawn), `{filters}`, `{date}` and `{version}`.

## See also

[`Widget_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Widget_AssociationScatter.md),
its interactive twin.

Other figures:
[`Visualize_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_BiomarkerScreen.md),
[`Visualize_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CorrelationMatrix.md),
[`Visualize_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CrossTab.md),
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md),
[`Visualize_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_StratifiedSurvival.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  Visualize_AssociationScatter(
    Synthetic_Results,
    Synthetic_Participants,
    lSettings = list(
      x = list(measure = "TNF-alpha", visit = "Baseline"),
      y = list(measure = "IL-10", visit = "Baseline"),
      fit = "linear",
      title = "{y} against {x}"
    )
  )
}

```
