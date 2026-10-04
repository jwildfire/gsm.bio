# Group Comparison Figure

A static ggplot2 figure of the view the group comparison chart opens on:
one biomarker's value across the levels of a category, as boxes with the
participants as points, one panel per visit, with R's test of each panel
printed in its heading. It is the figure
[`Widget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Widget_GroupComparison.md)
draws in the browser, from the same settings, and its tests are
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md)'s
on the same rows.

## Usage

``` r
Visualize_GroupComparison(dfResults, dfParticipants = NULL, lSettings = list())
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
  filters, and offers its category columns to group, colour and panel
  by; it also says who the participants are. Without it a group comes
  from a column carried on the results rows. Default: `NULL`.

- lSettings:

  `list` bio.viz group comparison settings, as
  [`Widget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Widget_GroupComparison.md)
  takes them, and `title`, `subtitle` and `footnotes`. A figure is of
  one biomarker, so `start_value` must name it. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A `ggplot` object.

## Titles and footnotes

The settings `title`, `subtitle` and `footnotes` are text with named
placeholders, as the chart takes them: a name in braces is replaced by
the text of its value, and a name the figure does not have is left as
written. This figure fills `{measure}`, `{visits}`, `{value}`,
`{group}`, `{n}` (the participants drawn), `{filters}`, `{date}` and
`{version}`. The last line under the figure is always its own: the date
it was drawn, by gsm.bio, and R's method and counts behind each test
printed, with the R and gsm.bio versions that computed them.

## See also

[`Widget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Widget_GroupComparison.md),
its interactive twin, and
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
which computes the tests.

Other figures:
[`Run_Specifications()`](https://jwildfire.github.io/gsm.bio/reference/Run_Specifications.md),
[`Visualize_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_AssociationScatter.md),
[`Visualize_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_BiomarkerScreen.md),
[`Visualize_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CorrelationMatrix.md),
[`Visualize_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CrossTab.md),
[`Visualize_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_StratifiedSurvival.md)

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  Visualize_GroupComparison(
    Synthetic_Results,
    Synthetic_Participants,
    lSettings = list(
      start_value = "IL-6",
      visits = c("Week 4", "Week 8"),
      value_type = "change",
      baseline_visits = "Baseline",
      group_by = "ARM",
      title = "{measure}: {value} by {group}",
      subtitle = "{n} participants, at {visits}"
    )
  )
}

```
