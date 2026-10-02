# Package index

## Package

What gsm.bio is for and how its statistics are built.

- [`gsm.bio`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-package.md)
  [`gsm.bio-package`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-package.md)
  : gsm.bio: statistics and widgets for biomarker charts

## Statistics

One function per family of tests, each a thin wrapper around the base R
function the design names, and all returning the same result.

- [`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md)
  : Compare a numeric variable between groups
- [`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md)
  : Correlate two numeric variables
- [`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md)
  : Correlate every pair of several numeric variables
- [`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md)
  : Fit a line or a smooth to two numeric variables
- [`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md)
  : Test the association between two categories
- [`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)
  : Compare survival between groups
- [`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md)
  : Screen many biomarkers with one comparison
- [`StatisticsResult`](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md)
  [`statistics-result`](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md)
  : The result every statistics function returns

## Widgets

A bio.viz chart drawn from R, with its statistics computed by the
functions above when the widget is made and stored in the page, so a
saved page shows them with no R and no network.

- [`Widget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Widget_GroupComparison.md)
  : Group Comparison Widget

## Synthetic study

A made-up biomarker study with three planted effects of known size, for
proving the statistics and for the charts to draw.

- [`Synthetic_Results`](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Results.md)
  : Synthetic biomarker study: results
- [`Synthetic_Participants`](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Participants.md)
  : Synthetic biomarker study: participants
- [`Synthetic_Outcomes`](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Outcomes.md)
  : Synthetic biomarker study: outcomes
- [`Synthetic_Truth`](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Truth.md)
  : Synthetic biomarker study: the planted effects and their true sizes
