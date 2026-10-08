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
- [`Analyze_GroupDifferenceBy()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifferenceBy.md)
  : Compare a numeric variable between groups at each level of a column
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
- [`Analyze_DifferenceGrid()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_DifferenceGrid.md)
  : The standardised difference for every biomarker at every level of a
  column
- [`StatisticsResult`](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md)
  [`statistics-result`](https://jwildfire.github.io/gsm.bio/reference/StatisticsResult.md)
  : The result every statistics function returns

## Widgets

A bio.viz chart drawn from R, with its statistics computed by the
functions above when the widget is made and stored in the page, so a
saved page shows them with no R and no network.

- [`Widget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Widget_GroupComparison.md)
  : Group Comparison Widget
- [`Widget_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Widget_AssociationScatter.md)
  : Association Scatter Widget
- [`Widget_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CorrelationMatrix.md)
  : Correlation Matrix Widget
- [`Widget_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Widget_BiomarkerScreen.md)
  : Biomarker Screen Widget
- [`Widget_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CrossTab.md)
  : Cross-Tabulation Widget
- [`Widget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Widget_StratifiedSurvival.md)
  : Stratified Survival Widget

## Widgets in a Shiny page

The same widgets drawn in a Shiny page, where the R session behind the
page answers every statistic a chart asks for, so every view a reader
reaches has its numbers. shiny is suggested, not imported.

- [`Serve_Statistics()`](https://jwildfire.github.io/gsm.bio/reference/Serve_Statistics.md)
  : Answer the widgets' statistics from a Shiny session
- [`Widget_GroupComparisonOutput()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`renderWidget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`Widget_AssociationScatterOutput()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`renderWidget_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`Widget_CorrelationMatrixOutput()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`renderWidget_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`Widget_BiomarkerScreenOutput()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`renderWidget_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`Widget_CrossTabOutput()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`renderWidget_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`Widget_StratifiedSurvivalOutput()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  [`renderWidget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)
  : The widgets in a Shiny page

## Figures

The same views as static ggplot2 figures, for a report or a slide, with
the same statistics from the functions above, and the chart’s title,
subtitle and footnotes. ggplot2 is suggested, not imported.

- [`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md)
  : Group Comparison Figure
- [`Visualize_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_AssociationScatter.md)
  : Association Scatter Figure
- [`Visualize_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CorrelationMatrix.md)
  : Correlation Matrix Figure
- [`Visualize_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_BiomarkerScreen.md)
  : Biomarker Screen Figure
- [`Visualize_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CrossTab.md)
  : Cross-Tabulation Figure
- [`Visualize_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_StratifiedSurvival.md)
  : Stratified Survival Figure

## Tables

The same views’ statistics as tables, one row per statistic, written by
the program’s display rules, and an RTF writer for a report. r2rtf is
suggested, not imported.

- [`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md)
  : Group Comparison Table
- [`Table_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Table_AssociationScatter.md)
  : Association Scatter Table
- [`Table_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Table_CorrelationMatrix.md)
  : Correlation Matrix Table
- [`Table_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Table_BiomarkerScreen.md)
  : Biomarker Screen Table
- [`Table_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Table_CrossTab.md)
  : Cross-Tabulation Table
- [`Table_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Table_StratifiedSurvival.md)
  : Stratified Survival Table
- [`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)
  : Write a Statistics Table to RTF

## Batch

bio.viz’s chart specifications run against a dataset to figures and RTF
tables in a folder, one per specification or one per biomarker.

- [`Run_Specifications()`](https://jwildfire.github.io/gsm.bio/reference/Run_Specifications.md)
  : Run Chart Specifications to Figures and Tables

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
