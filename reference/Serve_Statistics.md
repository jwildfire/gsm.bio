# Answer the widgets' statistics from a Shiny session

Call it once in a Shiny server function. Every gsm.bio widget drawn in
the page with a `renderWidget_*()` function then asks this session for
its statistics: the chart sends the function's name, the rows it draws
and the arguments, and is sent back what the function returns. So every
view a reader reaches has its statistics, computed by this R, where a
saved page has only those stored when it was made.

## Usage

``` r
Serve_Statistics(session = shiny::getDefaultReactiveDomain())
```

## Arguments

- session:

  The Shiny session. By default the one whose server function is
  running.

## Value

The observer that answers, invisibly.

## What the session will run

The nine statistics functions and nothing else:
[`Analyze_GroupDifference()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifference.md),
[`Analyze_GroupDifferenceBy()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_GroupDifferenceBy.md),
[`Analyze_DifferenceGrid()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_DifferenceGrid.md),
[`Analyze_Correlation()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Correlation.md),
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md),
[`Analyze_Fit()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Fit.md),
[`Analyze_Contingency()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Contingency.md),
[`Analyze_Survival()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Survival.md)
and
[`Analyze_Screen()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_Screen.md).
The name a page sends is compared with that list. Any other name is
answered with a sentence saying so, and nothing is called. The rows and
the arguments are read as data and never as R code.

## What crosses to the server

The rows a chart draws travel with each request, because the chart works
them out in the page: a change from baseline, the filters and the cuts
are applied there, and R tests those rows and no others. Nothing is kept
between requests.

An answer is converted for the page exactly as a stored result is, so a
chart reads the two alike. The line under the chart says the result was
computed on this server, by which R and which gsm.bio.

## See also

The `renderWidget_*()` functions in
[gsm.bio-shiny](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md).

Other shiny:
[`gsm.bio-shiny`](https://jwildfire.github.io/gsm.bio/reference/gsm.bio-shiny.md)

## Examples

``` r
if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
  shiny::shinyApp(
    ui = shiny::fluidPage(Widget_GroupComparisonOutput("chart")),
    server = function(input, output, session) {
      Serve_Statistics()
      output$chart <- renderWidget_GroupComparison(
        Widget_GroupComparison(Synthetic_Results, Synthetic_Participants)
      )
    }
  )
}
```
