# The widgets in a Shiny page

An output function and a render function for each widget, as htmlwidgets
makes them. A widget drawn with its render function stores no results in
the page: its chart asks the Shiny session for every statistic, which
[`Serve_Statistics()`](https://jwildfire.github.io/gsm.bio/reference/Serve_Statistics.md),
called once in the server function, answers. Without that call the chart
is drawn and, after waiting twenty seconds for the session to say it
answers, says that statistics are unavailable because the session did
not answer in that time. A chart says the same when R is busy for longer
than that as it first asks, which a page cannot tell from a session that
answers none; it asks again by itself once the session does answer.

## Usage

``` r
Widget_GroupComparisonOutput(outputId, width = "100%", height = "auto")

renderWidget_GroupComparison(expr, env = parent.frame(), quoted = FALSE)

Widget_AssociationScatterOutput(outputId, width = "100%", height = "auto")

renderWidget_AssociationScatter(expr, env = parent.frame(), quoted = FALSE)

Widget_CorrelationMatrixOutput(outputId, width = "100%", height = "auto")

renderWidget_CorrelationMatrix(expr, env = parent.frame(), quoted = FALSE)

Widget_BiomarkerScreenOutput(outputId, width = "100%", height = "auto")

renderWidget_BiomarkerScreen(expr, env = parent.frame(), quoted = FALSE)

Widget_CrossTabOutput(outputId, width = "100%", height = "auto")

renderWidget_CrossTab(expr, env = parent.frame(), quoted = FALSE)

Widget_StratifiedSurvivalOutput(outputId, width = "100%", height = "auto")

renderWidget_StratifiedSurvival(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  `character` The output's name.

- width, height:

  The size, as CSS. By default the widget is as wide as its container
  and as tall as its chart.

- expr:

  An expression that makes the widget: a call to its `Widget_*()`
  function.

- env:

  The environment to evaluate `expr` in.

- quoted:

  `logical` Whether `expr` is already quoted.

## Value

An output function returns what a Shiny page is built from; a render
function returns what an output is assigned.

## Details

The stored results are not computed for a widget drawn this way, so the
page opens sooner than a saved page is made.

## The size of a chart's text

A chart sizes its text from the page's root font size. Shiny's default
page sets that to 10 pixels, which would draw a chart's text far smaller
than a saved page does, so an output function also writes one rule into
the page, `html { font-size: 100%; }`, which gives the root back to the
browser. Shiny's own text is sized in pixels and does not change.

## See also

[`Serve_Statistics()`](https://jwildfire.github.io/gsm.bio/reference/Serve_Statistics.md)

Other shiny:
[`RunApp()`](https://jwildfire.github.io/gsm.bio/reference/RunApp.md),
[`Serve_Statistics()`](https://jwildfire.github.io/gsm.bio/reference/Serve_Statistics.md)

## Examples

``` r
if (requireNamespace("shiny", quietly = TRUE)) {
  Widget_GroupComparisonOutput("chart")
}
#> <style>html { font-size: 100%; }</style>
#> <div class="Widget_GroupComparison html-widget html-widget-output shiny-report-size html-fill-item" id="chart" style="width:100%;height:auto;"></div>
```
