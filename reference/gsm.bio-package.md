# gsm.bio: statistics and widgets for biomarker charts

The statistics behind the bio.viz biomarker charts. A chart never
computes a test: it hands over a table with one row per participant and
receives the estimates, intervals, p-value, method name and counts that
R worked out.

## Design

Each statistic is a thin wrapper that fixes the inputs and the shape of
the answer around a function from the stats or survival package. Nothing
is reimplemented, and the statistics use nothing from any other package,
so the same source runs in a desktop session, in R in the browser and on
a server.

The tests the design names are imported here, once, for the functions
that wrap them:
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html),
[`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html),
[`stats::aov()`](https://rdrr.io/r/stats/aov.html),
[`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html),
[`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html),
[`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html),
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html),
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html),
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) and
[`stats::loess()`](https://rdrr.io/r/stats/loess.html) from stats;
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html),
[`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html),
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
and [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
from survival.

## Widgets

A widget draws a bio.viz chart from R. It computes the chart's
statistics with the functions above when it is made and stores them in
the page, so a saved page shows them with no R and no network: see
[`Widget_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Widget_GroupComparison.md),
[`Widget_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Widget_AssociationScatter.md),
[`Widget_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CorrelationMatrix.md),
[`Widget_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Widget_BiomarkerScreen.md),
[`Widget_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CrossTab.md)
and
[`Widget_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Widget_StratifiedSurvival.md).
The widgets are the only part of the package that uses htmlwidgets.

## Figures

Each chart has a static ggplot2 figure too, `Visualize_<Chart>()`, from
the same settings and with the same statistics: see
[`Visualize_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_GroupComparison.md)
and the functions beside it. ggplot2 is suggested, not imported.

## Status

Version 0.2.0 is being prepared: the statistics functions, a synthetic
biomarker study with known planted effects, a widget for each of
bio.viz's six charts, and from R a static figure, a statistics table,
RTF and batch runs of chart specifications. Version 0.1.0 is the latest
release.

## See also

Useful links:

- <https://github.com/jwildfire/gsm.bio>

- <https://jwildfire.github.io/gsm.bio/>

- Report bugs at <https://github.com/jwildfire/gsm.bio/issues>

## Author

**Maintainer**: obot <obot@users.noreply.github.com>
