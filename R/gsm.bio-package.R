#' gsm.bio: statistics and widgets for biomarker charts
#'
#' The statistics behind the bio.viz biomarker charts. A chart never computes
#' a test: it hands over a table with one row per participant and receives the
#' estimates, intervals, p-value, method name and counts that R worked out.
#'
#' @section Design:
#' Each statistic is a thin wrapper that fixes the inputs and the shape of the
#' answer around a function from the stats or survival package. Nothing is
#' reimplemented, and the statistics use nothing from any other package, so the
#' same source runs in a desktop session, in R in the browser and on a server.
#'
#' The tests the design names are imported here, once, for the functions that
#' wrap them: [stats::t.test()], [stats::wilcox.test()], [stats::aov()],
#' [stats::kruskal.test()], [stats::cor.test()], [stats::chisq.test()],
#' [stats::fisher.test()], [stats::p.adjust()], [stats::lm()] and
#' [stats::loess()] from stats;
#' [survival::Surv()], [survival::survdiff()], [survival::survfit()] and
#' [survival::coxph()] from survival.
#'
#' @section Widgets:
#' A widget draws a bio.viz chart from R. It computes the chart's statistics
#' with the functions above when it is made and stores them in the page, so a
#' saved page shows them with no R and no network: see
#' [Widget_GroupComparison()], [Widget_AssociationScatter()],
#' [Widget_CorrelationMatrix()], [Widget_BiomarkerScreen()], [Widget_CrossTab()]
#' and [Widget_StratifiedSurvival()]. The widgets are the only part of the
#' package that uses htmlwidgets.
#'
#' @section Figures:
#' Each chart has a static ggplot2 figure too, `Visualize_<Chart>()`, from the
#' same settings and with the same statistics: see
#' [Visualize_GroupComparison()] and the functions beside it. ggplot2 is
#' suggested, not imported.
#'
#' @section Status:
#' Version 0.2.0 is released: the statistics functions, a synthetic biomarker
#' study with known planted effects, a widget for each of bio.viz's six charts,
#' and from R a static figure, a statistics table, RTF and batch runs of chart
#' specifications. Version 0.3.0 is in development on `dev`.
#'
#' @importFrom stats aov chisq.test cor.test fisher.test kruskal.test p.adjust
#' @importFrom stats lm loess t.test wilcox.test
#' @importFrom survival Surv coxph survdiff survfit
"_PACKAGE"
