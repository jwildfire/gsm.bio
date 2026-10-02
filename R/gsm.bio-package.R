#' gsm.bio: statistics for biomarker charts
#'
#' The statistics behind the bio.viz biomarker charts. A chart never computes
#' a test: it hands over a table with one row per participant and receives the
#' estimates, intervals, p-value, method name and counts that R worked out.
#'
#' @section Design:
#' Each statistic is a thin wrapper that fixes the inputs and the shape of the
#' answer around a function from the stats or survival package. Nothing is
#' reimplemented, and nothing from any other package is used, so the same
#' source runs in a desktop session, in R in the browser and on a server.
#'
#' The tests the design names are imported here, once, for the functions that
#' wrap them: [stats::t.test()], [stats::wilcox.test()], [stats::aov()],
#' [stats::kruskal.test()], [stats::cor.test()], [stats::chisq.test()],
#' [stats::fisher.test()] and [stats::p.adjust()] from stats;
#' [survival::Surv()], [survival::survdiff()], [survival::survfit()] and
#' [survival::coxph()] from survival.
#'
#' @section Status:
#' Version 0.1.0 is in development. This first step sets up the package; the
#' statistics functions and a synthetic biomarker study with known planted
#' effects follow in the same version.
#'
#' @importFrom stats aov chisq.test cor.test fisher.test kruskal.test p.adjust
#' @importFrom stats t.test wilcox.test
#' @importFrom survival Surv coxph survdiff survfit
"_PACKAGE"
