#' Association Scatter Widget
#'
#' A widget that renders the bio.viz association scatter: one point per
#' participant, with a variable on each axis, a correlation coefficient printed
#' under each panel and, when asked for, a fitted line over the points. The
#' coefficient and the line are computed here, in R, by [Analyze_Correlation()]
#' and [Analyze_Fit()], and shipped with the page, so a saved page shows them
#' with no R and no network.
#'
#' @section What the page opens on:
#' The pair the settings name in `x` and `y`, each a variable as bio.viz writes
#' one: `list(measure = "IL-6", visit = "Week 4")` for a biomarker's result at a
#' visit, with `value = "change"`, `"fold_change"`, `"percent_change"` or
#' `"baseline"` for a value worked out from its baseline, or
#' `list(col = "AGE")` for a participant-level number. With neither named the
#' chart opens on the first two biomarkers at the first visit.
#'
#' @section Statistics shipped with the page:
#' The chart computes no coefficient and no line. It asks R for a coefficient
#' per panel and, for a linear fit or a smooth, a line per panel, and in a
#' widget the answers are worked out when the widget is made and stored in the
#' page, on the rows the chart draws in each panel of the view the settings
#' open on: its two variables, colour, panel column, filters and scales.
#'
#' For that view the widget stores both coefficients, Pearson's and
#' Spearman's, and both lines, the linear fit and the smooth, whichever the
#' settings open on: four results for each panel. The Method control and the
#' Fitted line control change what is asked of the same rows and nothing else,
#' so a reader of a saved page can move them and still be answered. The line
#' y = x is the chart's own and needs no R.
#'
#' A reader who moves any other control, to another variable, value type,
#' visit, colour, panel column, filter or scale, is told that statistics are
#' unavailable for that view, and a fitted line is not drawn. The page never
#' shows one view's numbers under another.
#'
#' On a logarithmic axis R is given the base-10 logarithm of the value, as the
#' chart gives it, so a coefficient or a line there is of the values as
#' plotted. The scale is part of what a stored result is found by.
#'
#' The page states which R computed the results: the R version, the gsm.bio
#' version and the time (see [StatisticsResult]). So that R and the chart
#' resolve the same rows, the widget names the baseline visits to the chart
#' outright, as [Widget_GroupComparison()] does.
#'
#' @inheritSection Widget_GroupComparison Filters
#' @inheritSection Widget_GroupComparison Bundles
#'
#' @inheritParams Widget_GroupComparison
#' @param dfParticipants `data.frame` One row per participant, or `NULL`. With
#'   it the chart has filters, offers its category columns to colour and panel
#'   by and its numeric columns on either axis; it also says who the
#'   participants are. Without it a colour or a number comes from a column
#'   carried on the results rows. Default: `NULL`.
#' @param lSettings `list` bio.viz association scatter settings, under
#'   bio.viz's own names; laid over the chart's defaults in the page, so only
#'   overrides are needed. For example `x` and `y` (the two variables),
#'   `color_by`, `panel_by`, `x_scale`, `y_scale`, `fit` (`"none"`,
#'   `"identity"`, `"linear"` or `"smooth"`), `method` (`"pearson"` or
#'   `"spearman"`) and `baseline_visits`. The settings `connection` and `back`
#'   cannot be given; `statistic` can only be `"Analyze_Correlation"` or `NULL`
#'   for no statistics line, and `fit_statistic` only `"Analyze_Fit"` or `NULL`
#'   for no linear fit and no smooth. Default: `list()`.
#'
#' @return An `htmlwidget`. Its payload `x` carries `dfResults`,
#'   `dfParticipants`, `lSettings`, `bDebug`, whether a width and a height were
#'   left to the widget (`bAutoWidth`, `bAutoHeight`), and `lStatistics`: the
#'   stored results, each with `name`, `args`, `dataId`, `rows` and `value`, and
#'   `computed_by`, the R version, gsm.bio version and time that computed them.
#'
#' @examples
#' # TNF-alpha against IL-10 at Baseline, the pair the synthetic study plants a
#' # correlation of 0.6 in, coloured by arm, with R's straight line and its
#' # band. Under the chart: Pearson's coefficient of everyone drawn and of each
#' # arm, and the slope and intercept of each line. The Method and Fitted line
#' # controls are answered from the page; a filter or another variable is a
#' # view that was not computed, and says so.
#' lColumns <- list(
#'   list(value_col = "ARM", label = "Arm"),
#'   list(value_col = "SEX", label = "Sex"),
#'   list(value_col = "RESPONSE", label = "Response")
#' )
#'
#' Widget_AssociationScatter(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     x = list(measure = "TNF-alpha", visit = "Baseline"),
#'     y = list(measure = "IL-10", visit = "Baseline"),
#'     baseline_visits = "Baseline",
#'     color_by = "ARM",
#'     fit = "linear",
#'     groups = lColumns,
#'     filters = lColumns
#'   )
#' )
#'
#' @seealso [Analyze_Correlation()] and [Analyze_Fit()], which compute every
#'   stored result.
#' @family widgets
#' @export
Widget_AssociationScatter <- function(
    dfResults,
    dfParticipants = NULL,
    lSettings = list(),
    width = NULL,
    height = NULL,
    elementId = NULL,
    bDebug = FALSE) {
  Widget_CheckInputs(dfResults, dfParticipants, lSettings, bDebug)
  if ("back" %in% names(lSettings)) {
    stop(
      "Setting 'back' cannot be given: it is a way back to a chart that opened this one, and a widget is opened by none",
      call. = FALSE
    )
  }
  lConfig <- AssociationScatter_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lNamed <- Widget_NameBaseline(lConfig, lSettings, dfResults)
  lNamed <- Widget_NameFilters(lNamed$config, lNamed$settings, dfResults, dfParticipants)

  Widget_Create(
    "Widget_AssociationScatter", dfResults, dfParticipants, lNamed$settings,
    AssociationScatter_StoredResults(dfResults, dfParticipants, lNamed$config),
    width, height, elementId, bDebug
  )
}
