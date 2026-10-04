#' Correlation Matrix Widget
#'
#' A widget that renders the bio.viz correlation matrix: a grid over a set of
#' variables, each pair a cell holding its correlation coefficient and its own
#' pair count, and a click on a cell opens that pair's association scatter in
#' place. The coefficients are computed here, in R, by
#' [Analyze_CorrelationMatrix()], and each cell's scatter by
#' [Analyze_Correlation()] (and [Analyze_Fit()] for a fitted line), and shipped
#' with the page, so a saved page shows them with no R and no network.
#'
#' @section What the page opens on:
#' Across biomarkers (`mode = "biomarkers"`, the default), every biomarker the
#' limit allows at one visit, `visit`, the first visit when none is named; or
#' those named in `biomarkers`. Across visits (`mode = "visits"`), one
#' biomarker, `measure`, at each visit, or those named in `visits`. Every
#' variable has the same value type, `value_type`. The grid draws at most
#' `limit` variables, twelve by default.
#'
#' The grid prints no p-value, by design: [Analyze_CorrelationMatrix()]
#' returns none. A cell's scatter prints that pair's coefficient with its
#' interval and p-value.
#'
#' @section Statistics shipped with the page:
#' The chart computes no coefficient. It asks R once for the grid, on a frame
#' with one row per participant and one column per variable, keeping a
#' participant who has only some of the values, so that each cell is of the
#' participants who have both of its values. The widget stores R's answer for
#' the grid the settings open on.
#'
#' A click on a cell opens the association scatter for its pair, with the
#' column's variable on the x axis and the row's on the y axis, and the scatter
#' asks R for its own statistics. For every cell of the grid, on either side of
#' the diagonal, the widget stores exactly what that scatter asks when it opens:
#' its coefficient, by the grid's method, under the grid's filters, and a
#' fitted line when the setting `scatter` asks for one. On the synthetic study,
#' twelve biomarkers at Baseline, that is one grid and 132 scatters.
#'
#' A reader who moves a control of the grid, or of a scatter, to a view that
#' was not computed is told that statistics are unavailable for it: the grid's
#' cells stay empty, and a scatter prints the same. The page never shows one
#' view's numbers under another.
#'
#' The minimum number of pairs is R's: `min_pairs` is handed to R only when it
#' is set. The page states which R computed the results, and the widget names
#' the baseline visits to the chart outright, as [Widget_GroupComparison()]
#' does.
#'
#' @inheritSection Widget_GroupComparison Filters
#' @inheritSection Widget_GroupComparison Bundles
#'
#' @inheritParams Widget_GroupComparison
#' @param dfParticipants `data.frame` One row per participant, or `NULL`. With
#'   it the chart has filters; it also says who the participants are. Default:
#'   `NULL`.
#' @param lSettings `list` bio.viz correlation matrix settings, under bio.viz's
#'   own names; laid over the chart's defaults in the page, so only overrides
#'   are needed. For example `mode`, `visit`, `biomarkers`, `measure`,
#'   `visits`, `value_type`, `method`, `min_pairs`, `limit` and
#'   `baseline_visits`, and `scatter`, a list of settings for the association
#'   scatter a cell opens (its `groups`, `numbers`, `color_by`, `fit`). The
#'   setting `connection` cannot be given; `statistic` can only be
#'   `"Analyze_CorrelationMatrix"` or `NULL` for no coefficients; and `scatter`
#'   cannot name what the grid hands the scatter itself: `x`, `y`, `method`,
#'   `filters`, `connection` or `back`. Default: `list()`.
#'
#' @return An `htmlwidget`. Its payload `x` carries `dfResults`,
#'   `dfParticipants`, `lSettings`, `bDebug`, whether a width and a height were
#'   left to the widget (`bAutoWidth`, `bAutoHeight`), and `lStatistics`: the
#'   stored results, each with `name`, `args`, `dataId`, `rows` and `value`, and
#'   `computed_by`, the R version, gsm.bio version and time that computed them.
#'
#' @examples
#' # Every biomarker of the synthetic study at Baseline, biomarker against
#' # biomarker. The cell of TNF-alpha with IL-10 holds the correlation the study
#' # plants; a click on it, or on any other cell, opens that pair's scatter with
#' # its coefficient, its interval and its p-value, all stored in the page.
#' lColumns <- list(
#'   list(value_col = "ARM", label = "Arm"),
#'   list(value_col = "SEX", label = "Sex"),
#'   list(value_col = "RESPONSE", label = "Response")
#' )
#'
#' Widget_CorrelationMatrix(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     visit = "Baseline",
#'     baseline_visits = "Baseline",
#'     filters = lColumns,
#'     scatter = list(groups = lColumns)
#'   )
#' )
#'
#' @seealso [Analyze_CorrelationMatrix()], which computes the grid, and
#'   [Widget_AssociationScatter()], the chart a cell opens.
#' @family widgets
#' @export
Widget_CorrelationMatrix <- function(
    dfResults,
    dfParticipants = NULL,
    lSettings = list(),
    width = NULL,
    height = NULL,
    elementId = NULL,
    bDebug = FALSE) {
  Widget_CheckInputs(dfResults, dfParticipants, lSettings, bDebug)
  # Text as UTF-8, marked so, before anything is computed: the page carries it
  # so whatever the session's locale.
  dfResults <- Widget_Utf8(dfResults)
  dfParticipants <- Widget_Utf8(dfParticipants)
  lSettings <- Widget_Utf8(lSettings)
  # No settings for the scatter is none, and the page is given none.
  if ("scatter" %in% names(lSettings) && length(lSettings$scatter) == 0L) {
    lSettings["scatter"] <- list(NULL)
  }
  lConfig <- CorrelationMatrix_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lNamed <- Widget_NameBaseline(lConfig, lSettings, dfResults)
  lNamed <- Widget_NameFilters(lNamed$config, lNamed$settings, dfResults, dfParticipants)

  Widget_Create(
    "Widget_CorrelationMatrix", dfResults, dfParticipants, lNamed$settings,
    CorrelationMatrix_StoredResults(dfResults, dfParticipants, lNamed$config),
    width, height, elementId, bDebug
  )
}
