#' Biomarker Screen Widget
#'
#' A widget that renders the bio.viz biomarker screen: one row per biomarker,
#' each with its estimate and interval drawn on one shared axis and its raw and
#' adjusted p-values, and a click on a row opens that biomarker's own chart in
#' place. The rows are computed here, in R, by [Analyze_Screen()], and the
#' chart each row opens by [Analyze_GroupDifference()] or
#' [Analyze_Correlation()] (and [Analyze_Fit()] for a fitted line), and shipped
#' with the page, so a saved page shows them with no R and no network.
#'
#' @section What the page opens on:
#' Every biomarker the Biomarker control offers, at one visit, `visit` (the
#' first visit when none is named), with one value type, `value_type`. A
#' difference (`comparison = "difference"`, the default) compares two groups of
#' one column, `group_by` and `levels` (the first column with two groups or
#' more, and its first two groups, when none are named); each row is the
#' standardised difference (Hedges' g) with Welch's p-value. A correlation
#' (`comparison = "correlation"`) correlates every biomarker with one variable,
#' `with`, a biomarker at a visit or a participant-level number, by `method`;
#' the biomarker that is that variable is not a row of its own. The p-values
#' are adjusted across the rows by `adjustment`, Benjamini-Hochberg by default.
#'
#' @section Statistics shipped with the page:
#' The chart computes no estimate, interval or p-value. It asks R once for the
#' whole screen, on a frame with one row per participant and one column per
#' biomarker, keeping a participant who has only some of the biomarkers, so that
#' each row is of the participants who have its value. The widget stores R's
#' answer for the screen the settings open on.
#'
#' A click on a row opens that biomarker's chart, which asks R for its own
#' statistics. For a difference it is the group comparison, on the biomarker,
#' at the screen's one visit, with only the two groups and Welch's test; for a
#' correlation it is the association scatter, with the biomarker along the
#' bottom and the variable up the side, by the same method. For every row the
#' widget stores exactly what that chart asks when it opens, under the screen's
#' filters. On the synthetic study, a difference between the arms is one screen
#' and twelve group comparisons.
#'
#' A reader who moves a control of the screen, or of the chart a row opens, to
#' a view that was not computed is told that statistics are unavailable for
#' it: the screen's rows stay empty, and the chart prints the same. The page
#' never shows one view's numbers under another.
#'
#' The page states which R computed the results, and the widget names the
#' baseline visits to the chart outright, as [Widget_GroupComparison()] does.
#'
#' @inheritSection Widget_GroupComparison Bundles
#'
#' @inheritParams Widget_GroupComparison
#' @param dfParticipants `data.frame` One row per participant, or `NULL`. With
#'   it the chart has filters, and the columns of groups and the numbers it
#'   offers are read from it. Default: `NULL`.
#' @param lSettings `list` bio.viz biomarker screen settings, under bio.viz's
#'   own names; laid over the chart's defaults in the page, so only overrides
#'   are needed. For example `comparison`, `visit`, `value_type`, `group_by`,
#'   `levels`, `with`, `method`, `adjustment`, `sort`, `limit`, `groups`,
#'   `numbers`, `filters` and `baseline_visits`; and `group_comparison` and
#'   `association_scatter`, lists of settings for the chart a row opens. The
#'   setting `connection` cannot be given; `statistic` can only be
#'   `"Analyze_Screen"` or `NULL` for no rows; and the settings for the chart a
#'   row opens cannot name what the screen hands that chart itself: for the
#'   group comparison `start_value`, `visits`, `value_type`, `group_by`,
#'   `levels` and `test`, for the association scatter `x`, `y` and `method`, and
#'   for either `filters`, `connection`, `waiting_note` or `back`. Default:
#'   `list()`.
#'
#' @return An `htmlwidget`. Its payload `x` carries `dfResults`,
#'   `dfParticipants`, `lSettings`, `bDebug`, whether a width and a height were
#'   left to the widget (`bAutoWidth`, `bAutoHeight`), and `lStatistics`: the
#'   stored results, each with `name`, `args`, `dataId`, `rows` and `value`, and
#'   `computed_by`, the R version, gsm.bio version and time that computed them.
#'
#' @examples
#' # Every biomarker's change from Baseline to Week 4 in the synthetic study,
#' # Placebo against Treatment. IL-6 is the biomarker the study plants a
#' # difference in; a click on its row, or on any other, opens that biomarker's
#' # group comparison with its test, all stored in the page.
#' lColumns <- list(
#'   list(value_col = "ARM", label = "Arm"),
#'   list(value_col = "SEX", label = "Sex"),
#'   list(value_col = "RESPONSE", label = "Response")
#' )
#'
#' Widget_BiomarkerScreen(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     visit = "Week 4",
#'     value_type = "change",
#'     group_by = "ARM",
#'     baseline_visits = "Baseline",
#'     groups = lColumns,
#'     filters = lColumns,
#'     group_comparison = list(groups = lColumns)
#'   )
#' )
#'
#' @seealso [Analyze_Screen()], which computes the rows, and
#'   [Widget_GroupComparison()] and [Widget_AssociationScatter()], the charts a
#'   row opens.
#' @family widgets
#' @export
Widget_BiomarkerScreen <- function(
    dfResults,
    dfParticipants = NULL,
    lSettings = list(),
    width = NULL,
    height = NULL,
    elementId = NULL,
    bDebug = FALSE) {
  Widget_CheckInputs(dfResults, dfParticipants, lSettings, bDebug)
  # No settings for the chart a row opens is none, and the page is given none.
  for (strKey in c("group_comparison", "association_scatter")) {
    if (strKey %in% names(lSettings) && length(lSettings[[strKey]]) == 0L) {
      lSettings[strKey] <- list(NULL)
    }
  }
  lConfig <- BiomarkerScreen_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lNamed <- Widget_NameBaseline(lConfig, lSettings, dfResults)

  Widget_Create(
    "Widget_BiomarkerScreen", dfResults, dfParticipants, lNamed$settings,
    BiomarkerScreen_StoredResults(dfResults, dfParticipants, lNamed$config),
    width, height, elementId, bDebug
  )
}
