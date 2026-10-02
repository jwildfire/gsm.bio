#' Group Comparison Widget
#'
#' A widget that renders the bio.viz group comparison chart: one biomarker
#' value across the levels of a category, as boxes, violins or points, with a
#' test of the groups printed under each panel. The tests are computed here, in
#' R, by [Analyze_GroupDifference()], and shipped with the page, so a saved page
#' shows them with no R and no network.
#'
#' @section Statistics shipped with the page:
#' The chart computes no test. It asks R for one test per panel, and in a
#' widget the answers are worked out when the widget is made and stored in the
#' page. For each biomarker the Biomarker control offers, the widget stores
#' [Analyze_GroupDifference()]'s answer for each panel of that biomarker's view
#' at the widget's settings, and for each visit the Visit control offers, on the
#' rows the chart draws in that panel: the value type, baseline, group, panel
#' column, filters, scale, test and pairwise switch the settings open on.
#'
#' A stored result is found by the function's name, its arguments and the
#' identity of the panel's rows together. A reader who moves a control to a
#' view that was not computed, such as another test, another group or a filter,
#' is told that statistics are unavailable for that view. The page never shows
#' one view's numbers under another.
#'
#' The page states which R computed the results: the R version, the gsm.bio
#' version and the time. A result is the answer of the R that computed it (see
#' [StatisticsResult]), so the same view computed live by another version of R
#' can differ slightly.
#'
#' So that R and the chart resolve the same rows, the widget names the baseline
#' visits to the chart outright: when `baseline_visits` is not given, it is set
#' to the first visit in visit order, which is what the chart would choose.
#'
#' @section Bundles:
#' The widget loads bio.viz's bundle and the copy of safety.viz's bundle that
#' bio.viz itself builds its chart from. Both are copied from bio.viz, with the
#' bio.viz commit and a checksum per file recorded beside them in
#' `system.file("htmlwidgets", "lib", "SOURCE.json", package = "gsm.bio")`. The
#' safety.viz copy is a stand-in until gsm.safety carries a safety.viz bundle
#' with the kit the chart is built from; the record says where it came from.
#'
#' @param dfResults `data.frame` Long-format results, one row per participant,
#'   biomarker and visit. Column names are supplied by `lSettings`; the
#'   defaults expect `USUBJID`/`TEST`/`STRESN`/`VISIT`/`VISITNUM`/`STRESU`, the
#'   columns of [Synthetic_Results].
#' @param dfParticipants `data.frame` One row per participant, or `NULL`. With
#'   it the chart has filters, and offers its category columns to group, colour
#'   and panel by; it also says who the participants are. Without it a group
#'   comes from a column carried on the results rows. Default: `NULL`.
#' @param lSettings `list` bio.viz group comparison settings, under bio.viz's
#'   own names; laid over the chart's defaults in the page, so only overrides
#'   are needed. For example `start_value`, `visits`, `value_type`,
#'   `baseline_visits`, `group_by`, `color_by`, `panel_by`, `test` and
#'   `pairwise`. The setting `connection` is the widget's to make and cannot be
#'   given, and `statistic` can only be `"Analyze_GroupDifference"` or `NULL`
#'   for no statistics line. Default: `list()`.
#' @param width `character` Width of the widget as a CSS unit. Default: `NULL`.
#' @param height `character` Height of the widget as a CSS unit. Default:
#'   `NULL`, as tall as the chart.
#' @param elementId `character` ID of the widget's HTML element. Default:
#'   `NULL`.
#' @param bDebug `logical` Print debug messages in the browser console?
#'   Default: `FALSE`.
#'
#' @return An `htmlwidget`. Its payload `x` carries `dfResults`,
#'   `dfParticipants`, `lSettings`, `bDebug` and `lStatistics`: the stored
#'   results, each with `name`, `args`, `dataId`, `rows` and `value`, and
#'   `computed_by`, the R version, gsm.bio version and time that computed them.
#'
#' @examples
#' # Change from Baseline, by arm, on the synthetic study. Open IL-6, the
#' # biomarker the study plants a difference in, to see R's test under it.
#' Widget_GroupComparison(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     start_value = "IL-6",
#'     value_type = "change",
#'     baseline_visits = "Baseline",
#'     group_by = "ARM",
#'     groups = list(
#'       list(value_col = "ARM", label = "Arm"),
#'       list(value_col = "SEX", label = "Sex"),
#'       list(value_col = "RESPONSE", label = "Response")
#'     )
#'   )
#' )
#'
#' @seealso [Analyze_GroupDifference()], which computes every stored result.
#' @family widgets
#' @export
Widget_GroupComparison <- function(
    dfResults,
    dfParticipants = NULL,
    lSettings = list(),
    width = NULL,
    height = NULL,
    elementId = NULL,
    bDebug = FALSE) {
  if (!is.data.frame(dfResults)) {
    stop("dfResults is not a data.frame", call. = FALSE)
  }
  if (!is.null(dfParticipants) && !is.data.frame(dfParticipants)) {
    stop("dfParticipants is not a data.frame or NULL", call. = FALSE)
  }
  if (!is.list(lSettings) || is.data.frame(lSettings)) {
    stop("lSettings must be a list, but not a data.frame", call. = FALSE)
  }
  if (length(lSettings) > 0L && (is.null(names(lSettings)) || !all(nzchar(names(lSettings))))) {
    stop("lSettings must name every setting", call. = FALSE)
  }
  if (!(is.logical(bDebug) && length(bDebug) == 1L && !is.na(bDebug))) {
    stop("bDebug is not a logical", call. = FALSE)
  }
  if ("connection" %in% names(lSettings)) {
    stop(
      "Setting 'connection' cannot be given: the widget makes the chart's connection to R from the results it stores",
      call. = FALSE
    )
  }

  lConfig <- GroupComparison_Settings(lSettings)
  for (strKey in c("id_col", "measure_col", "value_col", "visit_col")) {
    if (!lConfig[[strKey]] %in% names(dfResults)) {
      stop("Column '", lConfig[[strKey]], "' (setting '", strKey, "') not found in dfResults", call. = FALSE)
    }
  }
  if (!is.null(dfParticipants)) {
    strParticipantIdCol <- if (is.null(lConfig$participant_id_col)) lConfig$id_col else lConfig$participant_id_col
    if (!strParticipantIdCol %in% names(dfParticipants)) {
      stop(
        "Column '", strParticipantIdCol, "' (setting '",
        if (is.null(lConfig$participant_id_col)) "id_col" else "participant_id_col", "') not found in dfParticipants",
        call. = FALSE
      )
    }
  }

  # The baseline visits are named to the chart outright, so R and the chart
  # cannot resolve them differently: the first visit in visit order, which is
  # what the chart chooses when none is named.
  if (is.null(lConfig$baseline_visits)) {
    chrFirstVisit <- Core_First(Core_Visits(dfResults, GroupComparison_CoreSettings(lConfig)))
    if (length(chrFirstVisit) == 1L) {
      lConfig$baseline_visits <- chrFirstVisit
      lSettings$baseline_visits <- chrFirstVisit
    }
  }

  lStored <- GroupComparison_StoredResults(dfResults, dfParticipants, lConfig)
  x <- list(
    dfResults = dfResults,
    dfParticipants = dfParticipants,
    lSettings = lSettings,
    bDebug = bDebug,
    bAutoHeight = is.null(height),
    lStatistics = list(
      computed_by = StoredResultsProvenance(),
      results = lapply(lStored, function(lResult) {
        list(
          name = lResult$name, args = lResult$args, dataId = lResult$dataId, rows = lResult$rows,
          value = StoredValue(lResult$value)
        )
      })
    )
  )
  htmlwidgets::createWidget(
    name = "Widget_GroupComparison",
    x = x,
    width = width,
    height = height,
    package = "gsm.bio",
    elementId = elementId,
    sizingPolicy = WidgetSizingPolicy()
  )
}
