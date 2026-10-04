#' Group Comparison Widget
#'
#' A widget that renders the bio.viz group comparison chart: one biomarker
#' value across the levels of a category, as boxes, violins or points, with a
#' test of the groups printed under each panel. The tests are computed here, in
#' R, by [Analyze_GroupDifference()], and shipped with the page, so a saved page
#' shows them with no R and no network.
#'
#' @section What the page opens on:
#' The chart opens on an overview of every biomarker at every visit, one row
#' per biomarker, unless `start_value` names a biomarker to open. A row of the
#' overview, or the Biomarker control, opens one biomarker alone, with its
#' visits as panels and a test under each. The overview itself prints no test.
#' For a change, a fold change or a percent change from one baseline visit, the
#' baseline visit has no panel: there the value is the same for everyone.
#'
#' @section Statistics shipped with the page:
#' The chart computes no test. It asks R for one test per panel, and in a
#' widget the answers are worked out when the widget is made and stored in the
#' page. For each biomarker the Biomarker control offers, the widget stores
#' [Analyze_GroupDifference()]'s answer for each panel the chart draws when
#' that biomarker is opened at the widget's settings, on the rows the chart
#' draws in that panel: the visits, value type, baseline, group, panel column,
#' filters, scale, test and pairwise switch the settings open on. With no
#' `visits` named that is every visit the chart draws.
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
#' @section Filters:
#' With a participant table the chart has filters, set by the setting
#' `filters` under safety.viz's rules: a filter opens on its `start` when the
#' data has it and otherwise on All, a filter set `all = FALSE` has no All and
#' opens on its first value, and `multiple = TRUE` lets several values through.
#' R works out what each filter opens on as the chart does, and stores the
#' results for those participants.
#'
#' The first value of an `all = FALSE` filter is the one exception, because
#' the chart lists a filter's values in the order of the reader's browser,
#' which R cannot know: for a letter with an accent or for punctuation it can
#' differ from R's order, by code point. So the widget hands the chart R's
#' first value as the filter's `start`, and the page opens on the participants
#' R computed for, though that value may not be the first in the list.
#'
#' @section Bundles:
#' The widget loads bio.viz's bundle and the copy of safety.viz's bundle that
#' bio.viz itself builds its chart from. Both are copied from bio.viz, with the
#' bio.viz commit and a checksum per file recorded beside them in
#' `system.file("htmlwidgets", "lib", "SOURCE.json", package = "gsm.bio")`. They
#' are bio.viz v0.1.0 and safety.viz v1.9.0, the first safety.viz with the kit
#' the chart is built from, as bio.viz takes it from safety.viz's `dev` branch;
#' the record says from which commit. gsm.safety carries an earlier safety.viz
#' without the kit, and once it carries v1.9.0 the widgets can take the bundle
#' from there.
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
#'   are needed. For example `start_value` (the biomarker to open; `NULL` is
#'   the overview), `visits` (`NULL` is every visit), `value_type`,
#'   `baseline_visits`, `group_by`, `color_by`, `panel_by`, `test` and
#'   `pairwise`. The setting `connection` is the widget's to make and cannot be
#'   given, and `statistic` can only be `"Analyze_GroupDifference"` or `NULL`
#'   for no statistics line. Default: `list()`.
#' @param width `character` Width of the widget as a CSS unit. Default: `NULL`,
#'   as wide as its container.
#' @param height `character` Height of the widget as a CSS unit. Default:
#'   `NULL`, as tall as the chart.
#' @param elementId `character` ID of the widget's HTML element. Default:
#'   `NULL`.
#' @param bDebug `logical` Print debug messages in the browser console?
#'   Default: `FALSE`.
#'
#' @return An `htmlwidget`. Its payload `x` carries `dfResults`,
#'   `dfParticipants`, `lSettings`, `bDebug`, whether a width and a height were
#'   left to the widget (`bAutoWidth`, `bAutoHeight`), and `lStatistics`: the stored
#'   results, each with `name`, `args`, `dataId`, `rows` and `value`, and
#'   `computed_by`, the R version, gsm.bio version and time that computed them.
#'
#' @examples
#' # Change from Baseline, by arm, on the synthetic study. `start_value` opens
#' # IL-6, the biomarker the study plants a difference in, so the page shows what
#' # the widget is for at once: a panel for each visit after Baseline, with R's
#' # test under each. Without `start_value` the chart opens on its overview of
#' # every biomarker, which prints no test; "All Biomarkers" in the Biomarker
#' # control goes there, and a click on a biomarker's row comes back.
#' # Moving a filter or the Test control leaves the views that were computed:
#' # the line then says that statistics are unavailable for the view.
#' lColumns <- list(
#'   list(value_col = "ARM", label = "Arm"),
#'   list(value_col = "SEX", label = "Sex"),
#'   list(value_col = "RESPONSE", label = "Response")
#' )
#'
#' Widget_GroupComparison(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     start_value = "IL-6",
#'     value_type = "change",
#'     baseline_visits = "Baseline",
#'     group_by = "ARM",
#'     groups = lColumns,
#'     filters = lColumns
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
  Widget_CheckInputs(dfResults, dfParticipants, lSettings, bDebug)
  # Text as UTF-8, marked so, before anything is computed: the page carries it
  # so whatever the session's locale.
  dfResults <- Widget_Utf8(dfResults)
  dfParticipants <- Widget_Utf8(dfParticipants)
  lSettings <- Widget_Utf8(lSettings)
  lConfig <- GroupComparison_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lNamed <- Widget_NameBaseline(lConfig, lSettings, dfResults)
  lNamed <- Widget_NameFilters(lNamed$config, lNamed$settings, dfResults, dfParticipants)
  lNamed$settings <- Widget_NameCuts(lNamed$config, lNamed$settings, c("group_by", "panel_by"))

  Widget_Create(
    "Widget_GroupComparison", dfResults, dfParticipants, lNamed$settings,
    GroupComparison_StoredResults(dfResults, dfParticipants, lNamed$config),
    width, height, elementId, bDebug
  )
}
