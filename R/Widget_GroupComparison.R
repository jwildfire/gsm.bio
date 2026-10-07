#' Group Comparison Widget
#'
#' A widget that renders the bio.viz group comparison chart: how one biomarker
#' value differs between the levels of a category. It draws at three levels: a
#' trend tile for every biomarker, one biomarker across its visits, and one
#' visit alone as boxes, violins or points. The tests of the groups it prints
#' are computed here, in R, by [Analyze_GroupDifferenceBy()] and
#' [Analyze_GroupDifference()], and shipped with the page, so a saved page
#' shows them with no R and no network.
#'
#' @section What the page opens on:
#' The chart opens on a trend tile for every biomarker, unless `start_value`
#' names a biomarker to open. A tile draws one line per group across the
#' visits, through each group's median or, by `tile_summary`, its mean, on the
#' biomarker's own value axis. The tiles print no test and ask R for nothing.
#'
#' A tile, or the Biomarker control, opens one biomarker over time: every visit
#' it has in one picture, the groups side by side at each visit, drawn by
#' `time_mark` as boxes, as means with their standard errors or as medians with
#' their quartiles. Under each visit is the number in each group and R's test
#' of the groups there. For a change, a fold change or a percent change from
#' one baseline visit, the baseline visit is drawn and not tested: there the
#' value is the same for everyone.
#'
#' A visit's name under the picture opens that visit alone, with its marks, a
#' second grouping, panels and pairwise comparisons, and a test under each
#' panel. All in the Visit control leads back to the biomarker over time.
#'
#' A page whose settings name a biomarker and no visit opened on a panel for
#' every visit before bio.viz had the picture over time. It now opens on that
#' picture; name the visits to draw as panels in `visits`.
#'
#' @section Statistics shipped with the page:
#' The chart computes no test. It asks R, and in a widget the answers are
#' worked out when the widget is made and stored in the page. Nothing is stored
#' for the trend tiles, which ask R for nothing. For each biomarker the
#' Biomarker control offers, at the widget's settings, the page stores:
#'
#' - for the biomarker over time, [Analyze_GroupDifferenceBy()]'s answer for
#'   the test under every visit, in one result: with the p-values as R gives
#'   them, and, when `visit_adjustment` names an adjustment, with R's
#'   adjustment across the visits as well, so the Adjust across visits control
#'   is answered either way;
#' - for each visit the biomarker has, [Analyze_GroupDifference()]'s answer
#'   for the panel a click on that visit opens, or for each of its panels when
#'   `panel_by` names a panel column;
#' - and, when `visits` names some of the visits, the same for the panels the
#'   page opens that biomarker on.
#'
#' Each is computed on the rows the chart draws: the value type, baseline,
#' group, filters, scale, test and pairwise switch the settings open on.
#'
#' A stored result is found by the function's name, its arguments and the
#' identity of the rows together. A reader who moves a control to a view that
#' was not computed, such as another test, another group, a filter, the
#' adjustment the settings did not name, or unscheduled visits switched on or
#' off, is told so where the result would be. Under one biomarker over time the
#' row of tests reads "Statistics unavailable", and under a panel the line
#' reads "Statistics are unavailable for this view: the page holds no stored
#' result for it, and no R is attached to compute one." The page never shows
#' one view's numbers under another.
#'
#' The page states which R computed the results: the R version, the gsm.bio
#' version and the time. A result is the answer of the R that computed it (see
#' [StatisticsResult]), so the same view computed live by another version of R
#' can differ slightly.
#'
#' So that R and the chart resolve the same rows, the widget names the baseline
#' visits to the chart outright: when `baseline_visits` is not given, it is set
#' to the first visit the chart draws, in visit order, which is what the chart
#' would choose.
#'
#' @section Unscheduled visits:
#' Unscheduled visits are left out at every level unless `unscheduled_visits`
#' is `TRUE`: they are not on a tile, not a visit of the picture over time, not
#' a panel, and not the baseline a change is measured from. The page says how
#' many are left out, and its Unscheduled visits control draws them. R stores
#' results for the visits the page opens on and no others, so with the control
#' moved the page says that statistics are unavailable.
#'
#' A visit is unscheduled when `unscheduled_visit_values` names it. With no
#' list, it is one whose name matches `unscheduled_visit_pattern`, which by
#' default is any name holding "unscheduled" or "early termination", in either
#' case. The pattern is a regular expression the reader's browser reads, and R
#' reads only the patterns that mean the same in both: words of letters, digits
#' and spaces set side by side with `|`, with or without the flag `i`. Any other
#' pattern is refused; name the visits in `unscheduled_visit_values` instead.
#' So that R and the page cannot find different visits, the widget hands the
#' page the visits R found as `unscheduled_visit_values`.
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
#' bio.viz itself builds its chart from. Both are copied from bio.viz's `dev`
#' branch, with the bio.viz commit and a checksum per file recorded beside them
#' in `system.file("htmlwidgets", "lib", "SOURCE.json", package = "gsm.bio")`.
#' When the copy was made from a tagged bio.viz release, the record names the
#' release; otherwise it names the commit alone. The safety.viz bundle
#' is v1.9.0, the first with the kit the chart is built from, as bio.viz takes
#' it from safety.viz's `dev` branch; the record says from which commit.
#' gsm.safety carries an earlier safety.viz without the kit, and once it
#' carries v1.9.0 the widgets can take the bundle from there.
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
#'   the trend tiles), `visits` (`NULL` is every visit), `value_type`,
#'   `baseline_visits`, `group_by`, `color_by`, `panel_by`, `test` and
#'   `pairwise`. The settings of the three levels:
#'
#'   - `tile_summary`: what a tile's line goes through, `"median"` (the
#'     default) or `"mean"`.
#'   - `tile_min_spread`: the least a tile's value axis spans, in standard
#'     deviations of the results at the baseline visit; a number, zero or
#'     more. Default: `1.25`.
#'   - `time_mark`: what one biomarker over time is drawn as, `"box"` (the
#'     default), `"mean_se"` or `"median_iqr"`.
#'   - `visit_adjustment`: how R adjusts the p-values across the visits of one
#'     biomarker over time, by the name `p.adjust()` gives it: `"none"` (the
#'     default), `"holm"` or `"BH"`.
#'   - `unscheduled_visits`: `TRUE` draws unscheduled visits. Default: `FALSE`.
#'   - `unscheduled_visit_values`: the unscheduled visits, by name. Default:
#'     `NULL`, which reads `unscheduled_visit_pattern`.
#'   - `unscheduled_visit_pattern`: the pattern an unscheduled visit's name
#'     matches when no visit is named. Default:
#'     `"/unscheduled|early termination/i"`.
#'
#'   The setting `connection` is the widget's to make and cannot be given.
#'   `statistic` can only be `"Analyze_GroupDifference"`, or `NULL` for no
#'   statistics at all, and `statistic_by_visit` can only be
#'   `"Analyze_GroupDifferenceBy"`, or `NULL` for no test under the visits of
#'   one biomarker over time. Default: `list()`.
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
#' # Change from Baseline, by arm, on the synthetic study. The page opens on a
#' # trend tile for every biomarker, which prints no test. The tile of IL-6,
#' # the biomarker the study plants a difference in, opens IL-6 over time, with
#' # the number in each arm and R's test under each visit after Baseline. A
#' # visit's name there opens that visit alone, with R's test under its panel;
#' # All in the Visit control leads back, and "All Biomarkers" in the Biomarker
#' # control leads back to the tiles.
#' # Moving a filter or the Test control leaves the views that were computed:
#' # the page then says that statistics are unavailable for the view.
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
#'     value_type = "change",
#'     baseline_visits = "Baseline",
#'     group_by = "ARM",
#'     groups = lColumns,
#'     filters = lColumns
#'   )
#' )
#'
#' @seealso [Analyze_GroupDifferenceBy()] and [Analyze_GroupDifference()], which
#'   compute every stored result.
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
  # The unscheduled visits first: the baseline a change is measured from is
  # the first visit of those the chart draws.
  lNamed <- Widget_NameUnscheduled(lConfig, lSettings, dfResults)
  lNamed <- Widget_NameBaseline(lNamed$config, lNamed$settings, dfResults)
  lNamed <- Widget_NameFilters(lNamed$config, lNamed$settings, dfResults, dfParticipants)
  lNamed$settings <- Widget_NameCuts(lNamed$config, lNamed$settings, c("group_by", "panel_by"))

  Widget_Create(
    "Widget_GroupComparison", dfResults, dfParticipants, lNamed$settings,
    GroupComparison_StoredResults(dfResults, dfParticipants, lNamed$config),
    width, height, elementId, bDebug
  )
}
