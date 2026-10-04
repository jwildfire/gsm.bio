#' Stratified Survival Widget
#'
#' A widget that renders the bio.viz stratified survival chart: a Kaplan-Meier
#' curve for each group on one endpoint of an outcomes table, with the number
#' at risk beneath, and under them R's log-rank test, each group's median
#' survival and the hazard ratio. The groups are a column, or a biomarker or a
#' participant-level number cut into groups by the shared cut rule. The test is
#' computed here, in R, by [Analyze_Survival()], and shipped with the page, so
#' a saved page shows it with no R and no network. A click on a curve, or on a
#' count at risk, lists those participants.
#'
#' @section The outcomes table:
#' One row per participant and endpoint, with a time and a flag, as ADaM's
#' time-to-event dataset holds it: `PARAMCD` names the endpoint, `PARAM` labels
#' it, `AVAL` is the time and `CNSR` the flag, 1 for censored. The settings
#' `endpoint_col`, `endpoint_label_col`, `time_col`, `censor_col` and
#' `outcome_id_col` (the participant's id, `id_col` when `NULL`) name other
#' columns. A flag the other way round, 1 for an event, is named by
#' `event_col`; named alone it is the flag, and exactly one of the two is
#' named. A participant with no row for the endpoint, more than one, a time or
#' flag that is missing or not a number, a flag that is not 0 or 1, or a
#' negative time is left out, and the chart says how many and why.
#'
#' @section What the page opens on:
#' The endpoint is `endpoint`, the first the outcomes table has when none is
#' named. The groups are `group_by`: a column's name, or a cut variable,
#' `list(measure, visit, cut)` for a biomarker or
#' `list(col, type = "number", cut)` for a number, cut at its `"median"`,
#' `"tertiles"`, `"quartiles"` or at typed points; with none named, the first
#' category column. A column's groups are in order of name; a cut's run low to
#' high, labelled with their bounds. The setting `cuts` lists more cut
#' variables the Groups control offers, and a cut line can be dragged on the
#' chart's histogram.
#'
#' @inheritSection Widget_CrossTab The cut rule
#'
#' @section Statistics shipped with the page:
#' The chart computes no test. It asks R once for its curves, with one row per
#' participant drawn: the id, the time, the group and the flag. A cut's groups
#' are handed to R high to low, so the hazard ratio is the higher group's
#' hazard over the lower's; a column's are in order of code point. The widget
#' stores R's answer for the view the settings open on. A reader who moves the
#' endpoint, the groups, a cut line or a filter to a view that was not computed
#' is told that statistics are unavailable for it; the page never shows one
#' view's test under another.
#'
#' @inheritSection Widget_GroupComparison Filters
#' @inheritSection Widget_GroupComparison Bundles
#'
#' @inheritParams Widget_GroupComparison
#' @param dfParticipants `data.frame` One row per participant, or `NULL`. With
#'   it the chart has filters, offers its category columns to the Groups
#'   control, and its numbers can be cut. Default: `NULL`.
#' @param lSettings `list` bio.viz stratified survival settings, under
#'   bio.viz's own names; laid over the chart's defaults in the page, so only
#'   overrides are needed. For example `endpoint`, `group_by`, `cuts`,
#'   `at_risk_times`, `groups`, `filters`, `baseline_visits` and the outcome
#'   columns. The setting `connection` cannot be given, and `statistic` can
#'   only be `"Analyze_Survival"` or `NULL` for no statistics line. Default:
#'   `list()`.
#' @param dfOutcomes `data.frame` The outcomes table: one row per participant
#'   and endpoint, with a time and a flag (see "The outcomes table"), or
#'   `NULL`, when the chart says it needs one and nothing is stored. Default:
#'   `NULL`.
#'
#' @return An `htmlwidget`. Its payload `x` carries `dfResults`,
#'   `dfParticipants`, `dfOutcomes` when it is given, `lSettings`, `bDebug`,
#'   whether a width and a height were left to the widget (`bAutoWidth`,
#'   `bAutoHeight`), and `lStatistics`: the stored results, each with `name`,
#'   `args`, `dataId`, `rows` and `value`, and `computed_by`, the R version,
#'   gsm.bio version and time that computed them.
#'
#' @examples
#' # Event-free survival in the synthetic study by CRP at Baseline cut at its
#' # median: the study plants worse survival with high CRP. R's log-rank test,
#' # medians and hazard ratio are stored in the page.
#' Widget_StratifiedSurvival(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     endpoint = "EFS",
#'     group_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
#'     cuts = list(list(measure = "CRP", visit = "Baseline", cut = "tertiles"))
#'   ),
#'   dfOutcomes = Synthetic_Outcomes
#' )
#'
#' # The same endpoint by arm.
#' Widget_StratifiedSurvival(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(group_by = "ARM"),
#'   dfOutcomes = Synthetic_Outcomes
#' )
#'
#' @seealso [Analyze_Survival()], which computes the test, and
#'   [Synthetic_Outcomes], the synthetic study's outcomes table.
#' @family widgets
#' @export
Widget_StratifiedSurvival <- function(
    dfResults,
    dfParticipants = NULL,
    lSettings = list(),
    dfOutcomes = NULL,
    width = NULL,
    height = NULL,
    elementId = NULL,
    bDebug = FALSE) {
  Widget_CheckInputs(dfResults, dfParticipants, lSettings, bDebug)
  if (!is.null(dfOutcomes) && !is.data.frame(dfOutcomes)) {
    Widget_CheckOutcomes(dfOutcomes, NULL)
  }
  # Text as UTF-8, marked so, before anything is computed: the page carries it
  # so whatever the session's locale.
  dfResults <- Widget_Utf8(dfResults)
  dfParticipants <- Widget_Utf8(dfParticipants)
  dfOutcomes <- Widget_Utf8(dfOutcomes)
  lSettings <- Widget_Utf8(lSettings)
  lConfig <- StratifiedSurvival_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  Widget_CheckOutcomes(dfOutcomes, lConfig)
  lNamed <- Widget_NameBaseline(lConfig, lSettings, dfResults)
  lNamed <- Widget_NameFilters(lNamed$config, lNamed$settings, dfResults, dfParticipants)
  lNamed$settings <- Widget_NameCuts(lNamed$config, lNamed$settings, "group_by")

  Widget_Create(
    "Widget_StratifiedSurvival", dfResults, dfParticipants, lNamed$settings,
    StratifiedSurvival_StoredResults(dfResults, dfParticipants, dfOutcomes, lNamed$config),
    width, height, elementId, bDebug, dfOutcomes
  )
}
