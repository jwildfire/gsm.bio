#' Cross-Tabulation Widget
#'
#' A widget that renders the bio.viz cross-tabulation: a two-way table of
#' counts, with its totals and percentages, beside stacked bars of the same
#' numbers, and under it a test of the table. Either variable is a column, or a
#' biomarker or a participant-level number cut into groups by the shared cut
#' rule. The test is computed here, in R, by [Analyze_Contingency()], and
#' shipped with the page, so a saved page shows it with no R and no network. A
#' click on a count lists that cell's participants.
#'
#' @section What the page opens on:
#' The rows are `row_by` and the columns `col_by`: each a column's name, or a
#' cut variable, `list(measure, visit, cut)` for a biomarker or
#' `list(col, type = "number", cut)` for a number, cut at its `"median"`,
#' `"tertiles"`, `"quartiles"` or at typed points. With neither named the table
#' is of the first two category columns. A column's categories are in order
#' by name, with numbers as numbers (so "2 mg" comes before "10 mg", and "a"
#' before "B"), as the chart draws them; a cut's run low to high, labelled with
#' their bounds. The setting
#' `cuts` lists more cut variables the Rows and Columns controls offer.
#'
#' @section The cut rule:
#' A cut is the one bio.viz uses in every chart: the points are [stats::quantile()]
#' with its default, type 7, on the participants the filters keep who have a
#' value, or the typed points as written; a participant is in the group
#' [base::cut()] puts them in with `right = TRUE`, so a value equal to a point
#' falls in the lower group; a bound is written to four significant digits.
#'
#' @section Statistics shipped with the page:
#' The chart computes no test. It asks R once for the table, with one row per
#' participant who has a category each way, and the categories in the table's
#' order. The widget stores R's answer for the table the settings open on, by
#' chi-square and by Fisher's exact test, so the Test control is answered
#' either way. A reader who moves the rows, the columns or a filter to a view
#' that was not computed is told that statistics are unavailable for it; the
#' page never shows one table's test under another.
#'
#' @inheritSection Widget_GroupComparison Filters
#' @inheritSection Widget_GroupComparison Bundles
#'
#' @inheritParams Widget_GroupComparison
#' @param dfParticipants `data.frame` One row per participant, or `NULL`. With
#'   it the chart has filters, offers its category columns to the Rows and
#'   Columns controls, and its numbers can be cut. Default: `NULL`.
#' @param lSettings `list` bio.viz cross-tabulation settings, under bio.viz's
#'   own names; laid over the chart's defaults in the page, so only overrides
#'   are needed. For example `row_by`, `col_by`, `percent` (`"row"`, `"col"` or
#'   `"none"`), `test` (`"chisq"`, `"fisher"` or `"none"`), `cuts`, `groups`,
#'   `filters` and `baseline_visits`. The setting `connection` cannot be given,
#'   and `statistic` can only be `"Analyze_Contingency"` or `NULL` for no
#'   statistics line. Default: `list()`.
#'
#' @return An `htmlwidget`. Its payload `x` carries `dfResults`,
#'   `dfParticipants`, `lSettings`, `bDebug`, whether a width and a height were
#'   left to the widget (`bAutoWidth`, `bAutoHeight`), and `lStatistics`: the
#'   stored results, each with `name`, `args`, `dataId`, `rows` and `value`, and
#'   `computed_by`, the R version, gsm.bio version and time that computed them.
#'
#' @examples
#' # Response by arm in the synthetic study, with R's chi-square test and
#' # Fisher's exact test of the table stored in the page.
#' Widget_CrossTab(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(row_by = "ARM", col_by = "RESPONSE")
#' )
#'
#' # Response by CRP at Baseline cut at its median.
#' Widget_CrossTab(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(
#'     row_by = "RESPONSE",
#'     col_by = list(measure = "CRP", visit = "Baseline", cut = "median")
#'   )
#' )
#'
#' @seealso [Analyze_Contingency()], which computes the test.
#' @family widgets
#' @export
Widget_CrossTab <- function(
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
  lConfig <- CrossTab_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lNamed <- Widget_NameBaseline(lConfig, lSettings, dfResults)
  lNamed <- Widget_NameFilters(lNamed$config, lNamed$settings, dfResults, dfParticipants)
  lNamed$settings <- Widget_NameCuts(lNamed$config, lNamed$settings, c("row_by", "col_by"))

  Widget_Create(
    "Widget_CrossTab", dfResults, dfParticipants, lNamed$settings,
    CrossTab_StoredResults(dfResults, dfParticipants, lNamed$config),
    width, height, elementId, bDebug
  )
}
