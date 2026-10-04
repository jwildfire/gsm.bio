#' Biomarker Screen Table
#'
#' The rows of the screen the biomarker screen opens on, as a table: one row
#' per biomarker, with R's estimate and its interval, the counts, the
#' unadjusted p-value and the p-value adjusted across the rows, written as the
#' chart prints them. The numbers are [Analyze_Screen()]'s on the frame the
#' chart hands R.
#'
#' @inheritSection Table_GroupComparison Display rules
#'
#' @section Titles and footnotes:
#' As for `Visualize_BiomarkerScreen()`: `{heading}`, `{comparison}`,
#' `{visit}`, `{endpoint}`, `{biomarkers}`, `{n}`, `{filters}`, `{date}` and
#' `{version}`.
#'
#' @inheritParams Widget_BiomarkerScreen
#' @param lSettings `list` bio.viz biomarker screen settings, as
#'   [Widget_BiomarkerScreen()] takes them, and `title`, `subtitle` and
#'   `footnotes`. Default: `list()`.
#'
#' @return A `data.frame` of text: `Biomarker`, then `Statistic`, `Method`,
#'   `Estimate`, `Counts`, `p-value`, `Adjusted p-value` and `Note`, with the
#'   attributes of [Table_GroupComparison()].
#'
#' @examples
#' Table_BiomarkerScreen(
#'   Synthetic_Results,
#'   Synthetic_Participants,
#'   lSettings = list(visit = "Week 4", value_type = "change", group_by = "ARM")
#' )
#'
#' @seealso `Visualize_BiomarkerScreen()` and [Widget_BiomarkerScreen()].
#' @family tables
#' @export
Table_BiomarkerScreen <- function(dfResults, dfParticipants = NULL, lSettings = list(), dfOutcomes = NULL) {
  lTitles <- Table_Inputs(dfResults, dfParticipants, lSettings, dfOutcomes)
  lConfig <- BiomarkerScreen_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  Widget_CheckOutcomes(dfOutcomes, lConfig)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- BiomarkerScreen_State(dfResults, dfParticipants, lConfig, dfOutcomes)
  Chart_StopIfNobody("Table_BiomarkerScreen", dfResults, dfParticipants, lConfig, lState$filters)
  lRequests <- BiomarkerScreen_Requests(dfResults, dfParticipants, lConfig, lState, dfOutcomes)
  if (length(lRequests) == 0L) {
    strWhy <- if (is.null(lConfig$statistic)) {
      "the setting 'statistic' is NULL, which asks R for no rows"
    } else if (lState$comparison == "difference" && length(lState$levels) != 2L) {
      "there is no column with two groups to compare"
    } else if (lState$comparison == "correlation" && is.null(lState$with)) {
      "there is no variable to correlate with"
    } else if (lState$comparison == "hazard" && is.null(lState$endpoint)) {
      "the outcomes table has no endpoint"
    } else {
      "no participant has a value of any biomarker at the visit, after the filters, or a change is read at the one baseline visit"
    }
    Core_Stop("Table_BiomarkerScreen() has no statistic to show: ", strWhy)
  }
  lAnswer <- Chart_Answer(lRequests, list(Analyze_Screen = Analyze_Screen))[[1]]
  lValue <- lAnswer$value
  dfScreen <- lValue$rows
  chrEstimates <- c(
    difference = paste0("Standardised difference (Hedges", intToUtf8(0x2019L), " g)"),
    correlation = paste0(if (lState$method == "spearman") "Spearman" else "Pearson", intToUtf8(0x2019L), if (lState$method == "spearman") "s rho" else "s r"),
    hazard = "Hazard ratio, High / Low"
  )
  chrGroups <- if (lState$comparison == "difference") lState$levels else if (lState$comparison == "hazard") c("High", "Low") else NULL
  dfRows <- do.call(rbind, lapply(seq_len(nrow(dfScreen)), function(iRow) {
    lRow <- as.list(dfScreen[iRow, ])
    bShown <- identical(lRow$status, "ok") && !is.na(lRow$estimate)
    strCounts <- if (!is.null(chrGroups) && !is.na(lRow$n_1) && !is.na(lRow$n_2)) {
      paste0(chrGroups[1], " n = ", lRow$n_1, ", ", chrGroups[2], " n = ", lRow$n_2)
    } else {
      paste("n =", lRow$counts)
    }
    data.frame(
      Biomarker = lRow$biomarker, Statistic = chrEstimates[[lState$comparison]], Method = lRow$method,
      Estimate = if (bShown) Table_Estimate(c(list(name = "Estimate", group = NA), lRow[c("estimate", "lower", "upper", "level")]), FALSE) else "",
      Counts = strCounts,
      `p-value` = if (bShown && !is.na(lRow$p_unadjusted)) Output_P(lRow$p_unadjusted) else "",
      `Adjusted p-value` = if (bShown && !is.na(lRow$p_value)) Output_P(lRow$p_value) else "",
      Note = if (bShown) Table_Note(list(adjustment = lRow$adjustment)) else if (is.na(lRow$reason)) "Not computed." else lRow$reason,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }))
  strValue <- lState$value_type
  chrValueWords <- c(raw = "Result", baseline = "Baseline value", chrOutputValueLabels[c("change", "fold_change", "percent_change")])
  strWords <- paste0(chrValueWords[[strValue]], if (strValue == "baseline") "" else paste0(" at ", lState$visit))
  dfEndpoints <- Chart_Endpoints(dfOutcomes, lConfig)
  strEndpoint <- if (lState$comparison == "hazard") dfEndpoints$label[match(lState$endpoint, dfEndpoints$endpoint)] else ""
  strHeading <- if (lState$comparison == "difference") {
    paste0(strWords, ": ", if (length(lState$levels) == 2L) paste(lState$levels[1], "against", lState$levels[2]) else "two groups", ", standardised difference")
  } else if (lState$comparison == "hazard") {
    paste0(strWords, ": hazard ratio, high against low, on ", strEndpoint)
  } else {
    paste0(strWords, ": correlation with ", BiomarkerScreen_VariableName(lState$with))
  }
  chrComparisons <- c(
    difference = "Difference between two groups", correlation = "Correlation with one variable",
    hazard = "Hazard ratio, high against low"
  )
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(
      heading = strHeading, comparison = chrComparisons[[lState$comparison]],
      visit = if (is.null(lState$visit)) "" else lState$visit, endpoint = strEndpoint,
      biomarkers = nrow(dfScreen), n = lAnswer$rows
    )
  )
  Table_Make(dfRows, lTitles, lValues, list(lValue), rep(1L, nrow(dfRows)), strOf = "biomarkers")
}
