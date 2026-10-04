#' Cross-Tabulation Figure
#'
#' A static ggplot2 figure of the table the cross-tabulation opens on: a bar
#' per row of the table, stacked by its columns as the chart's bars are, each
#' segment labelled with its count, and R's test of the table printed under the
#' figure. With `percent = "row"` (the default) a bar is the row's percentages;
#' with `"col"` a bar is a column's; with `"none"` the counts themselves. It is
#' the table [Widget_CrossTab()] draws in the browser, from the same settings,
#' and its test is [Analyze_Contingency()]'s on the same rows.
#'
#' @section Titles and footnotes:
#' The settings `title`, `subtitle` and `footnotes` are text with named
#' placeholders, as for [Visualize_GroupComparison()]. This figure fills
#' `{rows}` and `{columns}` (what they are, as the controls name them), `{n}`
#' (the participants in the table), `{filters}`, `{date}` and `{version}`.
#'
#' @inheritParams Widget_CrossTab
#' @param lSettings `list` bio.viz cross-tabulation settings, as
#'   [Widget_CrossTab()] takes them, and `title`, `subtitle` and `footnotes`.
#'   Default: `list()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   Visualize_CrossTab(
#'     Synthetic_Results,
#'     Synthetic_Participants,
#'     lSettings = list(
#'       row_by = "RESPONSE",
#'       col_by = list(measure = "CRP", visit = "Baseline", cut = "median"),
#'       title = "{rows} by {columns}"
#'     )
#'   )
#' }
#'
#' @seealso [Widget_CrossTab()], its interactive twin.
#' @family figures
#' @export
Visualize_CrossTab <- function(dfResults, dfParticipants = NULL, lSettings = list()) {
  lTitles <- Figure_Inputs("Visualize_CrossTab", dfResults, dfParticipants, lSettings)
  lConfig <- CrossTab_Settings(lSettings)
  Widget_CheckColumns(lConfig, dfResults, dfParticipants)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- CrossTab_State(dfResults, dfParticipants, lConfig)
  Chart_StopIfNobody("Visualize_CrossTab", dfResults, dfParticipants, lConfig, lState$filters)
  lTable <- CrossTab_Table(dfResults, dfParticipants, lConfig, lState)
  if (lTable$total == 0L) {
    Core_Stop("Visualize_CrossTab(): there is no table to draw at these settings")
  }
  lAnswers <- Chart_Answer(
    CrossTab_Requests(dfResults, dfParticipants, lConfig, lState),
    list(Analyze_Contingency = Analyze_Contingency)
  )

  nCounts <- lTable$counts
  dfCells <- expand.grid(row = seq_along(lTable$row_levels), col = seq_along(lTable$col_levels))
  dfCells$n <- nCounts[cbind(dfCells$row, dfCells$col)]
  nRowTotals <- rowSums(nCounts)
  nColTotals <- colSums(nCounts)
  dfCells$share <- switch(lState$percent,
    row = 100 * dfCells$n / nRowTotals[dfCells$row],
    col = 100 * dfCells$n / nColTotals[dfCells$col],
    none = dfCells$n
  )
  dfCells$row <- factor(lTable$row_levels[dfCells$row], levels = rev(lTable$row_levels))
  dfCells$col <- factor(lTable$col_levels[dfCells$col], levels = lTable$col_levels)
  dfCells$label <- ifelse(dfCells$n > 0L, as.character(dfCells$n), "")

  strRows <- Output_GroupLabel(lState$row_by, lConfig)
  strColumns <- Output_GroupLabel(lState$col_by, lConfig)
  lValues <- c(
    Output_SharedPlaceholders(lState$filters, lConfig$filters),
    list(rows = strRows, columns = strColumns, n = lTable$total)
  )
  lFilled <- Output_Titles(lTitles, lValues, lapply(lAnswers, `[[`, "value"))
  # Each answer as the chart's line prints it (bio.viz, src/cross-tab/statistic.js,
  # `describeAnswer`): the test, then each estimate with an interval, Fisher's
  # odds ratio named by the categories R was handed.
  chrStatistics <- unlist(lapply(lAnswers, function(lResult) CrossTab_Lines(lResult)))

  gg <- ggplot2::ggplot(dfCells, Figure_Aes(x = "share", y = "row", fill = "col")) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE), width = 0.7) +
    ggplot2::geom_text(Figure_Aes(label = "label"), position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE), size = 3, colour = "white") +
    ggplot2::scale_fill_manual(values = Figure_Palette(length(lTable$col_levels))) +
    ggplot2::labs(
      x = switch(lState$percent, row = "Percent of the row", col = "Percent of the column", none = "Participants"),
      y = strRows, fill = strColumns
    )
  Figure_Finish(gg, lFilled, chrStatistics)
}
