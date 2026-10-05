# What every static figure shares: ggplot2, which gsm.bio suggests and does
# not import, the inputs, the frame of a figure (its title, subtitle,
# statistics and footnotes, as R/output.R writes them), and the theme.
#
# A figure is the same view its interactive chart opens on, worked out by the
# same rules (R/core.R, R/chart.R and the chart's own file), with its
# statistics from the same Analyze_*() calls on the same rows. Nothing here is
# exported.

# ggplot2 is suggested, not imported: a figure needs it, and says so.
Figure_HasGgplot <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}

Figure_NeedGgplot <- function(strFunction) {
  if (!Figure_HasGgplot()) {
    stop(
      strFunction, "() draws a static figure with ggplot2, which is not installed. ",
      "Install it with install.packages(\"ggplot2\"); the widgets do not need it.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# The tables and the settings a figure is given, checked as a widget's are,
# with the title, subtitle and footnotes checked as the chart checks them.
Figure_Inputs <- function(strFunction, dfResults, dfParticipants, lSettings, dfOutcomes = NULL) {
  Figure_NeedGgplot(strFunction)
  Widget_CheckInputs(dfResults, dfParticipants, lSettings, FALSE)
  if (!is.null(dfOutcomes) && !is.data.frame(dfOutcomes)) {
    Widget_CheckOutcomes(dfOutcomes, NULL)
  }
  Output_CheckTitles(Core_Overlay(lOutputTitleDefaults, lSettings[intersect(names(lSettings), names(lOutputTitleDefaults))]))
}

# Text wrapped to a width, its lines joined, as a figure prints it.
Figure_Wrap <- function(chrText, nWidth = 100L) {
  vapply(chrText, function(strText) paste(strwrap(strText, width = nWidth), collapse = "\n"), character(1), USE.NAMES = FALSE)
}

# The figure with its title and subtitle above it, and under it the
# statistics it printed and its footnotes, the figure's own last.
Figure_Finish <- function(gg, lTitles, chrStatistics = character(0)) {
  chrUnder <- c(chrStatistics, lTitles$footnotes)
  gg +
    ggplot2::labs(
      title = lTitles$title, subtitle = lTitles$subtitle,
      caption = paste(Figure_Wrap(chrUnder), collapse = "\n")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(hjust = 0, colour = "#52616f", size = 8),
      plot.subtitle = ggplot2::element_text(colour = "#3e4c59"),
      strip.text = ggplot2::element_text(hjust = 0),
      legend.position = "bottom"
    )
}

# safety.viz's categorical palette, as the charts colour their groups.
chrFigurePalette <- c(
  "#2563eb", "#059669", "#d97706", "#9333ea", "#dc2626", "#0891b2", "#65a30d", "#db2777", "#4b5563", "#ca8a04"
)

Figure_Palette <- function(nLevels) {
  rep_len(chrFigurePalette, max(nLevels, 1L))
}

# A mapping of aesthetics to columns, by the columns' names, with no symbols
# in the package's code: `Figure_Aes(x = "x", y = "y")` is `aes(x = x, y = y)`.
Figure_Aes <- function(...) {
  do.call(ggplot2::aes, lapply(list(...), as.name))
}
