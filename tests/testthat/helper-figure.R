# What the figure tests share. Nothing here is exported.

# A figure as text, for a snapshot: its title, subtitle, axis titles and the
# lines under it, and for each layer its geometry and the data it draws, each
# numeric column summarised to four significant digits and each other column
# by its values and their counts. It is read from the figure as gsm.bio made
# it, not from ggplot2's built plot, so it is the same whatever ggplot2's
# version.
chrFigureDescription <- function(gg) {
  lLabels <- gg$labels
  Line <- function(strName, xValue) if (is.null(xValue)) character(0) else paste0(strName, ": ", xValue)
  chrOut <- c(
    Line("title", lLabels$title), Line("subtitle", lLabels$subtitle),
    Line("x", lLabels$x), Line("y", lLabels$y),
    # The lines under it unwrapped: where a line breaks depends on how wide
    # the session's locale counts a character.
    Line("caption", gsub("\n", " ", lLabels$caption, fixed = TRUE))
  )
  Summary <- function(dfData) {
    if (!is.data.frame(dfData)) {
      return("  (the plot's data)")
    }
    c(
      paste0("  rows: ", nrow(dfData)),
      unlist(lapply(names(dfData), function(strColumn) {
        xColumn <- dfData[[strColumn]]
        if (is.numeric(xColumn)) {
          nShown <- xColumn[is.finite(xColumn)]
          paste0(
            "  ", strColumn, ": ", sum(is.na(xColumn)), " NA; ",
            if (length(nShown)) paste(signif(c(min(nShown), max(nShown), mean(nShown), sum(nShown)), 4), collapse = " / ") else "none"
          )
        } else {
          # In the factor's order, or by code point: never by the locale's.
          chrText <- gsub("\n", " ", as.character(xColumn), fixed = TRUE)
          chrLevels <- if (is.factor(xColumn)) gsub("\n", " ", levels(xColumn), fixed = TRUE) else Core_SortText(unique(chrText[!is.na(chrText)]))
          tbl <- table(factor(chrText, levels = chrLevels), useNA = "ifany")
          if (length(tbl) > 12L) {
            paste0("  ", strColumn, ": ", length(tbl), " distinct values, ", sum(is.na(chrText)), " NA")
          } else {
            paste0("  ", strColumn, ": ", paste0(names(tbl), " x", as.integer(tbl), collapse = " | "))
          }
        }
      }))
    )
  }
  chrOut <- c(chrOut, "data:", Summary(gg$data))
  for (iLayer in seq_along(gg$layers)) {
    lLayer <- gg$layers[[iLayer]]
    chrOut <- c(chrOut, paste0("layer ", iLayer, ": ", class(lLayer$geom)[1]), Summary(lLayer$data))
  }
  chrOut
}

# The date drawn and the versions, which change from day to day and session to
# session, written as placeholders in a snapshot; and text that is not ASCII
# written as its code points, the same in every locale.
strFigureStable <- function(chrLines) {
  chrLines <- iconv(enc2utf8(chrLines), "UTF-8", "ASCII", sub = "Unicode")
  chrLines <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}", "<date>", chrLines)
  chrLines <- gsub("R [0-9]+\\.[0-9]+\\.[0-9]+", "R <version>", chrLines)
  gsub("gsm\\.bio [0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)?", "gsm.bio <version>", chrLines)
}

# The lines a figure prints under itself, as one text.
strFigureCaption <- function(gg) {
  gsub("\n", " ", gg$labels$caption, fixed = TRUE)
}
