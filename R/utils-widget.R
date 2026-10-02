# What every gsm.bio widget shares: how it is sized, and how R's answers are
# written so that a page reads them as the chart's connection expects.

#' Default sizing policy for gsm.bio widgets
#'
#' A bio.viz chart lays out as a normal document (controls beside the chart,
#' the statistics line and the listing beneath), so the widgets default to the
#' full container width rather than htmlwidgets' fixed 960px. This is the
#' policy gsm.safety's widgets use.
#'
#' @return An [htmlwidgets::sizingPolicy()].
#'
#' @keywords internal
#' @noRd
WidgetSizingPolicy <- function() {
  htmlwidgets::sizingPolicy(
    defaultWidth = "100%",
    browser.defaultWidth = "100%",
    viewer.defaultWidth = "100%",
    knitr.defaultWidth = "100%"
  )
}

#' Write an R value in the shape a page reads it
#'
#' Turns what a statistics function returned into plain nested lists that
#' become JSON in the shape bio.viz's connection gives a result computed in the
#' browser (bio.viz, docs/r-connection.md, "What R receives and returns"), so a
#' stored result reads the same as a live one:
#'
#' | R value | Becomes | In the page |
#' |---|---|---|
#' | data frame, at any depth | an unnamed list of rows, each a named list | an array of row objects |
#' | named list, named vector | a named list | an object |
#' | unnamed list | an unnamed list | an array, whatever its length |
#' | unnamed vector of length one | itself | a single value |
#' | unnamed vector of any other length | an unnamed list | an array |
#' | factor | text | text |
#' | `NA`, `NULL` | `NULL` | `null` |
#'
#' Nothing is left for the JSON writer to decide: a named vector's names are
#' kept by making it a list, and a table has one shape whatever its size.
#'
#' @param xValue The value: a statistics result, or any member of one.
#'
#' @return The value as nested lists, single values and `NULL`.
#'
#' @keywords internal
#' @noRd
StoredValue <- function(xValue) {
  if (is.null(xValue)) {
    return(NULL)
  }
  if (is.data.frame(xValue)) {
    return(lapply(seq_len(nrow(xValue)), function(iRow) {
      lapply(xValue, function(xColumn) StoredValue(xColumn[[iRow]]))
    }))
  }
  if (is.factor(xValue)) {
    xValue <- as.character(xValue)
  }
  bNamed <- !is.null(names(xValue)) && length(xValue) > 0L && all(nzchar(names(xValue)))
  if (is.list(xValue)) {
    lValue <- lapply(xValue, StoredValue)
    return(if (bNamed) lValue else unname(lValue))
  }
  if (!bNamed && length(xValue) == 1L) {
    return(if (is.na(xValue)) NULL else xValue)
  }
  lValue <- lapply(seq_along(xValue), function(iValue) if (is.na(xValue[[iValue]])) NULL else unname(xValue[iValue]))
  if (bNamed) stats::setNames(lValue, names(xValue)) else lValue
}

#' Which R computed a page's stored results
#'
#' A stored result is the answer of the R that built the widget, and R's own
#' answer can differ between versions (see [StatisticsResult]). The page says
#' which R answered, from this record.
#'
#' @return A named list: `r_version`, `gsm_bio_version`, `platform` and
#'   `computed_at`, the time in UTC as ISO 8601 text.
#'
#' @keywords internal
#' @noRd
StoredResultsProvenance <- function() {
  list(
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    gsm_bio_version = unname(getNamespaceVersion("gsm.bio")),
    platform = R.version$platform,
    computed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}
