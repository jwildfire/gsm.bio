#' Run Chart Specifications to Figures and Tables
#'
#' Reads a list of bio.viz chart specifications, the format a bio.viz chart
#' writes with `specification()` (bio.viz, docs/output.md, "Specifications"),
#' and draws each against the tables given: a static figure by the chart's
#' `Visualize_*()` function and an RTF table by its `Table_*()` function and
#' [Write_RTF()], into a folder, with a manifest of what was written. One
#' specification can be run across every biomarker, one figure and one table
#' per biomarker.
#'
#' @section A specification is data:
#' A specification is read by jsonlite as data: text, numbers, `true`, `false`,
#' `null`, lists and objects. Nothing in it is evaluated. A title or a setting
#' that looks like code is text, filled and drawn as text.
#'
#' Each specification is read as bio.viz reads one, and refused, with a
#' sentence naming why, as bio.viz refuses it:
#' - one of another `format` or `format_version`, or with a member the format
#'   does not have;
#' - one whose `bio_viz_version` is not text;
#' - a chart bio.viz does not have, or a setting the chart does not have;
#' - a value the chart refuses;
#' - a filter whose operator is not `in`, that names no column or a column
#'   another filter is on, or that lists a value twice;
#' - `__proto__`, `constructor` or `prototype` as a column;
#' - nesting deeper than 64.
#'
#' A refused specification is a row of the manifest with its reason. The rest
#' still run.
#'
#' The filters in force, `{ column, operator: "in", values }`, are laid onto the
#' chart's `filters` setting as where each starts, as bio.viz does. A value is
#' compared as text.
#'
#' @section Across every biomarker:
#' With `bAcrossBiomarkers`, the setting that holds a chart's biomarker takes
#' each biomarker the chart offers in turn:
#' - the group comparison's `start_value`;
#' - the association scatter's `x`, or `y` when `x` is not a biomarker;
#' - the correlation matrix's `measure`, across visits;
#' - a cut biomarker in the cross-tabulation's `row_by` or `col_by`;
#' - a cut biomarker in the stratified survival chart's `group_by`.
#'
#' A chart that is already of every biomarker, such as the screen or a matrix
#' across biomarkers, or one with no biomarker to take, is one view.
#'
#' @param xSpecifications The specifications: a JSON file, JSON text, or the
#'   list jsonlite reads one as. A JSON array of specifications, or one
#'   specification.
#' @param dfResults `data.frame` The results table, one row per participant,
#'   biomarker and visit.
#' @param dfParticipants `data.frame` One row per participant, or `NULL`.
#'   Default: `NULL`.
#' @param dfOutcomes `data.frame` The outcomes table, for the stratified
#'   survival chart and the screen's hazard ratio, or `NULL`. Default: `NULL`.
#' @param strFolder `character` The folder to write into, made if it is not
#'   there. A file already there with an output's name is replaced.
#' @param bAcrossBiomarkers `logical` Whether to run each specification across
#'   every biomarker: one value for all, or one per specification. Default:
#'   `FALSE`.
#' @param chrFormats `character` The figure's formats, any of `"png"`, `"pdf"`
#'   and `"svg"`. An SVG needs the svglite package. Default: `"png"`.
#' @param bTables `logical` Whether to write each table to RTF, which needs
#'   r2rtf. Default: `TRUE`.
#' @param nWidth,nHeight `numeric` A figure's size, in inches. Default: `9`
#'   by `6`.
#'
#' @return A `data.frame`, the manifest, one row per output. It has these
#'   columns:
#'   - `specification`: the specification's place in the list;
#'   - `chart`;
#'   - `biomarker`: `NA` when the view is not one of several;
#'   - `status`: `"written"`, `"refused"` (the specification could not be
#'     read), or `"failed"` (it was read and could not be drawn);
#'   - `reason`;
#'   - `title` and `subtitle`, filled;
#'   - `statistics`: the lines printed under the figure;
#'   - `figure`: the figure's files, separated by `;`;
#'   - `table`: the RTF table's file.
#'
#'   File names are relative to `strFolder`. The manifest is also written there
#'   as `manifest.json`.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("r2rtf", quietly = TRUE)) {
#'   strSpecifications <- '[{
#'     "format": "bio.viz specification", "format_version": 1, "bio_viz_version": "0.1.0",
#'     "chart": "group-comparison",
#'     "settings": {
#'       "start_value": "IL-6", "visits": ["Week 4"], "value_type": "change",
#'       "baseline_visits": ["Baseline"], "group_by": "ARM", "title": "{measure} by {group}"
#'     },
#'     "filters": []
#'   }]'
#'   Run_Specifications(
#'     strSpecifications,
#'     Synthetic_Results,
#'     Synthetic_Participants,
#'     strFolder = tempfile("figures")
#'   )
#' }
#'
#' @seealso [Visualize_GroupComparison()] and [Table_GroupComparison()] and the
#'   functions beside them, which draw each output.
#' @family figures
#' @export
Run_Specifications <- function(
    xSpecifications,
    dfResults,
    dfParticipants = NULL,
    dfOutcomes = NULL,
    strFolder,
    bAcrossBiomarkers = FALSE,
    chrFormats = "png",
    bTables = TRUE,
    nWidth = 9,
    nHeight = 6) {
  Figure_NeedGgplot("Run_Specifications")
  Spec_NeedJsonlite("Run_Specifications")
  if (!is.data.frame(dfResults)) {
    stop("dfResults is not a data.frame", call. = FALSE)
  }
  if (!(is.character(strFolder) && length(strFolder) == 1L && !is.na(strFolder) && nzchar(strFolder))) {
    stop("strFolder must be the name of the folder to write into", call. = FALSE)
  }
  if (!(is.character(chrFormats) && length(chrFormats) > 0L && all(chrFormats %in% c("png", "pdf", "svg")) && !anyDuplicated(chrFormats))) {
    stop("chrFormats must be one or more of \"png\", \"pdf\" and \"svg\", each once", call. = FALSE)
  }
  if ("svg" %in% chrFormats && !requireNamespace("svglite", quietly = TRUE)) {
    stop("An SVG figure is written by svglite, which is not installed. Install it with install.packages(\"svglite\").", call. = FALSE)
  }
  if (!(is.logical(bTables) && length(bTables) == 1L && !is.na(bTables))) {
    stop("bTables is not a logical", call. = FALSE)
  }
  if (bTables && !Table_HasR2rtf()) {
    stop("Run_Specifications() writes its tables to RTF with r2rtf, which is not installed. Install it with install.packages(\"r2rtf\"), or give bTables = FALSE.", call. = FALSE)
  }
  lSpecs <- Spec_Parse(xSpecifications)
  if (!(is.logical(bAcrossBiomarkers) && length(bAcrossBiomarkers) %in% c(1L, length(lSpecs)) && !anyNA(bAcrossBiomarkers))) {
    stop("bAcrossBiomarkers must be TRUE or FALSE, once or once for each specification", call. = FALSE)
  }
  bAcross <- rep_len(bAcrossBiomarkers, length(lSpecs))
  dir.create(strFolder, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(strFolder)) {
    stop("The folder '", strFolder, "' could not be made", call. = FALSE)
  }

  lRows <- list()
  Row <- function(iSpec, strChart, strBiomarker, strStatus, strReason = NA_character_, lDrawn = list()) {
    data.frame(
      specification = iSpec, chart = strChart, biomarker = strBiomarker, status = strStatus, reason = strReason,
      title = if (is.null(lDrawn$title)) NA_character_ else lDrawn$title,
      subtitle = if (is.null(lDrawn$subtitle)) NA_character_ else lDrawn$subtitle,
      statistics = if (is.null(lDrawn$statistics)) NA_character_ else lDrawn$statistics,
      figure = if (is.null(lDrawn$figure)) NA_character_ else lDrawn$figure,
      table = if (is.null(lDrawn$table)) NA_character_ else lDrawn$table,
      stringsAsFactors = FALSE
    )
  }
  for (iSpec in seq_along(lSpecs)) {
    lRead <- tryCatch(Spec_ReadChecked(lSpecs[[iSpec]]), error = function(cndError) conditionMessage(cndError))
    if (is.character(lRead)) {
      strChart <- if (Spec_IsObject(lSpecs[[iSpec]]) && Spec_IsText(lSpecs[[iSpec]]$chart)) lSpecs[[iSpec]]$chart else NA_character_
      lRows[[length(lRows) + 1L]] <- Row(iSpec, strChart, NA_character_, "refused", lRead)
      next
    }
    lViews <- if (bAcross[iSpec]) Spec_Expand(lRead, dfResults, dfParticipants) else stats::setNames(list(lRead), NA_character_)
    for (iView in seq_along(lViews)) {
      strBiomarker <- names(lViews)[iView]
      strStem <- sprintf("%02d-%s%s", iSpec, lRead$chart, if (is.na(strBiomarker)) "" else paste0("-", Batch_Slug(strBiomarker)))
      lDrawn <- tryCatch(
        Batch_Draw(lViews[[iView]], dfResults, dfParticipants, dfOutcomes, strFolder, strStem, chrFormats, bTables, nWidth, nHeight),
        error = function(cndError) conditionMessage(cndError)
      )
      lRows[[length(lRows) + 1L]] <- if (is.character(lDrawn)) {
        Row(iSpec, lRead$chart, strBiomarker, "failed", lDrawn)
      } else {
        Row(iSpec, lRead$chart, strBiomarker, "written", lDrawn$reason, lDrawn)
      }
    }
  }
  dfManifest <- Reduce(rbind, lRows)
  rownames(dfManifest) <- NULL
  jsonlite::write_json(dfManifest, file.path(strFolder, "manifest.json"), dataframe = "rows", na = "null", auto_unbox = TRUE, pretty = TRUE)
  dfManifest
}

# A biomarker's name as part of a file's name.
Batch_Slug <- function(strText) {
  strSlug <- gsub("[^a-z0-9]+", "-", tolower(iconv(strText, "UTF-8", "ASCII//TRANSLIT", sub = "")))
  gsub("^-+|-+$", "", strSlug)
}

# One view drawn: its figure in each format and its table to RTF. A table that
# cannot be made (the chart tests nothing at the settings) leaves the figure
# written and says why.
Batch_Draw <- function(lRead, dfResults, dfParticipants, dfOutcomes, strFolder, strStem, chrFormats, bTables, nWidth, nHeight) {
  lSettings <- lRead$settings
  gg <- switch(lRead$chart,
    "group-comparison" = Visualize_GroupComparison(dfResults, dfParticipants, lSettings),
    "association-scatter" = Visualize_AssociationScatter(dfResults, dfParticipants, lSettings),
    "correlation-matrix" = Visualize_CorrelationMatrix(dfResults, dfParticipants, lSettings),
    "biomarker-screen" = Visualize_BiomarkerScreen(dfResults, dfParticipants, lSettings, dfOutcomes = dfOutcomes),
    "cross-tab" = Visualize_CrossTab(dfResults, dfParticipants, lSettings),
    "stratified-survival" = Visualize_StratifiedSurvival(dfResults, dfParticipants, lSettings, dfOutcomes = dfOutcomes)
  )
  chrFigures <- paste0(strStem, ".", chrFormats)
  chrNotes <- character(0)
  for (iFormat in seq_along(chrFormats)) {
    strFile <- file.path(strFolder, chrFigures[iFormat])
    if (chrFormats[iFormat] == "pdf" && Batch_HasCairo()) {
      # A PDF by cairo, which draws every character a figure holds: the sign of
      # a cut, an apostrophe.
      ggplot2::ggsave(strFile, gg, width = nWidth, height = nHeight, device = grDevices::cairo_pdf)
    } else if (chrFormats[iFormat] == "pdf") {
      # Without cairo the pdf device draws only Latin-1, and a character beyond
      # it as a dot: the figure is written, and its row says so.
      bDots <- FALSE
      withCallingHandlers(
        ggplot2::ggsave(strFile, gg, width = nWidth, height = nHeight, device = "pdf"),
        warning = function(cndWarning) {
          if (grepl("conversion failure on", conditionMessage(cndWarning), fixed = TRUE)) {
            bDots <<- TRUE
            invokeRestart("muffleWarning")
          }
        }
      )
      if (bDots) {
        chrNotes <- c(chrNotes, "The PDF was drawn without cairo, which this R cannot load, so a character beyond Latin-1 is a dot in it; the PNG and the RTF table have every character.")
      }
    } else {
      ggplot2::ggsave(strFile, gg, width = nWidth, height = nHeight, device = chrFormats[iFormat], dpi = 150)
    }
  }
  strTable <- NULL
  if (bTables) {
    dfTable <- tryCatch(switch(lRead$chart,
      "group-comparison" = Table_GroupComparison(dfResults, dfParticipants, lSettings),
      "association-scatter" = Table_AssociationScatter(dfResults, dfParticipants, lSettings),
      "correlation-matrix" = Table_CorrelationMatrix(dfResults, dfParticipants, lSettings),
      "biomarker-screen" = Table_BiomarkerScreen(dfResults, dfParticipants, lSettings, dfOutcomes = dfOutcomes),
      "cross-tab" = Table_CrossTab(dfResults, dfParticipants, lSettings),
      "stratified-survival" = Table_StratifiedSurvival(dfResults, dfParticipants, lSettings, dfOutcomes = dfOutcomes)
    ), error = function(cndError) conditionMessage(cndError))
    if (is.character(dfTable)) {
      chrNotes <- c(chrNotes, paste("No table:", dfTable))
    } else {
      strTable <- paste0(strStem, ".rtf")
      Write_RTF(dfTable, file.path(strFolder, strTable))
    }
  }
  list(
    title = gg$labels$title, subtitle = gg$labels$subtitle,
    # What the figure prints: each panel's heading, where a panel has its own
    # test, then the lines under it.
    statistics = gsub("\n", " ", paste(c(if (is.factor(gg$data$heading)) levels(gg$data$heading), gg$labels$caption), collapse = " | "), fixed = TRUE),
    figure = paste(chrFigures, collapse = ";"), table = strTable,
    reason = if (length(chrNotes) > 0L) paste(chrNotes, collapse = " ") else NA_character_
  )
}

# Whether this R can draw with cairo: asked once, by drawing nothing.
Batch_HasCairo <- local({
  bCairo <- NULL
  function() {
    if (is.null(bCairo)) {
      bCairo <<- isTRUE(capabilities("cairo")) && tryCatch(
        {
          strProbe <- tempfile(fileext = ".pdf")
          grDevices::cairo_pdf(strProbe)
          grDevices::dev.off()
          TRUE
        },
        error = function(cndError) FALSE,
        warning = function(cndWarning) FALSE
      )
    }
    bCairo
  }
})
