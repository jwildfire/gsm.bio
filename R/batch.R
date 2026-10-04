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
#' that looks like code is text, filled and drawn as text. No name in a
#' specification chooses a function: a chart's statistics are computed by
#' calling its own `Analyze_*()` functions from a fixed list (the one place the
#' package calls a function it is handed, through `do.call()`).
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
#' - nesting deeper than 64;
#' - a list or object setting of a shape the chart does not take, in bio.viz's
#'   words.
#'
#' A refused specification is a row of the manifest with its reason. The rest
#' still run. The tests hold the reader to bio.viz's own, run in node on the
#' same specifications, over every setting of every chart. It differs on
#' purpose in two places:
#' - a chart's `statistic`, and the scatter's `fit_statistic`, must be the
#'   chart's own `Analyze_*()` function or `null`: bio.viz takes the name of
#'   any R function, and gsm.bio computes only its own;
#' - a specification given as text is checked for depth, as one given as an
#'   object is; bio.viz checks only an object (bio.viz#74).
#'
#' A refusal of a value the chart's own rules check (a choice it does not
#' have, a number out of range) is in R's words, which are not always
#' bio.viz's; which values are refused is the same.
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
#' across biomarkers, or one with no biomarker to take, is one view. The
#' scatter's other axis keeps its own biomarker out of the views only when the
#' two axes are at the same visit. A cut named by its rule (a median, tertiles)
#' is worked out on each biomarker's values; explicit cut points are applied
#' to every biomarker unchanged, and each row's `reason` says so.
#'
#' @section What the tables cannot honour:
#' A view is drawn as its chart draws it from the tables given. Where a
#' setting names something the tables lack (a column, a biomarker, a visit),
#' or a filter is on a column that is not a filter or on a value its column
#' lacks, the chart draws what it falls back to, and the row's `reason` says
#' what was not drawn as asked, in the words of bio.viz's notices; its status
#' stays `"written"`. A view whose filters keep no participant is `"failed"`
#' with "No participant passes the filters.", as the chart's footnote says.
#'
#' @param xSpecifications The specifications: a JSON file, JSON text, or the
#'   list `jsonlite::read_json()` (or `jsonlite::fromJSON(simplifyVector =
#'   FALSE)`) reads one as. A JSON array of specifications, or one
#'   specification. `fromJSON()`'s default simplifies them into a data frame
#'   or vectors, which are refused with a sentence that says so.
#' @param dfResults `data.frame` The results table, one row per participant,
#'   biomarker and visit.
#' @param dfParticipants `data.frame` One row per participant, or `NULL`.
#'   Default: `NULL`.
#' @param dfOutcomes `data.frame` The outcomes table, for the stratified
#'   survival chart and the screen's hazard ratio, or `NULL`. Default: `NULL`.
#' @param strFolder `character` The folder to write into, made if it is not
#'   there. A file already there with an output's name is replaced, and any
#'   other file there is left as it is: a run does not empty the folder. A
#'   view that fails part way leaves none of its files.
#' @param bAcrossBiomarkers `logical` Whether to run each specification across
#'   every biomarker: one value for all, or one per specification. Default:
#'   `FALSE`.
#' @param chrFormats `character` The figure's formats, any of `"png"`, `"pdf"`
#'   and `"svg"`. An SVG needs the svglite package. A PDF is drawn by cairo
#'   where this R can load it; where it cannot (a Mac without XQuartz, for
#'   one), by the pdf device, which draws a character beyond Latin-1, such as
#'   the sign of a cut, as a dot or a stand-in such as `<=`, and the row's
#'   `reason` says so. Default:
#'   `"png"`.
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
#'   - `reason`: why a specification was refused or a view failed; for a
#'     view written, what was not drawn as asked, a note of explicit cut
#'     points, a table that could not be made, or a PDF drawn without cairo;
#'     `NA` when there is nothing to say;
#'   - `participants`: how many participants the view's filters keep;
#'   - `title` and `subtitle`, filled;
#'   - `statistics`: the lines printed under the figure;
#'   - `figure`: the figure's files, separated by `;`;
#'   - `table`: the RTF table's file.
#'
#'   File names are relative to `strFolder`, each output's its own: the
#'   specification's place, the chart, and the biomarker's name as a file name
#'   can hold it (its place among the views when nothing of the name can be
#'   kept), numbered `-2`, `-3` and on when two would be the same. The manifest
#'   is also written there as `manifest.json`.
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
  chrStems <- character(0)
  Row <- function(iSpec, strChart, strBiomarker, strStatus, strReason = NA_character_, lDrawn = list(), nPassing = NA_integer_) {
    data.frame(
      specification = iSpec, chart = strChart, biomarker = strBiomarker, status = strStatus, reason = strReason,
      participants = as.integer(nPassing),
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
      strStem <- Batch_Stem(iSpec, lRead$chart, strBiomarker, iView, chrStems)
      chrStems <- c(chrStems, strStem)
      lOpened <- Batch_Opened(lViews[[iView]], dfResults, dfParticipants, dfOutcomes)
      nPassing <- Batch_Passing(lOpened, dfResults, dfParticipants)
      strNotices <- Batch_Notices(lViews[[iView]], lOpened, dfParticipants)
      strNote <- if (is.null(lViews[[iView]]$note)) NA_character_ else lViews[[iView]]$note
      Reason <- function(strMore) {
        chrReason <- stats::na.omit(c(strNote, strNotices, strMore))
        if (length(chrReason) == 0L) NA_character_ else paste(chrReason, collapse = " ")
      }
      if (identical(nPassing, 0L)) {
        # The chart draws nobody, and says so in its footnote.
        lRows[[length(lRows) + 1L]] <- Row(iSpec, lRead$chart, strBiomarker, "failed", Reason("No participant passes the filters."), nPassing = 0L)
        next
      }
      lDrawn <- tryCatch(
        Batch_Draw(lViews[[iView]], dfResults, dfParticipants, dfOutcomes, strFolder, strStem, chrFormats, bTables, nWidth, nHeight),
        error = function(cndError) conditionMessage(cndError)
      )
      lRows[[length(lRows) + 1L]] <- if (is.character(lDrawn)) {
        Row(iSpec, lRead$chart, strBiomarker, "failed", Reason(lDrawn), nPassing = nPassing)
      } else {
        Row(iSpec, lRead$chart, strBiomarker, "written", Reason(lDrawn$reason), lDrawn, nPassing)
      }
    }
  }
  dfManifest <- do.call(rbind, lRows)
  rownames(dfManifest) <- NULL
  jsonlite::write_json(dfManifest, file.path(strFolder, "manifest.json"), dataframe = "rows", na = "null", auto_unbox = TRUE, pretty = TRUE)
  dfManifest
}

# A chart's settings as R reads them, with each default.
Batch_Settings <- function(strChart, lSettings) {
  switch(strChart,
    "group-comparison" = GroupComparison_Settings(lSettings),
    "association-scatter" = AssociationScatter_Settings(lSettings),
    "correlation-matrix" = CorrelationMatrix_Settings(lSettings),
    "biomarker-screen" = BiomarkerScreen_Settings(lSettings),
    "cross-tab" = CrossTab_Settings(lSettings),
    "stratified-survival" = StratifiedSurvival_Settings(lSettings)
  )
}

# What a view's chart opens on, as its figure reads it: the settings, with
# the names a widget gives, and the state the chart's rules in R open on.
Batch_Opened <- function(lRead, dfResults, dfParticipants, dfOutcomes) {
  lSettings <- lRead$settings
  lConfig <- Batch_Settings(lRead$chart, lSettings)
  lConfig <- Widget_NameFilters(Widget_NameBaseline(lConfig, lSettings, dfResults)$config, lSettings, dfResults, dfParticipants)$config
  lState <- switch(lRead$chart,
    "group-comparison" = GroupComparison_State(dfResults, dfParticipants, lConfig),
    "association-scatter" = AssociationScatter_State(dfResults, dfParticipants, lConfig),
    "correlation-matrix" = CorrelationMatrix_State(dfResults, dfParticipants, lConfig),
    "biomarker-screen" = BiomarkerScreen_State(dfResults, dfParticipants, lConfig, dfOutcomes),
    "cross-tab" = CrossTab_State(dfResults, dfParticipants, lConfig),
    "stratified-survival" = StratifiedSurvival_State(dfResults, dfParticipants, dfOutcomes, lConfig)
  )
  list(config = lConfig, state = lState)
}

# How many participants a view's filters keep, as its chart opens them.
Batch_Passing <- function(lOpened, dfResults, dfParticipants) {
  as.integer(Chart_Passing(dfResults, dfParticipants, lOpened$config, lOpened$state$filters))
}

# The settings each chart's notices compare (bio.viz, each chart's
# `viewSettings`) that the tables can leave undrawn, by the name a notice gives
# each (bio.viz's SETTING_NAMES), and the field of the chart's state in R that
# holds what is drawn.
lBatchViewed <- list(
  "group-comparison" = c(start_value = "measure", visits = "visits", group_by = "group_by", levels = "levels", color_by = "color_by", panel_by = "panel_by"),
  "association-scatter" = c(x = "x", y = "y", color_by = "color_by", panel_by = "panel_by"),
  "correlation-matrix" = c(visit = "visit", biomarkers = "biomarkers", measure = "measure", visits = "visits"),
  "biomarker-screen" = c(endpoint = "endpoint", visit = "visit", group_by = "group_by", levels = "levels", with = "with"),
  "cross-tab" = c(row_by = "row_by", col_by = "col_by"),
  "stratified-survival" = c(endpoint = "endpoint", group_by = "group_by")
)
chrBatchSettingNames <- c(
  row_by = "Rows", col_by = "Columns", group_by = "Groups", color_by = "Colour", panel_by = "Panels", start_value = "Biomarker",
  measure = "Biomarker", biomarkers = "Biomarkers", visit = "Visit", visits = "Visits", endpoint = "Endpoint", comparison = "Compare",
  x = "X axis", y = "Y axis", with = "With"
)

# What the chart draws that the specification asked otherwise, as bio.viz's
# chart notices say it (bio.viz, src/shared/chartHost.js, `noticesOf`): a
# setting that names what the tables do not have, so the chart draws another,
# or none; and a filter on a column that is not a filter, or on a value its
# column does not have. A view with none draws as asked. Returns the sentence,
# or NA.
Batch_Notices <- function(lRead, lOpened, dfParticipants) {
  lConfig <- lOpened$config
  lState <- lOpened$state
  # A value as a notice says it: text as it is, a list of texts joined, and
  # anything else as JSON.
  Said <- function(xValue) {
    if (is.character(xValue) && length(xValue) == 1L) {
      return(xValue)
    }
    if ((is.character(xValue) || is.list(xValue)) && is.null(names(xValue)) && all(vapply(xValue, function(x) is.character(x) && length(x) == 1L, logical(1)))) {
      return(paste(unlist(xValue), collapse = ", "))
    }
    as.character(jsonlite::toJSON(xValue, auto_unbox = TRUE, null = "null", digits = NA))
  }
  # A setting compared by what it holds: its values as text, by name.
  Held <- function(xValue) {
    xFlat <- unlist(xValue)
    if (is.null(xFlat)) {
      return(character(0))
    }
    chrHeld <- Core_Text(xFlat)
    if (!is.null(names(xFlat))) chrHeld <- chrHeld[order(names(xFlat))]
    stats::setNames(chrHeld, sort(names(xFlat)))
  }
  chrNotices <- character(0)
  chrViewed <- lBatchViewed[[lRead$chart]]
  for (strKey in names(chrViewed)) {
    xAsked <- lConfig[[strKey]]
    if (!strKey %in% names(lRead$settings) || is.null(xAsked)) next
    xDrawn <- lState[[chrViewed[[strKey]]]]
    if (identical(Held(xAsked), Held(xDrawn)) && is.null(xDrawn) == is.null(xAsked)) next
    strName <- if (strKey %in% names(chrBatchSettingNames)) chrBatchSettingNames[[strKey]] else paste0("`", strKey, "`")
    chrNotices <- c(chrNotices, paste0(
      strName, ": ", Said(xAsked), " is not in the tables, so the chart draws ", if (is.null(xDrawn)) "none" else Said(xDrawn), "."
    ))
  }
  strIdCol <- if (is.null(lConfig$participant_id_col)) lConfig$id_col else lConfig$participant_id_col
  for (lFilter in lRead$filters) {
    strColumn <- lFilter$column
    if (!strColumn %in% names(lState$filters)) {
      chrNotices <- c(chrNotices, if (identical(strColumn, strIdCol)) {
        paste0("Filter ", strColumn, ": the participant id is not a filter.")
      } else {
        paste0("Filter ", strColumn, ": the participant table has no such column, so it is not a filter.")
      })
      next
    }
    xNow <- lState$filters[[strColumn]]
    chrDrawn <- if (is.null(xNow)) NULL else Core_Text(xNow)
    if (!is.null(chrDrawn) && identical(unname(chrDrawn), unname(lFilter$values))) next
    chrMissing <- lFilter$values[!lFilter$values %in% chrDrawn]
    chrNotices <- c(chrNotices, paste0(
      "Filter ", strColumn, ": ", paste(chrMissing, collapse = ", "), if (length(chrMissing) == 1L) " is not one of its values" else " are not among its values",
      ", so it is at ", if (is.null(chrDrawn)) "All" else paste(chrDrawn, collapse = ", "), "."
    ))
  }
  if (length(chrNotices) == 0L) NA_character_ else paste("Not drawn as the specification asks:", paste(chrNotices, collapse = " "))
}

# The name an output's files share: the specification's place, the chart,
# and the biomarker's name as a file's name can hold it, or its place among the
# views when nothing of the name can be kept; numbered -2, -3 and on when an
# output before it in the run has the name already.
Batch_Stem <- function(iSpec, strChart, strBiomarker, iView, chrTaken) {
  strStem <- sprintf("%02d-%s", iSpec, strChart)
  if (!is.na(strBiomarker)) {
    strSlug <- Batch_Slug(strBiomarker)
    strStem <- paste0(strStem, "-", if (nzchar(strSlug)) strSlug else as.character(iView))
  }
  strUnique <- strStem
  iCopy <- 1L
  while (strUnique %in% chrTaken) {
    iCopy <- iCopy + 1L
    strUnique <- paste0(strStem, "-", iCopy)
  }
  strUnique
}

# A biomarker's name as part of a file's name.
Batch_Slug <- function(strText) {
  strSlug <- gsub("[^a-z0-9]+", "-", tolower(iconv(strText, "UTF-8", "ASCII//TRANSLIT", sub = "")))
  gsub("^-+|-+$", "", strSlug)
}

# One figure file written in one format. Returns TRUE when a PDF drawn without
# cairo has a character beyond Latin-1, which it draws as a dot or a stand-in.
Batch_Save <- function(strFile, gg, strFormat, nWidth, nHeight) {
  if (strFormat == "pdf" && Batch_HasCairo()) {
    # A PDF by cairo, which draws every character a figure holds: the sign of
    # a cut, an apostrophe.
    ggplot2::ggsave(strFile, gg, width = nWidth, height = nHeight, device = grDevices::cairo_pdf)
    return(FALSE)
  }
  if (strFormat == "pdf") {
    # Without cairo the pdf device draws only Latin-1, and a character beyond
    # it as a dot (macOS) or a stand-in such as <= (Linux), and warns from
    # mbcsToSbcs either way: the figure is written, and its row says so.
    bDots <- FALSE
    withCallingHandlers(
      ggplot2::ggsave(strFile, gg, width = nWidth, height = nHeight, device = "pdf"),
      warning = function(cndWarning) {
        if (grepl("'mbcsToSbcs'", conditionMessage(cndWarning), fixed = TRUE)) {
          bDots <<- TRUE
          invokeRestart("muffleWarning")
        }
      }
    )
    return(bDots)
  }
  ggplot2::ggsave(strFile, gg, width = nWidth, height = nHeight, device = strFormat, dpi = 150)
  FALSE
}

# One view drawn: its figure in each format and its table to RTF. A table that
# cannot be made (the chart tests nothing at the settings) leaves the figure
# written and says why. A view that fails part way leaves none of its files:
# the ones it wrote are taken away again, and the error is the view's.
Batch_Draw <- function(lRead, dfResults, dfParticipants, dfOutcomes, strFolder, strStem, chrFormats, bTables, nWidth, nHeight) {
  chrWritten <- character(0)
  tryCatch(
    Batch_DrawFiles(lRead, dfResults, dfParticipants, dfOutcomes, strFolder, strStem, chrFormats, bTables, nWidth, nHeight, function(strFile) {
      chrWritten <<- c(chrWritten, strFile)
    }),
    error = function(cndError) {
      chrLeft <- chrWritten[file.exists(chrWritten)]
      if (length(chrLeft) > 0L) unlink(chrLeft)
      stop(conditionMessage(cndError), call. = FALSE)
    }
  )
}

Batch_DrawFiles <- function(lRead, dfResults, dfParticipants, dfOutcomes, strFolder, strStem, chrFormats, bTables, nWidth, nHeight, fnWriting) {
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
  bDots <- FALSE
  for (iFormat in seq_along(chrFormats)) {
    strFile <- file.path(strFolder, chrFigures[iFormat])
    fnWriting(strFile)
    bDots <- Batch_Save(strFile, gg, chrFormats[iFormat], nWidth, nHeight) || bDots
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
      fnWriting(file.path(strFolder, strTable))
      Write_RTF(dfTable, file.path(strFolder, strTable))
    }
  }
  if (bDots) {
    # The outputs that have every character: the other figures asked for, and
    # the table.
    chrWhole <- c(toupper(setdiff(chrFormats, "pdf")), if (!is.null(strTable)) "RTF table")
    chrNotes <- c(chrNotes, paste0(
      "The PDF was drawn without cairo, which this R cannot load, so a character beyond Latin-1 is not drawn as itself in it ",
      "(it is a dot, or a stand-in such as <= for the sign of a cut)",
      if (length(chrWhole) > 0L) paste0("; the ", paste(chrWhole, collapse = " and the "), if (length(chrWhole) == 1L) " has" else " have", " every character"), "."
    ))
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
