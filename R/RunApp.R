# The app: the six charts in one Shiny page, on the tables it is given (#72).
#
# Shiny does two jobs here and no more: it holds the tables, and its session
# answers the charts' statistics (R/serve.R). The charts are the six widgets,
# with the controls they have; no control of a chart is made again as a Shiny
# input.

# The charts the app lists, in order: the name a chart's settings are given
# under, which is its widget's name less `Widget_`, and what the list calls it.
chrAppCharts <- c(
  GroupComparison = "Group comparison",
  AssociationScatter = "Association scatter",
  CorrelationMatrix = "Correlation matrix",
  BiomarkerScreen = "Biomarker screen",
  CrossTab = "Cross-tabulation",
  StratifiedSurvival = "Stratified survival"
)

App_Stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

# The columns the charts read under gsm.bio's default names, by table.
App_Columns <- function() {
  list(
    results = unlist(lCoreDefaults[c("id_col", "measure_col", "value_col", "visit_col", "visit_order_col")], use.names = FALSE),
    participants = lCoreDefaults$id_col,
    outcomes = c(
      lCoreDefaults$id_col,
      unlist(lOutcomeDefaults[c("endpoint_col", "endpoint_label_col", "time_col", "censor_col")], use.names = FALSE)
    )
  )
}

# One table as the app takes it: a data frame with the columns the charts read,
# or a sentence saying what it lacks.
App_CheckTable <- function(dfTable, strArg, strWhat, chrColumns) {
  if (!is.data.frame(dfTable)) {
    App_Stop("`", strArg, "` must be a data frame: the ", strWhat, " table.")
  }
  chrMissing <- setdiff(chrColumns, names(dfTable))
  if (length(chrMissing) > 0L) {
    App_Stop(
      "`", strArg, "` has no column named ", paste0("`", chrMissing, "`", collapse = ", "),
      ". The app reads the ", strWhat, " table under gsm.bio's column names (",
      paste(chrColumns, collapse = ", "), "): rename the columns before calling RunApp()."
    )
  }
  dfTable
}

# The tables the app opens on: the ones given, or the synthetic study when no
# results table is. Participants and outcomes are never taken from the
# synthetic study for a results table of the caller's own.
App_Study <- function(dfResults, dfParticipants, dfOutcomes) {
  lColumns <- App_Columns()
  if (is.null(dfResults)) {
    if (!is.null(dfParticipants) || !is.null(dfOutcomes)) {
      App_Stop("`dfResults` is needed with `dfParticipants` or `dfOutcomes`: the charts are drawn from the results table.")
    }
    return(list(
      results = gsm.bio::Synthetic_Results, participants = gsm.bio::Synthetic_Participants,
      outcomes = gsm.bio::Synthetic_Outcomes, source = "the synthetic study that ships with gsm.bio"
    ))
  }
  list(
    results = App_CheckTable(dfResults, "dfResults", "results", lColumns$results),
    participants = if (!is.null(dfParticipants)) App_CheckTable(dfParticipants, "dfParticipants", "participants", lColumns$participants),
    outcomes = if (!is.null(dfOutcomes)) App_CheckTable(dfOutcomes, "dfOutcomes", "outcomes", lColumns$outcomes),
    source = "the tables this app was started with"
  )
}

# The settings of each chart, by the chart's name.
App_Settings <- function(lSettings) {
  if (!is.list(lSettings) || is.data.frame(lSettings) || (length(lSettings) > 0L && is.null(names(lSettings)))) {
    App_Stop("`lSettings` must be a named list: a chart's settings under its name, one of ", paste(names(chrAppCharts), collapse = ", "), ".")
  }
  chrUnknown <- setdiff(names(lSettings), names(chrAppCharts))
  if (length(chrUnknown) > 0L) {
    App_Stop(
      "`lSettings` names ", paste0("`", chrUnknown, "`", collapse = ", "), ", which is no chart of the app. The charts are ",
      paste(names(chrAppCharts), collapse = ", "), "."
    )
  }
  for (strChart in names(lSettings)) {
    if (!is.list(lSettings[[strChart]])) {
      App_Stop("`lSettings$", strChart, "` must be a list of that chart's settings, under bio.viz's setting names.")
    }
  }
  lSettings
}

App_MaxUpload <- function(nMaxUploadMB) {
  if (!is.numeric(nMaxUploadMB) || length(nMaxUploadMB) != 1L || is.na(nMaxUploadMB) || nMaxUploadMB <= 0) {
    App_Stop("`nMaxUploadMB` must be one number above zero: the largest file the app accepts, in megabytes.")
  }
  nMaxUploadMB
}

# Why a chart cannot be drawn on these tables, as a sentence, or NULL.
App_Lacks <- function(strChart, lStudy) {
  if (identical(strChart, "StratifiedSurvival") && is.null(lStudy$outcomes)) {
    return("The stratified survival chart reads an outcomes table, with a time and a censor flag for each participant, and this app has none.")
  }
  NULL
}

# What the app looks like beyond its charts: a few rules, in the page.
strAppStyle <- "
.gsm-bio-app { max-width: 1400px; margin: 0 auto; padding: 0 16px 24px; }
.gsm-bio-app h1 { font-size: 1.4rem; margin: 16px 0 4px; }
.gsm-bio-app h2 { font-size: 1.1rem; margin: 20px 0 4px; }
.gsm-bio-app .gsm-bio-app-source, .gsm-bio-app .gsm-bio-app-foot, .gsm-bio-app .gsm-bio-app-what { color: #52616f; font-size: .85rem; }
.gsm-bio-app .gsm-bio-app-foot { margin-top: 24px; }
.gsm-bio-app .gsm-bio-app-lacks { margin: 24px 0; }
.gsm-bio-app .gsm-bio-app-problem { color: #a4262c; }
.gsm-bio-app .gsm-bio-app-columns { display: flex; flex-wrap: wrap; gap: 0 16px; }
.gsm-bio-app .gsm-bio-app-columns .form-group { min-width: 180px; }
"

# The names of the Data view's inputs and outputs: a file, the place its
# columns are asked for, and one select per column.
App_Id <- function(strWhat, strTable, strColumn = NULL) {
  paste(c("gsm_bio", strWhat, strTable, strColumn), collapse = "_")
}

App_DataView <- function() {
  lTables <- App_Tables()
  lParts <- lapply(names(lTables), function(strTable) {
    lTable <- lTables[[strTable]]
    shiny::tagList(
      shiny::tags$h2(paste0(lTable$label, if (lTable$needed) "" else ", optional")),
      shiny::tags$p(class = "gsm-bio-app-what", paste0(toupper(substr(lTable$what, 1L, 1L)), substring(lTable$what, 2L), ".")),
      shiny::fileInput(App_Id("file", strTable), label = NULL, accept = chrAppFileTypes, width = "100%"),
      shiny::uiOutput(App_Id("columns", strTable))
    )
  })
  shiny::tagList(
    shiny::tags$p(
      "To draw the charts on a study of your own, choose its results table as a .csv, .xpt or .sas7bdat file, ",
      "say which column is which, and press the button. The file is read by R on this server and held in memory ",
      "for this session only."
    ),
    lParts,
    shiny::actionButton("gsm_bio_apply", "Draw the charts on these files", class = "btn-primary"),
    shiny::uiOutput("gsm_bio_data_said")
  )
}

App_Ui <- function() {
  Tab <- function(strChart) {
    shiny::tabPanel(
      chrAppCharts[[strChart]],
      value = strChart,
      # A chart that lacks a table it needs has a sentence in its place: the
      # session decides which, because a reader can load other tables.
      if (identical(strChart, "StratifiedSurvival")) {
        shiny::uiOutput("gsm_bio_place_StratifiedSurvival")
      } else {
        Widget_Output(paste0("Widget_", strChart), strChart, "100%", "auto")
      }
    )
  }
  lBy <- StoredResultsProvenance()
  shiny::fluidPage(
    title = "gsm.bio",
    shiny::tags$head(shiny::tags$style(shiny::HTML(strAppStyle))),
    shiny::tags$div(
      class = "gsm-bio-app",
      shiny::tags$h1("Biomarker charts"),
      shiny::tags$p(class = "gsm-bio-app-source", shiny::textOutput("gsm_bio_source", inline = TRUE)),
      # One chart is drawn at a time: Shiny draws an output when it is shown.
      shiny::navlistPanel(
        id = "gsm_bio_chart", well = FALSE, widths = c(2, 10), selected = names(chrAppCharts)[1],
        shiny::tabPanel("Data", value = "Data", App_DataView()),
        Tab("GroupComparison"), Tab("AssociationScatter"), Tab("CorrelationMatrix"),
        Tab("BiomarkerScreen"), Tab("CrossTab"), Tab("StratifiedSurvival")
      ),
      shiny::tags$p(
        class = "gsm-bio-app-foot",
        paste0(
          "gsm.bio ", lBy$gsm_bio_version, ". Every statistic is computed on request by R ", lBy$r_version,
          " on this server, by gsm.bio's own functions."
        )
      )
    )
  )
}

# The Data view's side of the session: the files as they are read, the columns
# asked for under each, and the button that draws the charts on them.
App_DataServer <- function(input, output, session, rStudy) {
  lTables <- App_Tables()
  # Each file a reader has chosen: its name with its table, or with the
  # sentence saying why it could not be read.
  rFiles <- shiny::reactiveValues()
  rSaid <- shiny::reactiveVal(NULL)
  for (strEach in names(lTables)) {
    local({
      strTable <- strEach
      lTable <- lTables[[strTable]]
      shiny::observeEvent(input[[App_Id("file", strTable)]], {
        lChosen <- input[[App_Id("file", strTable)]]
        strName <- lChosen$name[1]
        rFiles[[strTable]] <- tryCatch(
          list(name = strName, table = App_ReadFile(lChosen$datapath[1], strName)),
          error = function(cndError) list(name = strName, problem = conditionMessage(cndError))
        )
      })
      output[[App_Id("columns", strTable)]] <- shiny::renderUI({
        lFile <- rFiles[[strTable]]
        if (is.null(lFile)) {
          return(NULL)
        }
        if (!is.null(lFile$problem)) {
          return(shiny::tags$p(class = "gsm-bio-app-problem", lFile$problem))
        }
        chrColumns <- names(lFile$table)
        shiny::tagList(
          shiny::tags$p(class = "gsm-bio-app-what", sprintf(
            "%s: %s rows, %s columns. Which column is which?",
            lFile$name, format(nrow(lFile$table), big.mark = ","), length(chrColumns)
          )),
          shiny::tags$div(
            class = "gsm-bio-app-columns",
            lapply(names(lTable$columns), function(strColumn) {
              shiny::selectInput(
                App_Id("column", strTable, strColumn),
                label = lTable$columns[[strColumn]],
                choices = c("Not said yet" = "", chrColumns),
                selected = App_Guess(chrColumns, strColumn),
                selectize = FALSE
              )
            })
          )
        )
      })
    })
  }
  shiny::observeEvent(input$gsm_bio_apply, {
    lNew <- tryCatch(
      {
        if (is.null(rFiles$results)) {
          App_Stop("Choose a results file first: the charts are drawn from the results table.")
        }
        lMapped <- list()
        for (strTable in names(lTables)) {
          lFile <- rFiles[[strTable]]
          if (is.null(lFile)) next
          if (!is.null(lFile$problem)) {
            App_Stop(lFile$problem)
          }
          chrNeed <- names(lTables[[strTable]]$columns)
          chrChosen <- vapply(chrNeed, function(strColumn) {
            strChosen <- input[[App_Id("column", strTable, strColumn)]]
            if (is.null(strChosen)) "" else strChosen
          }, character(1))
          lMapped[[strTable]] <- App_MapTable(lFile$table, chrChosen, lTables[[strTable]], lFile$name)
        }
        chrFiles <- vapply(names(lMapped), function(strTable) rFiles[[strTable]]$name, character(1))
        list(
          results = lMapped$results, participants = lMapped$participants, outcomes = lMapped$outcomes,
          source = paste0(paste(chrFiles, collapse = ", "), ", loaded in this session")
        )
      },
      error = function(cndError) conditionMessage(cndError)
    )
    if (is.character(lNew)) {
      # The tables already drawn stay as they are.
      rSaid(list(problem = TRUE, text = lNew))
    } else {
      rStudy(lNew)
      rSaid(list(problem = FALSE, text = paste0("The charts are drawn on ", lNew$source, ". Choose a chart from the list.")))
    }
  })
  output$gsm_bio_data_said <- shiny::renderUI({
    lSaid <- rSaid()
    if (is.null(lSaid)) {
      return(NULL)
    }
    shiny::tags$p(class = if (lSaid$problem) "gsm-bio-app-problem" else "gsm-bio-app-what", lSaid$text)
  })
  invisible(NULL)
}

App_Server <- function(lStudy, lSettings) {
  function(input, output, session) {
    Serve_Statistics(session)
    # The tables the charts are drawn on: the ones the app was started with,
    # until a reader loads others in the Data view.
    rStudy <- shiny::reactiveVal(lStudy)
    App_DataServer(input, output, session, rStudy)
    Of <- function(strChart) {
      lGiven <- lSettings[[strChart]]
      if (is.null(lGiven)) list() else lGiven
    }
    output$gsm_bio_source <- shiny::renderText(paste0("Drawn on ", rStudy()$source, "."))
    output$GroupComparison <- renderWidget_GroupComparison(
      Widget_GroupComparison(rStudy()$results, rStudy()$participants, lSettings = Of("GroupComparison"))
    )
    output$AssociationScatter <- renderWidget_AssociationScatter(
      Widget_AssociationScatter(rStudy()$results, rStudy()$participants, lSettings = Of("AssociationScatter"))
    )
    output$CorrelationMatrix <- renderWidget_CorrelationMatrix(
      Widget_CorrelationMatrix(rStudy()$results, rStudy()$participants, lSettings = Of("CorrelationMatrix"))
    )
    output$BiomarkerScreen <- renderWidget_BiomarkerScreen(
      Widget_BiomarkerScreen(rStudy()$results, rStudy()$participants, lSettings = Of("BiomarkerScreen"), dfOutcomes = rStudy()$outcomes)
    )
    output$CrossTab <- renderWidget_CrossTab(
      Widget_CrossTab(rStudy()$results, rStudy()$participants, lSettings = Of("CrossTab"))
    )
    output$gsm_bio_place_StratifiedSurvival <- shiny::renderUI({
      strLacks <- App_Lacks("StratifiedSurvival", rStudy())
      if (is.null(strLacks)) {
        Widget_Output("Widget_StratifiedSurvival", "StratifiedSurvival", "100%", "auto")
      } else {
        shiny::tags$p(class = "gsm-bio-app-lacks", strLacks)
      }
    })
    output$StratifiedSurvival <- renderWidget_StratifiedSurvival({
      shiny::req(is.null(App_Lacks("StratifiedSurvival", rStudy())))
      Widget_StratifiedSurvival(rStudy()$results, rStudy()$participants, lSettings = Of("StratifiedSurvival"), dfOutcomes = rStudy()$outcomes)
    })
  }
}

#' Run the six charts as one Shiny app
#'
#' A Shiny app with a list of the six bio.viz charts beside one chart drawn at
#' a time, on the tables it is given or, given none, on the synthetic study
#' that ships with the package. The R session behind the page answers every
#' statistic a chart asks for ([Serve_Statistics()]), so a reader who changes a
#' test, a group or a filter gets R's result for that view, and the line under
#' the chart says it was computed on this server and by which R.
#'
#' The charts are the package's widgets with the controls they have. Shiny
#' holds the tables and answers the statistics, and does nothing else: no
#' control of a chart is made again as a Shiny input.
#'
#' @section The tables:
#' The app reads its tables under gsm.bio's column names, so a table with
#' other names is renamed before the call:
#'
#' - results, needed: `USUBJID`, `TEST`, `STRESN`, `VISIT` and `VISITNUM`, one
#'   row per participant, biomarker and visit;
#' - participants, optional: `USUBJID` and whatever columns describe a
#'   participant. With it a chart offers groups and filters; without it a
#'   chart has none;
#' - outcomes, optional: `USUBJID`, `PARAMCD`, `PARAM`, `AVAL` and `CNSR`.
#'   Without it the stratified survival chart is replaced by a sentence saying
#'   so.
#'
#' A table that lacks a column is refused with a sentence naming the column.
#' Called with no table, the app opens on [Synthetic_Results],
#' [Synthetic_Participants] and [Synthetic_Outcomes].
#'
#' @section A reader's own files:
#' The first entry of the list is the Data view. A reader chooses a results
#' file there, and optionally a participants and an outcomes file, each a
#' `.csv`, `.xpt` or `.sas7bdat` file. R reads it on the server. Under each
#' file the view asks which of its columns is each one the charts need, filled
#' in already where a column has gsm.bio's own name. On the button the columns
#' are renamed to gsm.bio's names and the charts are drawn on the reader's
#' tables.
#'
#' Nothing is drawn on a table until every column is said: a column left
#' unsaid, a column chosen twice, a result that is text and a file R cannot
#' read are each answered with a sentence, and the tables already drawn stay.
#' A column of the file that already had one of gsm.bio's names, and was not
#' the one chosen for it, is kept with `_original` added to its name.
#'
#' A file is held in the session's memory and nowhere else. Nothing is written
#' to the server beyond Shiny's own temporary copy of an upload, which goes
#' when the session ends, and nothing is kept between sessions. A `.xpt` or
#' `.sas7bdat` file is read with haven, which is suggested, not imported:
#' without it the view says so and reads `.csv` files only.
#'
#' @section On a server:
#' `RunApp()` returns the app and starts nothing itself, so the same call
#' serves an R session, where printing the app runs it, and the last line of
#' an `app.R` on a server such as Posit Connect:
#'
#' ```r
#' library(shiny)
#' library(gsm.bio)
#' dfResults <- readRDS("results.rds")
#' RunApp(dfResults)
#' ```
#'
#' The tables are held in the R session's memory. The rows a chart draws are
#' sent to the session with each request for a statistic, and the session runs
#' the nine `Analyze_*` functions and no other.
#'
#' shiny is suggested, not imported: without it `RunApp()` stops with a
#' sentence naming the package to install.
#'
#' @param dfResults `data.frame` The results table, or `NULL` for the synthetic
#'   study.
#' @param dfParticipants `data.frame` The participants table, or `NULL` for
#'   none.
#' @param dfOutcomes `data.frame` The outcomes table, or `NULL` for none.
#' @param lSettings `list` Settings for the charts, a list for each under its
#'   chart's name: `GroupComparison`, `AssociationScatter`, `CorrelationMatrix`,
#'   `BiomarkerScreen`, `CrossTab` or `StratifiedSurvival`. Each is what that
#'   chart's widget takes as `lSettings`, under bio.viz's setting names. A
#'   chart not named opens on its defaults.
#' @param nMaxUploadMB `numeric` The largest file the app accepts from a
#'   reader, in megabytes. Shiny's own limit is 5.
#'
#' @return A Shiny app object. Printing it runs the app.
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   # The synthetic study.
#'   RunApp()
#'
#'   # A study's own tables, with the group comparison opened on one biomarker
#'   # by arm.
#'   RunApp(
#'     Synthetic_Results, Synthetic_Participants,
#'     lSettings = list(GroupComparison = list(start_value = "CRP", group_by = "ARM"))
#'   )
#' }
#'
#' @seealso [Serve_Statistics()] and the output and render functions in
#'   [gsm.bio-shiny], which the app is made of.
#' @family shiny
#' @export
RunApp <- function(dfResults = NULL, dfParticipants = NULL, dfOutcomes = NULL, lSettings = list(), nMaxUploadMB = 100) {
  Serve_NeedShiny("RunApp")
  lStudy <- App_Study(dfResults, dfParticipants, dfOutcomes)
  lSettings <- App_Settings(lSettings)
  nMaxUploadMB <- App_MaxUpload(nMaxUploadMB)
  shiny::shinyApp(
    ui = App_Ui(),
    server = App_Server(lStudy, lSettings),
    # The limit is the app's own, set when it starts and for as long as it runs.
    onStart = function() {
      lWas <- options(shiny.maxRequestSize = nMaxUploadMB * 1024^2)
      shiny::onStop(function() options(lWas))
    }
  )
}
