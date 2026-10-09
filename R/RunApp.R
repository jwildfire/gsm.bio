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
# synthetic study for a results table of the caller's own. `source` is what
# they are in a sentence, and `name` in the few words of the study chip (#84).
App_Study <- function(dfResults, dfParticipants, dfOutcomes) {
  lColumns <- App_Columns()
  if (is.null(dfResults)) {
    if (!is.null(dfParticipants) || !is.null(dfOutcomes)) {
      App_Stop("`dfResults` is needed with `dfParticipants` or `dfOutcomes`: the charts are drawn from the results table.")
    }
    return(list(
      results = gsm.bio::Synthetic_Results, participants = gsm.bio::Synthetic_Participants,
      outcomes = gsm.bio::Synthetic_Outcomes, source = "the synthetic study that ships with gsm.bio",
      name = "Synthetic study"
    ))
  }
  list(
    results = App_CheckTable(dfResults, "dfResults", "results", lColumns$results),
    participants = if (!is.null(dfParticipants)) App_CheckTable(dfParticipants, "dfParticipants", "participants", lColumns$participants),
    outcomes = if (!is.null(dfOutcomes)) App_CheckTable(dfOutcomes, "dfOutcomes", "outcomes", lColumns$outcomes),
    source = "the tables this app was started with", name = "This app's tables"
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

# The names of the Data view's inputs and outputs: a file and its card, the
# place its columns are asked for, one select per column with its tag, and the
# control that takes the file away.
App_Id <- function(strWhat, strTable, strColumn = NULL) {
  paste(c("gsm_bio", strWhat, strTable, strColumn), collapse = "_")
}

# The Data page (#85): a rail that counts what is left to do, and beside it
# the tables the charts are drawn on, a card for each table a reader may
# choose a file for, and the button. It is written for the tables the app
# opens on, and the session writes the rail again as a reader goes.
App_DataView <- function(lStudy = App_Study(NULL, NULL, NULL)) {
  lTables <- App_Tables()
  lCards <- lapply(names(lTables), function(strTable) {
    lTable <- lTables[[strTable]]
    shiny::tags$section(
      class = "gsm-bio-app-card gsm-bio-app-file", id = App_Id("card", strTable),
      shiny::tags$div(
        class = "gsm-bio-app-file-head",
        shiny::tags$h3(lTable$label),
        if (lTable$needed) App_TagOf("need", "needed") else App_TagOf("optional", "optional"),
        shiny::tags$span(class = "gsm-bio-app-file-what", paste0(lTable$what, "; a ", App_TypesSaid(), " file"))
      ),
      # Shiny's own file control, drawn as a place to choose a file.
      shiny::fileInput(
        App_Id("file", strTable),
        label = NULL, accept = chrAppFileTypes, width = "100%",
        buttonLabel = sprintf("Choose the %s file", tolower(lTable$label)), placeholder = "No file chosen"
      ),
      # The chosen file: the session writes it, with the columns it asks for.
      shiny::uiOutput(App_Id("columns", strTable))
    )
  })
  shiny::tagList(
    shiny::tags$aside(
      class = "gsm-bio-app-rail", `aria-label` = "What is left to do",
      shiny::tags$p(class = "gsm-bio-app-kicker", "Workflow"),
      shiny::tags$div(id = "gsm_bio_rail", class = "shiny-html-output", App_Rail(App_Steps(lStudy, list(), list(), FALSE))),
      shiny::tags$p(
        class = "gsm-bio-app-step-note gsm-bio-app-session",
        "A file you choose is read by R on this server and held in this session's memory only. Nothing is kept when the session ends."
      )
    ),
    shiny::tags$div(
      class = "gsm-bio-app-cards",
      # What is loaded: the tables the charts are drawn on, ten rows at a time.
      shiny::tags$section(
        class = "gsm-bio-app-card gsm-bio-app-viewer",
        shiny::tags$h2("The tables the charts are drawn on"),
        # A tab for each table there is: the session writes them, because a
        # reader can load other tables.
        shiny::uiOutput("gsm_bio_view_tabs"),
        shiny::uiOutput("gsm_bio_view"),
        shiny::tags$div(
          class = "gsm-bio-app-turn",
          shiny::actionButton("gsm_bio_view_previous", "Previous rows"),
          shiny::actionButton("gsm_bio_view_next", "Next rows")
        )
      ),
      shiny::tags$h2(class = "gsm-bio-app-unseen", "A study of your own"),
      lCards,
      shiny::tags$div(
        class = "gsm-bio-app-draw",
        shiny::tags$div(
          class = "gsm-bio-app-draw-button",
          shiny::actionButton("gsm_bio_apply", "Draw the charts on these files", class = "btn-primary"),
          # The files the button would draw, by name.
          shiny::tags$div(
            id = "gsm_bio_data_files", class = "shiny-html-output",
            shiny::tags$p(class = "gsm-bio-app-what", App_Will(list()))
          )
        ),
        # What R said of the last press: why nothing was drawn, or that the
        # charts are drawn and which are ready.
        shiny::uiOutput("gsm_bio_data_said", `aria-live` = "polite")
      )
    )
  )
}

# The page: the header band with the pills and the study chip, the page of
# Data or of one chart under it, and the footer line (R/app-shell.R, #84). It
# is written for the tables the app opens on, and the session writes the chip
# and the pills again when a reader draws the charts on others.
App_Ui <- function(lStudy = App_Study(NULL, NULL, NULL)) {
  Chart <- function(strChart) {
    shiny::tags$div(
      class = "gsm-bio-app-card",
      # A chart that lacks a table it needs has a sentence in its place: the
      # session decides which, because a reader can load other tables.
      if (identical(strChart, "StratifiedSurvival")) {
        shiny::uiOutput("gsm_bio_place_StratifiedSurvival")
      } else {
        Widget_Output(paste0("Widget_", strChart), strChart, "100%", "auto")
      }
    )
  }
  lPages <- c(
    list(Data = shiny::tags$div(class = "gsm-bio-app-data", App_DataView(lStudy))),
    stats::setNames(lapply(names(chrAppCharts), Chart), names(chrAppCharts))
  )
  lTabs <- App_Tabs(lStudy, lPages)
  shiny::bootstrapPage(
    # The page is titled as the app is named, and says what language it is in.
    title = "Biomarker charts", lang = "en",
    shiny::tags$head(
      App_Fonts(),
      shiny::tags$style(shiny::HTML(strAppStyle))
    ),
    shiny::tags$div(
      class = "gsm-bio-app",
      App_Head(lStudy, lTabs$pills),
      shiny::tags$main(class = "gsm-bio-app-main", lTabs$pages),
      App_Foot()
    ),
    shiny::tags$script(shiny::HTML(strAppScript))
  )
}

# The Data view's side of the session: the files as they are read, the columns
# asked for under each, the control that takes a file away, the rail's count
# of what is left, and the button that draws the charts on the files.
App_DataServer <- function(input, output, session, rStudy) {
  lTables <- App_Tables()
  chrTables <- stats::setNames(names(lTables), names(lTables))
  # Each file a reader has chosen: its name with its table, or with the
  # sentence saying why it could not be read.
  rFiles <- shiny::reactiveValues()
  # What R said of the last press of the button, and whether the charts are
  # drawn on the files as they are now.
  rSaid <- shiny::reactiveVal(NULL)
  rDrawn <- shiny::reactiveVal(FALSE)
  # The files held, in the tables' order.
  rHeld <- shiny::reactive({
    Filter(Negate(is.null), lapply(chrTables, function(strTable) rFiles[[strTable]]))
  })
  # The reader's column for one the charts need: what its select says, and
  # before the page has said, the column of that name if the file has one.
  Chosen <- function(strTable, strColumn, chrColumns) {
    strChosen <- input[[App_Id("column", strTable, strColumn)]]
    if (length(strChosen) != 1L) {
      return(App_Guess(chrColumns, strColumn))
    }
    if (strChosen %in% chrColumns) strChosen else ""
  }
  rChosen <- shiny::reactive({
    lapply(chrTables, function(strTable) {
      lFile <- rFiles[[strTable]]
      if (is.null(lFile$table)) {
        return(NULL)
      }
      vapply(names(lTables[[strTable]]$columns), function(strColumn) Chosen(strTable, strColumn, names(lFile$table)), character(1))
    })
  })
  # The files are other files: what R said of the last press was of the ones
  # there were, and the charts are not drawn on these.
  Changed <- function() {
    rSaid(NULL)
    rDrawn(FALSE)
  }
  # A column said after the button was pressed: R's refusal was of what was
  # said then, and the charts, if they were drawn, are not drawn on this.
  lSaidBefore <- NULL
  shiny::observeEvent(rChosen(), {
    lSaidNow <- rChosen()
    if (!is.null(lSaidBefore) && !identical(lSaidBefore, lSaidNow)) {
      if (isTRUE(shiny::isolate(rSaid())$problem)) {
        rSaid(NULL)
      }
      rDrawn(FALSE)
    }
    lSaidBefore <<- lSaidNow
  })
  for (strEach in names(lTables)) {
    local({
      strTable <- strEach
      lTable <- lTables[[strTable]]
      # A file input's value is the page's to set as well as Shiny's (#97):
      # App_Take() reads it only when it is an upload, and returns whatever
      # it was sent.
      shiny::observeEvent(input[[App_Id("file", strTable)]], {
        rFiles[[strTable]] <- App_Take(input[[App_Id("file", strTable)]])
        Changed()
      })
      # A file is taken away with the control in its card (#85). It is not a
      # control of a chart: it is the Data page's own, as the button is.
      shiny::observeEvent(input[[App_Id("remove", strTable)]], {
        rFiles[[strTable]] <- NULL
        Changed()
      })
      output[[App_Id("columns", strTable)]] <- shiny::renderUI({
        lFile <- rFiles[[strTable]]
        if (is.null(lFile)) {
          return(NULL)
        }
        xHead <- shiny::tags$div(
          class = "gsm-bio-app-chosen-head",
          shiny::tags$span(class = "gsm-bio-app-chosen-name", lFile$name),
          shiny::actionButton(
            App_Id("remove", strTable), "Remove",
            class = "gsm-bio-app-remove", `aria-label` = paste("Remove", lFile$name), `data-file` = App_Id("file", strTable)
          )
        )
        if (!is.null(lFile$problem)) {
          return(shiny::tagList(
            xHead,
            shiny::tags$p(class = "gsm-bio-app-problem", lFile$problem),
            shiny::tags$p(class = "gsm-bio-app-what", if (lTable$needed) {
              "Choose another results file, or remove this one."
            } else {
              sprintf(
                "Choose another file, or remove this one: the charts can be drawn without %s %s table.",
                if (identical(strTable, "outcomes")) "an" else "a", strTable
              )
            })
          ))
        }
        chrColumns <- names(lFile$table)
        shiny::tagList(
          xHead,
          shiny::tags$p(class = "gsm-bio-app-what", sprintf(
            "%s: %s rows, %s columns. Which column is which?",
            lFile$name, format(nrow(lFile$table), big.mark = ","), length(chrColumns)
          )),
          shiny::tags$div(
            class = "gsm-bio-app-columns",
            lapply(names(lTable$columns), function(strColumn) {
              strGuess <- App_Guess(chrColumns, strColumn)
              xSelect <- shiny::selectInput(
                App_Id("column", strTable, strColumn),
                label = lTable$columns[[strColumn]],
                choices = c("Not said yet" = "", chrColumns),
                selected = strGuess,
                selectize = FALSE
              )
              shiny::tags$div(
                class = "gsm-bio-app-ask", `data-column` = strColumn,
                # A select that must have a value: the page marks it while it
                # has none, by a rule of its own style.
                App_Change(xSelect, "select", function(xTag) shiny::tagAppendAttributes(xTag, required = NA)),
                # Its tag, as the card is written; the session writes it
                # again as the reader says.
                shiny::tags$span(id = App_Id("tag", strTable, strColumn), class = "shiny-html-output", App_Tag(strGuess, strColumn))
              )
            })
          ),
          # The file's first rows, under its own column names: what a reader
          # looks at to say which column is which.
          shiny::tags$p(class = "gsm-bio-app-what", sprintf(
            "The first %s of %s, as R read %s:",
            if (nrow(lFile$table) == 1L) "row" else paste(min(nAppPreviewRows, nrow(lFile$table)), "rows"),
            lFile$name, if (nrow(lFile$table) == 1L) "it" else "them"
          )),
          App_RowsTable(utils::head(lFile$table, nAppPreviewRows))
        )
      })
      for (strOne in names(lTable$columns)) {
        local({
          strColumn <- strOne
          output[[App_Id("tag", strTable, strColumn)]] <- shiny::renderUI({
            lFile <- rFiles[[strTable]]
            shiny::req(lFile$table)
            App_Tag(Chosen(strTable, strColumn, names(lFile$table)), strColumn)
          })
        })
      }
    })
  }
  output$gsm_bio_rail <- shiny::renderUI({
    App_Rail(App_Steps(rStudy(), rHeld(), rChosen(), rDrawn()))
  })
  output$gsm_bio_data_files <- shiny::renderUI({
    shiny::tags$p(class = "gsm-bio-app-what", App_Will(rHeld()))
  })
  shiny::observeEvent(input$gsm_bio_apply, {
    lNew <- tryCatch(
      {
        lHeld <- rHeld()
        if (is.null(lHeld$results)) {
          App_Stop("Choose a results file first: the charts are drawn from the results table.")
        }
        lSaid <- rChosen()
        lMapped <- list()
        for (strTable in names(lHeld)) {
          lFile <- lHeld[[strTable]]
          if (!is.null(lFile$problem)) {
            App_Stop(lFile$problem)
          }
          lMapped[[strTable]] <- App_MapTable(lFile$table, lSaid[[strTable]], lTables[[strTable]], lFile$name)
        }
        # The files by name, each under its table: what the study chip (#84)
        # and the rail call a reader's study.
        chrFiles <- vapply(lHeld, function(lFile) lFile$name, character(1))
        list(
          results = lMapped$results, participants = lMapped$participants, outcomes = lMapped$outcomes,
          source = paste0(paste(chrFiles, collapse = ", "), ", loaded in this session"),
          files = chrFiles
        )
      },
      error = function(cndError) conditionMessage(cndError)
    )
    if (is.character(lNew)) {
      # The tables already drawn stay as they are.
      rSaid(list(problem = TRUE, text = lNew))
    } else {
      rStudy(lNew)
      rSaid(list(problem = FALSE, study = lNew))
      rDrawn(TRUE)
    }
  })
  output$gsm_bio_data_said <- shiny::renderUI({
    lSaid <- rSaid()
    if (is.null(lSaid)) {
      return(NULL)
    }
    if (lSaid$problem) shiny::tags$p(class = "gsm-bio-app-problem", lSaid$text) else App_Ready(lSaid$study)
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
    App_ViewServer(input, output, session, rStudy)
    Of <- function(strChart) {
      lGiven <- lSettings[[strChart]]
      if (is.null(lGiven)) list() else lGiven
    }
    App_ShellServer(output, rStudy)
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
#' A Shiny app of the six bio.viz charts, one drawn at a time and chosen from
#' a row of pills in the page's header, on the tables it is given or, given
#' none, on the synthetic study that ships with the package. The R session
#' behind the page answers every statistic a chart asks for
#' ([Serve_Statistics()]), so a reader who changes a test, a group or a filter
#' gets R's result for that view, and the line under the chart says it was
#' computed on this server and by which R.
#'
#' The charts are the package's widgets with the controls they have. Shiny
#' holds the tables and answers the statistics, and does nothing else: no
#' control of a chart is made again as a Shiny input. The Data page's own
#' controls are Shiny inputs: the three files, a select for each column of a
#' chosen file, the control that takes a file away, and the button.
#'
#' @section The page:
#' A header band carries the app's name, "Biomarker charts", with the mark
#' gsm.bio and its version beside it; a row of pills, Data first and then the
#' six charts; and a chip that says what the charts are drawn on, on every
#' page, and opens Data when it is pressed. The chart has the page's width
#' under the band, and one footer line says which R computes the statistics
#' and that a reader's files are held for the session only.
#'
#' - A pill is a link of Shiny's own tab set. A keyboard reaches the chosen
#'   pill with the Tab key and walks the row with the arrow keys, and a pill
#'   pointed at says in one line what its chart draws.
#' - A chart that cannot be drawn on the tables there are has its pill dimmed,
#'   with the reason as its hover text. The pill still opens the chart's page,
#'   which holds the same sentence.
#' - On a phone the header is two rows and the pills scroll sideways in their
#'   own row, with the chosen one brought into view.
#' - The page asks Google Fonts for the two fonts bio.viz's and safety.viz's
#'   sites use, Instrument Sans and Instrument Serif, and for nothing else
#'   outside its own server. It does not wait for them: where they cannot be
#'   reached, as behind a firewall, the page is drawn in the system's fonts.
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
#'   so, and its pill is dimmed.
#'
#' A table that lacks a column is refused with a sentence naming the column.
#' Called with no table, the app opens on [Synthetic_Results],
#' [Synthetic_Participants] and [Synthetic_Outcomes].
#'
#' @section A reader's own files:
#' The first pill opens the Data page. It has a card for each table: results,
#' which the charts need, and participants and outcomes, which are optional.
#' A reader chooses a file in a card, each a `.csv`, `.xpt` or `.sas7bdat`
#' file, and R reads it on the server. The card then asks which of the file's
#' columns is each one the charts need. A column that has gsm.bio's own name
#' is filled in already and tagged "same name"; one the reader has still to
#' say is amber and tagged "say which". On the button the columns are renamed
#' to gsm.bio's names and the charts are drawn on the reader's tables, and the
#' page lists the charts that are ready, each with what it draws. Each opens
#' from that list, and a chart that lacks a table says which.
#'
#' A rail beside the cards, above them on a phone, counts what is left in
#' three steps: the files chosen and any R could not read, the columns still
#' to say, and the charts that are ready. Under the steps it says what the
#' charts are drawn on now.
#'
#' Nothing is drawn on a table until every column is said: a column left
#' unsaid, a column chosen twice, a result that is text and a file R cannot
#' read are each answered with a sentence beside the button, and the tables
#' already drawn stay. A file R cannot read is also reported in the card it
#' was chosen in. A column of the file that already had one of gsm.bio's
#' names, and was not the one chosen for it, is kept with `_original` added to
#' its name.
#'
#' A file is taken away with the Remove control in its card. Until it is, it
#' is one of the files the button draws: the page names them all beside the
#' button, so a file chosen for an earlier study is seen before it is drawn
#' with a later one. An optional file R could not read holds the button until
#' it is removed or another is chosen in its place.
#'
#' The Data page also shows what is loaded: the tables the charts are drawn
#' on, ten rows at a time, with where each came from and its rows and columns.
#' A file just chosen shows its first rows under its own column names, so a
#' reader can tell which column is which. Values are shown as R holds them. A
#' number is written in full to the fifteen digits that identify it, never as
#' `1e+05`, unless it is a thousand million million or more, or smaller than
#' a part in that many; those are left in R's scientific form.
#'
#' A file is held in the session's memory and nowhere else. Nothing is written
#' to the server beyond Shiny's own temporary copy of an upload, which goes
#' when the session ends, and nothing is kept between sessions. A `.xpt` or
#' `.sas7bdat` file is read with haven, which is suggested, not imported:
#' without it the page says so and reads `.csv` files only.
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
    ui = App_Ui(lStudy),
    server = App_Server(lStudy, lSettings),
    # The limit is the app's own, set when it starts and for as long as it runs.
    onStart = function() {
      lWas <- options(shiny.maxRequestSize = nMaxUploadMB * 1024^2)
      shiny::onStop(function() options(lWas))
    }
  )
}
