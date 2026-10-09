# The widgets in a Shiny page, with their statistics answered by the R session
# behind it (#71).
#
# A saved page looks a statistic up among the results stored in it. A widget
# drawn in a Shiny page stores none: its chart asks the session, through the
# server form of bio.viz's connection to R. The widget's script
# (inst/htmlwidgets/shared/gsm.bio.widget.js) sends each request as text, the
# function's name with the rows and the arguments, and Serve_Statistics()
# answers it with the function's result, converted by StoredValue() as a
# stored result is, so the chart reads the one as it reads the other.
#
# The session runs the nine statistics functions and nothing else. The name a
# page sends is compared with the list below; any other is answered with a
# sentence and nothing is called. What a page sends is data: it is never
# parsed or evaluated as R.

# The functions a page may ask for: the package's statistics, each under its
# own name. A fixed list, written out: a name is looked up in it and nowhere
# else, so nothing a page sends is ever resolved as an R object.
Serve_Functions <- function() {
  list(
    Analyze_GroupDifference = Analyze_GroupDifference,
    Analyze_GroupDifferenceBy = Analyze_GroupDifferenceBy,
    Analyze_DifferenceGrid = Analyze_DifferenceGrid,
    Analyze_Correlation = Analyze_Correlation,
    Analyze_CorrelationMatrix = Analyze_CorrelationMatrix,
    Analyze_Fit = Analyze_Fit,
    Analyze_Contingency = Analyze_Contingency,
    Analyze_Survival = Analyze_Survival,
    Analyze_Screen = Analyze_Screen
  )
}

# The Shiny input the widget's script sends requests as, and the message the
# answers go back as. The script names the same two.
strServeInput <- "gsm_bio_request"
strServeMessage <- "gsm-bio-answer"

Serve_HasShiny <- function() {
  requireNamespace("shiny", quietly = TRUE)
}

Serve_NeedShiny <- function(strFunction) {
  if (!Serve_HasShiny()) {
    stop(
      strFunction, "() draws a widget in a Shiny page, and shiny is not installed. ",
      "Install it with install.packages(\"shiny\"); the widgets themselves do not need it.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Whether a widget is being made for a Shiny page that answers its statistics
#'
#' True while a `renderWidget_*()` function evaluates its widget. A widget made
#' then stores no results and tells its script to ask the session.
#'
#' @keywords internal
#' @noRd
Widget_IsServed <- function() {
  isTRUE(getOption("gsm.bio.served"))
}

#' Make a widget as one a Shiny session answers
#'
#' Evaluates the widget with the mark [Widget_IsServed()] reads set, and takes
#' the mark off again whatever happens.
#'
#' @param xWidget The call that makes the widget, not yet evaluated.
#'
#' @keywords internal
#' @noRd
Widget_Served <- function(xWidget) {
  lWas <- options(gsm.bio.served = TRUE)
  on.exit(options(lWas), add = TRUE)
  force(xWidget)
}

#' The answer to one request
#'
#' @param lRequest `list` One request as the page sent it: `id`, `name`,
#'   `columns` and `args`.
#'
#' @return A named list: `id`, `ok`, and `value` (the function's result, as
#'   [StoredValue()] makes it) or `message` (why there is none). `NULL` for a
#'   request with no usable `id`, which cannot be answered.
#'
#' @keywords internal
#' @noRd
Serve_Answer <- function(lRequest) {
  if (!is.list(lRequest) || is.null(names(lRequest))) {
    return(NULL)
  }
  xId <- lRequest$id
  if (!is.numeric(xId) || length(xId) != 1L || is.na(xId)) {
    return(NULL)
  }
  Refuse <- function(...) list(id = xId, ok = FALSE, message = paste0(...))
  # The page asks first whether this session answers statistics at all.
  if (isTRUE(lRequest$hello)) {
    return(list(id = xId, ok = TRUE, value = NULL))
  }
  lFunctions <- Serve_Functions()
  strName <- lRequest$name
  if (!is.character(strName) || length(strName) != 1L || is.na(strName) || !strName %in% names(lFunctions)) {
    return(Refuse(
      "This server runs gsm.bio's statistics functions and no other: ",
      paste(names(lFunctions), collapse = ", "), "."
    ))
  }
  lColumns <- lRequest$columns
  if (!is.null(lColumns) && (!is.list(lColumns) || (length(lColumns) > 0L && is.null(names(lColumns))))) {
    return(Refuse("The rows of a request are sent as named columns."))
  }
  lArgs <- lRequest$args
  if (!is.null(lArgs) && (!is.list(lArgs) || (length(lArgs) > 0L && is.null(names(lArgs))))) {
    return(Refuse("The arguments of a request are sent by name."))
  }
  tryCatch(
    {
      dfData <- if (length(lColumns) == 0L) {
        data.frame()
      } else {
        as.data.frame(lColumns, stringsAsFactors = FALSE, check.names = FALSE)
      }
      # Called as every stored result is, by Chart_Answer() from the fixed list.
      lAnswered <- Chart_Answer(list(list(name = strName, args = as.list(lArgs), dataId = NULL, data = dfData)), lFunctions)
      list(id = xId, ok = TRUE, value = StoredValue(lAnswered[[1]]$value))
    },
    error = function(cndError) Refuse(conditionMessage(cndError))
  )
}

#' The answers to the requests a page sent
#'
#' @param strRequests `character` The requests, as the widget's script wrote
#'   them: the JSON text of an array, each member `{ id, name, columns, args }`,
#'   with `columns` one array per column and `null` for a missing value.
#'
#' @return An unnamed list of answers, one per request that can be answered,
#'   in the order asked. Empty when the text is not such an array.
#'
#' @keywords internal
#' @noRd
Serve_Reply <- function(strRequests) {
  if (!is.character(strRequests) || length(strRequests) != 1L || is.na(strRequests)) {
    return(list())
  }
  # An array becomes a vector, with null as NA, and an object a named list: a
  # column arrives as R holds one, and an argument as the functions take it.
  lRequests <- tryCatch(
    jsonlite::fromJSON(strRequests, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
    error = function(cndError) NULL
  )
  if (!is.list(lRequests) || !is.null(names(lRequests))) {
    return(list())
  }
  Filter(Negate(is.null), lapply(lRequests, Serve_Answer))
}

#' Answer the widgets' statistics from a Shiny session
#'
#' Call it once in a Shiny server function. Every gsm.bio widget drawn in the
#' page with a `renderWidget_*()` function then asks this session for its
#' statistics: the chart sends the function's name, the rows it draws and the
#' arguments, and is sent back what the function returns. So every view a
#' reader reaches has its statistics, computed by this R, where a saved page
#' has only those stored when it was made.
#'
#' @section What the session will run:
#' The nine statistics functions and nothing else: [Analyze_GroupDifference()],
#' [Analyze_GroupDifferenceBy()], [Analyze_DifferenceGrid()],
#' [Analyze_Correlation()], [Analyze_CorrelationMatrix()], [Analyze_Fit()],
#' [Analyze_Contingency()], [Analyze_Survival()] and [Analyze_Screen()]. The
#' name a page sends is compared with that list. Any other name is answered
#' with a sentence saying so, and nothing is called. The rows and the arguments
#' are read as data and never as R code.
#'
#' @section What crosses to the server:
#' The rows a chart draws travel with each request, because the chart works
#' them out in the page: a change from baseline, the filters and the cuts are
#' applied there, and R tests those rows and no others. Nothing is kept
#' between requests.
#'
#' An answer is converted for the page exactly as a stored result is, so a
#' chart reads the two alike. The line under the chart says the result was
#' computed on this server, by which R and which gsm.bio.
#'
#' @param session The Shiny session. By default the one whose server function
#'   is running.
#'
#' @return The observer that answers, invisibly.
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(Widget_GroupComparisonOutput("chart")),
#'     server = function(input, output, session) {
#'       Serve_Statistics()
#'       output$chart <- renderWidget_GroupComparison(
#'         Widget_GroupComparison(Synthetic_Results, Synthetic_Participants)
#'       )
#'     }
#'   )
#' }
#'
#' @seealso The `renderWidget_*()` functions in [gsm.bio-shiny].
#' @family shiny
#' @export
Serve_Statistics <- function(session = shiny::getDefaultReactiveDomain()) {
  Serve_NeedShiny("Serve_Statistics")
  if (is.null(session)) {
    stop("Serve_Statistics() is called inside a Shiny server function: there is no session here.", call. = FALSE)
  }
  # The requests are sent under one name for the whole page, so the page's own
  # session answers, whichever module's server function this is called in.
  xPage <- session$rootScope()
  xObserver <- shiny::observeEvent(xPage$input[[strServeInput]],
    {
      for (lAnswer in Serve_Reply(xPage$input[[strServeInput]])) {
        xPage$sendCustomMessage(strServeMessage, lAnswer)
      }
    },
    domain = xPage
  )
  invisible(xObserver)
}

#' The widgets in a Shiny page
#'
#' An output function and a render function for each widget, as htmlwidgets
#' makes them. A widget drawn with its render function stores no results in
#' the page: its chart asks the Shiny session for every statistic, which
#' [Serve_Statistics()], called once in the server function, answers. Without
#' that call the chart is drawn and, after waiting twenty seconds for the
#' session to say it answers, says that statistics are unavailable and why.
#'
#' The stored results are not computed for a widget drawn this way, so the page
#' opens sooner than a saved page is made.
#'
#' @param outputId `character` The output's name.
#' @param width,height The size, as CSS. By default the widget is as wide as
#'   its container and as tall as its chart.
#' @param expr An expression that makes the widget: a call to its `Widget_*()`
#'   function.
#' @param env The environment to evaluate `expr` in.
#' @param quoted `logical` Whether `expr` is already quoted.
#'
#' @return An output function returns what a Shiny page is built from; a render
#'   function returns what an output is assigned.
#'
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   Widget_GroupComparisonOutput("chart")
#' }
#'
#' @section The size of a chart's text:
#' A chart sizes its text from the page's root font size. Shiny's default page
#' sets that to 10 pixels, which would draw a chart's text far smaller than a
#' saved page does, so an output function also writes one rule into the page,
#' `html { font-size: 100%; }`, which gives the root back to the browser.
#' Shiny's own text is sized in pixels and does not change.
#'
#' @seealso [Serve_Statistics()]
#' @family shiny
#' @name gsm.bio-shiny
NULL

# A chart sizes its text from the page's root, as a saved page's chart does,
# where the root is the browser's own size. Shiny's default page (Bootstrap 3)
# sets the root to 10 pixels and sizes its own text in pixels, so a chart in it
# was drawn with text five eighths the size (#80). The rule gives the root
# back to the browser; it is written once a page however many widgets it has.
strShinyRoot <- "html { font-size: 100%; }"

Widget_Output <- function(strName, outputId, width, height) {
  Serve_NeedShiny(paste0(strName, "Output"))
  shiny::tagList(
    shiny::singleton(shiny::tags$style(shiny::HTML(strShinyRoot))),
    htmlwidgets::shinyWidgetOutput(outputId, strName, width, height, package = "gsm.bio")
  )
}

# The widget's expression is evaluated by Widget_Served(), found in an
# environment of its own between the caller's and the expression, so the
# widget is made as one this session answers.
Widget_Render <- function(strName, expr, env, fnOutput) {
  Serve_NeedShiny(paste0("render", strName))
  envServed <- new.env(parent = env)
  envServed[[".gsm_bio_served"]] <- Widget_Served
  htmlwidgets::shinyRenderWidget(as.call(list(as.name(".gsm_bio_served"), expr)), fnOutput, envServed, quoted = TRUE)
}

#' @rdname gsm.bio-shiny
#' @export
Widget_GroupComparisonOutput <- function(outputId, width = "100%", height = "auto") {
  Widget_Output("Widget_GroupComparison", outputId, width, height)
}

#' @rdname gsm.bio-shiny
#' @export
renderWidget_GroupComparison <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  Widget_Render("Widget_GroupComparison", expr, env, Widget_GroupComparisonOutput)
}

#' @rdname gsm.bio-shiny
#' @export
Widget_AssociationScatterOutput <- function(outputId, width = "100%", height = "auto") {
  Widget_Output("Widget_AssociationScatter", outputId, width, height)
}

#' @rdname gsm.bio-shiny
#' @export
renderWidget_AssociationScatter <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  Widget_Render("Widget_AssociationScatter", expr, env, Widget_AssociationScatterOutput)
}

#' @rdname gsm.bio-shiny
#' @export
Widget_CorrelationMatrixOutput <- function(outputId, width = "100%", height = "auto") {
  Widget_Output("Widget_CorrelationMatrix", outputId, width, height)
}

#' @rdname gsm.bio-shiny
#' @export
renderWidget_CorrelationMatrix <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  Widget_Render("Widget_CorrelationMatrix", expr, env, Widget_CorrelationMatrixOutput)
}

#' @rdname gsm.bio-shiny
#' @export
Widget_BiomarkerScreenOutput <- function(outputId, width = "100%", height = "auto") {
  Widget_Output("Widget_BiomarkerScreen", outputId, width, height)
}

#' @rdname gsm.bio-shiny
#' @export
renderWidget_BiomarkerScreen <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  Widget_Render("Widget_BiomarkerScreen", expr, env, Widget_BiomarkerScreenOutput)
}

#' @rdname gsm.bio-shiny
#' @export
Widget_CrossTabOutput <- function(outputId, width = "100%", height = "auto") {
  Widget_Output("Widget_CrossTab", outputId, width, height)
}

#' @rdname gsm.bio-shiny
#' @export
renderWidget_CrossTab <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  Widget_Render("Widget_CrossTab", expr, env, Widget_CrossTabOutput)
}

#' @rdname gsm.bio-shiny
#' @export
Widget_StratifiedSurvivalOutput <- function(outputId, width = "100%", height = "auto") {
  Widget_Output("Widget_StratifiedSurvival", outputId, width, height)
}

#' @rdname gsm.bio-shiny
#' @export
renderWidget_StratifiedSurvival <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  Widget_Render("Widget_StratifiedSurvival", expr, env, Widget_StratifiedSurvivalOutput)
}
