# The Data view's viewer: the tables the charts are drawn on, ten rows at a
# time, and the first rows of a file a reader has just chosen (#80).
#
# The page of rows and the table it is shown as are plain functions, tested
# first. Then a session of the app, with no browser; then the page itself.

# A table as the page shows it: its header, and its rows cell by cell.
lShownTable <- function(strHtml) {
  Cells <- function(strRow, strTag) {
    chrCells <- regmatches(strRow, gregexpr(sprintf("<%s[^>]*>.*?</%s>", strTag, strTag), strRow, perl = TRUE))[[1]]
    gsub("<[^>]+>", "", chrCells)
  }
  strFlat <- gsub("\n\\s*", "", strHtml)
  chrRows <- regmatches(strFlat, gregexpr("<tr>.*?</tr>", strFlat, perl = TRUE))[[1]]
  list(header = Cells(chrRows[1], "th"), rows = lapply(chrRows[-1], Cells, strTag = "td"))
}

test_that("a page of a table is ten of its rows, the last page what is left, and a page before the first or past the last is the first or the last (#80)", {
  lFirst <- App_ViewPage(Synthetic_Results, 1L)
  expect_identical(lFirst[c("page", "pages", "from", "to")], list(page = 1L, pages = 1148L, from = 1L, to = 10L))
  expect_identical(lFirst$rows, Synthetic_Results[1:10, ])
  lLast <- App_ViewPage(Synthetic_Results, 1148L)
  expect_identical(lLast[c("page", "from", "to")], list(page = 1148L, from = 11471L, to = 11472L))
  expect_identical(lLast$rows, Synthetic_Results[11471:11472, ])
  expect_identical(App_ViewPage(Synthetic_Results, 0L)$page, 1L)
  expect_identical(App_ViewPage(Synthetic_Results, -3L)$page, 1L)
  expect_identical(App_ViewPage(Synthetic_Results, 99999L)$page, 1148L)
  expect_identical(App_ViewPage(Synthetic_Results, NA)$page, 1L)
  # A table of exactly one page, and one of no rows.
  expect_identical(App_ViewPage(Synthetic_Results[1:10, ], 2L)[c("page", "pages", "to")], list(page = 1L, pages = 1L, to = 10L))
  lNone <- App_ViewPage(Synthetic_Results[0, ], 1L)
  expect_identical(lNone[c("page", "pages", "from", "to")], list(page = 1L, pages = 1L, from = 0L, to = 0L))
  expect_identical(nrow(lNone$rows), 0L)
})

test_that("rows are shown under the table's own column names, each with its number, with values as R holds them, a missing one as NA, and nothing of a value read as markup (#80)", {
  skip_if_not_installed("shiny")
  dfRows <- data.frame(
    SUBJ = c("S-001", "<b>S-002</b>", NA),
    RESULT = c(1.23456789012345, NA, 1e-12),
    `A name with spaces` = c(TRUE, FALSE, NA),
    WHEN = as.Date(c("2026-01-31", NA, "2026-02-01")),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  strHtml <- as.character(App_RowsTable(dfRows, nFirst = 1231L))
  lShown <- lShownTable(strHtml)
  expect_identical(lShown$header, c("Row", "SUBJ", "RESULT", "A name with spaces", "WHEN"))
  expect_identical(lShown$rows, list(
    c("1,231", "S-001", "1.23456789012345", "TRUE", "2026-01-31"),
    c("1,232", "&lt;b&gt;S-002&lt;/b&gt;", "NA", "FALSE", "NA"),
    c("1,233", "NA", "1e-12", "NA", "2026-02-01")
  ))
  expect_false(grepl("<b>", strHtml, fixed = TRUE))
  # No rows: the header alone.
  expect_identical(lShownTable(as.character(App_RowsTable(dfRows[0, ])))$rows, list())
})

test_that("in a session the viewer shows the synthetic study's results as the app opens, turns its pages, shows the participants and the outcomes, and follows a reader's file once it is drawn (#80)", {
  skip_if_not_installed("shiny")
  lWas <- options(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
  on.exit(options(lWas), add = TRUE)
  dfOwn <- Synthetic_Results[Synthetic_Results$TEST %in% c("CRP", "IL-6"), ]
  names(dfOwn)[match(c("USUBJID", "TEST", "STRESN"), names(dfOwn))] <- c("SUBJ", "MARKER", "RESULT")
  strDir <- tempfile("gsm-bio-viewer")
  dir.create(strDir)
  strOwn <- file.path(strDir, "labs.csv")
  utils::write.csv(dfOwn, strOwn, row.names = FALSE, na = "")
  Html <- function(xOutput) as.character(xOutput$html)
  Said <- function(strHtml) gsub("<[^>]+>", "", regmatches(strHtml, regexpr("<p class=\"gsm-bio-app-what\">.*?</p>", strHtml, perl = TRUE)))
  Row <- function(dfTable, iRow) c(format(iRow, big.mark = ","), unname(vapply(dfTable, function(xColumn) App_Cell(xColumn)[iRow], character(1))))

  shiny::testServer(RunApp(), {
    # As the app opens: the results the charts are drawn on, their first ten rows.
    strView <- Html(output$gsm_bio_view)
    expect_identical(
      Said(strView),
      "Results, from the synthetic study that ships with gsm.bio: 11,472 rows, 6 columns. Rows 1 to 10, page 1 of 1,148. NA is a missing value."
    )
    lShown <- lShownTable(strView)
    expect_identical(lShown$header, c("Row", names(Synthetic_Results)))
    expect_length(lShown$rows, 10L)
    expect_identical(lShown$rows[[1]], Row(Synthetic_Results, 1L))
    expect_identical(lShown$rows[[10]], Row(Synthetic_Results, 10L))

    # The pages turn, and stop at the first.
    session$setInputs(gsm_bio_view_next = 1)
    expect_match(Said(Html(output$gsm_bio_view)), "Rows 11 to 20, page 2 of 1,148.", fixed = TRUE)
    expect_identical(lShownTable(Html(output$gsm_bio_view))$rows[[1]], Row(Synthetic_Results, 11L))
    session$setInputs(gsm_bio_view_next = 2)
    expect_match(Said(Html(output$gsm_bio_view)), "Rows 21 to 30, page 3 of 1,148.", fixed = TRUE)
    session$setInputs(gsm_bio_view_previous = 1)
    session$setInputs(gsm_bio_view_previous = 2)
    session$setInputs(gsm_bio_view_previous = 3)
    expect_match(Said(Html(output$gsm_bio_view)), "Rows 1 to 10, page 1 of 1,148.", fixed = TRUE)

    # Another table starts at its first rows.
    session$setInputs(gsm_bio_view_next = 3)
    session$setInputs(gsm_bio_view_table = "participants")
    strView <- Html(output$gsm_bio_view)
    expect_identical(
      Said(strView),
      sprintf(
        "Participants, from the synthetic study that ships with gsm.bio: 200 rows, %d columns. Rows 1 to 10, page 1 of 20. NA is a missing value.",
        ncol(Synthetic_Participants)
      )
    )
    expect_identical(lShownTable(strView)$header, c("Row", names(Synthetic_Participants)))
    expect_identical(lShownTable(strView)$rows[[3]], Row(Synthetic_Participants, 3L))
    # Its last page is the last: Next there changes nothing.
    for (iPress in 4:30) session$setInputs(gsm_bio_view_next = iPress)
    expect_match(Said(Html(output$gsm_bio_view)), "Rows 191 to 200, page 20 of 20.", fixed = TRUE)
    session$setInputs(gsm_bio_view_table = "outcomes")
    expect_match(Said(Html(output$gsm_bio_view)), sprintf("^Outcomes, from the synthetic study that ships with gsm.bio: %s rows", format(nrow(Synthetic_Outcomes), big.mark = ",")))
    expect_identical(lShownTable(Html(output$gsm_bio_view))$header, c("Row", names(Synthetic_Outcomes)))

    # A file just chosen: its first five rows under its own column names, and
    # the viewer still on what the charts are drawn on.
    session$setInputs(gsm_bio_file_results = data.frame(name = "labs.csv", size = file.size(strOwn), type = "", datapath = strOwn, stringsAsFactors = FALSE))
    strAsked <- Html(output$gsm_bio_columns_results)
    expect_match(strAsked, "The first 5 rows of labs.csv, as R read them:", fixed = TRUE)
    lPreview <- lShownTable(strAsked)
    expect_identical(lPreview$header, c("Row", names(dfOwn)))
    expect_length(lPreview$rows, 5L)
    dfRead <- App_ReadFile(strOwn, "labs.csv")
    expect_identical(lPreview$rows[[5]], Row(dfRead, 5L))
    expect_match(Said(Html(output$gsm_bio_view)), "^Outcomes, from the synthetic study that ships with gsm.bio")

    # Once it is drawn the viewer follows: the reader's table under gsm.bio's
    # names, and, with no outcomes loaded, the results in place of the table
    # that is no longer there.
    session$setInputs(gsm_bio_column_results_USUBJID = "SUBJ", gsm_bio_column_results_TEST = "MARKER", gsm_bio_column_results_STRESN = "RESULT")
    session$setInputs(gsm_bio_column_results_VISIT = "VISIT", gsm_bio_column_results_VISITNUM = "VISITNUM")
    session$setInputs(gsm_bio_apply = 1)
    expect_identical(output$gsm_bio_source, "Drawn on labs.csv, loaded in this session.")
    strView <- Html(output$gsm_bio_view)
    expect_identical(
      Said(strView),
      sprintf(
        "Results, from labs.csv, loaded in this session: %s rows, 6 columns. Rows 1 to 10, page 1 of %s. NA is a missing value.",
        format(nrow(dfOwn), big.mark = ","), format(ceiling(nrow(dfOwn) / 10), big.mark = ",")
      )
    )
    lShown <- lShownTable(strView)
    expect_setequal(lShown$header, c("Row", names(Synthetic_Results)))
    expect_true(all(c("USUBJID", "TEST", "STRESN") %in% lShown$header))
    expect_false(any(c("SUBJ", "MARKER", "RESULT") %in% lShown$header))
    expect_identical(App_Loaded(list(results = dfOwn)), c(Results = "results"))
  })
})

test_that("in a browser the Data view shows the rows the charts are drawn on, turns to the next ten, shows a chosen file's first rows, and shows the file once the charts are drawn on it (#80)", {
  NeedApp()
  dfOwn <- Synthetic_Results
  names(dfOwn)[match(c("USUBJID", "TEST", "STRESN"), names(dfOwn))] <- c("SUBJ", "MARKER", "RESULT")
  strDir <- tempfile("gsm-bio-viewer")
  dir.create(strDir)
  strOwn <- file.path(strDir, "labs.csv")
  utils::write.csv(dfOwn, strOwn, row.names = FALSE, na = "")

  lApp <- lRunApp("RunApp()")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)
  lPage$Evaluate("document.querySelector('a[data-value=\"Data\"]').click()")
  strSaid <- "(document.querySelector('#gsm_bio_view .gsm-bio-app-what') || {}).textContent"
  Shown <- function(strWithin) {
    lPage$Evaluate(sprintf(
      "({ header: Array.from(document.querySelectorAll('%s thead th')).map((cell) => cell.textContent), rows: Array.from(document.querySelectorAll('%s tbody tr')).map((row) => Array.from(row.cells).map((cell) => cell.textContent)) })",
      strWithin, strWithin
    ))
  }
  expect_true(bWaitFor(lPage, sprintf("(%s || '').includes('Rows 1 to 10, page 1 of 1,148.')", strSaid)), label = "the viewer shows the first rows")
  expect_identical(
    lPage$Evaluate(strSaid),
    "Results, from the synthetic study that ships with gsm.bio: 11,472 rows, 6 columns. Rows 1 to 10, page 1 of 1,148. NA is a missing value."
  )
  lShown <- Shown("#gsm_bio_view")
  expect_identical(unlist(lShown$header), c("Row", names(Synthetic_Results)))
  expect_length(lShown$rows, 10L)
  expect_identical(unlist(lShown$rows[[1]])[1:2], c("1", Synthetic_Results$USUBJID[1]))
  # The table the select offers are the three the study has.
  expect_identical(
    unlist(lPage$Evaluate("Array.from(document.querySelector('#gsm_bio_view_table').options).map((option) => option.value)")),
    c("results", "participants", "outcomes")
  )

  lPage$Evaluate("document.querySelector('#gsm_bio_view_next').click()")
  expect_true(bWaitFor(lPage, sprintf("(%s || '').includes('Rows 11 to 20, page 2 of 1,148.')", strSaid)), label = "the next ten rows")
  expect_identical(unlist(Shown("#gsm_bio_view")$rows[[1]])[1:2], c("11", Synthetic_Results$USUBJID[11]))

  # A file chosen: its first five rows, under its own names.
  lPage$Upload("#gsm_bio_file_results", strOwn)
  expect_true(bWaitFor(lPage, "document.querySelector('#gsm_bio_columns_results table')"), label = "the chosen file's first rows")
  lPreview <- Shown("#gsm_bio_columns_results")
  expect_identical(unlist(lPreview$header), c("Row", names(dfOwn)))
  expect_length(lPreview$rows, 5L)
  expect_true(isTRUE(lPage$Evaluate("document.querySelector('#gsm_bio_columns_results').textContent.includes('The first 5 rows of labs.csv, as R read them:')")))

  # Drawn on the file: the viewer shows it, from its first rows, and offers
  # the one table there now is.
  for (strPair in list(c("USUBJID", "SUBJ"), c("TEST", "MARKER"), c("STRESN", "RESULT"))) {
    lPage$Evaluate(sprintf(
      "(() => { const node = document.querySelector('#gsm_bio_column_results_%s'); node.value = '%s'; node.dispatchEvent(new Event('change', { bubbles: true })); return node.value; })()",
      strPair[1], strPair[2]
    ))
  }
  lPage$Evaluate("document.querySelector('#gsm_bio_apply').click()")
  expect_true(
    bWaitFor(lPage, sprintf("(%s || '').startsWith('Results, from labs.csv, loaded in this session: 11,472 rows, 6 columns. Rows 1 to 10, page 1 of 1,148.')", strSaid)),
    label = "the viewer follows the file"
  )
  lShown <- Shown("#gsm_bio_view")
  expect_true(all(c("USUBJID", "TEST", "STRESN") %in% unlist(lShown$header)))
  expect_identical(
    unlist(lPage$Evaluate("Array.from(document.querySelector('#gsm_bio_view_table').options).map((option) => option.value)")),
    "results"
  )
  expect_identical(lPage$Errors(), character(0))
})

test_that("a chart in a Shiny page has text the size it has in a saved page: an output gives the page's root font size back to the browser, once a page (#80)", {
  skip_if_not_installed("shiny")
  strOne <- as.character(Widget_GroupComparisonOutput("one"))
  expect_match(strOne, "<style>html { font-size: 100%; }</style>", fixed = TRUE)
  expect_match(strOne, "id=\"one\"", fixed = TRUE)
  # However many widgets a page has, the rule is written once.
  strPage <- as.character(shiny::fluidPage(Widget_GroupComparisonOutput("one"), Widget_CrossTabOutput("two"), App_Ui()))
  expect_identical(lengths(regmatches(strPage, gregexpr("html { font-size: 100%; }", strPage, fixed = TRUE))), 1L)

  NeedApp()
  # The same chart saved as a file, and its text there.
  strSizes <- "(() => { const size = (selector) => getComputedStyle(document.querySelector(selector)).fontSize; return { root: getComputedStyle(document.documentElement).fontSize, foot: size('.bv-foot-line'), control: size('.html-widget label') }; })()"
  lSaved <- lOpenPage(strSavedFile(Widget_GroupComparison(Synthetic_Results, Synthetic_Participants)))
  on.exit(lSaved$Close(), add = TRUE)
  lWant <- lSaved$Evaluate(strSizes)
  expect_identical(lWant$root, "16px")
  lApp <- lRunApp("RunApp()")
  on.exit(lApp$Stop(), add = TRUE)
  lPage <- lOpenPage(NULL, strAddress = lApp$address)
  on.exit(lPage$Close(), add = TRUE)
  expect_true(bWaitFor(lPage, "document.querySelector('#GroupComparison .bv-foot-line')"), label = "the chart's footnote is drawn")
  expect_identical(lPage$Evaluate(strSizes), lWant)
  # Shiny's own text is sized in pixels, and stays as it was.
  expect_identical(lPage$Evaluate("getComputedStyle(document.body).fontSize"), "14px")
})
