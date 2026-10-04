# bio.viz's rules for a figure's title, subtitle and footnotes, the footnote
# written for it, and its statistics line, written in R (#37). The cases are
# bio.viz's own (tests/unit/output/titles.test.js, docs/output.md, and
# src/r/formatStatistic.js), with the one difference R/output.R names: a figure
# is drawn by gsm.bio from R's answers in the same session.

test_that("a placeholder is replaced by the text of its value once, left to right; a name not given is left as written; nothing is evaluated (#37)", {
  expect_identical(
    Output_FillText("{measure} at {visit}: {n} participants", list(measure = "CRP", visit = "Week 4", n = 186)),
    "CRP at Week 4: 186 participants"
  )
  expect_identical(Output_FillText("{measure} by {arm}", list(measure = "CRP")), "CRP by {arm}")
  expect_identical(Output_FillText("[{a}][{b}][{c}]", list(a = NULL, b = NA, c = 0)), "[][][0]")
  for (strTemplate in c(
    "${1 + 1} and ${globalThis.x = 1}", "<script>window.hacked = true</script>",
    "{{constructor.constructor(\"return 1\")()}}", "{ measure }", "{__proto__}", "{toString}"
  )) {
    expect_identical(Output_FillText(strTemplate, list(measure = "CRP")), strTemplate, label = strTemplate)
  }
  lValues <- list(measure = "${process.exit(1)}", visit = "<img src=x onerror=alert(1)>", n = "{measure}", group = "{{7*7}}")
  expect_identical(
    Output_FillText("{measure} | {visit} | {n} | {group}", lValues),
    "${process.exit(1)} | <img src=x onerror=alert(1)> | {measure} | {{7*7}}"
  )
  expect_identical(Output_FillText("{{measure}}", list(measure = "CRP")), "{CRP}")
  expect_identical(Output_FillText("no placeholders", list(measure = "CRP")), "no placeholders")
  expect_identical(Output_PlaceholdersIn("{measure} at {visit}, {measure} again; {{n}}"), c("measure", "visit", "n"))
  # A number is written as it reads.
  expect_identical(Output_FillText("{x}", list(x = 0.1 + 0.2)), "0.30000000000000004")
})

test_that("a title or subtitle that is not text, or footnotes that are not texts, are refused; one footnote is a list of one, empty ones dropped (#37)", {
  expect_identical(lOutputTitleDefaults, list(title = NULL, subtitle = NULL, footnotes = NULL))
  # As every chart of the copied bundle has them.
  for (strSetting in c("fit_statistic", "row_by", "at_risk_times", "start_value")) {
    expect_identical(lBundleDefaults(strSetting)[names(lOutputTitleDefaults)], lOutputTitleDefaults, label = strSetting)
  }
  lChecked <- Output_CheckTitles(list(title = "CRP", subtitle = "{n} participants", footnotes = "Synthetic."))
  expect_identical(lChecked$footnotes, "Synthetic.")
  expect_identical(Output_CheckTitles(list(footnotes = list("One.", "", " ", "Two.")))$footnotes, c("One.", "Two."))
  expect_error(Output_CheckTitles(list(title = 42)), "Setting 'title' must be text, which may hold placeholders such as \\{n\\}, or NULL for none")
  expect_error(Output_CheckTitles(list(subtitle = c("a", "b"))), "Setting 'subtitle' must be text")
  expect_error(Output_CheckTitles(list(footnotes = list("One.", list(text = "Two.")))), "Setting 'footnotes' must be text, or a list of texts, or NULL for none")
})

test_that("the figure's own footnote gives the date drawn, gsm.bio's version, and R's method and counts behind every statistic, with the versions that computed them (#37)", {
  expect_identical(Output_DateDrawn(as.POSIXct("2026-10-04 23:30:00", tz = "UTC")), "2026-10-04")
  strDrawn <- "Drawn on 2026-10-04 by gsm.bio 0.2.0."
  strBy <- Output_ComputedBy()
  expect_match(strBy, "^computed by R [0-9]+\\.[0-9]+\\.[0-9]+ with gsm\\.bio [0-9.]+$")
  expect_identical(Output_AutomaticFootnote(list(), strDate = "2026-10-04", strVersion = "0.2.0"), paste(strDrawn, "No statistic was asked of R."))
  lWelch <- list(method = "Welch Two Sample t-test", counts = list(Placebo = 95L, Treatment = 91L))
  expect_identical(
    Output_AutomaticFootnote(list(lWelch), strDate = "2026-10-04", strVersion = "0.2.0"),
    paste0(strDrawn, " Statistics: Welch Two Sample t-test (Placebo n = 95, Treatment n = 91); ", strBy, ".")
  )
  expect_identical(
    Output_AutomaticFootnote(
      list(list(method = "Pearson's product-moment correlation", counts = 200L), list(method = "Linear regression", counts = 200L)),
      strDate = "2026-10-04", strVersion = "0.2.0"
    ),
    paste0(strDrawn, " Statistics: Pearson's product-moment correlation (n = 200); Linear regression (n = 200); ", strBy, ".")
  )
  expect_identical(
    Output_AutomaticFootnote(list(list(status = "too_small")), strDate = "2026-10-04", strVersion = "0.2.0"),
    paste0(strDrawn, " Statistics: no statistic; ", strBy, ".")
  )
  expect_identical(Output_CountsText(list(CRP = 185, `D-dimer` = 179, Ferritin = 184, IL6 = 186, IL8 = 185), "biomarkers"), "n = 179 to 186 across 5 biomarkers")
  expect_identical(Output_CountsText(list(a = 5, b = 5, c = 5, d = 5, e = 5), "variables"), "n = 5 across 5 variables")
  expect_null(Output_CountsText(NULL))
  expect_identical(Output_CountsText(200L), "n = 200")
})

test_that("R's answer is printed as the chart's statistics line prints it (#37)", {
  lWelch <- list(status = "ok", method = "Welch Two Sample t-test", counts = list(Placebo = 95L, Treatment = 91L), p_value = 0.0123, adjustment = "none")
  expect_identical(Output_StatisticText(lWelch), "Welch Two Sample t-test: p = 0.012 (Placebo n = 95, Treatment n = 91). Exploratory, unadjusted.")
  expect_identical(Output_StatisticText(modifyList(lWelch, list(p_value = 2e-12))), "Welch Two Sample t-test: p < 0.001 (Placebo n = 95, Treatment n = 91). Exploratory, unadjusted.")
  expect_identical(Output_StatisticText(modifyList(lWelch, list(p_value = 0.9999))), "Welch Two Sample t-test: p > 0.999 (Placebo n = 95, Treatment n = 91). Exploratory, unadjusted.")
  expect_identical(Output_StatisticText(modifyList(lWelch, list(adjustment = "BH"))), "Welch Two Sample t-test: p = 0.012 (Placebo n = 95, Treatment n = 91). Exploratory, adjusted (Benjamini-Hochberg).")
  expect_identical(
    Output_StatisticText(list(status = "too_small", method = "Log-rank test", reason = "Not computed: > 10 has 2. The minimum group size is 5.", counts = list(a = 2L, b = 198L))),
    "Not computed: > 10 has 2. The minimum group size is 5. Counts: a n = 2, b n = 198."
  )
  expect_identical(
    Output_StatisticText(list(status = "error", reason = "boom", counts = 10L)),
    "R reported an error: boom (n = 10)."
  )
  expect_identical(Output_StatisticText(list(status = "ok", method = "m", counts = 3L, p_value = NA)), "p-value not shown: the result has no p-value between 0 and 1.")
  # An estimate, a median that was not reached, and an estimate with no interval.
  expect_identical(
    Output_EstimateText(list(name = "Hazard ratio", group = "> 2.783 / ≤ 2.783", estimate = 3.52317, lower = 2.43004, upper = 5.10712, level = 0.95)),
    enc2utf8("Hazard ratio (> 2.783 / ≤ 2.783): 3.523, 95% confidence interval 2.43 to 5.107.")
  )
  expect_identical(
    Output_EstimateText(list(name = "Median", group = "Low", estimate = 23.3157, lower = 17.32, upper = NA, level = 0.95)),
    "Median (Low): 23.32, 95% confidence interval 17.32 to not reached."
  )
  expect_identical(Output_EstimateText(list(name = "cor", group = NA, estimate = 0.63842)), "cor: 0.6384.")
})

test_that("the filters and the variables are said in the chart's words (#37)", {
  lSpecs <- list(list(value_col = "SEX", label = "Sex"), list(value_col = "ARM", label = "Arm"))
  expect_identical(Output_FiltersText(list(SEX = "F", ARM = c("Placebo", "Treatment")), lSpecs), "Sex is F; Arm is Placebo or Treatment")
  expect_identical(Output_FiltersText(list(SEX = NULL), lSpecs), "none")
  expect_identical(Output_Label(list(measure = "CRP", visit = "Baseline", value = "raw", cut = "median")), "CRP at Baseline, cut at the median")
  expect_identical(Output_Label(list(measure = "CRP", visit = "Week 4", value = "change", cut = list(1, 2.5, 4))), "CRP at Week 4, change from baseline, cut at 1, 2.5 and 4")
  expect_identical(Output_Label(list(measure = "IL-6", value = "baseline")), "IL-6 at baseline")
  expect_identical(Output_Label(list(col = "AGE", type = "number", cut = "tertiles")), "AGE, cut at the tertiles")
  lConfig <- list(unit_col = "STRESU", measure_col = "TEST")
  expect_identical(Output_AxisTitle(Synthetic_Results, lConfig, list(measure = "IL-6", visit = "Week 4", value = "change")), "IL-6 at Week 4, change from baseline (pg/mL)")
  expect_identical(Output_AxisTitle(Synthetic_Results, lConfig, list(measure = "IL-6", visit = "Week 4", value = "percent_change")), "IL-6 at Week 4, percent change from baseline (%)")
  expect_identical(Output_AxisTitle(Synthetic_Results, lConfig, list(col = "AGE"), list(list(value_col = "AGE", label = "Age"))), "Age")
})

test_that("the figure's own footnote names every method R used and every adjustment, as bio.viz's final rules do (#37)", {
  lGroup <- lReadJson(testthat::test_path("fixtures", "bio.viz"), "group-statistics-r.json")$results
  lScreen <- lReadJson(testthat::test_path("fixtures", "bio.viz"), "screen-statistics-r.json")$results
  ValueOf <- function(lResults, strCase) Filter(function(lResult) lResult$case == strCase, lResults)[[1]]$value
  strBy <- Output_ComputedBy()
  Say <- function(lValue, strOf = "groups") Output_AutomaticFootnote(list(lValue), strOf, strDate = "2026-10-04", strVersion = "0.2.0")
  strDrawn <- "Drawn on 2026-10-04 by gsm.bio 0.2.0."
  # bio.viz's EXP-AUTO-004, on the same recorded answers.
  expect_identical(
    Say(ValueOf(lGroup, "kruskal-pairwise")),
    paste0(
      strDrawn, " Statistics: Kruskal-Wallis rank sum test, with Wilcoxon rank sum test with continuity correction and ",
      "Wilcoxon rank sum exact test (Placebo F n = 42, Placebo M n = 53, Treatment F n = 42, Treatment M n = 49), ",
      "p-values adjusted by Holm; ", strBy, "."
    )
  )
  expect_match(
    Say(ValueOf(lScreen, "difference-week-4-change"), "biomarkers"),
    "Statistics: Welch Two Sample t-test \\(n = [0-9]+ to [0-9]+ across 12 biomarkers\\), p-values adjusted by Benjamini-Hochberg; "
  )
  expect_match(Say(ValueOf(lScreen, "difference-week-4-change-holm"), "biomarkers"), ", p-values adjusted by Holm; ", fixed = TRUE)
  lWelch <- list(method = "Welch Two Sample t-test", counts = list(Placebo = 95L, Treatment = 91L))
  expect_false(grepl("adjusted", Say(lWelch)))
  # The same on an answer as R returns it, its rows a data frame.
  dfThree <- data.frame(y = c(1:10, 3:12, 6:15) + 0.5, x = rep(c("a", "b", "c"), each = 10))
  lLive <- Analyze_GroupDifference(dfThree, "y", "x", strMethod = "anova", bPairwise = TRUE)
  expect_match(Say(lLive), "Statistics: One-way analysis of variance, with Welch Two Sample t-test (a n = 10, b n = 10, c n = 10), p-values adjusted by Holm; ", fixed = TRUE)
  # bio.viz's EXP-AUTO-006: four groups named, five summarised, numeric text read as a number.
  lFour <- list(a = 1, b = 2, c = 3, d = 4)
  expect_identical(Output_CountsText(lFour), "a n = 1, b n = 2, c n = 3, d n = 4")
  expect_identical(Output_CountsText(c(lFour, list(e = 5))), "n = 1 to 5 across 5 groups")
  expect_identical(Output_CountsText("200"), "n = 200")
  expect_identical(Output_CountsText(list(a = "7", b = 9)), "a n = 7, b n = 9")
  expect_null(Output_CountsText("many"))
  # An answer R could not compute is said as R said it; R's error is said so,
  # and counted when another answer was computed.
  expect_identical(Say(list(status = "too_small", counts = list(a = 2L, b = 198L))), paste0(strDrawn, " Statistics: no statistic (a n = 2, b n = 198); ", strBy, "."))
  expect_identical(Say(list(status = "error", method = "m", reason = "boom")), paste(strDrawn, "Statistics: R reported an error."))
  expect_identical(
    Output_AutomaticFootnote(list(lWelch, list(status = "error", reason = "boom")), strDate = "2026-10-04", strVersion = "0.2.0"),
    paste0(strDrawn, " Statistics: Welch Two Sample t-test (Placebo n = 95, Treatment n = 91); ", strBy, ". 1 of 2 could not be computed.")
  )
})

test_that("a title or subtitle of only white space is no title (#37)", {
  lTitles <- Output_Titles(list(title = "   ", subtitle = "\n\t", footnotes = NULL), list(date = "2026-10-04"), list())
  expect_null(lTitles$title)
  expect_null(lTitles$subtitle)
})
