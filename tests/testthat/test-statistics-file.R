# The single statistics source file (#3, #4): inst/statistics/statistics.R is
# the one definition of every statistics function. The package's exported
# functions are built from it, the installed package ships it, and it runs as
# it is in a bare R session with only stats and survival attached.

chrStatisticsExports <- c(
  "Analyze_GroupDifference", "Analyze_Correlation", "Analyze_CorrelationMatrix", "Analyze_Contingency",
  "Analyze_Survival", "Analyze_Screen"
)

strStatisticsFile <- function() {
  system.file("statistics", "statistics.R", package = "gsm.bio")
}

# Every result the tests below look at, as the browser would ask for it: the
# function's name, then named arguments in the shapes JSON delivers, a list of
# single values where an argument takes several.
lStatisticsCalls <- function() {
  lTruth <- Synthetic_Truth
  strX <- paste(lTruth$Correlation$Biomarkers[1], "@ Week 4")
  strY <- paste(lTruth$Correlation$Biomarkers[2], "@ Week 4")
  list(
    t = list("Analyze_GroupDifference", list(
      strValueCol = "Change", strGroupCol = "ARM", strMethod = "t",
      chrGroups = as.list(lTruth$GroupDifference$Groups)
    )),
    wilcoxon = list("Analyze_GroupDifference", list(strValueCol = "Change", strGroupCol = "ARM", strMethod = "wilcoxon")),
    anova = list("Analyze_GroupDifference", list(strValueCol = "Change", strGroupCol = "ARM_SEX", strMethod = "anova")),
    kruskal = list("Analyze_GroupDifference", list(
      strValueCol = "Change", strGroupCol = "ARM_SEX", strMethod = "kruskal", strPAdjust = "BH"
    )),
    group_too_small = list("Analyze_GroupDifference", list(strValueCol = "Change", strGroupCol = "ARM", nMinGroup = 96L)),
    group_error = list("Analyze_GroupDifference", list(strValueCol = "Nope", strGroupCol = "ARM")),
    pearson = list("Analyze_Correlation", list(strXCol = strX, strYCol = strY, strMethod = "pearson", strGroupCol = "ARM")),
    spearman = list("Analyze_Correlation", list(strXCol = strX, strYCol = strY, strMethod = "spearman")),
    correlation_too_small = list("Analyze_Correlation", list(strXCol = strX, strYCol = strY, nMinGroup = 201)),
    matrix_pearson = list("Analyze_CorrelationMatrix", list(chrCols = list(strX, strY, "Change", "AGE"))),
    matrix_spearman = list("Analyze_CorrelationMatrix", list(chrCols = c(strX, strY), strMethod = "spearman")),
    matrix_too_small = list("Analyze_CorrelationMatrix", list(chrCols = c(strX, strY), nMinPairs = 201)),
    chisq = list("Analyze_Contingency", list(strRowCol = "ARM", strColCol = "RESPONSE", strMethod = "chisq")),
    chisq_larger = list("Analyze_Contingency", list(strRowCol = "ARM_SEX", strColCol = "RESPONSE")),
    chisq_sparse = list("Analyze_Contingency", list(strRowCol = "AGE_DECADE", strColCol = "RESPONSE", nMinGroup = 1)),
    fisher = list("Analyze_Contingency", list(strRowCol = "ARM", strColCol = "RESPONSE", strMethod = "fisher")),
    fisher_larger = list("Analyze_Contingency", list(strRowCol = "ARM_SEX", strColCol = "RESPONSE", strMethod = "fisher")),
    contingency_too_small = list("Analyze_Contingency", list(strRowCol = "ARM", strColCol = "RESPONSE", nMinGroup = 101)),
    survival = list("Analyze_Survival", list(
      strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL", strCensorCol = "CNSR",
      chrGroups = as.list(lTruth$Survival$Groups)
    )),
    survival_four = list("Analyze_Survival", list(strTimeCol = "AVAL", strGroupCol = "ARM_SEX", strCensorCol = "CNSR")),
    survival_too_small = list("Analyze_Survival", list(
      strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL", strCensorCol = "CNSR", nMinGroup = 101
    )),
    survival_error = list("Analyze_Survival", list(strTimeCol = "AVAL", strGroupCol = "CRP_LEVEL")),
    screen_difference = list("Analyze_Screen", list(
      chrCols = as.list(paste(chrSyntheticBiomarkers(), "change")), strComparison = "difference",
      strGroupCol = "ARM", chrGroups = as.list(lTruth$GroupDifference$Groups)
    )),
    screen_correlation = list("Analyze_Screen", list(
      chrCols = setdiff(paste(chrSyntheticBiomarkers(), "@ Week 4"), strY), strComparison = "correlation",
      strWithCol = strY, strCorMethod = "spearman", strPAdjust = "holm"
    )),
    screen_hazard = list("Analyze_Screen", list(
      chrCols = paste(chrSyntheticBiomarkers(), "@ Baseline"), strComparison = "hazard",
      strTimeCol = "AVAL", strCensorCol = "CNSR"
    )),
    screen_one = list("Analyze_Screen", list(
      chrCols = "Change", strComparison = "difference", strGroupCol = "ARM"
    )),
    screen_too_small = list("Analyze_Screen", list(
      chrCols = c("Change", "AGE"), strComparison = "difference", strGroupCol = "ARM", nMinGroup = 101
    ))
  )
}

lRunCalls <- function(lCalls, dfFrame, envFunctions) {
  lapply(lCalls, function(lCall) {
    do.call(get(lCall[[1]], envir = envFunctions), c(list(dfFrame), lCall[[2]]))
  })
}

# Every function a piece of code calls, by name. Anything reached through
# `pkg::name` is reported as "pkg::name".
chrCalledFunctions <- function(xCode) {
  if (is.call(xCode)) {
    xHead <- xCode[[1]]
    if (identical(xHead, quote(`::`)) || identical(xHead, quote(`:::`))) {
      # `pkg::name` itself, whether it is then called or only read.
      return(paste0(as.character(xCode[[2]]), "::", as.character(xCode[[3]])))
    }
    chrHead <- if (is.symbol(xHead)) as.character(xHead) else chrCalledFunctions(xHead)
    return(c(chrHead, unlist(lapply(as.list(xCode)[-1], chrCalledFunctions))))
  }
  if (is.pairlist(xCode) || is.expression(xCode)) {
    return(unlist(lapply(as.list(xCode), chrCalledFunctions)))
  }
  character(0)
}

# The same object, whatever environment it was defined in and whether or not
# its source references were kept: a package installed with its source keeps
# them on every function, the outer ones and the ones written inside them.
bSameDefinition <- function(xPackage, xFile) {
  if (is.function(xPackage) && is.function(xFile)) {
    xPackage <- utils::removeSource(xPackage)
    xFile <- utils::removeSource(xFile)
  }
  identical(xPackage, xFile, ignore.environment = TRUE, ignore.bytecode = TRUE, ignore.srcref = TRUE)
}

test_that("the statistics file ships in the installed package where system.file() finds it (#3)", {
  expect_true(nzchar(strStatisticsFile()))
  expect_true(file.exists(strStatisticsFile()))
  expect_identical(basename(dirname(strStatisticsFile())), "statistics")
})

test_that("each exported function is identical to its definition in the statistics file (#3, #4)", {
  envFile <- new.env(parent = globalenv())
  sys.source(strStatisticsFile(), envir = envFile)
  chrExports <- chrStatisticsExports

  expect_setequal(getNamespaceExports("gsm.bio"), chrExports)
  expect_true(all(chrExports %in% ls(envFile)))
  for (strName in chrExports) {
    fnPackage <- getExportedValue("gsm.bio", strName)
    fnFile <- get(strName, envir = envFile)
    # Same arguments, same defaults and same body. Only the environment the
    # function was defined in differs, which is the point.
    expect_true(
      bSameDefinition(fnPackage, fnFile),
      label = paste(strName, "in the package is identical to its definition in the file")
    )
    # The comparison can fail: a function is not identical to a different one.
    expect_false(bSameDefinition(fnPackage, get("Stat_Result", envir = envFile)))
    expect_identical(environment(fnPackage), asNamespace("gsm.bio"))
  }

  # Everything else the file defines is in the namespace too, identical and not
  # exported: there is no second copy of a helper or a constant either.
  for (strName in setdiff(ls(envFile), chrExports)) {
    expect_true(
      bSameDefinition(get(strName, envir = asNamespace("gsm.bio")), get(strName, envir = envFile)),
      label = paste(strName, "in the namespace is identical to its definition in the file")
    )
  }
  expect_identical(get("nMinGroupDefault", envir = envFile), 5L)
})

test_that("the statistics file calls base R, stats and survival and nothing else, and never evaluates text (#3, #4)", {
  exprFile <- parse(strStatisticsFile(), keep.source = FALSE)
  envFile <- new.env(parent = globalenv())
  sys.source(strStatisticsFile(), envir = envFile)
  chrCalled <- unique(chrCalledFunctions(exprFile))
  bQualified <- grepl("::", chrCalled, fixed = TRUE)

  # Every qualified call is to stats or to survival, and both are used.
  expect_setequal(sub("::.*$", "", chrCalled[bQualified]), c("stats", "survival"))
  for (strCall in chrCalled[bQualified]) {
    expect_true(
      exists(sub("^.*::", "", strCall), envir = asNamespace(sub("::.*$", "", strCall)), inherits = FALSE),
      label = paste(strCall, "exists")
    )
  }
  # From survival, only the four functions the design names.
  expect_setequal(
    grep("^survival::", chrCalled, value = TRUE),
    c("survival::Surv", "survival::survdiff", "survival::survfit", "survival::coxph")
  )
  # Every other call is to base R or to something the file itself defines, so
  # nothing relies on a package being attached.
  chrBare <- chrCalled[!bQualified]
  bKnown <- vapply(chrBare, function(strName) {
    exists(strName, envir = baseenv(), inherits = FALSE) || exists(strName, envir = envFile, inherits = FALSE)
  }, logical(1))
  # The exceptions are fnCall, an argument, a function the file passes itself,
  # and Limit, a function defined inside the one that calls it.
  expect_setequal(chrBare[!bKnown], c("fnCall", "Limit"))

  # No package is attached or loaded, and nothing is parsed or evaluated.
  chrForbidden <- c(
    "library", "require", "requireNamespace", "loadNamespace", "attachNamespace",
    "eval", "evalq", "eval.parent", "parse", "str2lang", "str2expression", "source", "sys.source",
    "do.call", "match.fun", "get", "get0", "mget", "assign", "system", "system2"
  )
  expect_identical(intersect(all.names(exprFile), chrForbidden), character(0))
})

test_that("no result on the synthetic study holds a factor, a matrix, a classed object or a bare vector for a collection (#3, #4)", {
  lResults <- lRunCalls(lStatisticsCalls(), dfSyntheticFrame(), asNamespace("gsm.bio"))

  for (strCall in names(lResults)) {
    ExpectResultShape(lResults[[strCall]])
  }
  # Every status is exercised, and the collections are all exercised non-empty.
  expect_setequal(vapply(lResults, function(lResult) lResult$status, character(1)), c("ok", "too_small", "error"))
  expect_gt(nrow(lResults$anova$rows), 0)
  expect_gt(nrow(lResults$pearson$rows), 0)
  expect_identical(nrow(lResults$matrix_spearman$rows), 1L)
  # A warning every version of R raises, so the list is exercised non-empty.
  expect_identical(lResults$chisq_sparse$warnings, list("Chi-squared approximation may be incorrect"))
  expect_gt(length(lResults$spearman$notes), 0)
  expect_true(is.list(lResults$t$counts) && is.integer(lResults$chisq$counts))
  expect_identical(nrow(lResults$survival$rows), 2L)
  expect_identical(nrow(lResults$screen_hazard$rows), 12L)
  expect_identical(nrow(lResults$screen_one$rows), 1L)
  expect_identical(lResults$screen_too_small$status, "too_small")
})

test_that("the statistics file runs in a bare R session with only stats and survival attached, and gives the same results (#3, #4)", {
  strFrame <- tempfile(fileext = ".rds")
  strCalls <- tempfile(fileext = ".rds")
  strOut <- tempfile(fileext = ".rds")
  strScript <- tempfile(fileext = ".R")
  on.exit(unlink(c(strFrame, strCalls, strOut, strScript)))
  saveRDS(dfSyntheticFrame(), strFrame)
  saveRDS(lStatisticsCalls(), strCalls)
  # The child is given the file and the data and nothing else: it sources the
  # file at top level, as R in the browser would, and calls each function by
  # name with named arguments.
  writeLines(c(
    "chrArgs <- commandArgs(trailingOnly = TRUE)",
    "chrSearchBefore <- search()",
    "source(chrArgs[1])",
    "chrDefined <- ls()",
    "dfFrame <- readRDS(chrArgs[2])",
    "lCalls <- readRDS(chrArgs[3])",
    "lResults <- lapply(lCalls, function(lCall) do.call(lCall[[1]], c(list(dfFrame), lCall[[2]])))",
    "saveRDS(list(",
    "  search_before = chrSearchBefore, search_after = search(),",
    "  namespaces = loadedNamespaces(), defined = chrDefined, results = lResults",
    "), chrArgs[4])"
  ), strScript)

  chrOutput <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "--default-packages=stats,survival", shQuote(c(strScript, strStatisticsFile(), strFrame, strCalls, strOut))),
    stdout = TRUE, stderr = TRUE, env = "R_TESTS="
  ))
  expect_null(attr(chrOutput, "status"), label = paste(c("the bare session's exit status;", chrOutput), collapse = " "))
  # Nothing was printed: no warning escaped a function.
  expect_identical(as.character(chrOutput), character(0))
  expect_true(file.exists(strOut))
  lBare <- readRDS(strOut)

  # Only stats and survival were attached, before and after, and no other
  # package was loaded but the ones those two load themselves: not gsm.bio and
  # not testthat.
  chrBareSearch <- c(".GlobalEnv", "package:survival", "package:stats", "Autoloads", "package:base")
  expect_identical(lBare$search_before, chrBareSearch)
  expect_identical(lBare$search_after, chrBareSearch)
  expect_true(all(c("stats", "survival") %in% lBare$namespaces))
  expect_true(all(lBare$namespaces %in% c(
    "base", "stats", "utils", "graphics", "grDevices", "compiler", "methods",
    "survival", "splines", "Matrix", "lattice", "grid"
  )), label = paste("the bare session loaded only:", paste(lBare$namespaces, collapse = " ")))
  expect_false(any(c("gsm.bio", "testthat", "effectsize") %in% lBare$namespaces))
  expect_true(all(getNamespaceExports("gsm.bio") %in% lBare$defined))

  # The same answers as the package, to the last bit, for every method.
  lPackage <- lRunCalls(lStatisticsCalls(), dfSyntheticFrame(), asNamespace("gsm.bio"))
  expect_identical(names(lBare$results), names(lPackage))
  for (strCall in names(lPackage)) {
    expect_identical(lBare$results[[strCall]], lPackage[[strCall]], label = paste("the bare session's", strCall))
  }
})

test_that("the help pages state the minimum group size the file defines, and never call it agreed or validated (#3, #4)", {
  lRd <- if (bSourceTree()) tools::Rd_db(dir = strSourceRoot()) else tools::Rd_db("gsm.bio")
  nDefault <- get("nMinGroupDefault", envir = asNamespace("gsm.bio"))
  for (strTopic in chrStatisticsExports) {
    strText <- paste(as.character(lRd[[paste0(strTopic, ".Rd")]]), collapse = "")
    expect_match(strText, paste0("which is ", nDefault, "."), fixed = TRUE, label = strTopic)
  }
  strShape <- paste(as.character(lRd[["StatisticsResult.Rd"]]), collapse = "")
  expect_match(strShape, paste0("The default is ", nDefault, "."), fixed = TRUE)
  expect_match(strShape, "not an agreed or validated", fixed = TRUE)
  for (strMember in chrResultMembers) {
    expect_match(strShape, paste0("\\code{", strMember, "}"), fixed = TRUE, label = strMember)
  }
  # A result is the answer of the R that ran it, and the page says so.
  expect_match(strShape, "the answer of the R that computed it", fixed = TRUE)
})
