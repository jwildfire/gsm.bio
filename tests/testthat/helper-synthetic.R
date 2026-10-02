# The synthetic study in the forms the tests need. Nothing here is exported.

# One value per participant, in the participant table's order.
nResultAt <- function(strBiomarker, strVisit) {
  dfRows <- Synthetic_Results[
    Synthetic_Results$TEST == strBiomarker & Synthetic_Results$VISIT == strVisit,
  ]
  dfRows$STRESN[match(Synthetic_Participants$USUBJID, dfRows$USUBJID)]
}

# The one-row-per-participant frame a chart hands to a statistics function: the
# participant table, every biomarker at Baseline and at Week 4 as a column
# named "<biomarker> @ <visit>", the change the planted group difference is
# stated in, and a four-level group.
dfSyntheticFrame <- function() {
  dfFrame <- Synthetic_Participants
  for (strBiomarker in unique(Synthetic_Results$TEST)) {
    for (strVisit in c("Baseline", "Week 4")) {
      dfFrame[[paste(strBiomarker, "@", strVisit)]] <- nResultAt(strBiomarker, strVisit)
    }
  }
  lGroup <- Synthetic_Truth$GroupDifference
  dfFrame$Change <- nResultAt(lGroup$Biomarker, lGroup$Visit) -
    nResultAt(lGroup$Biomarker, lGroup$BaselineVisit)
  dfFrame$ARM_SEX <- paste(dfFrame$ARM, dfFrame$SEX)
  dfFrame
}

# Run a base R call and keep the warnings it raises, to compare with the ones
# a statistics function captured.
lWithWarnings <- function(fnCall) {
  chrWarnings <- character(0)
  xValue <- withCallingHandlers(fnCall(), warning = function(cndWarning) {
    chrWarnings <<- c(chrWarnings, conditionMessage(cndWarning))
    invokeRestart("muffleWarning")
  })
  list(value = xValue, warnings = as.list(unique(chrWarnings)))
}
