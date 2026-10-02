# Generate the synthetic biomarker study.
#
# Everything here is made up. No real study data is read, and nothing is
# copied or adapted from any real study: the participants, the values and the
# outcomes are random draws from the model below, and the biomarker names are
# generic analyte names with levels chosen only to look plausible.
#
# Three effects are planted, each an exact parameter of the generating model
# (lPlanted, below), and every other biomarker is null by construction. The
# same list ships as the package data Synthetic_Truth, so a test reads the
# truth from where the generator read it.
#
# From the package root:
#
#   Rscript data-raw/synthetic-study.R
#
# writes data/Synthetic_*.rda and inst/extdata/synthetic_*.csv. The script uses
# base R and stats only. Define strOutputRoot before sourcing it to write the
# same files under another directory, which is how the test suite reruns it.

# ---- The planted effects: generator parameters and documented truth --------

lPlanted <- list(
  Seed = 364L,
  GroupDifference = list(
    Biomarker = "IL-6",
    Visit = "Week 4",
    BaselineVisit = "Baseline",
    GroupCol = "ARM",
    Groups = c("Treatment", "Placebo"),
    Estimand = "Difference in mean change from baseline, Treatment minus Placebo",
    Value = -1.5
  ),
  Correlation = list(
    Biomarkers = c("TNF-alpha", "IL-10"),
    Visit = "Baseline",
    Method = "pearson",
    Estimand = "Pearson correlation between the two biomarkers",
    Value = 0.6
  ),
  Survival = list(
    Biomarker = "CRP",
    Visit = "Baseline",
    Cut = "median",
    Groups = c("High", "Low"),
    Endpoint = "EFS",
    Estimand = "Hazard ratio, high against low baseline level",
    Value = 2.5
  )
)

# ---- The rest of the model -------------------------------------------------

lModel <- list(
  nParticipants = 200L,
  chrVisits = c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12"),
  nVisitNum = c(0L, 2L, 4L, 8L, 12L),
  # Share of a biomarker's variance that is between participants; the rest is
  # between visits within a participant.
  nBetweenShare = 0.6,
  # After Baseline: a participant misses a whole visit, or one result is not
  # analysable and is carried as a missing value. Baseline is complete.
  nMissedVisit = 0.05,
  nMissingResult = 0.02,
  # Event-free survival, in months: the hazard in the low group, the dropout
  # hazard, and the administrative cut-off.
  nBaseHazard = log(2) / 18,
  nDropoutHazard = log(2) / 48,
  nFollowUp = 24,
  nValueDigits = 3L,
  nTimeDigits = 2L
)

# One row per biomarker. A "normal" biomarker is Location + Scale * z; a
# "lognormal" one is Location * exp(Scale * z), so Location is its median.
dfPanel <- data.frame(
  TEST = c(
    "IL-6", "CRP", "TNF-alpha", "IL-10", "IFN-gamma", "IL-2",
    "IL-1beta", "IL-8", "VEGF", "Ferritin", "D-dimer", "LDH"
  ),
  STRESU = c(
    "pg/mL", "mg/L", "pg/mL", "pg/mL", "pg/mL", "pg/mL",
    "pg/mL", "pg/mL", "pg/mL", "ng/mL", "mg/L", "U/L"
  ),
  Distribution = c(
    "normal", "lognormal", "normal", "normal", "lognormal", "normal",
    "normal", "lognormal", "normal", "lognormal", "lognormal", "normal"
  ),
  Location = c(8, 3, 12, 6, 4, 5, 2.5, 15, 250, 120, 0.4, 180),
  Scale = c(1.5, 0.6, 3, 1.5, 0.5, 1, 0.5, 0.45, 50, 0.5, 0.4, 25),
  stringsAsFactors = FALSE
)

# Round through the decimal text, so a stored number is exactly the double a
# CSV reader gets back from the file.
RoundTo <- function(nValue, nDigits) {
  as.numeric(formatC(nValue, digits = nDigits, format = "f"))
}

MakeSyntheticStudy <- function(lPlanted, lModel, dfPanel) {
  # Pin every generator, so the draws are the same on every R version, and put
  # the session's own generator back afterwards.
  chrKind <- RNGkind()
  nOldSeed <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  }
  on.exit({
    RNGkind(chrKind[1], chrKind[2], chrKind[3])
    if (is.null(nOldSeed)) {
      # No generator state before: leave none, so the session seeds itself.
      rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", nOldSeed, envir = globalenv())
    }
  })
  set.seed(
    lPlanted$Seed,
    kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection"
  )

  nParticipants <- lModel$nParticipants
  nVisits <- length(lModel$chrVisits)
  nBiomarkers <- nrow(dfPanel)

  # ---- Participants: nothing here is related to any biomarker or outcome ---
  dfParticipants <- data.frame(
    USUBJID = sprintf("BIO-%03d", seq_len(nParticipants)),
    ARM = sample(rep(c("Placebo", "Treatment"), each = nParticipants / 2)),
    SEX = ifelse(stats::runif(nParticipants) < 0.5, "F", "M"),
    AGE = as.integer(pmin(pmax(round(stats::rnorm(nParticipants, 55, 12)), 18), 85)),
    BMIBL = RoundTo(pmax(stats::rnorm(nParticipants, 27, 4), 16), 1),
    RESPONSE = ifelse(stats::runif(nParticipants) < 0.4, "Responder", "Non-responder"),
    stringsAsFactors = FALSE
  )

  # ---- Results: standard normal scores, then each biomarker's own scale ----
  # mBetween is one draw per participant and biomarker; aWithin is one per
  # participant, visit and biomarker.
  mBetween <- matrix(stats::rnorm(nParticipants * nBiomarkers), nParticipants, nBiomarkers)
  aWithin <- array(
    stats::rnorm(nParticipants * nVisits * nBiomarkers),
    c(nParticipants, nVisits, nBiomarkers)
  )

  # Planted correlation. The second biomarker's scores are rho times the
  # first's plus an independent part, between and within participants alike,
  # so the two are bivariate normal with correlation rho at every visit.
  nRho <- lPlanted$Correlation$Value
  iFirst <- match(lPlanted$Correlation$Biomarkers[1], dfPanel$TEST)
  iSecond <- match(lPlanted$Correlation$Biomarkers[2], dfPanel$TEST)
  mBetween[, iSecond] <- nRho * mBetween[, iFirst] + sqrt(1 - nRho^2) * mBetween[, iSecond]
  aWithin[, , iSecond] <- nRho * aWithin[, , iFirst] + sqrt(1 - nRho^2) * aWithin[, , iSecond]

  aValue <- array(NA_real_, c(nParticipants, nVisits, nBiomarkers))
  for (iBiomarker in seq_len(nBiomarkers)) {
    mScore <- sqrt(lModel$nBetweenShare) * mBetween[, iBiomarker] +
      sqrt(1 - lModel$nBetweenShare) * aWithin[, , iBiomarker]
    aValue[, , iBiomarker] <- if (dfPanel$Distribution[iBiomarker] == "normal") {
      dfPanel$Location[iBiomarker] + dfPanel$Scale[iBiomarker] * mScore
    } else {
      dfPanel$Location[iBiomarker] * exp(dfPanel$Scale[iBiomarker] * mScore)
    }
  }

  # Planted group difference. One arm's values move by a constant at every
  # visit after Baseline, so the difference between the arms in mean change
  # from Baseline is that constant, whatever the visit.
  lGroup <- lPlanted$GroupDifference
  iShifted <- match(lGroup$Biomarker, dfPanel$TEST)
  bShifted <- dfParticipants[[lGroup$GroupCol]] == lGroup$Groups[1]
  iPostBaseline <- which(lModel$chrVisits != lGroup$BaselineVisit)
  aValue[bShifted, iPostBaseline, iShifted] <- aValue[bShifted, iPostBaseline, iShifted] + lGroup$Value

  aValue[] <- RoundTo(aValue, lModel$nValueDigits)

  # Documented imperfections, completely at random and never at Baseline.
  mMissed <- matrix(FALSE, nParticipants, nVisits)
  mMissed[, iPostBaseline] <- stats::runif(nParticipants * length(iPostBaseline)) < lModel$nMissedVisit
  aMissing <- array(FALSE, c(nParticipants, nVisits, nBiomarkers))
  aMissing[, iPostBaseline, ] <-
    stats::runif(nParticipants * length(iPostBaseline) * nBiomarkers) < lModel$nMissingResult

  # One row per participant, biomarker and visit, participants outermost.
  dfGrid <- expand.grid(
    iVisit = seq_len(nVisits),
    iBiomarker = seq_len(nBiomarkers),
    iParticipant = seq_len(nParticipants)
  )
  mIndex <- cbind(dfGrid$iParticipant, dfGrid$iVisit, dfGrid$iBiomarker)
  dfResults <- data.frame(
    USUBJID = dfParticipants$USUBJID[dfGrid$iParticipant],
    VISIT = lModel$chrVisits[dfGrid$iVisit],
    VISITNUM = lModel$nVisitNum[dfGrid$iVisit],
    TEST = dfPanel$TEST[dfGrid$iBiomarker],
    STRESU = dfPanel$STRESU[dfGrid$iBiomarker],
    STRESN = ifelse(aMissing[mIndex], NA_real_, aValue[mIndex]),
    stringsAsFactors = FALSE
  )
  dfResults <- dfResults[!mMissed[cbind(dfGrid$iParticipant, dfGrid$iVisit)], ]
  rownames(dfResults) <- NULL

  # ---- Outcomes ------------------------------------------------------------
  # Planted survival difference. The groups are the stored Baseline values of
  # one biomarker split at their median, the cut a chart would make; event
  # times are exponential, with the high group's hazard a constant multiple of
  # the low group's. Dropout and the end of follow-up censor independently.
  lSurvival <- lPlanted$Survival
  nBaseline <- aValue[
    , match(lSurvival$Visit, lModel$chrVisits), match(lSurvival$Biomarker, dfPanel$TEST)
  ]
  bHigh <- nBaseline > stats::median(nBaseline)
  nHazard <- lModel$nBaseHazard * ifelse(bHigh, lSurvival$Value, 1)
  nEventTime <- -log(stats::runif(nParticipants)) / nHazard
  nDropoutTime <- -log(stats::runif(nParticipants)) / lModel$nDropoutHazard
  nCensorTime <- pmin(nDropoutTime, lModel$nFollowUp)
  dfOutcomes <- data.frame(
    USUBJID = dfParticipants$USUBJID,
    PARAMCD = lSurvival$Endpoint,
    PARAM = "Event-free survival (months)",
    AVAL = RoundTo(pmin(nEventTime, nCensorTime), lModel$nTimeDigits),
    CNSR = as.integer(nEventTime > nCensorTime),
    stringsAsFactors = FALSE
  )

  list(
    Results = dfResults,
    Participants = dfParticipants,
    Outcomes = dfOutcomes,
    Truth = lPlanted
  )
}

lStudy <- MakeSyntheticStudy(lPlanted, lModel, dfPanel)

# ---- Write the package data and the CSV files ------------------------------

strRoot <- if (exists("strOutputRoot")) strOutputRoot else "."
dir.create(file.path(strRoot, "data"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(strRoot, "inst", "extdata"), showWarnings = FALSE, recursive = TRUE)

Synthetic_Results <- lStudy$Results
Synthetic_Participants <- lStudy$Participants
Synthetic_Outcomes <- lStudy$Outcomes
Synthetic_Truth <- lStudy$Truth

for (strName in c("Synthetic_Results", "Synthetic_Participants", "Synthetic_Outcomes", "Synthetic_Truth")) {
  save(
    list = strName,
    file = file.path(strRoot, "data", paste0(strName, ".rda")),
    compress = "xz",
    version = 3
  )
}

# The CSV files are for bio.viz to vendor: plain, unquoted, a missing value as
# an empty field.
for (strTable in c("Results", "Participants", "Outcomes")) {
  dfTable <- lStudy[[strTable]]
  bText <- vapply(dfTable, is.character, logical(1))
  stopifnot(!any(grepl("[,\"\n]", unlist(dfTable[bText]))))
  utils::write.csv(
    dfTable,
    file.path(strRoot, "inst", "extdata", paste0("synthetic_", tolower(strTable), ".csv")),
    row.names = FALSE,
    quote = FALSE,
    na = "",
    eol = "\n"
  )
}
