# gsm.bio

The statistics behind the bio.viz biomarker charts: plain R functions that wrap the stats and survival packages, and a seeded synthetic study to prove them on. `dev` is the integration branch (merges on green checks); `main` is the release branch (@jwildfire's review).

# Standards
The obot program's standards are mandatory here: the issue contract, ways of working and
developer guidelines in jwildfire/obot.roadmap `docs/` (on disk at ~/obot.roadmap/docs/
in a cloud environment). Work runs one requirement per session (`/requirement-session <hub requirement>`).

# Commands

- `devtools::document()` — regenerate `man/` and `NAMESPACE` after any roxygen change; it must leave no diff.
- `devtools::test()` — the testthat suite (edition 3). Tests that read files outside the built package (the repository guards, the rerun of the data script) run here and skip under check; CI runs the suite a second time from the source tree, where a skip fails the run.
- `devtools::check()` — must be clean (no errors, warnings or notes) before a PR opens; CI runs the same check as `R-CMD-check` and fails on a note. Where R cannot reach its time service the check adds one note, `unable to verify current time`, which is about the machine and not the package: run it with `_R_CHECK_SYSTEM_CLOCK_=0` set, as CI does.
- `Rscript data-raw/synthetic-study.R` — regenerates the synthetic study: `data/Synthetic_*.rda` and `inst/extdata/synthetic_*.csv`. It is seeded, so it must leave no diff unless the model changed.
- `pkgdown::build_site()` — builds the reference site into `docs/` (not committed); CI deploys it from `dev` to the `gh-pages` branch.

# Conventions

- Every `test_that()` name ends with the issue it proves, `(#N)`; `tests/testthat/test-qcthat-convention.R` fails the suite otherwise.
- Imports are stats and survival and nothing else; `tests/testthat/test-package.R` fails the suite otherwise. A package needed only to check a result goes under Suggests.
- A statistic is a thin wrapper around the base R function the design names. Nothing is reimplemented except the standardised difference.
- Argument and variable prefixes follow gsm.core: `df` data frame, `l` list, `str` character scalar, `chr` character vector, `n` numeric, `b` logical.
- Public or synthetic data only.
- `Synthetic_Truth` is the data generator's own parameter list. A test of a planted effect reads the truth from it and never types the number again.
- `NEWS.md` is always current on `dev`: unreleased work goes under the `vX.Y.Z (Upcoming)` heading as it lands, one user-facing bullet per feature linking its hub requirement and PR.
- One branch per task, `<task-number>-<slug>`, off `dev`; the PR body carries `Closes #<task>` and the definition-of-done evidence.
