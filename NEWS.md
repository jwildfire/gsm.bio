<!--
NEWS.md is the running release log and the draft of each release's notes:
newest section first; unreleased work accumulates under a vX.Y.Z (Upcoming)
heading that loses the suffix when the release is cut; the GitHub release
publishes from the section verbatim.
-->

# gsm.bio v0.1.0 (Upcoming)

The first version of gsm.bio: the statistics the bio.viz biomarker charts print, computed by R, and a synthetic study to prove them on. It is being built on `dev` one step at a time, and this section grows as each step lands. The demo page is added here when the release is prepared.

## What's new

- **gsm.bio installs from GitHub.** `remotes::install_github("jwildfire/gsm.bio@dev")` installs a package that loads and depends on nothing but the stats and survival packages. The statistics functions are the next steps of this version. ([obot.roadmap#364](https://github.com/jwildfire/obot.roadmap/issues/364), [#1](https://github.com/jwildfire/gsm.bio/issues/1), [#5](https://github.com/jwildfire/gsm.bio/pull/5))
- **A synthetic biomarker study with known answers.** `Synthetic_Results`, `Synthetic_Participants` and `Synthetic_Outcomes` are a made-up study of 200 participants, twelve biomarkers and five visits, with one endpoint. Three effects are planted and every other biomarker is null: IL-6 changes from baseline by 1.5 pg/mL less in the Treatment arm than in Placebo, TNF-alpha and IL-10 have a Pearson correlation of 0.6, and participants with high baseline CRP have 2.5 times the hazard of those with low. `Synthetic_Truth` holds each true size, so a test can check that a statistic recovers it. The tables are also CSV files under `inst/extdata/` for the charts to use. No real study data is used. ([obot.roadmap#364](https://github.com/jwildfire/obot.roadmap/issues/364), [#2](https://github.com/jwildfire/gsm.bio/issues/2), [#6](https://github.com/jwildfire/gsm.bio/pull/6))

## Also in this release

- **Checks on every pull request.** `R CMD check` runs on each pull request to `dev` and fails on an error, a warning or a note. ([#1](https://github.com/jwildfire/gsm.bio/issues/1), [#5](https://github.com/jwildfire/gsm.bio/pull/5))
- **A reference site.** The pkgdown site is built on each pull request and deployed from `dev` to <https://jwildfire.github.io/gsm.bio/>. ([#1](https://github.com/jwildfire/gsm.bio/issues/1), [#5](https://github.com/jwildfire/gsm.bio/pull/5))
- **The suite also runs from the source tree in CI.** `R CMD check` cannot see the data script or the repository's own files, so the check job runs the tests a second time where they can, and fails if any is skipped. ([#2](https://github.com/jwildfire/gsm.bio/issues/2), [#6](https://github.com/jwildfire/gsm.bio/pull/6))
- **Tests name the issue they prove.** Every test name ends with its issue number, and a guard test fails the suite when one does not. ([#1](https://github.com/jwildfire/gsm.bio/issues/1), [#5](https://github.com/jwildfire/gsm.bio/pull/5))
