<!--
NEWS.md is the running release log and the draft of each release's notes:
newest section first; unreleased work accumulates under a vX.Y.Z (Upcoming)
heading that loses the suffix when the release is cut; the GitHub release
publishes from the section verbatim.
-->

# gsm.bio v0.1.0 (Upcoming)

The first version of gsm.bio: the statistics the bio.viz biomarker charts print, computed by R, and a synthetic study to prove them on. It is being built on `dev` one step at a time, and this section grows as each step lands. The demo page is added here when the release is prepared.

## What's new

- **gsm.bio installs from GitHub.** `remotes::install_github("jwildfire/gsm.bio@dev")` installs a package that loads and depends on nothing but the stats and survival packages. It holds no statistics function and no data yet; those are the next steps of this version. ([obot.roadmap#364](https://github.com/jwildfire/obot.roadmap/issues/364), [#1](https://github.com/jwildfire/gsm.bio/issues/1))

## Also in this release

- **Checks on every pull request.** `R CMD check` runs on each pull request to `dev` and fails on an error, a warning or a note. ([#1](https://github.com/jwildfire/gsm.bio/issues/1))
- **A reference site.** The pkgdown site is built on each pull request and deployed from `dev` to <https://jwildfire.github.io/gsm.bio/>. ([#1](https://github.com/jwildfire/gsm.bio/issues/1))
- **Tests name the issue they prove.** Every test name ends with its issue number, and a guard test fails the suite when one does not. ([#1](https://github.com/jwildfire/gsm.bio/issues/1))
