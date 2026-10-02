# gsm.bio

`gsm.bio` holds the statistics behind the [bio.viz](https://github.com/jwildfire/bio.viz) biomarker charts. A chart never computes a test: it hands over a table with one row per participant, and R returns the estimates, intervals, p-value, method name and counts the chart prints.

Each statistic is a thin wrapper around a function from the stats or survival package, so the number on the chart is R's own. The package imports nothing else, which lets the same functions run in a desktop session, in R in the browser and on a server.

## Installation

Version 0.1.0 is in development on the `dev` branch:

```r
# install.packages("remotes")
remotes::install_github("jwildfire/gsm.bio@dev")
```

## Status

The package is set up and installs; the statistics functions and a seeded synthetic biomarker study arrive next, in the same version. [NEWS.md](https://github.com/jwildfire/gsm.bio/blob/dev/NEWS.md) lists what has landed, and the reference site is at <https://jwildfire.github.io/gsm.bio/>.

The design is on the obot roadmap: [bio.viz and gsm.bio](https://jwildfire.github.io/obot.roadmap/requirements/design/353_design.html).

## Licence

Apache License 2.0. See [LICENSE](https://github.com/jwildfire/gsm.bio/blob/dev/LICENSE).
