# gsm.bio: statistics for biomarker charts

The statistics behind the bio.viz biomarker charts. A chart never
computes a test: it hands over a table with one row per participant and
receives the estimates, intervals, p-value, method name and counts that
R worked out.

## Design

Each statistic is a thin wrapper that fixes the inputs and the shape of
the answer around a function from the stats or survival package. Nothing
is reimplemented, and nothing from any other package is used, so the
same source runs in a desktop session, in R in the browser and on a
server.

The tests the design names are imported here, once, for the functions
that wrap them:
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html),
[`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html),
[`stats::aov()`](https://rdrr.io/r/stats/aov.html),
[`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html),
[`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html),
[`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html),
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) and
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) from stats;
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html),
[`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html),
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
and [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
from survival.

## Status

Version 0.1.0 is in development. This first step sets up the package;
the statistics functions and a synthetic biomarker study with known
planted effects follow in the same version.

## See also

Useful links:

- <https://github.com/jwildfire/gsm.bio>

- <https://jwildfire.github.io/gsm.bio/>

- Report bugs at <https://github.com/jwildfire/gsm.bio/issues>

## Author

**Maintainer**: obot <obot@users.noreply.github.com>
