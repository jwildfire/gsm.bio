# Synthetic biomarker study: the planted effects and their true sizes

The three effects planted in the synthetic study, each with its true
size. This list is the generator's own parameter list: the script
`data-raw/synthetic-study.R` read these values to make the data, so a
test that asserts against them asserts against the truth and not a copy
of it.

## Usage

``` r
Synthetic_Truth
```

## Format

A list of four:

- Seed:

  `integer` The seed the study was generated with.

- GroupDifference:

  A list: `Biomarker`; `Visit`, the visit tests use, and
  `BaselineVisit`; `GroupCol`, the column of
  [Synthetic_Participants](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Participants.md)
  that holds the groups; `Groups`, in the order of the subtraction;
  `Estimand`, in words; and `Value`, the true difference.

- Correlation:

  A list: `Biomarkers`, the pair; `Visit`, the visit tests use;
  `Method`, `"pearson"`; `Estimand`; and `Value`, the true coefficient.

- Survival:

  A list: `Biomarker`; `Visit`; `Cut`, `"median"`; `Groups`, numerator
  first; `Endpoint`, the `PARAMCD` in
  [Synthetic_Outcomes](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Outcomes.md);
  `Estimand`; and `Value`, the true hazard ratio.

## Source

Written by `data-raw/synthetic-study.R`.

## Details

Each true size is an exact parameter of the generating model, stated in
the terms the matching statistic estimates:

|  |  |  |
|----|----|----|
| Effect | Where | Truth |
| Difference in mean change from Baseline, Treatment minus Placebo | IL-6, at each visit after Baseline | -1.5 pg/mL |
| Pearson correlation | TNF-alpha and IL-10, at each visit | 0.6 |
| Hazard ratio, high against low | Baseline CRP split at its median, event-free survival | 2.5 |

Every other biomarker is null by construction. The study was generated
once, with seed 364, and each truth lies inside the 95 percent interval
base R computes from the data.

## See also

[Synthetic_Results](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Results.md),
[Synthetic_Participants](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Participants.md),
[Synthetic_Outcomes](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Outcomes.md)
