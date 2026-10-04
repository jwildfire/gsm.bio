# each figure draws what its snapshot holds (#37)

    Code
      writeLines(strFigureStable(chrFigureDescription(gg)))
    Output
      title: IL-6: Change from baseline by Arm
      subtitle: 200 participants, at Week 4, Week 8
      x: Arm
      y: IL-6, change from baseline (pg/mL)
      caption: Synthetic study from gsm.bio. Filters: none. Drawn on <date> by gsm.bio <version>. Statistics: Welch Two Sample t-test (Placebo n = 95, Treatment n = 91); Welch Two Sample t-test (Placebo n = 93, Treatment n = 95); computed by R <version> with gsm.bio <version>.
      data:
        rows: 374
        id: 200 distinct values, 0 NA
        y: 0 NA; -4.541 / 4.217 / -0.5901 / -220.7
        x: Placebo x188 | Treatment x186
        heading: Week 4 Welch Two Sample t-test: p < 0.001 (Placebo n = 95, Treatment n = 91). Exploratory, unadjusted. Difference in means (Placebo - Treatment): 1.235, 95% confidence interval 0.844 to 1.626. x186 | Week 8 Welch Two Sample t-test: p < 0.001 (Placebo n = 93, Treatment n = 95). Exploratory, unadjusted. Difference in means (Placebo - Treatment): 1.358, 95% confidence interval 0.9882 to 1.729. x188
      layer 1: GeomBoxplot
        (the plot's data)
      layer 2: GeomPoint
        (the plot's data)

---

    Code
      writeLines(strFigureStable(chrFigureDescription(gg)))
    Output
      title: IL-10 at Baseline (pg/mL) against TNF-alpha at Baseline (pg/mL)
      subtitle: 200 participants
      x: TNF-alpha at Baseline (pg/mL)
      y: IL-10 at Baseline (pg/mL)
      caption: Pearson's product-moment correlation: p < 0.001 (n = 200). Exploratory, unadjusted. Pearson<U+2019>s r: 0.6384, 95% confidence interval 0.5482 to 0.7139. Linear regression: p < 0.001 (n = 200). Exploratory, unadjusted. Slope: 0.3275, 95% confidence interval 0.2721 to 0.3828. Intercept: 2.028, 95% confidence interval 1.356 to 2.7. R-squared: 0.4075. Drawn on <date> by gsm.bio <version>. Statistics: Pearson's product-moment correlation (n = 200); Linear regression (n = 200); computed by R <version> with gsm.bio <version>.
      data:
        rows: 200
        id: 200 distinct values, 0 NA
        x: 0 NA; 2.938 / 17.96 / 11.79 / 2357
        y: 0 NA; 1.048 / 10.03 / 5.887 / 1177
        colour: All x200
        panel:  x200
      layer 1: GeomRibbon
        rows: 50
        x: 0 NA; 2.938 / 17.96 / 10.45 / 522.5
        fit: 0 NA; 2.99 / 7.911 / 5.45 / 272.5
        lower: 0 NA; 2.475 / 7.532 / 5.165 / 258.3
        upper: 0 NA; 3.506 / 8.289 / 5.735 / 286.8
        colour: All x50
        panel:  x50
      layer 2: GeomLine
        rows: 50
        x: 0 NA; 2.938 / 17.96 / 10.45 / 522.5
        fit: 0 NA; 2.99 / 7.911 / 5.45 / 272.5
        lower: 0 NA; 2.475 / 7.532 / 5.165 / 258.3
        upper: 0 NA; 3.506 / 8.289 / 5.735 / 286.8
        colour: All x50
        panel:  x50
      layer 3: GeomPoint
        (the plot's data)

---

    Code
      writeLines(strFigureStable(chrFigureDescription(gg)))
    Output
      title: Result at Baseline, biomarker against biomarker
      subtitle: 12 biomarkers, 200 participants
      caption: Pearson's product-moment correlation, pair by pair: 66 pairs of 12 variables, each on the participants who have both of its values. Drawn on <date> by gsm.bio <version>. Statistics: Pearson's product-moment correlation (n = 200 across 12 variables); computed by R <version> with gsm.bio <version>.
      data:
        rows: 132
        x: CRP x11 | D-dimer x11 | Ferritin x11 | IFN-gamma x11 | IL-1beta x11 | IL-2 x11 | IL-6 x11 | IL-8 x11 | IL-10 x11 | LDH x11 | TNF-alpha x11 | VEGF x11
        y: VEGF x11 | TNF-alpha x11 | LDH x11 | IL-10 x11 | IL-8 x11 | IL-6 x11 | IL-2 x11 | IL-1beta x11 | IFN-gamma x11 | Ferritin x11 | D-dimer x11 | CRP x11
        estimate: 0 NA; -0.1824 / 0.6384 / -0.003785 / -0.4996
        label: 32 distinct values, 0 NA
      layer 1: GeomTile
        (the plot's data)
      layer 2: GeomText
        (the plot's data)

---

    Code
      writeLines(strFigureStable(chrFigureDescription(gg)))
    Output
      title: Change from baseline at Week 4: Placebo against Treatment, standardised difference
      subtitle: 12 biomarkers, 187 participants
      x: Standardised difference (Hedges<U+2019> g)
      caption: Welch Two Sample t-test, one row per biomarker: 12 of 12 computed. The adjusted p-values are adjusted by Benjamini-Hochberg across the 12 biomarkers that have a p-value. Each row: Standardised difference (Hedges<U+2019> g), Placebo less Treatment, with its 95% confidence interval on one axis without units. p: Welch Two Sample t-test, unadjusted, and adjusted by Benjamini-Hochberg across the 12 biomarkers with a p-value. Exploratory, adjusted (Benjamini-Hochberg). Drawn on <date> by gsm.bio <version>. Statistics: Welch Two Sample t-test (n = 179 to 186 across 12 biomarkers); computed by R <version> with gsm.bio <version>.
      data:
        rows: 12
        biomarker: CRP x1 | D-dimer x1 | Ferritin x1 | IFN-gamma x1 | IL-10 x1 | IL-1beta x1 | IL-2 x1 | IL-6 x1 | IL-8 x1 | LDH x1 | TNF-alpha x1 | VEGF x1
        estimate: 0 NA; -0.1621 / 0.9133 / 0.1349 / 1.619
        lower: 0 NA; -0.451 / 0.6111 / -0.1549 / -1.858
        upper: 0 NA; 0.1272 / 1.213 / 0.4244 / 5.092
        said: -0.09193 (-0.379 to 0.1954); Unadjusted: p = 0.530; Benjamini-Hochberg: p = 0.636; n, Placebo / Treatment: 94 / 91 x1 | -0.09502 (-0.3836 to 0.1939); Unadjusted: p = 0.520; Benjamini-Hochberg: p = 0.636; n, Placebo / Treatment: 91 / 92 x1 | -0.1621 (-0.451 to 0.1272); Unadjusted: p = 0.272; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 93 / 90 x1 | 0.005833 (-0.2805 to 0.2921); Unadjusted: p = 0.968; Benjamini-Hochberg: p = 0.968; n, Placebo / Treatment: 95 / 91 x1 | 0.06825 (-0.2197 to 0.3561); Unadjusted: p = 0.642; Benjamini-Hochberg: p = 0.700; n, Placebo / Treatment: 93 / 91 x1 | 0.1288 (-0.1587 to 0.416); Unadjusted: p = 0.383; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 94 / 91 x1 | 0.1298 (-0.1577 to 0.417); Unadjusted: p = 0.378; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 94 / 91 x1 | 0.1342 (-0.1542 to 0.4222); Unadjusted: p = 0.362; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 94 / 90 x1 | 0.1544 (-0.1356 to 0.4441); Unadjusted: p = 0.297; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 93 / 89 x1 | 0.1746 (-0.118 to 0.4667); Unadjusted: p = 0.243; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 90 / 89 x1 | 0.2589 (-0.03126 to 0.5484); Unadjusted: p = 0.080; Benjamini-Hochberg: p = 0.481; n, Placebo / Treatment: 93 / 90 x1 | 0.9133 (0.6111 to 1.213); Unadjusted: p < 0.001; Benjamini-Hochberg: p < 0.001; n, Placebo / Treatment: 95 / 91 x1
        row: TNF-alpha   -0.1621 (-0.451 to 0.1272); Unadjusted: p = 0.272; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 93 / 90 x1 | IL-10   -0.09502 (-0.3836 to 0.1939); Unadjusted: p = 0.520; Benjamini-Hochberg: p = 0.636; n, Placebo / Treatment: 91 / 92 x1 | CRP   -0.09193 (-0.379 to 0.1954); Unadjusted: p = 0.530; Benjamini-Hochberg: p = 0.636; n, Placebo / Treatment: 94 / 91 x1 | IL-2   0.005833 (-0.2805 to 0.2921); Unadjusted: p = 0.968; Benjamini-Hochberg: p = 0.968; n, Placebo / Treatment: 95 / 91 x1 | Ferritin   0.06825 (-0.2197 to 0.3561); Unadjusted: p = 0.642; Benjamini-Hochberg: p = 0.700; n, Placebo / Treatment: 93 / 91 x1 | IFN-gamma   0.1288 (-0.1587 to 0.416); Unadjusted: p = 0.383; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 94 / 91 x1 | IL-8   0.1298 (-0.1577 to 0.417); Unadjusted: p = 0.378; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 94 / 91 x1 | LDH   0.1342 (-0.1542 to 0.4222); Unadjusted: p = 0.362; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 94 / 90 x1 | VEGF   0.1544 (-0.1356 to 0.4441); Unadjusted: p = 0.297; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 93 / 89 x1 | D-dimer   0.1746 (-0.118 to 0.4667); Unadjusted: p = 0.243; Benjamini-Hochberg: p = 0.575; n, Placebo / Treatment: 90 / 89 x1 | IL-1beta   0.2589 (-0.03126 to 0.5484); Unadjusted: p = 0.080; Benjamini-Hochberg: p = 0.481; n, Placebo / Treatment: 93 / 90 x1 | IL-6   0.9133 (0.6111 to 1.213); Unadjusted: p < 0.001; Benjamini-Hochberg: p < 0.001; n, Placebo / Treatment: 95 / 91 x1
      layer 1: GeomVline
        rows: 1
        xintercept: 0 NA; 0 / 0 / 0 / 0
      layer 2: GeomErrorbar
        (the plot's data)
      layer 3: GeomPoint
        (the plot's data)

---

    Code
      writeLines(strFigureStable(chrFigureDescription(gg)))
    Output
      title: Response by CRP at Baseline, cut at the median
      subtitle: 200 participants
      x: Percent of the row
      y: Response
      caption: Pearson's Chi-squared test with Yates' continuity correction: p = 0.305 (n = 200). Exploratory, unadjusted. Drawn on <date> by gsm.bio <version>. Statistics: Pearson's Chi-squared test with Yates' continuity correction (n = 200); computed by R <version> with gsm.bio <version>.
      data:
        rows: 4
        row: Responder x2 | Non-responder x2
        col: <U+2264> 2.783 x2 | > 2.783 x2
        n: 0 NA; 33 / 67 / 50 / 200
        share: 0 NA; 44.59 / 55.41 / 50 / 200
        label: 33 x1 | 41 x1 | 59 x1 | 67 x1
      layer 1: GeomCol
        (the plot's data)
      layer 2: GeomText
        (the plot's data)

---

    Code
      writeLines(strFigureStable(chrFigureDescription(gg)))
    Output
      title: Event-free survival (months) by CRP at Baseline, cut at the median
      subtitle: 200 participants
      x: Event-free survival (months)
      y: Kaplan-Meier estimate
      caption: Log-rank test: p < 0.001 (> 2.783 n = 100, <U+2264> 2.783 n = 100). Exploratory, unadjusted. Median (<U+2264> 2.783): 23.32, 95% confidence interval 17.32 to not reached. Median (> 2.783): 8.28, 95% confidence interval 5.24 to 9.71. Hazard ratio, high over low (> 2.783 / <U+2264> 2.783): 3.523, 95% confidence interval 2.43 to 5.107. Drawn on <date> by gsm.bio <version>. Statistics: Log-rank test (> 2.783 n = 100, <U+2264> 2.783 n = 100); computed by R <version> with gsm.bio <version>.
      data:
        rows: 163
        group: <U+2264> 2.783 x69 | > 2.783 x94
        time: 0 NA; 0 / 24 / 9.056 / 1476
        surv: 0 NA; 0.0654 / 1 / 0.6212 / 101.3
        lower: 0 NA; 0.0268 / 1 / 0.5321 / 86.73
        upper: 0 NA; 0.1281 / 1 / 0.6945 / 113.2
        censored: 0 NA; 0 / 31 / 0.411 / 67
      layer 1: GeomRibbon
        rows: 324
        group: <U+2264> 2.783 x137 | > 2.783 x187
        time: 0 NA; 0 / 24 / 9.112 / 2952
        lower: 0 NA; 0.0268 / 1 / 0.5342 / 173.1
        upper: 0 NA; 0.1281 / 1 / 0.6966 / 225.7
      layer 2: GeomStep
        (the plot's data)
      layer 3: GeomPoint
        rows: 33
        group: <U+2264> 2.783 x26 | > 2.783 x7
        time: 0 NA; 0.14 / 24 / 11.47 / 378.7
        surv: 0 NA; 0.0654 / 0.99 / 0.6653 / 21.95
        lower: 0 NA; 0.0268 / 0.9311 / 0.5714 / 18.86
        upper: 0 NA; 0.1281 / 0.9986 / 0.7393 / 24.4
        censored: 0 NA; 1 / 31 / 2.03 / 67

