# Correlation Matrix Table

The coefficients of the grid the correlation matrix opens on, as a
table: one row per pair of variables, with R's coefficient and its
interval and the pair's own count. A matrix reports no p-values, so none
is written. The numbers are
[`Analyze_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Analyze_CorrelationMatrix.md)'s
on the frame the chart hands R.

## Usage

``` r
Table_CorrelationMatrix(dfResults, dfParticipants = NULL, lSettings = list())
```

## Arguments

- dfResults:

  `data.frame` Long-format results, one row per participant, biomarker
  and visit. Column names are supplied by `lSettings`; the defaults
  expect `USUBJID`/`TEST`/`STRESN`/`VISIT`/`VISITNUM`/`STRESU`, the
  columns of
  [Synthetic_Results](https://jwildfire.github.io/gsm.bio/reference/Synthetic_Results.md).

- dfParticipants:

  `data.frame` One row per participant, or `NULL`. With it the chart has
  filters; it also says who the participants are. Default: `NULL`.

- lSettings:

  `list` bio.viz correlation matrix settings, as
  [`Widget_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CorrelationMatrix.md)
  takes them, and `title`, `subtitle` and `footnotes`. Default:
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A `data.frame` of text: `Variable 1` and `Variable 2`, then `Statistic`,
`Method`, `Estimate`, `Counts`, `p-value` and `Note`, with the
attributes of
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md).

## Titles and footnotes

As for
[`Visualize_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CorrelationMatrix.md):
`{heading}`, `{variables}`, `{visit}`, `{value}`, `{n}`, `{filters}`,
`{date}` and `{version}`.

## Display rules

A p-value is written to three decimals, `p < 0.001` below that and
`p > 0.999` above, never with stars. Every row is labelled exploratory,
with its adjustment named when it has one. An estimate is written to
four significant digits with its confidence interval. A statistic R did
not compute has no p-value, and its note is R's reason.

## See also

[`Visualize_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Visualize_CorrelationMatrix.md)
and
[`Widget_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Widget_CorrelationMatrix.md).

Other tables:
[`Table_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Table_AssociationScatter.md),
[`Table_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Table_BiomarkerScreen.md),
[`Table_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Table_CrossTab.md),
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md),
[`Table_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Table_StratifiedSurvival.md),
[`Write_RTF()`](https://jwildfire.github.io/gsm.bio/reference/Write_RTF.md)

## Examples

``` r
Table_CorrelationMatrix(
  Synthetic_Results,
  Synthetic_Participants,
  lSettings = list(visit = "Baseline")
)
#>    Variable 1 Variable 2   Statistic                               Method
#> 1         CRP    D-dimer Correlation Pearson's product-moment correlation
#> 2         CRP   Ferritin Correlation Pearson's product-moment correlation
#> 3         CRP  IFN-gamma Correlation Pearson's product-moment correlation
#> 4         CRP   IL-1beta Correlation Pearson's product-moment correlation
#> 5         CRP       IL-2 Correlation Pearson's product-moment correlation
#> 6         CRP       IL-6 Correlation Pearson's product-moment correlation
#> 7         CRP       IL-8 Correlation Pearson's product-moment correlation
#> 8         CRP      IL-10 Correlation Pearson's product-moment correlation
#> 9         CRP        LDH Correlation Pearson's product-moment correlation
#> 10        CRP  TNF-alpha Correlation Pearson's product-moment correlation
#> 11        CRP       VEGF Correlation Pearson's product-moment correlation
#> 12    D-dimer   Ferritin Correlation Pearson's product-moment correlation
#> 13    D-dimer  IFN-gamma Correlation Pearson's product-moment correlation
#> 14    D-dimer   IL-1beta Correlation Pearson's product-moment correlation
#> 15    D-dimer       IL-2 Correlation Pearson's product-moment correlation
#> 16    D-dimer       IL-6 Correlation Pearson's product-moment correlation
#> 17    D-dimer       IL-8 Correlation Pearson's product-moment correlation
#> 18    D-dimer      IL-10 Correlation Pearson's product-moment correlation
#> 19    D-dimer        LDH Correlation Pearson's product-moment correlation
#> 20    D-dimer  TNF-alpha Correlation Pearson's product-moment correlation
#> 21    D-dimer       VEGF Correlation Pearson's product-moment correlation
#> 22   Ferritin  IFN-gamma Correlation Pearson's product-moment correlation
#> 23   Ferritin   IL-1beta Correlation Pearson's product-moment correlation
#> 24   Ferritin       IL-2 Correlation Pearson's product-moment correlation
#> 25   Ferritin       IL-6 Correlation Pearson's product-moment correlation
#> 26   Ferritin       IL-8 Correlation Pearson's product-moment correlation
#> 27   Ferritin      IL-10 Correlation Pearson's product-moment correlation
#> 28   Ferritin        LDH Correlation Pearson's product-moment correlation
#> 29   Ferritin  TNF-alpha Correlation Pearson's product-moment correlation
#> 30   Ferritin       VEGF Correlation Pearson's product-moment correlation
#> 31  IFN-gamma   IL-1beta Correlation Pearson's product-moment correlation
#> 32  IFN-gamma       IL-2 Correlation Pearson's product-moment correlation
#> 33  IFN-gamma       IL-6 Correlation Pearson's product-moment correlation
#> 34  IFN-gamma       IL-8 Correlation Pearson's product-moment correlation
#> 35  IFN-gamma      IL-10 Correlation Pearson's product-moment correlation
#> 36  IFN-gamma        LDH Correlation Pearson's product-moment correlation
#> 37  IFN-gamma  TNF-alpha Correlation Pearson's product-moment correlation
#> 38  IFN-gamma       VEGF Correlation Pearson's product-moment correlation
#> 39   IL-1beta       IL-2 Correlation Pearson's product-moment correlation
#> 40   IL-1beta       IL-6 Correlation Pearson's product-moment correlation
#> 41   IL-1beta       IL-8 Correlation Pearson's product-moment correlation
#> 42   IL-1beta      IL-10 Correlation Pearson's product-moment correlation
#> 43   IL-1beta        LDH Correlation Pearson's product-moment correlation
#> 44   IL-1beta  TNF-alpha Correlation Pearson's product-moment correlation
#> 45   IL-1beta       VEGF Correlation Pearson's product-moment correlation
#> 46       IL-2       IL-6 Correlation Pearson's product-moment correlation
#> 47       IL-2       IL-8 Correlation Pearson's product-moment correlation
#> 48       IL-2      IL-10 Correlation Pearson's product-moment correlation
#> 49       IL-2        LDH Correlation Pearson's product-moment correlation
#> 50       IL-2  TNF-alpha Correlation Pearson's product-moment correlation
#> 51       IL-2       VEGF Correlation Pearson's product-moment correlation
#> 52       IL-6       IL-8 Correlation Pearson's product-moment correlation
#> 53       IL-6      IL-10 Correlation Pearson's product-moment correlation
#> 54       IL-6        LDH Correlation Pearson's product-moment correlation
#> 55       IL-6  TNF-alpha Correlation Pearson's product-moment correlation
#> 56       IL-6       VEGF Correlation Pearson's product-moment correlation
#> 57       IL-8      IL-10 Correlation Pearson's product-moment correlation
#> 58       IL-8        LDH Correlation Pearson's product-moment correlation
#> 59       IL-8  TNF-alpha Correlation Pearson's product-moment correlation
#> 60       IL-8       VEGF Correlation Pearson's product-moment correlation
#> 61      IL-10        LDH Correlation Pearson's product-moment correlation
#> 62      IL-10  TNF-alpha Correlation Pearson's product-moment correlation
#> 63      IL-10       VEGF Correlation Pearson's product-moment correlation
#> 64        LDH  TNF-alpha Correlation Pearson's product-moment correlation
#> 65        LDH       VEGF Correlation Pearson's product-moment correlation
#> 66  TNF-alpha       VEGF Correlation Pearson's product-moment correlation
#>                                                 Estimate  Counts p-value
#> 1    -0.1274, 95% confidence interval -0.2615 to 0.01158 n = 200        
#> 2     0.02159, 95% confidence interval -0.1175 to 0.1599 n = 200        
#> 3     0.07135, 95% confidence interval -0.06806 to 0.208 n = 200        
#> 4      0.1587, 95% confidence interval 0.02044 to 0.2911 n = 200        
#> 5    -0.0701, 95% confidence interval -0.2068 to 0.06932 n = 200        
#> 6     0.01976, 95% confidence interval -0.1193 to 0.1581 n = 200        
#> 7    -0.1135, 95% confidence interval -0.2483 to 0.02565 n = 200        
#> 8   -0.06857, 95% confidence interval -0.2054 to 0.07084 n = 200        
#> 9    -0.02666, 95% confidence interval -0.1648 to 0.1125 n = 200        
#> 10  -0.05072, 95% confidence interval -0.1881 to 0.08864 n = 200        
#> 11   -0.02671, 95% confidence interval -0.1648 to 0.1124 n = 200        
#> 12   0.09559, 95% confidence interval -0.04373 to 0.2313 n = 200        
#> 13    0.05669, 95% confidence interval -0.0827 to 0.1939 n = 200        
#> 14    0.1209, 95% confidence interval -0.01815 to 0.2554 n = 200        
#> 15    0.01077, 95% confidence interval -0.1282 to 0.1493 n = 200        
#> 16     0.0828, 95% confidence interval -0.05659 to 0.219 n = 200        
#> 17   0.04558, 95% confidence interval -0.09376 to 0.1832 n = 200        
#> 18 -0.1443, 95% confidence interval -0.2775 to -0.005671 n = 200        
#> 19   -0.01227, 95% confidence interval -0.1508 to 0.1267 n = 200        
#> 20  -0.07589, 95% confidence interval -0.2124 to 0.06352 n = 200        
#> 21    -0.0106, 95% confidence interval -0.1491 to 0.1283 n = 200        
#> 22   -0.02203, 95% confidence interval -0.1603 to 0.1171 n = 200        
#> 23   0.07067, 95% confidence interval -0.06875 to 0.2074 n = 200        
#> 24  -0.04467, 95% confidence interval -0.1823 to 0.09466 n = 200        
#> 25    0.132, 95% confidence interval -0.006849 to 0.2659 n = 200        
#> 26  -0.04729, 95% confidence interval -0.1848 to 0.09205 n = 200        
#> 27     -0.01905, 95% confidence interval -0.1574 to 0.12 n = 200        
#> 28    -0.101, 95% confidence interval -0.2364 to 0.03827 n = 200        
#> 29    0.01963, 95% confidence interval -0.1194 to 0.1579 n = 200        
#> 30   -0.03188, 95% confidence interval -0.1699 to 0.1073 n = 200        
#> 31   0.002546, 95% confidence interval -0.1362 to 0.1412 n = 200        
#> 32    -0.02813, 95% confidence interval -0.1662 to 0.111 n = 200        
#> 33   -0.04752, 95% confidence interval -0.185 to 0.09182 n = 200        
#> 34     0.0131, 95% confidence interval -0.1259 to 0.1516 n = 200        
#> 35  -0.09172, 95% confidence interval -0.2276 to 0.04762 n = 200        
#> 36   0.05757, 95% confidence interval -0.08183 to 0.1948 n = 200        
#> 37  -0.1337, 95% confidence interval -0.2675 to 0.005162 n = 200        
#> 38    0.1038, 95% confidence interval -0.03543 to 0.2391 n = 200        
#> 39    -0.03623, 95% confidence interval -0.1741 to 0.103 n = 200        
#> 40    0.03298, 95% confidence interval -0.1062 to 0.1709 n = 200        
#> 41   -0.02945, 95% confidence interval -0.1675 to 0.1097 n = 200        
#> 42  -0.08792, 95% confidence interval -0.2239 to 0.05145 n = 200        
#> 43  -0.05742, 95% confidence interval -0.1946 to 0.08198 n = 200        
#> 44     0.01195, 95% confidence interval -0.127 to 0.1504 n = 200        
#> 45    0.01344, 95% confidence interval -0.1255 to 0.1519 n = 200        
#> 46   -0.07443, 95% confidence interval -0.211 to 0.06498 n = 200        
#> 47   -0.08067, 95% confidence interval -0.217 to 0.05873 n = 200        
#> 48  -0.06472, 95% confidence interval -0.2017 to 0.07469 n = 200        
#> 49    0.1072, 95% confidence interval -0.03202 to 0.2423 n = 200        
#> 50   -0.00149, 95% confidence interval -0.1402 to 0.1373 n = 200        
#> 51    -0.02712, 95% confidence interval -0.1652 to 0.112 n = 200        
#> 52   -0.02469, 95% confidence interval -0.1629 to 0.1144 n = 200        
#> 53    0.07963, 95% confidence interval -0.05977 to 0.216 n = 200        
#> 54  -0.1824, 95% confidence interval -0.3132 to -0.04477 n = 200        
#> 55     0.1515, 95% confidence interval 0.01306 to 0.2843 n = 200        
#> 56   -0.1167, 95% confidence interval -0.2514 to 0.02241 n = 200        
#> 57  -0.004427, 95% confidence interval -0.1431 to 0.1344 n = 200        
#> 58    0.03797, 95% confidence interval -0.1013 to 0.1758 n = 200        
#> 59    -0.1075, 95% confidence interval -0.2426 to 0.0317 n = 200        
#> 60    0.01106, 95% confidence interval -0.1279 to 0.1496 n = 200        
#> 61   -0.0564, 95% confidence interval -0.1936 to 0.08299 n = 200        
#> 62      0.6384, 95% confidence interval 0.5482 to 0.7139 n = 200        
#> 63  -0.05727, 95% confidence interval -0.1945 to 0.08212 n = 200        
#> 64   -0.04959, 95% confidence interval -0.187 to 0.08977 n = 200        
#> 65   0.04551, 95% confidence interval -0.09382 to 0.1831 n = 200        
#> 66   -0.1104, 95% confidence interval -0.2454 to 0.02873 n = 200        
#>                                                                               Note
#> 1  No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 2  No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 3  No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 4  No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 5  No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 6  No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 7  No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 8  No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 9  No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 10 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 11 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 12 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 13 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 14 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 15 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 16 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 17 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 18 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 19 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 20 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 21 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 22 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 23 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 24 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 25 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 26 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 27 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 28 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 29 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 30 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 31 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 32 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 33 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 34 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 35 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 36 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 37 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 38 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 39 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 40 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 41 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 42 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 43 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 44 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 45 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 46 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 47 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 48 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 49 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 50 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 51 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 52 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 53 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 54 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 55 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 56 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 57 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 58 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 59 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 60 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 61 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 62 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 63 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 64 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 65 No p-value: a matrix reports each coefficient, its interval and its pair count.
#> 66 No p-value: a matrix reports each coefficient, its interval and its pair count.
```
