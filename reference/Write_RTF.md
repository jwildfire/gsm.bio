# Write a Statistics Table to RTF

Writes a table that a `Table_*()` function returned to a Rich Text
Format file, as a report takes it: the title and subtitle above, the
table, and the footnotes beneath, the table's own last. It is written by
the r2rtf package, which gsm.bio suggests and does not import; nothing
is fetched, so it works offline.

## Usage

``` r
Write_RTF(dfTable, strFile, strOrientation = "landscape")
```

## Arguments

- dfTable:

  `data.frame` A table from
  [`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md)
  or one of the functions beside it, with its `title`, `subtitle` and
  `footnotes`.

- strFile:

  `character` The file to write, in a folder that exists. A file already
  there is replaced.

- strOrientation:

  `character` `"landscape"` (the default) or `"portrait"`.

## Value

`strFile`, invisibly.

## See also

[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md)
and the functions beside it, which make the tables.

Other tables:
[`Table_AssociationScatter()`](https://jwildfire.github.io/gsm.bio/reference/Table_AssociationScatter.md),
[`Table_BiomarkerScreen()`](https://jwildfire.github.io/gsm.bio/reference/Table_BiomarkerScreen.md),
[`Table_CorrelationMatrix()`](https://jwildfire.github.io/gsm.bio/reference/Table_CorrelationMatrix.md),
[`Table_CrossTab()`](https://jwildfire.github.io/gsm.bio/reference/Table_CrossTab.md),
[`Table_GroupComparison()`](https://jwildfire.github.io/gsm.bio/reference/Table_GroupComparison.md),
[`Table_StratifiedSurvival()`](https://jwildfire.github.io/gsm.bio/reference/Table_StratifiedSurvival.md)

## Examples

``` r
if (requireNamespace("r2rtf", quietly = TRUE)) {
  dfTable <- Table_CrossTab(
    Synthetic_Results,
    Synthetic_Participants,
    lSettings = list(row_by = "ARM", col_by = "RESPONSE", title = "{rows} by {columns}")
  )
  Write_RTF(dfTable, tempfile(fileext = ".rtf"))
}
```
