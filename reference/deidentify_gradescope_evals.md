# Deidentify Gradescope

This function de-identifies the exported Gradescope csv by swapping all
columns with identifiable data with the de-identified IDs in an existing
table. Note that Gradescope csv must have a column called
`Assignment Submission ID` and the ids table must have two columns:
`SID`, `Generated_ID`

## Usage

``` r
deidentify_gradescope_evals(
  gs_csv_path,
  ids_csv_path,
  output_path,
  ignored_nrows = 3
)
```

## Arguments

- gs_csv_path:

  path to exported Gradescope csv

- ids_csv_path:

  path to de-identified table csv

- output_path:

  path to de-idenfitied Gradescope output

- ignored_nrows:

  how many lines at the end of csv to exclude
