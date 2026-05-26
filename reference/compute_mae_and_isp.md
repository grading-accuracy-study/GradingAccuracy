# Calculate MAE and ISP

This function calculates the proportion of identical scores and mean
absolute error of rubric items between two Gradescope evaluation csv

## Usage

``` r
compute_mae_and_isp(file1, file2, metadata_file = NULL)
```

## Arguments

- file1:

  file path for first grades csv

- file2:

  file path for second grades csv

- metadata_file:

  optional path to a metadata JSON file. When supplied, rubric item
  point values are extracted and passed as weights to
  [`rubric_mae()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/rubric_mae.md),
  producing a point-weighted MAE.

## Value

a list
