# Extract Rubric Item Scores from a Metadata JSON File

Reads a metadata JSON file and returns the ordered numeric vector of
rubric item point values. This vector can be passed as the `weights`
argument to
[`rubric_mae()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/rubric_mae.md)
to compute a point-weighted MAE.

## Usage

``` r
scores_from_metadata(metadata_file, calibrated = TRUE)
```

## Arguments

- metadata_file:

  path to a metadata JSON file

- calibrated:

  logical; if `TRUE` (default) extract scores from
  `rubric$calibrated$scores`, otherwise from
  `rubric$uncalibrated$scores`

## Value

a numeric vector of point values, one per rubric item
