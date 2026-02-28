# Read Gradescope Evaluations CSV

This function reads in the .csv file from Gradescope's "Export
Evaluations" functionality. The formatting of this file is not in tidy
format because the last few lines include Point Values, Rubric Numbers
and Scoring Method. These are excluded when loading in the file.

## Usage

``` r
read_evals(csv_path, ignored_nrows = 3)
```

## Arguments

- csv_path:

  path to exported Gradescope evaluations csv

- ignored_nrows:

  how many of the last lines to ignore

## Value

a dataframe of Gradescope evaluations
