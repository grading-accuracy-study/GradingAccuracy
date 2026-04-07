# Deidentify Graders

This function swaps the original grader names with fake names from a
roster and saves the deidentified grades as a csv and the lookup table
for the graders as a csv.

## Usage

``` r
deidentify_graders(csv_path, roster_csv, output_path, ignored_nrows = 3)
```

## Arguments

- csv_path:

  path to exported Gradescope evaluations csv

- roster_csv:

  path to roster csv

- output_path:

  path to save deidentified grades

- ignored_nrows:

  how many of the last lines to ignore
