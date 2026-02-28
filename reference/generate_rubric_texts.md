# Generate Rubric Texts

This function generates or updated the rubric_items.csv with a grades
csv file's rubric items. The grades csv is saved with "R1", "R2", ...
labels for the rubric items as a new csv, and this function returns this
new grades csv as a dataframe.

## Usage

``` r
generate_rubric_texts(
  csv_path,
  output_folder,
  ignored_nrows = 3,
  existing = TRUE,
  pensieve = FALSE
)
```

## Arguments

- csv_path:

  path to exported Gradescope evaluations csv

- output_folder:

  folder for rubric_items.csv

- ignored_nrows:

  how many of the last lines to ignore

- existing:

  if there is an existing rubric_items.csv

- pensieve:

  if source is exported from Pensieve

## Value

a dataframe with rubric items in "R1", "R2",... format
