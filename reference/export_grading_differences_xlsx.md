# Export Differences between Two Graders as Excel Spreadsheet

This function saves an Excel spreadsheet with the differences in rubric
items between two graders with mismatched rubrics highlighted.

## Usage

``` r
export_grading_differences_xlsx(
  find_differences,
  existing,
  students = T,
  sheet_name,
  dir = "."
)
```

## Arguments

- find_differences:

  object from the function find_differences_table()

- existing:

  if workbook exists

- students:

  if true, comparing with students; if false, comparing with AI

- sheet_name:

  name of sheet in workbook

- dir:

  optionally, where workbook is saved
