# Find Differences with respect to Experts as Excel Spreadsheet

This function saves an Excel spreadsheet with the differences in rubric
items with respect to experts with mismatched rubrics highlighted.

## Usage

``` r
find_differences_wrt_experts_xlsx(
  file1,
  file2,
  existing,
  sheet_name,
  dir = "."
)
```

## Arguments

- file1:

  file path for first grades for comparison

- file2:

  file path for second grades for comparison

- existing:

  if workbook exists

- sheet_name:

  name of sheet in workbook

- dir:

  optionally, where workbook is saved
