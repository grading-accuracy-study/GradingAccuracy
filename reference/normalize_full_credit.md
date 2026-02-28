# Normalize Full Credit

This function "normalizes" the "Full Credit" toggle on the evaluations
by making all other rubric items TRUE to allow for comparisons across
individual rubric items. Note that `rubric_items` can be a string vector
of column names or a numeric vector of column indices.

## Usage

``` r
normalize_full_credit(evals, full_credit, rubric_items)
```

## Arguments

- evals:

  dataframe of Gradescope evaluations

- full_credit:

  column index or name for full credit column

- rubric_items:

  vector of rubric items that sum up to full credit

## Value

normalized evals dataframe
