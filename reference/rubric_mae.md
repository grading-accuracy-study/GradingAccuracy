# Mean Absolute Error of Rubric Items

If we had a rubric with two items, both worth one point, the error
contribution of one submission would be:  
For expert answer {1, 0},  
0 if grader is {1, 0}  
1 if grader is {0, 0} or {1, 1}  
2 if grader is {0, 1}  
It's recommended to
[`normalize_full_credit()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/normalize_full_credit.md)
for `eval1` and `eval2` prior to using this function.

## Usage

``` r
rubric_mae(eval1, eval2, weights = NULL)
```

## Arguments

- eval1:

  first dataframe of Gradescope evaluations

- eval2:

  second dataframe of Gradescope evaluations

- weights:

  optional numeric vector of point values, one per rubric item (in the
  same order as the R1, R2, ... columns). When `NULL` (default), all
  items are treated as equally weighted.

## Value

double for mean absolute error

## Details

When `weights` is supplied, each rubric item's disagreement is scaled by
its point value before summing, so a mismatch on a 1-point item
contributes more than a mismatch on a 0.5-point item. Use
[`scores_from_metadata()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/scores_from_metadata.md)
to extract the weights vector from a metadata JSON file.
