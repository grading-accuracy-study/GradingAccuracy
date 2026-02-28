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
rubric_mae(eval1, eval2, rubric_matching_list = NULL)
```

## Arguments

- eval1:

  first dataframe of Gradescope evaluations

- eval2:

  second dataframe of Gradescope evaluations

- rubric_matching_list:

  vector of rubric items to compare, if NULL, assume the same rubric

## Value

double for mean absolute error
