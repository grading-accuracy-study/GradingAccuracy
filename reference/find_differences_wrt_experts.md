# Find Differences with respect to Experts Table

Find the differences between AI grading and experts and return all
graded assignments that are different and a matrix of which rubrics are
mismatched.

## Usage

``` r
find_differences_wrt_experts(experts_file, ai_file)
```

## Arguments

- experts_file:

  file with expert graders

- ai_file:

  file with AI graders

## Value

a list of a df and a matrix
