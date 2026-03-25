# Find Differences with respect to Students Table

Find the differences between students grading and experts and return all
graded assignments that are different and a matrix of which rubrics are
mismatched. Note that all mismatches between AI and expert grading are
removed to prevent redundancy.

## Usage

``` r
find_differences_wrt_students(experts_file, student_file, ai_diffs)
```

## Arguments

- experts_file:

  file with experts graders

- student_file:

  file with ai graders

- ai_diffs:

  find_differences results for experts v. AI

## Value

a list of a df and a matrix
