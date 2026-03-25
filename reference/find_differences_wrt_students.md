# Find Differences with respect to Students Table

Find the differences between students grading and experts and return all
graded assignments that are different and a matrix of which rubrics are
mismatched. Note that all mismatches between AI and expert grading are
removed to prevent redundancy.

## Usage

``` r
find_differences_wrt_students(experts_file, ai_file, student_file)
```

## Arguments

- experts_file:

  file with expert graders

- ai_file:

  file with AI graders

- student_file:

  file with student graders

## Value

a list of a df and a matrix
