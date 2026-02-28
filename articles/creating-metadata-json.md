# Creating a metadata.JSON

``` r
library(GradingAccuracy)
```

## Requirements for metadata.json

A `metadata.json` file can be used to keep track of information on the
course-, assignment- and question-level. While additional arguments may
be added to keep track of other miscellaneous information, this package
requires that the `metadata.json` has a `course_info` object with the
following keys (and their corresponding values):

- `department` : department abbreviation (e.g. “STAT”, “DATA”)
- `course_number` : course number
- `course_name` : name of course (e.g. “Introduction to Probability”)
- `upper_div` : if the course is upper-division or lower-division
- `year` : year of the course
- `semester` : semester of the course (e.g. “Spring”, “Fall”)
- `assignment_name` : name of assignment (e.g. “Midterm 1”)
- `question_number` : number of question, including subparts (e.g. “5c”)
- `question_name` : name of question (e.g. “Data Visualization”)
- mode_of_question : mode of question (e.g. “fill in the blank”,
  “open-ended”)
- `medium_of_answer` : medium of student’s answer (e.g. “handwritten”,
  “typed”)
- `content_of_answer` : expected content of student’s answer
  (e.g. “English”, “math”, “code”)

``` yaml
{
  "course_info": {
    "department": "STAT",
    "course_number": "001",
    "course_name": "Introduction to Statistics",
    "upper_div": false,
    "year": 2020,
    "semester": "Fall",
    "assignment_name": "Midterm Exam",
    "question_number": "12a",
    "question_name": "Calculate Expected Value",
    "mode_of_question": "open-ended",
    "medium_of_answer": "handwritten",
    "content_of_answer": "math"
  }
}
```

## Validating a metadata.json

The
[`validate_metadata_json()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/validate_metadata_json.md)
function can be used to validate that all required arguments and keys
are present and correctly formatted.

### Correct metadata.json file

If you have a correct `metadata.json` file, like the example file above,
you will receive the following message:

``` r
validate_metadata_json(system.file("extdata", "metadata.json", package = "GradingAccuracy"))
#> ✔ The following file is successfully validated: /home/runner/work/_temp/Library/GradingAccuracy/extdata/metadata.json
```

Optionally, you can also use the `verbose` argument in order to print
out the listed course and assignment information stored in this JSON
file:

``` r
validate_metadata_json(system.file("extdata", "metadata.json", package = "GradingAccuracy"), verbose = T)
#> ✔ The following file is successfully validated: /home/runner/work/_temp/Library/GradingAccuracy/extdata/metadata.json
#> 
#> ── STAT 001 - Introduction to Statistics ───────────────────────────────────────
#> A lower-division course from Fall 2020
#> 
#> ── Calculate Expected Value
#> Question 12a from Midterm Exam
#> • Mode of Question: open-ended
#> • Medium of Answer: handwritten
#> • Content of Answer: math
```

### Potential Errors

#### Missing `course_info`

For example, if you are missing the `course_info` argument, like in the
JSON file below:

``` yaml
{
  "course": {
    "department": "STAT",
    "course_number": "001",
    "course_name": "Introduction to Statistics"
  }
}
```

you will get the following error:

``` r
validate_metadata_json(system.file("extdata", "missing-course-info.json", package = "GradingAccuracy"))
#> Error in `validate_metadata_json()`:
#> ! The course_info argument is missing from the following file:
#>   /home/runner/work/_temp/Library/GradingAccuracy/extdata/missing-course-info.json
```

#### Missing Required Keys

If you are missing the required keys, like in the JSON file below:

``` yaml
{
  "course_info": {
    "department": "STAT",
    "course_number": "001",
    "course_name": "Introduction to Statistics",
    "upper_div": false,
    "year": 2020,
    "semester": "Fall",
    "assignment_name": "Midterm Exam",
    "question_number": "12a",
    "question_name": "Calculate Expected Value"
  }
}
```

you will get the following error:

``` r
validate_metadata_json(system.file("extdata", "missing-keys.json", package = "GradingAccuracy"))
#> Error in `validate_metadata_json()`:
#> ! The following arguments are missing from course_info:
#>   "mode_of_question", "medium_of_answer", and "content_of_answer"
```

#### Incorrect Data Types

If the required keys have an incorrect data type, like if `upper-div` is
not a boolean value as in the example below:

``` yaml
{
  "course_info": {
    "department": "STAT",
    "course_number": "001",
    "course_name": "Introduction to Statistics",
    "upper_div": "yes",
    "year": 2020,
    "semester": "Fall",
    "assignment_name": "Midterm Exam",
    "question_number": "12a",
    "question_name": "Calculate Expected Value",
    "mode_of_question": "open-ended",
    "medium_of_answer": "handwritten",
    "content_of_answer": "math"
  }
}
```

you will get the following error:

``` r
validate_metadata_json(system.file("extdata", "wrong-boolean.json", package = "GradingAccuracy"))
#> Error in `validate_metadata_json()`:
#> ! "upper_div" should be a boolean.
```
