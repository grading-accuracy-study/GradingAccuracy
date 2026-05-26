# Creating a metadata.JSON

``` r

library(GradingAccuracy)
```

## Requirements for metadata.json

A `metadata.json` file can be used to keep track of information on the
course-, assignment- and question-level. While additional arguments may
be added to keep track of other miscellaneous information, this package
requires that the `metadata.json` has a `course_info` object and a
`rubric object`.

The `course_info` object must have the following keys (and their
corresponding values):

- `department` : department abbreviation (e.g. “STAT”, “DATA”)
- `course_number` : course number
- `course_name` : name of course (e.g. “Introduction to Probability”)
- `upper_div` : if the course is upper-division or lower-division
- `year` : year of the course
- `semester` : semester of the course (e.g. “Spring”, “Fall”)
- `assignment_name` : name of assignment (e.g. “Midterm 1”)
- `question_number` : number of question, including subparts (e.g. “5c”)
- `question_name` : name of question (e.g. “Data Visualization”)
- `mode_of_question` : mode of question (e.g. “fill in the blank”,
  “open-ended”)
- `medium_of_answer` : medium of student’s answer (e.g. “handwritten”,
  “typed”)
- `content_of_answer` : expected content of student’s answer
  (e.g. “English”, “math”, “code”)
- `scoring_type` : whether the scoring is positive (i.e. additive) or
  negative (i.e. subtractive)
- `is_proctored` : whether the assignment was taken in a proctored
  environment or if it was take-home
- `n_submissions`: the number of student submissions, which can be
  programmatically updated with
  [`update_scores_in_metadata()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/update_scores_in_metadata.md)
- `mean_score`: the mean score of experts using, which can be
  programmatically updated with
  [`update_scores_in_metadata()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/update_scores_in_metadata.md)
  (which uses `mean(experts$Score/max(experts$Score))`)
- `total_points`: the total number of points the question is worth
- `rubric_type`: the type of rubric (e.g. `"positive disjoint"`)

The `rubric` object must have a `calibrated` rubric object and
optionally an `uncalibrated` object that follows the same structure. A
rubric object (either `calibrated` or `uncalibrated`) must have the
following structure:

- `rubric_items` which is a nested list that has keys named as “R1”,
  “R2”, etc. with values as strings of the rubric description
- `scores`, which is a numbered vector that has the corresponding point
  value of rubric items “R1”, “R2”,…

### Correct Metadata.json with only a `calibrated` rubric

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
    "content_of_answer": "math",
    "scoring_type": "positive",
    "is_proctored": true,
    "n_submissions": 0,
    "mean_score": 0.0,
    "total_points": 2,
    "rubric_type": "positive disjoint"
  },
  "rubric": {
    "calibrated": {
      "rubric_items": {
        "R1": "Fully credit.",
        "R2": "Stated correct equation for expected value.",
        "R3": "Some work in the right direction",
        "R4": "Incorrect."
      },
      "scores": [1.0, 0.5, 0.5, 0.0]
    },
    "uncalibrated": null
  }
}
```

### Correct Metadata.json with a `calibrated` and `uncalibrated`rubric

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
    "content_of_answer": "math",
    "scoring_type": "positive",
    "is_proctored": true,
    "n_submissions": 0,
    "mean_score": 0.0,
    "total_points": 2,
    "rubric_type": "positive disjoint"
  },
  "rubric": {
    "calibrated": {
      "rubric_items": {
        "R1": "Fully credit.",
        "R2": "Stated correct equation for expected value.",
        "R3": "Some work in the right direction",
        "R4": "Incorrect."
      },
      "scores": [1.0, 0.5, 0.5, 0.0]
    },
    "uncalibrated": {
      "rubric_items": {
        "R1": "Fully credit.",
        "R2": "Started correct approach, but incorrect solution.",
        "R3": "Some work in the right direction"
      },
      "scores": [1.0, 0.5, 0.25]
    }
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

validate_metadata_json(system.file("extdata", "metadata-calibrated.json", package = "GradingAccuracy"))
#> ✔ The following file is successfully validated: /home/runner/work/_temp/Library/GradingAccuracy/extdata/metadata-calibrated.json
```

Optionally, you can also use the `verbose` argument in order to print
out the listed course and assignment information stored in this JSON
file:

``` r

validate_metadata_json(system.file("extdata", "metadata-calibrated.json", package = "GradingAccuracy"), verbose = T)
#> ✔ The following file is successfully validated: /home/runner/work/_temp/Library/GradingAccuracy/extdata/metadata-calibrated.json
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

### `course_info`: Potential Errors

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
    "question_name": "Calculate Expected Value",
    "n_submissions": 0,
    "mean_score": 0.0
  }
}
```

you will get the following error:

``` r

validate_metadata_json(system.file("extdata", "missing-keys.json", package = "GradingAccuracy"))
#> Error in `validate_metadata_json()`:
#> ! The rubric argument is missing from the following file:
#>   /home/runner/work/_temp/Library/GradingAccuracy/extdata/missing-keys.json
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
    "content_of_answer": "math",
    "scoring_type": "positive",
    "is_proctored": true,
    "n_submissions": 0,
    "mean_score": 0.0
  }
}
```

you will get the following error:

``` r

validate_metadata_json(system.file("extdata", "wrong-boolean.json", package = "GradingAccuracy"))
#> Error in `validate_metadata_json()`:
#> ! The rubric argument is missing from the following file:
#>   /home/runner/work/_temp/Library/GradingAccuracy/extdata/wrong-boolean.json
```

### `rubric`: Potential Errors

Note that if the `uncalibrated` rubric item is not empty, these checks
are applied to both `uncalibrated` and `calibrated`, but the checks are
only applied to the latter if `uncalibrated` is empty.

#### Mismatched Rubric Items and Scores

For example, if the number of `rubric_items` and the number of `scores`
are not equal, like in the JSON file below:

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
    "content_of_answer": "math",
    "scoring_type": "positive",
    "is_proctored": true,
    "n_submissions": 0,
    "mean_score": 0.0
  },
  "rubric": {
    "calibrated": {
      "rubric_items": {
        "R1": "Fully credit.",
        "R2": "Stated correct equation for expected value.",
        "R3": "Some work in the right direction"
      },
      "scores": [1.0, 0.75, 0.5, 0.25, 0.0]
    },
    "uncalibrated": null
  }
}
```

you will get the following error:

``` r

validate_metadata_json(system.file("extdata", "mismatched-rubric-scores.json", package = "GradingAccuracy"))
#> Error in `validate_metadata_json()`:
#> ! The following arguments are missing from course_info: "total_points"
#>   and "rubric_type"
```

#### Scores Are Not Numbers

If the `scores` are not numbers, like in the JSON file below:

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
    "content_of_answer": "math",
    "scoring_type": "positive",
    "is_proctored": true,
    "n_submissions": 0,
    "mean_score": 0.0
  },
  "rubric": {
    "calibrated": {
      "rubric_items": {
        "R1": "Fully credit.",
        "R2": "Stated correct equation for expected value.",
        "R3": "Some work in the right direction"
      },
      "scores": ["A", "B", "C"]
    },
    "uncalibrated": null
  }
}
```

you will get the following error:

``` r

validate_metadata_json(system.file("extdata", "incorrect-scores.json", package = "GradingAccuracy"))
#> Error in `validate_metadata_json()`:
#> ! The following arguments are missing from course_info: "total_points"
#>   and "rubric_type"
```

#### Incorrectly Named Keys

If you do not name the keys in `rubric items` according to expected
structure (e.g. “R1”, “R2”, …), like in the JSON file below:

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
    "content_of_answer": "math",
    "scoring_type": "positive",
    "is_proctored": true,
    "n_submissions": 0,
    "mean_score": 0.0
  },
  "rubric": {
    "calibrated": {
      "rubric_items": {
        "R0": "Fully credit.",
        "R1": "Stated correct equation for expected value.",
        "R2": "Some work in the right direction",
        "R3": "Incorrect."
      },
      "scores": [1.0, 0.5, 0.5, 0.0]
    },
    "uncalibrated": null
  }
}
```

you will get the following error:

``` r

validate_metadata_json(system.file("extdata", "incorrect-rubric-keys.json", package = "GradingAccuracy"))
#> Error in `validate_metadata_json()`:
#> ! The following arguments are missing from course_info: "total_points"
#>   and "rubric_type"
```
