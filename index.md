# Accuracy Calculations

## Deidentifying PDF Submissions of Students

Use this instructions on this
[repo](https://github.com/nikita-jaya/DeIdentificationScripts) to
deidentify Gradescope submissions.

## Load This Package

``` r

remotes::install_github("grading-accuracy-study/GradingAccuracy")
library(GradingAccuracy)
library(tidyverse)
```

## Check metadata JSON

Specify a folder where the de-identified, processed files are saved.

``` r

exported_folder <- "./Midterm Exam/"
```

Store the `metadata.json` in this folder, and check that it meets all
formatting requirements using
[`validate_metadata_json()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/validate_metadata_json.md).

``` r

validate_metadata_json(paste0(exported_folder, "metadata.json"),
                       verbose = T)
```

## Processing Exports from a Deidentified Gradescope

The following process is used for processing exports from a
de-identified Gradescope. This means that the only identifiable data we
are removing is the names of the graders. The students are already
de-identified in the Gradescope prior to exporting. This is often
relevant to the expert grades.

The `roster_csv` is a csv file with a column for “Name”, “Email” and a
false “SID”.

The following code using
[`deidentify_graders()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/deidentify_graders.md)
reads in the `original-experts.csv`, de-identifies the graders using the
`roster.csv`, and exports the csv with deidentified graders and a lookup
table for the real and fake names of the graders.

``` r

roster_csv <- "../Roster.csv"
deidentify_graders("original-experts.csv", roster_csv,
                   "experts-calibrated.csv")
```

## Normalize Full Credit

It’s occasionally necessary to normalize a full credit option. The
following code using
[`normalize_full_credit()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/normalize_full_credit.md)
does so by normalizing the full credit option to the equivalent rubric
items and removing the full credit option.

``` r

students <- read_evals("students-uncalibrated.csv") 
students <- normalize_full_credit(students, full_credit = 4,
                      rubric_items = c(5:8))
# remove full credit + rubric items
students <- students[, -c(4, 9:11)]
write_csv(students, "students-uncalibrated.csv")
```

Then, we use
[`generate_rubric_texts()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/generate_rubric_texts.md)
to create a `rubric_items.csv` that has the original rubric items and
their mapping to the “R1”, “R2” structure. This function also changes
the headers of the `experts-calibrated.csv` to the “R1”, “R2” structure
and saves it in the `exported_folder`. For the first grades that are
processed, `existing` should be false to create a new `rubric_items.csv`
file and true afterwards to keep updating.

``` r

expert <- generate_rubric_texts(csv_path = "experts-calibrated.csv",
                                 output_folder =exported_folder,
                                 existing = F)
```

The following code is some additional data-processing to remove
unnecessary columns.

``` r

read_evals(paste0(exported_folder, "experts-calibrated.csv")) |>
  select(Name, SID, Score:Tags) |>
  write_csv(paste0(exported_folder, "experts-calibrated.csv"))
```

The
[`update_scores()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/update_scores.md)
function updates the “Score” column in the `experts-calibrated.csv`
based on the point-values from the `metadata.json`. Remember to specify
whether you want to overwrite the original csv and/or if you want to use
the `calibrated` or `uncalibrated` rubric.

``` r

expert <- update_scores(csv = paste0(exported_folder, "experts-calibrated.csv"),
              metadata = paste0(exported_folder, "metadata.json"),
              overwrite = T, calibrated = T)
```

## Processing Exports from the Original Gradescope

The following process is used for processing exports from the original
Gradescope. This means that we are removing is the names of the
students. This is often relevant to the student grades.

We use
[`deidentify_gradescope_evals()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/deidentify_gradescope_evals.md)
to take the original export `original-students-calibrated.csv` and
deidentify using the `deidentified-lookup-table.csv`, so they can be
mapped to the other deidentified grades. The deidentified grades are
exported to `students-calibrated.csv`.

``` r

deidentify_gradescope_evals("original-students-calibrated.csv", 
                            "deidentified-lookup-table.csv",
                            "students-calibrated.csv")
```

We similarly use
[`generate_rubric_texts()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/generate_rubric_texts.md)
to change the headers into the “R1”, “R2” structure. Since the
`existing` argument defaults to false, this will add another row to the
`rubric-items.csv`.

``` r

student <- generate_rubric_texts(csv_path = "students-calibrated.csv",
                                 output_folder =exported_folder)
```

We drop NA values from the SID column because there are often student
grades that are unused for this study (due to random sampling or invalid
submissions).

``` r

read_evals(paste0(exported_folder, "students-calibrated.csv")) |>
  drop_na(SID) |>
  write_csv(paste0(exported_folder, "students-calibrated.csv"))
```

We similarly update the scores using the metadata point-values using
[`update_scores()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/update_scores.md).

``` r

student <- update_scores(csv = paste0(exported_folder, "students-calibrated.csv"),
              metadata = paste0(exported_folder, "metadata.json"),
              overwrite = T, calibrated = T)
```

Additionally, we can now use
[`update_scores_in_metadata()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/update_scores_in_metadata.md)
to update the `n_submissions` and `mean_score`. While these metrics are
computed based on the expert grades, there is a check to make sure that
there is an equal number of students in the student-graded and
expert-graded exports.

``` r

update_scores_in_metadata(folder = exported_folder,
                          file = paste0(exported_folder, "metadata.json"))
```

### Processing Exports from Pensive

Pensive exports have slight deviations from the Gradescope exports and
hence require unique data processing.

Pensive requires manual de-identification with the following code:

``` r

roster_pensieve <- read_csv("../Roster_Pensieve.csv")
roster <- read_csv(roster_csv) |>
  rbind(roster_pensieve)
# add SID
read_csv("original-pensive-calibrated.csv") |>
  left_join(roster, by = c("Name", "Email")) |>
  relocate(SID) |>
  write_csv("pensive-calibrated.csv")
```

We once again use
[`generate_rubric_texts()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/generate_rubric_texts.md),
but note the slightly different arguments.

``` r

pensieve <- generate_rubric_texts("pensive-calibrated.csv",
                                  ignored_nrows = 0, 
                                  pensieve = T,
                                  exported_folder)
```

We remove unnecessary columns.

``` r

read_csv(paste0(exported_folder, "pensive-calibrated.csv")) |>
  select(-c(`Assignment ID`, `Problem ID`, Email)) |>
  write_csv(paste0(exported_folder, "pensive-calibrated.csv")) 
```

Finally, we update the scores using the metadata point-values using
[`update_scores()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/update_scores.md).

``` r

pensive <- update_scores(csv = paste0(exported_folder, "pensive-calibrated.csv"),
              metadata = paste0(exported_folder, "metadata.json"),
              overwrite = T, calibrated = T)
```

## A Final Check

As a final check, it’s useful to make sure that all SIDs across all
files are present uniformly.

``` r

identical(sort(expert$SID),sort(pensive$SID))
identical(sort(expert$SID),sort(student$SID))
identical(sort(student$SID),sort(pensive$SID))
```
