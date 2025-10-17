# Accuracy Calculations

## Deidentifying Evaluations

Use this instructions on this [repo](https://github.com/nikita-jaya/DeIdentificationScripts) to deidentify Gradescope submissions.

The following code will de-identify the exported evaluations of the original Gradescope submissions:

```{r}
deidentify_gradescope_evals(gs_csv_path, ids_csv_path, output_path)
```

-   where `gs_csv_path` is the path to the original Gradescope evaluations

-   `ids_csv_path` is the lookup-table

-   `output_path` is the path to where the de-identified original Gradescope evaluations will be saved as a .csv

## Loading in Evaluations

The following code loads in the evaluations and normalized the "Full credit." rubric item.

```{r}
expert_evals <- read_evals(expert_csv_path) |>
  normalize_full_credit(expert_rubric_items)

student_evals <- read_evals(student_csv_path) |>
  normalize_full_credit(student_rubric_items)

```

-   where `expert_csv_path` and `student_csv_path` are the paths to the de-identified Gradescope evaluations of the expert graders and student graders respectively

-   where `expert_rubric_items` and `student_rubric_items` are the vectors of which rubric items are equivalent to marking an assignment as "Full credit." for the expert evaluations and student evaluations respectively (Note that these vectors can be string vectors with the column names or numeric vectors with the column indices

## Accuracy Calculations

### Proportion of Identical Scores

The following code will output the proportion of scores that are identical, the count of submissions that were compared, the average score of `expert_evals`, the average score of `student_evals`, and the average of the absolute difference between the two evaluations of the same assignment.

```{r}
identical_score_prop(expert_evals, student_evals)
```

### Mean Absolute Error

If we had a rubric with two items, both worth one point, the error contribution of one submission would be

-   For expert answer {1, 0}

    -   0 if grader is {1, 0}

    -   1 if grader is {0, 0} or {1, 1}

    -   2 if grader is {0, 1}

```{r}
rubric_mean_absolute_error(expert_evals, student_evals,
                           rubric_matching_list)
```

-   where `rubric_matching_list` is an R list with two vector of corresponding rubric items from each evaluation
