#' Calculate Proportion of Identical Scores
#'
#' This function calculates the proportion of identical scores
#' between two Gradescope evaluation dataframes
#'
#' @param eval1 first dataframe of Gradescope evaluations
#' @param eval2 second dataframe of Gradescope evaluations
#'
#' @return a single proportion
#'
#' @importFrom dplyr mutate rename summarize inner_join select n
#' @importFrom tidyr drop_na
#'
#' @export
identical_score_prop <- function(eval1, eval2){
  eval1 <- eval1 |>
    rename(Score1 = Score) |>
    select(SID, Score1)
  eval2 <- eval2 |>
    rename(Score2 = Score) |>
    select(SID, Score2)
  inner_join(eval1, eval2, by = "SID") |>
    drop_na() |>
    summarize(
      Proportion = mean(Score1 == Score2),
      Count = n(),
      `Avg of Score1` = mean(Score1),
      `Avg of Score2` = mean(Score2),
      `Avg Abs Diff` = mean(abs(Score1 - Score2))
    )
}

#' Mean Absolute Error of Rubric Items
#'
#' If we had a rubric with two items, both worth one point, the error contribution of one submission would be:\cr
#' For expert answer \{1, 0\}, \cr
#' 0 if grader is \{1, 0\} \cr
#' 1 if grader is \{0, 0\} or \{1, 1\} \cr
#' 2 if grader is \{0, 1\} \cr
#' It's recommended to `normalize_full_credit()` for `eval1` and `eval2` prior
#' to using this function.
#'
#' @param eval1 first dataframe of Gradescope evaluations
#' @param eval2 second dataframe of Gradescope evaluations
#' @param rubric_matching_list vector of rubric items that sum up to full credit
#'
#' @return normalized evals dataframe
#'
#' @export
rubric_mean_absolute_error <- function(eval1, eval2, rubric_matching_list){
  # convert rubric items of eval 1 into matrix
  rubric1 <- eval1[, c(rubric_matching_list[[1]])] |>
    as.matrix()
  rownames(rubric1) <- eval1$SID
  # convert rubric items of eval 2 into matrix
  rubric2 <- eval2[, c(rubric_matching_list[[2]])] |>
    as.matrix()
  rownames(rubric2) <- eval2$SID
  # find common students
  students <- intersect(rownames(rubric1), rownames(rubric2))
  # same students in same order
  rubric1 <- rubric1[students, , drop = FALSE]
  rubric2 <- rubric2[students, , drop = FALSE]
  # elementwise matrix comparison
  check_equal <- rubric1 != rubric2
  # mean absolute error calculation
  error_per_student <- rowSums(check_equal)
  MAE <- mean(error_per_student)
  # MAE per rubric item
  error_per_rubric <- colMeans(check_equal)
  n_rubric <- length(error_per_rubric)
  errors <- as.data.frame(t(c(MAE, error_per_rubric)))
  colnames(errors) <- c("Mean Absolute Error",
                        paste0("% Error with Rubric Item #",
                               1:n_rubric))
  return (errors)
}

#' Normalize Full Credit
#'
#' This function "normalizes" the "Full Credit" toggle
#' on the evaluations by making all other rubric items
#' TRUE to allow for comparisons across individual rubric
#' items. Note that `rubric_items` can be a string vector of
#' column names or a numeric vector of column indices.
#'
#' @param evals dataframe of Gradescope evaluations
#' @param rubric_items vector of rubric items that sum up to full credit
#'
#' @return normalized evals dataframe
#'
#' @export
normalize_full_credit <- function(evals, full_credit, rubric_items){
  full_credit_row <- evals[[full_credit]]
  evals[full_credit_row, rubric_items] <- TRUE
  return (evals)
}
