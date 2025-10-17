#' Calculate Proportion of Identical Scores
#'
#' This function calculates the proportion of identical scores
#' between two Gradescope evaluation dataframes
#'
#' @param eval1 first dataframe of Gradescope evaluations
#' @param eval2 first dataframe of Gradescope evaluations
#'
#' @return a single proportion
#'
#' @importFrom dplyr mutate rename summarize inner_join select
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


#' Normalize Full Credit
#'
#' This function "normalizes" the "Full Credit" toggle
#' on the evaluations by making all other rubric items
#' TRUE to allow for comparisons across individual rubric
#' items. Note that `rubric_items` can be a string vector of
#' colnames or a numeric vector of column indices.
#'
#' @param evals dataframe of Gradescope evaluations
#' @param rubric_items vector of rubric items that sum up to full credit
#'
#' @return normalized evals dataframe
#'
#' @export
normalize_full_credit <- function(evals, rubric_items){
  evals[evals$`Full credit.`, rubric_items] <- TRUE
  return (evals)
}
