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
#'@importFrom dplyr mutate rename
#'
#' @export
identical_score_prop <- function(eval1, eval2){
  eval1 <- eval1 |>
    rename(Score1 = Score) |>
    select(SID, Score1)
  eval2 <- eval2 |>
    rename(Score2 = Score) |>
    select(SID, Score2)
  combined <- inner_join(eval1, eval2, by = "SID")
  sum(combined$Score1 == combined$Score2)
}


#' Normalize Full Credit
#'
#' This function "normalizes" the "Full Credit" toggle
#' on the evaluations by making all other rubric items
#' TRUE to allow for comparisons across individual rubric
#' items.
#'
#' @param evals dataframe of Gradescope evaluations
#'
#' @export
normalize_full_credit <- function(evals){

}
