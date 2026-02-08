#' Calculate Proportion of Identical Scores
#'
#' This function calculates the proportion of identical scores
#' between two Gradescope evaluation dataframes
#'
#' @param eval1 first dataframe of Gradescope evaluations
#' @param eval2 second dataframe of Gradescope evaluations
#' @param rubric_matching_list this is only to check if the comparison is valid
#'
#' @return a single proportion
#'
#' @importFrom dplyr mutate rename summarize inner_join select n pull
#' @importFrom tidyr drop_na
#'
#' @export
isp <- function(eval1, eval2, rubric_matching_list){
  if (length(rubric_matching_list) == 1 && rubric_matching_list == "None"){
    return (NA)
  }

  if (!("SID" %in% colnames(eval1)) || !("SID" %in% colnames(eval2))){
    stop("Missing SID")
  }

  if (!("Score" %in% colnames(eval1)) || !("Score" %in% colnames(eval2))){
    stop("Missing Score")
  }

  eval1 <- eval1 |>
    rename(Score1 = Score) |>
    mutate(SID = as.character(SID)) |>
    select(SID, Score1)
  eval2 <- eval2 |>
    rename(Score2 = Score) |>
    mutate(SID = as.character(SID)) |>
    select(SID, Score2)
  inner_join(eval1, eval2, by = "SID") |>
    drop_na() |>
    summarize(
      Proportion = mean(Score1 == Score2)
    ) |>
    pull()
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
#' @param rubric_matching_list vector of rubric items to compare, if NULL, assume the same rubric
#'
#' @return double for mean absolute error
#'
#' @export
rubric_mae <- function(eval1, eval2, rubric_matching_list = NULL){
  if (length(rubric_matching_list) == 1 && rubric_matching_list == "None"){
    return (NA)
  }

  if (!("SID" %in% colnames(eval1)) || !("SID" %in% colnames(eval2))){
    stop("Missing SID")
  }

  if (is.null(rubric_matching_list)){
    rubric_items <- grep("^R[0-9]+$", names(eval1), value = TRUE)

    rubric_matching_list <- list(
      rubric_items,
      rubric_items
    )
  } else{
    rubric_matching_list <- list(
      rubric_matching_list[1, ],
      rubric_matching_list[2, ]
    )
  }
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
  mean(error_per_student)
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
#' @param full_credit column index or name for full credit column
#' @param rubric_items vector of rubric items that sum up to full credit
#'
#' @return normalized evals dataframe
#'
#' @export
normalize_full_credit <- function(evals, full_credit, rubric_items){
  if (is.numeric(rubric_items)){
    if (max(rubric_items) > ncol(evals)){
      stop("Rubric items not found")
    }
  } else{
    if (any(!(rubric_items %in% colnames(evals)))){
      stop("Rubric items not found")
    }
  }
  full_credit_row <- evals[[full_credit]]
  evals[full_credit_row, rubric_items] <- TRUE
  return (evals)
}
