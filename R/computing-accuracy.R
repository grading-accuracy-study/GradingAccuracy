#' Export Differences between Two Graders as Excel Spreadsheet
#'
#' This function saves an Excel spreadsheet with the differences
#' in rubric items between two graders with mismatched rubrics highlighted.
#'
#' @param find_differences object from the function find_differences_table()
#' @param existing if workbook exists
#' @param sheet_name name of sheet in workbook
#' @param students if true, comparing with students; if false, comparing with AI
#' @param dir optionally, where workbook is saved
#'
#' @importFrom openxlsx loadWorkbook createWorkbook removeWorksheet addWorksheet writeData createStyle addStyle saveWorkbook
#' @export
export_grading_differences_xlsx <- function(find_differences, existing, students = T,
                                            sheet_name, dir = "."){
  combined <- find_differences$combined
  mismatch_matrix <- find_differences$mismatch_matrix
  output_file <- paste0(dir, "/rubric_differences_wrt_AI.xlsx")
  if (students){
    output_file <- paste0(dir, "/rubric_differences_wrt_students.xlsx")
  }
  if (existing && file.exists(output_file)) {
    wb <- loadWorkbook(output_file)
  } else {
    wb <- createWorkbook()
  }

  # If sheet already exists, remove it
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }

  addWorksheet(wb, sheet_name)

  writeData(wb, sheet_name, combined)

  pink_style <- createStyle(fgFill = "#FFC0CB")
  rubric_cols <- colnames(mismatch_matrix)[-length(colnames(mismatch_matrix))]

  for (col in rubric_cols) {

    diff_students <- rownames(mismatch_matrix)[mismatch_matrix[, col]]

    for (student in diff_students) {

      rows_to_color <- which(combined$SID == student)
      col_index <- which(names(combined) == col)

      addStyle(
        wb,
        sheet = sheet_name,
        style = pink_style,
        rows = rows_to_color + 1,  # +1 for header
        cols = col_index,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  saveWorkbook(wb, output_file, overwrite = TRUE)
}

#' Export Differences between Two Graders as GT Table
#'
#' This function exports the differences in rubric items
#' in GT format between two graders with mismatched rubrics highlighted.
#'
#' @param find_differences object from the function find_differences_table()
#'
#' @returns a gt object
#' @importFrom gt gt cols_hide tab_style cell_fill cells_body
#' @export
export_grading_differences_gt <- function(find_differences){
  combined <- find_differences$combined
  mismatch_matrix <- find_differences$mismatch_matrix
  # create gt table for display
  gt_table <- combined |>
    gt::gt(groupname_col = "Name") |>
    gt::cols_hide(columns = "Name")
  rubric_cols <- colnames(mismatch_matrix)[-length(colnames(mismatch_matrix))]
  for (col in rubric_cols) {
    # Students where this rubric differs
    diff_students <- rownames(mismatch_matrix)[mismatch_matrix[, col]]
    gt_table <- gt_table |>
      gt::tab_style(
        style = gt::cell_fill(color = "pink"),
        locations = gt::cells_body(
          columns = col,
          rows = SID %in% diff_students
        )
      )
  }
  gt_table
}


#' Find Differences with respect to AI Table
#'
#' Find the differences between AI grading and experts and return
#' all graded assignments that are different and a matrix of which rubrics
#' are mismatched.
#'
#' @param experts_file file with expert graders
#' @param ai_file file with AI graders
#'
#' @return a list of a df and a matrix
#'
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows left_join relocate arrange desc
#' @importFrom tibble as_tibble
#' @export
find_differences_wrt_AI <- function(experts_file, ai_file){
  # load in data
  experts_eval <- readr::read_csv(experts_file, show_col_types = F)
  ai_eval <- readr::read_csv(ai_file, show_col_types = F)

  # find differences in rubric toggles
  diffs <- find_differences(experts_eval, ai_eval)
  `Absolute Error` <- diffs$error_per_student
  if (sum(`Absolute Error`) == 0){
    # all match
    return (NULL)
  }
  rubric1 <- diffs$rubric1
  rubric2 <- diffs$rubric2
  # add error
  rubric1 <- cbind(rubric1, `Absolute Error`)
  rubric2 <- cbind(rubric2, `Absolute Error`)
  # filter for students with errors
  rubric1 <- rubric1[(`Absolute Error`>0), ]
  rubric2 <- rubric2[(`Absolute Error`>0), ]

  # find names from original dataframes
  name_lookup <- NULL
  if ("Name" %in% colnames(experts_eval)) {
    name_lookup <- experts_eval[, c("SID", "Name")]
  } else {
    name_lookup <- ai_eval[, c("SID", "Name")]
  }
  name_lookup$SID <- as.character(name_lookup$SID)
  # Convert matrices back to data frames
  df1 <- tibble::as_tibble(rubric1)
  df2 <- tibble::as_tibble(rubric2)
  # add back SIDs
  df1$SID <- rownames(rubric1)
  df2$SID <- rownames(rubric2)
  # add grader
  df1$Grader <- sub("-.*$", "", basename(experts_file))
  df2$Grader <- sub("-.*$", "", basename(ai_file))
  # convert rubric items to booleans
  # Find rubric columns
  rubric_cols <- grep("^R[0-9]+$", names(df1), value = TRUE)
  df1[rubric_cols] <- lapply(df1[rubric_cols], as.logical)
  df2[rubric_cols] <- lapply(df2[rubric_cols], as.logical)
  # add names for easy lookup
  combined <- dplyr::bind_rows(df1, df2) |>
    dplyr::left_join(name_lookup, by = "SID") |>
    dplyr::relocate(Name, SID, Grader, `Absolute Error`) |>
    dplyr::arrange(dplyr::desc(`Absolute Error`), Name, SID, Grader)
  # Logical matrix of mismatches
  mismatch_matrix <- rubric1 != rubric2
  # Keep rownames for matching
  rownames(mismatch_matrix) <- rownames(rubric1)

  return(list(combined = combined, mismatch_matrix = mismatch_matrix))
}


#' Find Differences with respect to Students Table
#'
#' Find the differences between students grading and experts and return
#' all graded assignments that are different and a matrix of which rubrics
#' are mismatched. Note that all mismatches between AI and expert grading
#' are removed to prevent redundancy.
#'
#' @param experts_file file with experts graders
#' @param student_file file with student graders
#' @param ai_file file with ai graders
#' @param ai_diffs find_differences results for experts v. AI
#'
#' @return a list of a df and a matrix
#'
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows left_join relocate arrange desc filter
#' @importFrom tibble as_tibble
#' @export
find_differences_wrt_students <- function(experts_file, student_file,
                                          ai_file, ai_diffs){
  # load in data
  experts_eval <- readr::read_csv(experts_file, show_col_types = F)
  student_eval <- readr::read_csv(student_file, show_col_types = F)

  # find expert vs. AI differences to remove
  ai_errors <- rownames(ai_diffs$mismatch_matrix)
  experts_eval <- experts_eval |>
    filter(!(SID %in% ai_errors))
  student_eval <- student_eval |>
    filter(!(SID %in% ai_errors))

  # find differences in rubric toggles
  diffs <- find_differences(experts_eval, student_eval)
  `Absolute Error` <- diffs$error_per_student
  if (sum(`Absolute Error`) == 0){
    # all match
    return (NULL)
  }
  rubric1 <- diffs$rubric1
  rubric2 <- diffs$rubric2
  # add error
  rubric1 <- cbind(rubric1, `Absolute Error`)
  rubric2 <- cbind(rubric2, `Absolute Error`)
  # filter for students with errors
  rubric1 <- rubric1[(`Absolute Error`>0), ]
  rubric2 <- rubric2[(`Absolute Error`>0), ]

  # find names from original dataframes
  name_lookup <- NULL
  if ("Name" %in% colnames(experts_eval)) {
    name_lookup <- experts_eval[, c("SID", "Name")]
  } else if ("Name" %in% colnames(experts_eval)){
    name_lookup <- student_eval[, c("SID", "Name")]
  } else {
    ai_eval <- readr::read_csv(ai_file, show_col_types = F)
    name_lookup <- ai_eval[, c("SID", "Name")]
  }
  name_lookup$SID <- as.character(name_lookup$SID)
  # Convert matrices back to data frames
  df1 <- tibble::as_tibble(rubric1)
  df2 <- tibble::as_tibble(rubric2)
  # add back SIDs
  df1$SID <- rownames(rubric1)
  df2$SID <- rownames(rubric2)
  # add grader
  df1$Grader <- sub("-.*$", "", basename(experts_file))
  df2$Grader <- sub("-.*$", "", basename(student_file))
  # convert rubric items to booleans
  # Find rubric columns
  rubric_cols <- grep("^R[0-9]+$", names(df1), value = TRUE)
  df1[rubric_cols] <- lapply(df1[rubric_cols], as.logical)
  df2[rubric_cols] <- lapply(df2[rubric_cols], as.logical)
  # add names for easy lookup
  combined <- dplyr::bind_rows(df1, df2) |>
    dplyr::left_join(name_lookup, by = "SID") |>
    dplyr::relocate(Name, SID, Grader, `Absolute Error`) |>
    dplyr::arrange(dplyr::desc(`Absolute Error`), Name, SID, Grader)
  # Logical matrix of mismatches
  mismatch_matrix <- rubric1 != rubric2
  # Keep rownames for matching
  rownames(mismatch_matrix) <- rownames(rubric1)

  return(list(combined = combined, mismatch_matrix = mismatch_matrix))
}


#' Calculate MAE and ISP
#'
#' This function calculates the proportion of identical scores
#' and mean absolute error of rubric items
#' between two Gradescope evaluation csv
#'
#' @param file1 file path for first grades csv
#' @param file2 file path for second grades csv
#'
#' @return a list
#'
#' @importFrom readr read_csv
#'
#' @export
compute_mae_and_isp <- function(file1, file2){
  eval1 <- readr::read_csv(file1, show_col_types = FALSE)
  eval2 <- readr::read_csv(file2, show_col_types = FALSE)
  list(MAE = rubric_mae(eval1, eval2),
       ISP = isp(eval1, eval2))
}


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
#' @importFrom dplyr mutate rename summarize inner_join select n pull
#' @importFrom tidyr drop_na
#'
#' @export
isp <- function(eval1, eval2){
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
#'
#' @return double for mean absolute error
#'
#' @export
rubric_mae <- function(eval1, eval2){
  # find differences in rubric toggles
  error_per_student <- find_differences(eval1, eval2)$error_per_student
  # mean absolute error calculation
  mean(error_per_student)
}


find_differences <- function(eval1, eval2){
  if (!("SID" %in% colnames(eval1)) || !("SID" %in% colnames(eval2))){
    stop("Missing SID")
  }
  rubric_items1 <- grep("^R[0-9]+$", names(eval1), value = TRUE)
  rubric_items2 <- grep("^R[0-9]+$", names(eval2), value = TRUE)


  if (!identical(rubric_items1, rubric_items2)){
    stop("Mismatched rubrics")
  }

  rubric_matching_list <- list(
    rubric_items1,
    rubric_items2)
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

  error_per_student <- rowSums(check_equal)

  return (list(error_per_student = error_per_student,
              rubric1 = rubric1,
              rubric2 = rubric2))
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
