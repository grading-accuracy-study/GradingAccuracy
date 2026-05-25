#' Validate Metadata JSON
#'
#' This function validates a JSON file that stores metadata
#' about the assignment, its course and rubric items. The contents of the saved JSON
#' file can be printed out.
#'
#' @param file file path where metadata.json should be saved
#' @param verbose if course information should be printed out
#'
#'
#' @importFrom jsonlite read_json
#' @importFrom cli cli_abort cli_h1 cli_alert_success cli_text cli_h3 cli_ul cli_li
#'
#' @export
validate_metadata_json <- function(file = "./metadata.json", verbose = F){
  # load in metadata.json
  metadata <- jsonlite::read_json(file)
  if (!("course_info" %in% names(metadata))){
    cli::cli_abort("The course_info argument is missing from the following file: {.file {file}}")
  }
  if (!("rubric" %in% names(metadata))){
    cli::cli_abort("The rubric argument is missing from the following file: {.file {file}}")
  }

  ##### -----COURSE INFO CHECKS  -----#####
  course_info <- metadata$course_info
  # check for all necessary arguments
  args <- c("department", "course_number", "course_name", "upper_div",
            "year", "semester", "assignment_name", "question_number",
            "question_name", "mode_of_question", "medium_of_answer",
            "content_of_answer", "scoring_type", "is_proctored",
            "n_submissions", "mean_score", "total_points", "rubric_type")
  args_bool <- args %in% names(course_info)
  if (length(args) != sum(args_bool)){
    cli::cli_abort("The following arguments are missing from course_info: {.val {args[!args_bool]}}")
  }
  # check data types
  if (!is.logical(course_info$upper_div)){
    cli::cli_abort("{.val upper_div} should be a boolean.")
  }

  # check data types
  if (!is.logical(course_info$is_proctored)){
    cli::cli_abort("{.val is_proctored} should be a boolean.")
  }

  ##### -----RUBRIC CHECKS  -----#####
  uncalibrated <- metadata[["rubric"]][["uncalibrated"]]
  calibrated <- metadata[["rubric"]][["calibrated"]]
  if (is.null(calibrated)){
    cli::cli_abort("{.val calibrated} rubric is missing.")
  }
  ## Calibrated checks
  num_rubric <- length(calibrated[["rubric_items"]])
  # check equal number of rubric items and corresponding scores
  if (num_rubric != length(calibrated[["scores"]])){
    cli::cli_abort("{.val calibrated} rubric items is not the same length as {.val calibrated} scores.")
  }
  # check scores are numbers
  if (!is.numeric(unlist(calibrated[["scores"]]))){
    cli::cli_abort("{.val scores} of {.val calibrated} rubric is not numeric values.")
  }
  expected_rubric_keys <- paste0("R", 1:num_rubric)
  actual_rubric_keys <- names(calibrated[["rubric_items"]])
  if (!identical(expected_rubric_keys, actual_rubric_keys)){
    cli::cli_abort("{.val rubric_items} of {.val calibrated} rubric are misnamed (i.e. should be R1, R2, etc.).")
  }
  # if there is an uncalibrated rubric, same checks
  if (!is.null(uncalibrated) && length(uncalibrated) != 0){
    num_rubric <- length(uncalibrated[["rubric_items"]])
    # check equal number of rubric items and corresponding scores
    if (num_rubric != length(uncalibrated[["scores"]])){
      cli::cli_abort("{.val uncalibrated} rubric items is not the same length as {.val uncalibrated} scores.")
    }
    # check scores are numbers
    if (!is.numeric(unlist(uncalibrated[["scores"]]))){
      cli::cli_abort("{.val scores} of {.val uncalibrated} rubric is not numeric values.")
    }
    expected_rubric_keys <- paste0("R", 1:num_rubric)
    actual_rubric_keys <- names(uncalibrated[["rubric_items"]])
    if (!identical(expected_rubric_keys, actual_rubric_keys)){
      cli::cli_abort("{.val rubric_items} of {.val uncalibrated} rubric are misnamed (i.e. should be R1, R2, etc.).")
    }
  }
  ##### -----PRINTOUT MESSAGE  -----#####
  alert <- function(){
    full_course <- paste(course_info$department, course_info$course_number,
                         "-", course_info$course_name)
    # course-level information
    cli::cli_h1(full_course)
    upper_div <- ifelse(course_info$upper_div, "An upper-division course from",
                        "A lower-division course from")
    cli::cli_text("{upper_div} {course_info$semester} {course_info$year}")
    # assignment-level information
    cli::cli_h3(course_info$question_name)
    cli::cli_text("Question {course_info$question_number} from {course_info$assignment_name}")
    cli::cli_ul()
    cli::cli_li("Mode of Question: {course_info$mode_of_question}")
    cli::cli_li("Medium of Answer: {course_info$medium_of_answer}")
    cli::cli_li("Content of Answer: {course_info$content_of_answer}")
  }

  cli::cli_alert_success("The following file is successfully validated: {.file {file}}")

  if (verbose){
    alert()
  }
}


#' Update student scores based on metadata rubric items
#'
#' Computes total scores for each student submission by multiplying
#' rubric item responses by their corresponding point values defined
#' in a metadata JSON file.
#'
#'
#' @param csv the path to the student CSV file.
#' @param metadata  the path to the metadata JSON file
#' @param overwrite  whether to overwrite the original
#'   CSV file with updated scores
#' @param calibrated which rubric to apply
#'
#' @return A data frame containing the original student data with an
#'   updated `Score` column.
#'
#' @importFrom jsonlite read_json
#' @importFrom readr write_csv read_csv
#' @importFrom dplyr select all_of
#' @importFrom cli cli_alert_warning
#'
#' @export
update_scores <- function(csv, metadata = "./metadata.json",
                          overwrite = TRUE, calibrated){
  students <- read_csv(csv, show_col_types = FALSE)
  metadata <- jsonlite::read_json(metadata)
  if (calibrated){
    # use uncalibrated rubric b/c that's the original rubric
    rubric <- metadata[["rubric"]][["calibrated"]]
  } else{
    rubric <- metadata[["rubric"]][["uncalibrated"]]
  }
  rubric_pts <- rubric[["scores"]] |>
    unlist()
  rubric_items <- rubric[["rubric_items"]]|>
    names()
  rubric_mat <- students |>
    dplyr::select(dplyr::all_of(rubric_items)) |>
    as.matrix()
  scores <- t(t(rubric_mat) * rubric_pts) |>
    rowSums()
  new_scores <- students$SID[students["Score"] != scores]
  if (length(new_scores) != 0){
    cli::cli_alert_warning("The following students now have different scores: {.val {new_scores}}")
  }
  students["Score"] <- scores
  if (overwrite){
    write_csv(students, csv)
  }
  return (students)
}

#' Update Scores in metadata JSON
#'
#' Reads student and expert grading CSV files, computes
#' `n_submissions` and `mean_score`, and updates the corresponding fields in a
#' metadata JSON file.
#'
#'
#' @param folder the directory containing the CSV files
#' @param file the path to the metadata JSON file
#'
#'
#' @importFrom readr read_csv
#' @importFrom jsonlite read_json write_json
#' @importFrom cli cli_alert_warning
#'
#' @export
update_scores_in_metadata <- function(folder = "./",
                                      file = "./metadata.json"){
  if (file.exists(paste0(folder, "students-uncalibrated.csv"))){
    students <- read_csv(paste0(folder, "students-uncalibrated.csv"),
                         show_col_types = FALSE)
  } else{
    students <- read_csv(paste0(folder, "students-calibrated.csv"),
                         show_col_types = FALSE)
  }
  metadata <- jsonlite::read_json(file)
  experts <- read_csv(paste0(folder, "experts-calibrated.csv"),
                      show_col_types = FALSE)
  metadata[["course_info"]][["mean_score"]] = mean(experts$Score/max(experts$Score))
  metadata[["course_info"]][["n_submissions"]] = nrow(experts)
  jsonlite::write_json(metadata, file, pretty = T,
                       auto_unbox = T)
  if (nrow(students) != nrow(experts)){
    cli::cli_alert_warning("There is a different number of students between student-graded and expert-graded scores.")
  }
}
