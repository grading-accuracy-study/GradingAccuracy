#' Validate Metadata JSON
#'
#' This function validates a JSON file that stores metadata
#' about the assignment and its course. The contents of the saved JSON
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
  course_info <- metadata$course_info
  # check for all necessary arguments
  args <- c("department", "course_number", "course_name", "upper_div",
            "year", "semester", "assignment_name", "question_number",
            "question_name", "mode_of_question", "medium_of_answer",
            "content_of_answer", "scoring_type", "is_proctored")
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

  # Course Info Print Out
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
