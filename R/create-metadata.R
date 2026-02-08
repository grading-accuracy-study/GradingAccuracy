#' Create Metadata JSON
#'
#' This function creates a JSON file that stores metadata
#' about the assignment and its course. The contents of the saved JSON
#' file is also printed out.
#'
#' @param dir directory where metadata.json should be saved
#' @param department department abbreviation (e.g. "STAT", "DATA")
#' @param course_number course number
#' @param course_name name of course (e.g. "Introduction to Probability")
#' @param upper_div if the course is upper-division or lower-division
#' @param year year of the course
#' @param semester semester of the course (e.g. "Spring", "Fall")
#' @param assignment_name name of assignment (e.g. "Midterm 1")
#' @param question_number number of question, including subparts (e.g. "5c")
#' @param question_name optionally name of question (e.g. "Data Visualization")
#' @param mode_of_question mode of question (e.g. "fill in the blank", "open-ended")
#' @param medium_of_answer medium of student's answer (e.g. "handwritten", "typed")
#' @param content_of_answer expected content of student's answer (e.g. "English", "math", "code")
#'
#'
#' @importFrom jsonlite toJSON write_json
#'
#' @export

create_metadata_json <- function(dir = "", department, course_number, course_name,
                                 upper_div, year, semester, assignment_name,
                                 question_number, question_name = "", mode_of_question,
                                 medium_of_answer, content_of_answer){
  course_metadata <- list(
    department = department,
    course_number = course_number,
    course_name = course_name,
    upper_div = upper_div,
    year = year,
    semester = semester,
    assignment_name = assignment_name,
    question_number = question_number,
    question_name = question_name,
    mode_of_question = mode_of_question,
    medium_of_answer = medium_of_answer,
    content_of_answer = content_of_answer
  )

  jsonlite::write_json(course_metadata, paste0(dir, "metadata.json"),
                       pretty = TRUE, auto_unbox = TRUE)

  jsonlite::toJSON(course_metadata, pretty = TRUE)

}
