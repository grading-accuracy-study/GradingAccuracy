#' Generate Results Table
#'
#' This function parses through the data/scores folders and generates
#' the results table for grading accuracy comparison.
#'
#' @param dir optionally, specify directory
#'
#' @returns a dataframe
#'
#' @export
generate_results_table <- function(dir = "."){
  dir = paste0(dir , "/data/scores/")
  courses <- list.dirs(path = dir,
                       full.names = F, recursive = FALSE)
  results_table <- data.frame()
  for (course in courses){
    row <- generate_results_row(dir = paste0(dir, course))
    results_table <- rbind(results_table, row)
  }

  return (results_table)
}

#' @importFrom tibble tibble
#' @importFrom readr read_csv
#' @importFrom jsonlite fromJSON
generate_results_row <- function(dir){
  expert <- read_csv(paste0(dir, "/expert.csv"))
  student <- read_csv(paste0(dir, "/student.csv"))
  pensieve <- read_csv(paste0(dir, "/pensieve.csv"))
  metadata <- fromJSON(paste0(dir, "/metadata.json"))
  course_info <- metadata$course_info
  rubric_items <- metadata$rubric_items

  tibble::tibble(
    `Subject` = course_info$department,
    #`Course` = paste(course_info$department, course_info$course_number),
    #`Course Name` = course_info$course_name,
    `Course Level` = ifelse(course_info$upper_div,
                            "Upper Division", "Lower Division"),
    #`Year` = paste(course_info$semester, course_info$year),
    `Question Name` = course_info$question_name,
    #`Mode of Question` = course_info$mode_of_question,
    #`Medium of Answer` = course_info$medium_of_answer,
    #`Content of Answer` = course_info$content_of_answer,
    `MAE: Expert.v.Student` = rubric_mae(expert, student,
                                         rubric_items$expert.v.students),
    `MAE: Pensieve.v.Student` = rubric_mae(pensieve, student,
                                           rubric_items$pensieve.v.students),
    `MAE: Expert.v.Pensieve` = rubric_mae(expert, pensieve,
                                          rubric_items$expert.v.pensieve),
    `ISP: Expert.v.Student` = isp(expert, student,
                                  rubric_items$expert.v.students),
    `ISP: Pensieve.v.Student` = isp(pensieve, student,
                                    rubric_items$pensieve.v.students),
    `ISP: Expert.v.Pensieve` = isp(expert, pensieve,
                                   rubric_items$expert.v.pensieve),
  )
}
