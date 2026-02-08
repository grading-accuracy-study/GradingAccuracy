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
  for (course in courses){
    generate_results_row(dir = paste0(dir, course))
  }

}

#' @importFrom readr read_csv
#' @importFrom jsonlite fromJSON
generate_results_row <- function(dir){
  experts <- read_csv(paste0(dir, "/expert.csv"))
  student <- read_csv(paste0(dir, "/student.csv"))
  pensieve <- read_csv(paste0(dir, "/pensieve.csv"))
  metadata <- fromJSON(paste0(dir, "/metadata.json"))
}
