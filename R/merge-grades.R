#' Deidentify Gradescope
#'
#' This function de-identifies the exported
#' Gradescope csv by swapping all columns with
#' identifiable data with the de-identified IDs
#' in an existing table. Note that Gradescope csv
#' must have a column called `Assignment Submission ID` and the ids table
#' must have two columns: `SID`, `Generated_ID`
#'
#' @param gs_csv_path path to exported Gradescope csv
#' @param ids_csv_path path to de-identified table csv
#' @param output_path path to de-idenfitied Gradescope output
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr left_join mutate select join_by
#'
#' @export
deidentify_gradescope_evals <- function(gs_csv_path, ids_csv_path,
                                  output_path){

  gs_csv <- read_evals(gs_csv_path)
  ids_csv <- read_csv(ids_csv_path, show_col_types = FALSE)

  de_identified <- gs_csv |>
    mutate(`Assignment Submission ID` = as.numeric(`Assignment Submission ID`)) |>
    left_join(ids_csv, by = join_by("Assignment Submission ID" == "SID")) |>
    mutate(SID = Generated_ID) |>
    select(SID, Score:Comments, Tags)

    write_evals(de_identified = de_identified, output_path = output_path,
                original_path = gs_csv_path)

}


#' Read Gradescope Evaluations CSV
#'
#' This function reads in the .csv file from Gradescope's
#' "Export Evaluations" functionality. The formatting of this
#' file is not in tidy format because the last few lines include
#' Point Values, Rubric Numbers and Scoring Method. These are
#' excluded for the sake of this package.
#'
#' @param csv_path path to exported Gradescope evaluations csv
#' @param ignore_nrows how many of the last lines to ignore
#'
#' @importFrom readr read_csv
#'
#' @export
read_evals <- function(csv_path, ignore_nrows = 5){
  nrow = length(readLines(gs_csv_path)) - ignore_nrows
  read_csv(gs_csv_path, show_col_types = FALSE,
           n_max = nrow)
}

#' @importFrom readr write_csv
write_evals <- function(de_identified, output_path,
                        ignored_nrows = 5, original_path){
  # append remove lines to maintain original format
  write_csv(de_identified, output_path)
  deidentified_lines <- readLines(output_path)
  last_lines <- tail(readLines(original_path), ignored_nrows-1)
  writeLines(c(deidentified_lines, last_lines), output_path)

}
