students_lookup_table <- function(csv_path, roster_csv){
  gradescope_csv <- read_evals(csv_path)
  id_cols_removed <- c("Assignment Submission ID",
                       "Question Submission ID",
                       "First Name", "Last Name",
                       "Name", "Email")
  gradescope_csv <- gradescope_csv[, !names(gradescope_csv) %in% id_cols_removed]
  n <- nrow(gradescope_csv)
  roster <- read_csv(roster_csv)
  fake_ids <- sample(roster$SID, n, replace = F)
  lookup <- data.frame(SID = gradescope_csv$SID,
                      Generated_ID = fake_ids)
  write_csv(lookup, "deidentified-lookup-table.csv")
}

#' Deidentify Graders
#'
#' This function swaps the original grader names with fake names
#' from a roster and saves the deidentified grades as a csv
#' and the lookup table for the graders as a csv.
#'
#' @param csv_path path to exported Gradescope evaluations csv
#' @param roster_csv path to roster csv
#' @param output_path path to save deidentified grades
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr left_join join_by mutate select
#' @export
deidentify_graders <- function(csv_path, roster_csv,
                               output_path){
  gradescope_csv <- read_evals(csv_path) |>
    dplyr::filter(!is.na(Grader))
  graders <- unique(gradescope_csv$Grader)
  n <- length(graders)
  roster <- read_csv(roster_csv)
  fake_names <- sample(roster$Name, n, replace = F)
  lookup <- data.frame(Name = graders,
                       Generated_Names = fake_names)
  write_csv(lookup, file = "deidentified-lookup-table-grader.csv")
  gradescope_csv |>
    dplyr::left_join(lookup, by = dplyr::join_by("Grader" == "Name")) |>
    dplyr::mutate(Grader = Generated_Names) |>
    dplyr::select(-Generated_Names) |>
    write_evals(output_path = output_path,
                original_path = csv_path)
}
