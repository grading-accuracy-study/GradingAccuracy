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
#' @param ignored_nrows how many lines at the end of csv to exclude
#' @param output_path path to de-idenfitied Gradescope output
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr left_join mutate select join_by
#'
#' @export
deidentify_gradescope_evals <- function(gs_csv_path, ids_csv_path,
                                        output_path, ignored_nrows = 3){

  gs_csv <- read_evals(gs_csv_path, ignored_nrows = ignored_nrows)
  ids_csv <- read_csv(ids_csv_path, show_col_types = FALSE) |>
    mutate(SID = as.numeric(SID),
           Generated_ID = as.numeric(Generated_ID))

  de_identified <- gs_csv |>
    mutate(`Assignment Submission ID` = as.numeric(`Assignment Submission ID`)) |>
    left_join(ids_csv, by = join_by("Assignment Submission ID" == "SID")) |>
    mutate(SID = Generated_ID) |>
    select(SID, Score:Comments, Tags)

    write_evals(de_identified = de_identified, output_path = output_path,
                ignored_nrows = ignored_nrows, original_path = gs_csv_path)

}


#' Read Gradescope Evaluations CSV
#'
#' This function reads in the .csv file from Gradescope's
#' "Export Evaluations" functionality. The formatting of this
#' file is not in tidy format because the last few lines include
#' Point Values, Rubric Numbers and Scoring Method. These are
#' excluded when loading in the file.
#'
#' @param csv_path path to exported Gradescope evaluations csv
#' @param ignored_nrows how many of the last lines to ignore
#'
#' @return a dataframe of Gradescope evaluations
#'
#' @importFrom readr read_csv
#' @importFrom utils head
#'
#' @export
read_evals <- function(csv_path, ignored_nrows = 3){
  if (ignored_nrows == 0){
    df <- read_csv(csv_path, show_col_types = FALSE) |>
      mutate(SID = as.numeric(SID))
    return (df)
  }
  remove_last_lines = head(readLines(csv_path), -ignored_nrows) |>
    paste(collapse = "\n")
  read_csv(remove_last_lines, show_col_types = FALSE) |>
    mutate(SID = as.numeric(SID))
}

#' Generate Rubric Texts
#'
#' This function generates or updated the rubric_items.csv
#' with a grades csv file's rubric items. The grades csv is
#' saved with "R1", "R2", ... labels for the rubric items
#' as a new csv, and this function returns this new grades csv
#' as a dataframe.
#'
#' @param csv_path path to exported Gradescope evaluations csv
#' @param output_folder folder for rubric_items.csv
#' @param ignored_nrows how many of the last lines to ignore
#' @param existing if there is an existing rubric_items.csv
#'
#' @return a dataframe with rubric items in "R1", "R2",... format
#' @importFrom readr read_csv write_csv
#' @export
generate_rubric_texts <- function(csv_path, output_folder, ignored_nrows = 3,
                                  existing = TRUE){
  ## UPDATE RUBRIC ITEMS
  rubric_texts_path <- paste0(output_folder, "rubric_items.csv")
  grades_df <- read_evals(csv_path, ignored_nrows = ignored_nrows)
  rubric_items <- get_rubric_items(grades_df)
  n <- length(rubric_items)

  if (existing){
    rubric_texts <- read_csv(rubric_texts_path)
    current_n <- ncol(rubric_texts)-1
    row <- c(csv_path, rubric_items,
             rep(NA, current_n - n))
    # add more rubric-item columsn if needed
    if (n > current_n){
      new_cols <- paste0("R", (current_n + 1):n)
      df[new_cols] <- NA
    }
    rubric_texts <- rbind(rubric_texts, row)
  } else{
    row <- c(csv_path, rubric_items)
    rubric_texts <- as.data.frame(t(row))
    colnames(rubric_texts) <- c("File Path",paste0("R", 1:n))
  }
  write_csv(rubric_texts, file = rubric_texts_path)
  ## EXPORTED FILE
  new_names <- paste0("R", 1:n)
  colnames(grades_df)[match(rubric_items, colnames(grades_df))] <- new_names
  write_evals(grades_df, output_path = paste0(output_folder, csv_path),
              original_path = csv_path, ignored_nrows = ignored_nrows)
  return (grades_df)
}

get_rubric_items <- function(grades_df){
  start <- which(colnames(grades_df) == "Submission Time") + 1
  if (length(start) == 0){
    start <- which(colnames(grades_df) == "Score") + 1
  }
  end <- which(colnames(grades_df) == "Adjustment") - 1
  if (length(end) == 0){
    end <- length(colnames(grades_df))
  }
  rubric_items <- colnames(grades_df)[start:end]
  return (rubric_items[rubric_items != "SID"])
}

#' @importFrom readr write_csv
#' @importFrom utils tail
write_evals <- function(de_identified, output_path,
                        ignored_nrows = 3, original_path){
  # append remove lines to maintain original format
  write_csv(de_identified, output_path)
  if (ignored_nrows > 0){
    deidentified_lines <- readLines(output_path)
    # ignored_nrows + 1 due to whitespaces in csv file
    last_lines <- tail(readLines(original_path), ignored_nrows+1)
    writeLines(c(deidentified_lines, last_lines), output_path)
  }

}
