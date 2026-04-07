#' Calculate Differences in Expert Grades After QA/QC
#'
#' Find the differences in expert grades after comparing to AI grades
#' and to student grades.
#'
#' @param pre_dir directory before QA/QC
#' @param post_dir directory after QA/QC
#'
#' @return dataframe
#'
#' @importFrom tibble as_tibble
#' @export
expert_updates_table <- function(pre_dir, post_dir){
  assignments <- list.dirs(path = pre_dir,
                       full.names = F, recursive = FALSE)
  expert_updates_table <- data.frame()
  for (assign in assignments){
    row <- check_expert_updates_row(pre_dir = paste0(pre_dir, assign,"/"),
                                post_dir = paste0(post_dir, assign,"/"))
    expert_updates_table <- plyr::rbind.fill(expert_updates_table, row)
  }
  return (expert_updates_table)
}

#' @importFrom tibble tibble
check_expert_updates_row <- function(pre_dir, post_dir){
  # load data
  experts_pre <- load_as_rubric_mat(paste0(pre_dir, "experts-calibrated.csv"))
  experts_post <- load_as_rubric_mat(paste0(post_dir, "experts-calibrated.csv"))
  AI_grades <- load_as_rubric_mat(paste0(post_dir, "pensive-calibrated.csv"))
  # same order
  students <- rownames(experts_pre)
  if (!all(students %in% rownames(experts_post))){
    missing <- students[!(students %in% rownames(experts_post))]
    stop(paste0(basename(pre_dir), "- Mismatched students in experts-pre and experts-post:",
                missing, collapse = " "))

  }
  if (!all(students %in% rownames(AI_grades))){
    missing <- students[!(students %in% rownames(AI_grades))]
    stop(paste0(basename(pre_dir), "- Missing students in AI grades:",
                missing, collapse = " "))
  }
  experts_pre <- experts_pre[students, , drop = FALSE]
  experts_post <- experts_post[students, , drop = FALSE]
  # count how many differences
  changes <- rowSums(experts_pre != experts_post)
  total_changes <- sum(changes > 0)
  # students with updated grades
  diff_SIDs <- rownames(experts_pre)[changes > 0]
  # check original differences from pre-expert grades and AI
  ai_changes <- rowSums(experts_pre[diff_SIDs, , drop = F] != AI_grades[diff_SIDs,  , drop = F])
  total_ai_changes <- sum(ai_changes > 0)
  # total AI diffs
  diffs <- rowSums(experts_pre[students, ] != AI_grades[students,])
  total_ai_diffs <- sum(diffs > 0)
  # student QA/QC step
  total_stud_changes <- NA
  total_stud_diffs <- NA
  total_both_diffs <- NA
  if (file.exists(paste0(post_dir, "students-calibrated.csv"))){
    # changes from student-comparison
    total_stud_changes <- total_changes - total_ai_changes
    students_grades <- load_as_rubric_mat(paste0(pre_dir, "students-calibrated.csv"))
    if (!all(students %in% rownames(students_grades))){
      missing <- students[!(students %in% rownames(students_grades))]
      stop(paste0(basename(pre_dir), "- Missing students in student grades:", missing,
                 collapse = " "))
    }
    # total student diffs
    stud_diffs <- rowSums(experts_pre[students, ] != students_grades[students,])
    total_stud_diffs <- sum(stud_diffs > 0)
    # expert - both diffs
    total_both_diffs <- sum(stud_diffs > 0 & diffs > 0)
  }
  # create row
  expert_updates_row <- tibble::tibble(
    `Question Name` = basename(pre_dir),
    `Total Students` = length(students),
    `Expert-AI Diffs (pre)` = total_ai_diffs,
    `Expert-Student Diffs (pre)` = total_stud_diffs,
    `Expert-Both Diffs (pre)` = total_both_diffs,
    `Total QA/QC Changes` = total_changes,
    `Changes from AI Comparison` = total_ai_changes,
    `Changes from Student Comparison` = total_stud_changes
  )
  return(expert_updates_row)
}

#' @importFrom readr read_csv
load_as_rubric_mat <- function(df_path){
  df <- read_csv(df_path, show_col_types = FALSE)
  rubric_items <- grep("^R[0-9]+$", names(df), value = TRUE)
  rubric_mat <- df[, c(rubric_items)] |>
    as.matrix()
  rownames(rubric_mat) <- df$SID
  return(rubric_mat)
}
