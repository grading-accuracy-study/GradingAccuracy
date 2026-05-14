# Package index

## Data Cleaning and Preparation

These functions are for preparing the original exports for accuracy
computations, by deidentifying identifiable information and making any
adjustments to rubric items.

- [`read_evals()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/read_evals.md)
  : Read Gradescope Evaluations CSV
- [`validate_metadata_json()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/validate_metadata_json.md)
  : Validate Metadata JSON
- [`update_scores()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/update_scores.md)
  : Update student scores based on metadata rubric items
- [`update_scores_in_metadata()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/update_scores_in_metadata.md)
  : Update Scores in metadata JSON
- [`deidentify_graders()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/deidentify_graders.md)
  : Deidentify Graders
- [`deidentify_gradescope_evals()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/deidentify_gradescope_evals.md)
  : Deidentify Gradescope Evaluations
- [`generate_rubric_texts()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/generate_rubric_texts.md)
  : Generate Rubric Texts
- [`normalize_full_credit()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/normalize_full_credit.md)
  : Normalize Full Credit

## Accuracy Functions

These functions are for computing the accuracy rates between different
graders.

- [`generate_metadata_table()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/generate_metadata_table.md)
  : Generate Metadata Table
- [`generate_results_table()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/generate_results_table.md)
  : Generate Results Table
- [`generate_gt_results_table()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/generate_gt_results_table.md)
  : Generate Results Table in GT Format
- [`expert_updates_table()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/expert_updates_table.md)
  : Calculate Differences in Expert Grades After QA/QC
- [`export_grading_differences_xlsx()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/export_grading_differences_xlsx.md)
  : Export Differences between Two Graders as Excel Spreadsheet
- [`export_grading_differences_gt()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/export_grading_differences_gt.md)
  : Export Differences between Two Graders as GT Table
- [`find_differences_wrt_AI()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/find_differences_wrt_AI.md)
  : Find Differences with respect to AI Table
- [`find_differences_wrt_students()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/find_differences_wrt_students.md)
  : Find Differences with respect to Students Table
- [`compute_mae_and_isp()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/compute_mae_and_isp.md)
  : Calculate MAE and ISP
- [`isp()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/isp.md)
  : Calculate Proportion of Identical Scores
- [`rubric_mae()`](https://grading-accuracy-study.github.io/GradingAccuracy/reference/rubric_mae.md)
  : Mean Absolute Error of Rubric Items
