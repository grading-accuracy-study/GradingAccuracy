# Shared fixtures -----------------------------------------------------------

experts_csv <- system.file("extdata", "experts-calibrated.csv",
                            package = "GradingAccuracy")
students_csv <- system.file("extdata", "students-calibrated.csv",
                             package = "GradingAccuracy")
metadata_json <- system.file("extdata", "metadata.json",
                              package = "GradingAccuracy")

# compute_mae_and_isp - without metadata ------------------------------------

test_that("compute_mae_and_isp - returns MAE, wMAE (NA), and ISP without metadata", {
  result <- compute_mae_and_isp(experts_csv, students_csv)

  expect_named(result, c("MAE", "wMAE", "ISP"))
  expect_true(is.numeric(result$MAE))
  expect_true(is.na(result$wMAE))
  expect_true(is.numeric(result$ISP))
})

test_that("compute_mae_and_isp - MAE is non-negative", {
  result <- compute_mae_and_isp(experts_csv, students_csv)
  expect_gte(result$MAE, 0)
})

test_that("compute_mae_and_isp - ISP is between 0 and 1", {
  result <- compute_mae_and_isp(experts_csv, students_csv)
  expect_gte(result$ISP, 0)
  expect_lte(result$ISP, 1)
})

# compute_mae_and_isp - with metadata ---------------------------------------

test_that("compute_mae_and_isp - returns numeric wMAE when metadata supplied", {
  result <- compute_mae_and_isp(experts_csv, students_csv,
                                metadata_file = metadata_json)

  expect_named(result, c("MAE", "wMAE", "ISP"))
  expect_true(is.numeric(result$wMAE))
  expect_false(is.na(result$wMAE))
})

test_that("compute_mae_and_isp - wMAE is non-negative", {
  result <- compute_mae_and_isp(experts_csv, students_csv,
                                metadata_file = metadata_json)
  expect_gte(result$wMAE, 0)
})

test_that("compute_mae_and_isp - MAE unaffected by presence of metadata", {
  result_no_meta  <- compute_mae_and_isp(experts_csv, students_csv)
  result_with_meta <- compute_mae_and_isp(experts_csv, students_csv,
                                          metadata_file = metadata_json)

  expect_equal(result_no_meta$MAE, result_with_meta$MAE)
  expect_equal(result_no_meta$ISP, result_with_meta$ISP)
})

test_that("compute_mae_and_isp - wMAE equals MAE when all weights are equal", {
  # Write a temp metadata with equal weights matching the 4-item rubric
  equal_weight_meta <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      rubric = list(
        calibrated = list(scores = list(1, 1, 1, 1))
      )
    ),
    equal_weight_meta, auto_unbox = TRUE
  )
  result <- compute_mae_and_isp(experts_csv, students_csv,
                                metadata_file = equal_weight_meta)
  expect_equal(result$wMAE, result$MAE)
})

test_that("compute_mae_and_isp - wMAE correct for known example", {
  # experts vs students differ only on R3 for SID 1002 (weight 0.5)
  # All other rows match.  Expected wMAE = 0.5 / 5 = 0.1
  weights <- c(1.0, 0.5, 0.5, 0.0)
  result <- compute_mae_and_isp(experts_csv, students_csv,
                                metadata_file = metadata_json)
  expected_wmae <- (0 + 0.5 + 0 + 0 + 0) / 5
  expect_equal(result$wMAE, expected_wmae)
})

# compute_mae_and_isp - identical files -------------------------------------

test_that("compute_mae_and_isp - identical files give MAE=0, wMAE=0, ISP=1", {
  result <- compute_mae_and_isp(experts_csv, experts_csv,
                                metadata_file = metadata_json)

  expect_equal(result$MAE,  0)
  expect_equal(result$wMAE, 0)
  expect_equal(result$ISP,  1)
})
