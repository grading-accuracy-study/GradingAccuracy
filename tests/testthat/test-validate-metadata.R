test_that("update_scores - works correctly on calibrated", {
  csv <- system.file("extdata", "students-calibrated.csv", package = "GradingAccuracy")
  metadata <- system.file("extdata", "metadata-calibrated.json", package = "GradingAccuracy")
  df <- update_scores(csv, metadata, overwrite = F, calibrated = T)
  expect_identical(df$Score, c(1, 0.5, 0.5, 0, 1.0))
})

test_that("update_scores - works correctly on uncalibrated", {
  csv <- system.file("extdata", "students-uncalibrated.csv", package = "GradingAccuracy")
  metadata <- system.file("extdata", "metadata-uncalibrated.json", package = "GradingAccuracy")
  df <- update_scores(csv, metadata, overwrite = F, calibrated = F)
  expect_identical(df$Score, c(1, 0.25, 0.5, 0.25, 0.5))
})
