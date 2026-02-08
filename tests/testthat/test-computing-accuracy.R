test_that("isp - if no valid comparison", {
  actual_isp <- isp(eval1 = data.frame(), eval2 = data.frame(),
      rubric_matching_list = "None")
  expect_true(is.na(actual_isp))
})

test_that("isp - SIDs same order", {
  eval1 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.95, 0.75, 0.25, 0.5, 0.45)
  )
  eval2 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.9, 0.7, 0.25, 0.5, 0.45)
  )
  actual_isp <- isp(eval1, eval2, list())

  expect_equal(actual_isp, 0.6)
})

test_that("isp - SIDs in different order", {
  eval1 <- data.frame(
    SID = c(5555, 2222, 3333, 4444, 1111),
    Score = c(0.45, 0.75, 0.25, 0.5, 0.95)
  )
  eval2 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.9, 0.7, 0.25, 0.5, 0.45)
  )
  actual_isp <- isp(eval1, eval2, list())

  expect_equal(actual_isp, 0.6)
})

test_that("isp - extra studeent", {
  eval1 <- data.frame(
    SID = c(5555, 2222, 3333, 4444, 1111, 6666),
    Score = c(0.45, 0.75, 0.25, 0.5, 0.95, 0.76)
  )
  eval2 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.9, 0.7, 0.25, 0.5, 0.45)
  )
  actual_isp <- isp(eval1, eval2, list())

  expect_equal(actual_isp, 0.6)
})

test_that("isp - missing SID", {
  eval1 <- data.frame(
    Score = c(0.45, 0.75, 0.25, 0.5, 0.95, 0.76)
  )
  eval2 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.9, 0.7, 0.25, 0.5, 0.45)
  )
  expect_error(isp(eval1, eval2, list()))
})

test_that("isp - missing Score", {
  eval1 <- data.frame(
    SID = c(5555, 2222, 3333, 4444, 1111, 6666)
  )
  eval2 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.9, 0.7, 0.25, 0.5, 0.45)
  )
  expect_error(isp(eval1, eval2, list()))
})

# test_that("rubric_mae - if no valid comparison", {
#   actual_mae <- rubric_mae(eval1 = data.frame(), eval2 = data.frame(),
#                     rubric_matching_list = "None")
#   expect_true(is.na(actual_mae))
# })
#
# test_that("rubric_mae - no provided rubric matching", {
#
# })
#
#
# test_that("rubric_mae - provided rubric matching with indices", {
#
# })
#
#
# test_that("rubric_mae - provided rubric matching with rubric names", {
#
# })
#
# test_that("rubric_mae - missing SID", {
#
# })
#
# test_that("rubric_mae -  missing rubric items", {
#
# })
#
# test_that("normalize_full_credit - correctly formatted inputs", {
#
# })
