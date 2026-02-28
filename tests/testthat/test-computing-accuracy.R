test_that("isp - SIDs same order", {
  eval1 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.95, 0.75, 0.25, 0.5, 0.45)
  )
  eval2 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.9, 0.7, 0.25, 0.5, 0.45)
  )
  actual_isp <- isp(eval1, eval2)

  expect_equal(actual_isp, 0.6)
})

test_that("isp - perfect match", {
  eval1 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.95, 0.75, 0.25, 0.5, 0.45)
  )
  eval2 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.95, 0.75, 0.25, 0.5, 0.45)
  )
  actual_isp <- isp(eval1, eval2)

  expect_equal(actual_isp, 1)
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
  actual_isp <- isp(eval1, eval2)

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
  actual_isp <- isp(eval1, eval2)

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
  expect_error(isp(eval1, eval2))
})

test_that("isp - missing Score", {
  eval1 <- data.frame(
    SID = c(5555, 2222, 3333, 4444, 1111, 6666)
  )
  eval2 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    Score = c(0.9, 0.7, 0.25, 0.5, 0.45)
  )
  expect_error(isp(eval1, eval2))
})

test_that("rubric_mae - same SID order", {
  eval1 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    R1 = c(T, T, T, F, F),
    R2 = c(T, F, T, F, T)
  )
  eval2 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    R1 = c(T, T, F, F, F),
    R2 = c(T, T, T, T, T)
  )

  actual_mae <- rubric_mae(eval1 = eval1, eval2 = eval2)

  expect_equal(actual_mae, 0.6)
})

test_that("rubric_mae - different SID order", {
  eval1 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    R1 = c(T, T, T, F, F),
    R2 = c(T, F, T, F, T)
  )
  eval2 <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    R1 = c(T, F, T, F, F),
    R2 = c(T, T, T, T, T)
  )

  actual_mae <- rubric_mae(eval1 = eval1, eval2 = eval2)

  expect_equal(actual_mae, 0.6)
})

test_that("rubric_mae - different SID order, extra student", {
  eval1 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555, 6666),
    R1 = c(T, T, T, F, F, T),
    R2 = c(T, F, T, F, T, F)
  )
  eval2 <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    R1 = c(T, F, T, F, F),
    R2 = c(T, T, T, T, T)
  )

  actual_mae <- rubric_mae(eval1 = eval1, eval2 = eval2)

  expect_equal(actual_mae, 0.6)
})

test_that("rubric_mae - mismatched rubrics", {
  eval1 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    R1 = c(F, F, T, T, F),
    R2 = c(T, T, T, F, F),
    R3 = c(T, F, T, F, T)
  )
  eval2 <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    R1 = c(T, F, T, F, F),
    R2 = c(T, T, T, T, T)
  )

  expect_error(rubric_mae(eval1 = eval1, eval2 = eval2))
})

test_that("rubric_mae - mismatched rubrics, another example", {
  eval1 <- data.frame(
    SID = c(1111, 2222, 3333, 4444, 5555),
    R1 = c(F, F, T, T, F),
    R2 = c(T, T, T, F, F)
  )
  eval2 <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    R1 = c(T, F, T, F, F),
    R2 = c(T, T, T, T, T),
    R3 = c(T, F, T, F, T)
  )

  expect_error(rubric_mae(eval1 = eval1, eval2 = eval2))
})

test_that("rubric_mae - missing SID", {
  eval1 <- data.frame(
    R1 = c(T, T, T, F, F, T),
    R2 = c(T, F, T, F, T, F)
  )
  eval2 <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    R1 = c(T, F, T, F, F),
    R2 = c(T, T, T, T, T)
  )

  expect_error(rubric_mae(eval1 = eval1, eval2 = eval2,
                          rubric_matching_list = NULL))
})


test_that("normalize_full_credit - reformat all, indices", {
  eval_before <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, T, T, T, T),
    R2 = c(F, F, F, F, F),
    R3 = c(F, F, F, F, F)
  )

  eval_after <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, T, T, T, T),
    R2 = c(T, T, T, T, T),
    R3 = c(T, T, T, T, T)
  )

  actual_after <- normalize_full_credit(evals = eval_before, full_credit = 2,
                                        rubric_items = c(3, 4))

  expect_equal(eval_after, actual_after)
})

test_that("normalize_full_credit - reformat all, row names", {
  eval_before <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, T, T, T, T),
    R2 = c(F, F, F, F, F),
    R3 = c(F, F, F, F, F)
  )

  eval_after <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, T, T, T, T),
    R2 = c(T, T, T, T, T),
    R3 = c(T, T, T, T, T)
  )

  actual_after <- normalize_full_credit(evals = eval_before, full_credit = "Full_Credit",
                                        rubric_items = c("R2", "R3"))

  expect_equal(eval_after, actual_after)
})

test_that("normalize_full_credit - reformat few, indices", {
  eval_before <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, F, F, T, T),
    R2 = c(F, T, F, F, F),
    R3 = c(F, F, F, F, F)
  )

  eval_after <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, F, F, T, T),
    R2 = c(T, T, F, T, T),
    R3 = c(T, F, F, T, T)
  )

  actual_after <- normalize_full_credit(evals = eval_before, full_credit = 2,
                                        rubric_items = c(3, 4))

  expect_equal(eval_after, actual_after)
})

test_that("normalize_full_credit - reformat few, row names", {
  eval_before <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, F, F, T, T),
    R2 = c(F, T, F, F, F),
    R3 = c(F, F, F, F, F)
  )

  eval_after <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, F, F, T, T),
    R2 = c(T, T, F, T, T),
    R3 = c(T, F, F, T, T)
  )

  actual_after <- normalize_full_credit(evals = eval_before, full_credit = "Full_Credit",
                                        rubric_items = c("R2", "R3"))

  expect_equal(eval_after, actual_after)
})

test_that("normalize_full_credit - missing rubric items, row names", {
  eval_before <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, F, F, T, T),
    R2 = c(F, T, F, F, F),
    R3 = c(F, F, F, F, F)
  )

  expect_error(normalize_full_credit(evals = eval_before, full_credit = "Full_Credit",
                                        rubric_items = c("R2", "R4")))
})

test_that("normalize_full_credit - missing rubric items, indices", {
  eval_before <- data.frame(
    SID = c(1111, 3333, 2222, 4444, 5555),
    `Full_Credit` = c(T, F, F, T, T),
    R2 = c(F, T, F, F, F),
    R3 = c(F, F, F, F, F)
  )

  expect_error(normalize_full_credit(evals = eval_before, full_credit = "Full_Credit",
                                     rubric_items = c(3, 6)))
})
