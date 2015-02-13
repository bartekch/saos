context("test searching functionality")

test_that("arguments are checked for correctness", {
  
  expect_error(search_judgments(judgmentDateFrom = "a"))
})