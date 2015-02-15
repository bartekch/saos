context("test searching functionality")

test_that("arguments are checked for correctness", {
  
  expect_error(search_judgments(judgmentDateFrom = "a"))
})


test_that("search with no results returns proper object", {
  expect_identical(search_judgments(judgmentDateTo = "0001-01-01", verbose = FALSE),
                   empty_search_result())
  expect_identical(search_judgments(limit = 0, verbose = FALSE), 
                   empty_search_result())
})