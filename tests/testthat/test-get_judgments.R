context("test get_judgments functionality")

test_that("get with no results returns proper object", {
  empty <- list()
  class(empty) <- c("saos_judgments", "list")
  
  expect_identical(get_judgments(1234567890, verbose = FALSE), empty)
  expect_identical(get_judgments(empty_search_result(), verbose = FALSE), empty) 
})