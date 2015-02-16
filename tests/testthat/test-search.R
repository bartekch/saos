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


test_that("c and [ methods works properly", {
  s <- search_judgments(limit = 10, verbose = FALSE)
  s0 <- empty_search_result()
  expect_that(inherits(c(s, s), "saos_search"), is_true())
  expect_that(inherits(c(s, s0), "saos_search"), is_true())
  expect_that(inherits(c(s0, s0), "saos_search"), is_true())
  expect_that(inherits(s[1:5], "saos_search"), is_true())
})