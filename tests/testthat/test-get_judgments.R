context("test get_judgments functionality")

test_that("get with no results returns proper object", {
  empty <- empty_get_result()
  expect_identical(suppressMessages(get_judgments(1234567890, verbose = FALSE)),
                   empty)
  expect_identical(get_judgments(empty_search_result(), verbose = FALSE), empty) 
})


test_that("c and [ methods works properly", {
  g <- get_judgments(sample(1e3, 10), verbose = FALSE)
  g0 <- empty_get_result()
  expect_that(inherits(c(g, g), "saos_judgments"), is_true())
  expect_that(inherits(c(g, g0), "saos_judgments"), is_true())
  expect_that(inherits(c(g0, g0), "saos_judgments"), is_true())
  expect_that(inherits(g[1:5], "saos_judgments"), is_true())
})