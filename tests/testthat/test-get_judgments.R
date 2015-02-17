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
  expect_is(c(g, g), c("saos_judgments", "list"))
  expect_is(c(g, g0), c("saos_judgments", "list"))
  expect_is(c(g0, g0), c("saos_judgments", "list"))
  expect_is(g[1:5], c("saos_judgments", "list"))
  expect_equal(length(c(g, g)), 20)
  expect_equal(length(c(g, g0)), 10)
  expect_equal(length(c(g0, g0)), 0)
})