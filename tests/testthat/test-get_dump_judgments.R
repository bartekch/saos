context("test get_dump_judgments functionality")

g <- get_dump_judgments(start_date = "1994-01-01", end_date = "1995-01-01", 
                        verbose = FALSE)

test_that("proper objects are returned", {
  expect_is(g, c("saos_judgments_dump", "list"))
  expect_identical(get_dump_judgments(end_date = "0001-01-01", verbose = FALSE),
                   empty_dump_result())
})


test_that("c and [ methods works properly", {
  g0 <- empty_dump_result()
  expect_is(c(g, g), c("saos_judgments_dump", "list"))
  expect_is(c(g, g0), c("saos_judgments_dump", "list"))
  expect_is(c(g0, g0), c("saos_judgments_dump", "list"))
  expect_is(g[1:5], c("saos_judgments_dump", "list"))
  expect_equal(length(c(g, g)), 2 * length(g))
  expect_equal(length(c(g, g0)), length(g))
  expect_equal(length(c(g0, g0)), 0)
})