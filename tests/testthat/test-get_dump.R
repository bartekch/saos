context("test get_dump_judgments functionality")

test_that("get_dump with no results returns proper object", {
  expect_identical(get_dump_judgments(end_date = "0001-01-01", verbose = FALSE),
                   empty_dump_result())
})


test_that("c and [ methods works properly", {
  g <- get_dump_judgments(start_date = "1994-01-20", end_date = "1994-01-25", 
                          verbose = FALSE)
  g0 <- empty_dump_result()
  expect_that(inherits(c(g, g), "saos_judgments_dump"), is_true())
  expect_that(inherits(c(g, g0), "saos_judgments_dump"), is_true())
  expect_that(inherits(c(g0, g0), "saos_judgments_dump"), is_true())
  expect_that(inherits(g[1:5], "saos_judgments_dump"), is_true())
})