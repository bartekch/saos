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
  expect_that(inherits(c(g, g), "saos_judgments_dump"), is_true())
  expect_that(inherits(c(g, g0), "saos_judgments_dump"), is_true())
  expect_that(inherits(c(g0, g0), "saos_judgments_dump"), is_true())
  expect_that(inherits(g[1:5], "saos_judgments_dump"), is_true())
})