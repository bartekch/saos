context("test other get_dump_ functionalities")

test_that("proper objects are returned", {
  skip_on_cran()
  expect_is(get_dump_courts(), "list")
  expect_is(get_dump_scChambers(), "list")
  expect_is(get_dump_enrichments(), "list")
})

test_that("data file are up-to-date", {
  expect_identical(courts, get_dump_courts(simplify = TRUE))
  expect_identical(scchambers, get_dump_scChambers(simplify = TRUE))
})