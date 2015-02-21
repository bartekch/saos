context("test parsing search query")

test_that("arguments are parsed correctly", {
  expect_warning(print_query(all = list()))
  expect_output(print_query(all = c("kodeks", "cywilny")),
                "all=\"kodeks\" \"cywilny\"")
  expect_output(print_query(all = list(include = c("kodeks", "cywilny"),
                                        exclude = c("dobra osobiste"))),
                "all=\"kodeks\" \"cywilny\" -\"dobra osobiste\"")
  expect_output(print_query(judgmentTypes = c("DECISION", "REASONS")),
                "judgmentTypes=DECISION,REASONS")
  expect_output(print_query(judgmentDateFrom = "2015-01-01"),
                "judgmentDateFrom=2015-01-01")
  expect_output(print_query(judgmentDateFrom = as.POSIXct(1e6, origin = "1990-01-01")),
                "judgmentDateFrom=1990-01-12")
})