context("test searching functionality")

test_that("arguments are checked for correctness", {
  expect_error(search_judgments(all = 1))
  expect_error(search_judgments(all = NA))
  expect_error(search_judgments(all = list(include = 0)))
  expect_error(search_judgments(all = list(exclude = 0)))
  expect_error(search_judgments(judgeName = 0))
  expect_error(search_judgments(judgeName = list()))
  expect_error(search_judgments(judgeName = c("a", "b")))
  expect_error(search_judgments(ccCourtId = -1))
  expect_error(search_judgments(ccCourtId = 0))
  expect_error(search_judgments(ccCourtId = 1.5))
  expect_error(search_judgments(judgmentDateFrom = "a"))
  expect_error(search_judgments(judgmentDateFrom = 2))
  expect_error(search_judgments(judgmentDateFrom = "01-01-2001"))
  expect_error(search_judgments(judgmentDateFrom = "01-01-01"))
  expect_error(search_judgments(judgmentDateFrom = "2001-20-01"))
})

test_that("arguments are parsed correctly", {
  expect_warning(search_judgments(all = list(), limit = 1, verbose = FALSE))
  expect_message(search_judgments(all = c("kodeks", "cywilny"), limit = 1),
                 "all=\"kodeks\" \"cywilny\"")
  expect_message(search_judgments(all = list(include = c("kodeks", "cywilny"), 
                                             exclude = c("dobra osobiste")), limit = 1),
                 "all=\"kodeks\" \"cywilny\" -\"dobra osobiste\"")
  expect_message(search_judgments(judgmentTypes = c("DECISION", "REASONS"), limit = 1),
                 "judgmentTypes=DECISION,REASONS")
  expect_message(search_judgments(judgmentDateFrom = "2015-01-01", limit = 1),
                 "judgmentDateFrom=2015-01-01")
  expect_message(search_judgments(judgmentDateFrom = as.POSIXct(1e6, origin = "1990-01-01"),
                                  limit = 1),
                 "judgmentDateFrom=1990-01-12")
})

  
s <- search_judgments(sortingField = "JUDGMENT_DATE", sortingDirection = "DESC",
                      limit = 20, verbose = FALSE)
s0 <- empty_search_result()

test_that("search returns proper object", {
  expect_is(s, c("saos_search", "list"))
  
  expect_identical(search_judgments(judgmentDateTo = "0001-01-01", verbose = FALSE),
                   s0)
  expect_identical(search_judgments(limit = 0, verbose = FALSE), s0)
})

test_that("no new fields appear", {
  expect_identical(names(table(sapply(s, names))),
                   c("courtCases", "division", "href", "id", "judges", "judgmentDate",
                     "judgmentType", "keywords", "textContent"))
})

test_that("c and [ methods works properly", {
  l <- length(s)
  expect_is(c(s, s), c("saos_search", "list"))
  expect_is(c(s, s0), c("saos_search", "list"))
  expect_is(c(s0, s0), c("saos_search", "list"))
  expect_is(s[1:5], c("saos_search", "list"))
  expect_equal(length(c(s, s)), 2*l)
  expect_equal(length(c(s, s0)), l)
  expect_equal(length(c(s0, s0)), 0)
})