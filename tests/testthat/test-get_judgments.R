context("test get_judgments functionality")

s <- search_judgments(sortingField = "JUDGMENT_DATE", sortingDirection = "DESC",
                      limit = 20, verbose = FALSE)
g <- get_judgments(s, verbose = FALSE)
g0 <- empty_get_result()

test_that("get returns proper object", {
  expect_is(g, c("saos_judgments", "list"))
  
  expect_identical(suppressMessages(get_judgments(1234567890, verbose = FALSE)),
                   g0)
  expect_identical(get_judgments(empty_search_result(), verbose = FALSE), g0) 
})

test_that("no new fields appear", {
  expect_identical(names(table(sapply(g, names))),
                   c("courtCases", "courtReporters", "courtType", "decision",
                     "division", "href", "id", "judges", "judgmentDate", 
                     "judgmentType", "keywords", "legalBases",
                     "referencedCourtCases", "referencedRegulations", "source",
                     "summary", "textContent"))
})

test_that("c and [ methods works properly", {
  expect_is(c(g, g), c("saos_judgments", "list"))
  expect_is(c(g, g0), c("saos_judgments", "list"))
  expect_is(c(g0, g0), c("saos_judgments", "list"))
  expect_is(g[1:5], c("saos_judgments", "list"))
  expect_equal(length(c(g, g)), 40)
  expect_equal(length(c(g, g0)), 20)
  expect_equal(length(c(g0, g0)), 0)
})