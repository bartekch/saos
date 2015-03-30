context("test extracting functionality")

test_that("extracting from empty lists returns empty data frame", {
  expect_empty_equal <- function(method, element, empty_template){
    eval(bquote(expect_identical(extract(.(method), .(element)), .(empty_template))))
  }
  options(stringsAsFactors = FALSE)
  
  methods <- list(empty_search = empty_search_result(),
                  empty_get = empty_get_result(),
                  empty_dump = empty_dump_result())
  templates <- list(id = data.frame(),
                    courtCases = data.frame(caseNumber = character()),
                    judgmentType = data.frame(judgmentType = character()),
                    judges = data.frame(name = character(),
                                        "function" = character(), specialRoles = character()),
                    textContent = data.frame(textContent = character()),
                    keywords = data.frame(keywords = character()),
                    division = data.frame(division.id = integer(), 
                                           name = character(), href = character()),
                    judgmentDate = data.frame(judgmentDate = character()),
                    courtType = data.frame(courtType = character()),
                    source = data.frame(code = character(), judgmentUrl = character(), 
                                        judgmentId = character(),publisher = character(),
                                        reviser = character(), publicationDate = character()),
                    courtReporters = data.frame(courtReporters = character()),
                    decision = data.frame(decision = character()),
                    summary = data.frame(summary = character()),
                    legalBases = data.frame(legalBases = character()),
                    referencedRegulations = data.frame(journalTitle = character(), journalNo = integer(),
                                                       journalYear = integer(), journalEntry = integer(),
                                                       text = character()),
                    referencedCourtCases = data.frame(referencedCourtCases = character()),
                    personnelType = data.frame(personnelType = character()),
                    chambers = data.frame(chambers.id = integer()),
                    judgmentResult = data.frame(judgmentResult = character()),
                    receiptDate = data.frame(receiptDate = character()),
                    meansOfAppeal = data.frame(meansOfAppeal = character()),
                    lowerCourtJudgments = data.frame(lowerCourtJudgments = character()),
                    href = data.frame(href = character()))
  templates <- lapply(templates, function(temp) data.frame(id = integer(), temp))
  
  # elements appropiate for all methods
  for (method in methods) {
    for (element in names(templates[1:8])) {
      expect_empty_equal(method, element, templates[[element]])
    }
  }
  
  # elements for saos_judgments and saos_dump_judgments
  for (method in methods[2:3]) {
    for (element in names(templates[9:22])) {
      expect_empty_equal(method, element, templates[[element]])
    }
  }
  
  expect_empty_equal(methods$empty_get, "judgmentForm", 
                     data.frame(id = integer(), judgmentForm = character()))
  expect_empty_equal(methods$empty_dump, "form", 
                     data.frame(id = integer(), form = character()))
  
  # "href" for saos_search and saos_judgments
  for (method in methods[1:2]) {
    expect_empty_equal(method, "href", templates[["href"]])
  }
})