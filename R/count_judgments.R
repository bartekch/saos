#' Count number of results for a given search
#'
#' Count the number of results for a given search. Useful to check before
#'  attempting to download a huge number of records, which could be very
#'  memory and time consuming.
#'     
#' @template query_param
#' 
#' @return the number of results (integer)
#' 
#' @seealso \code{\link[saos]{search_judgments}}
#' 
#' @examples \dontrun{
#'  count_judgments(judgmentDateFrom="2014-01-01")
#'  count_judgments(judgeName="Maria Tyszel", judgmentDateTo="2014-06-30")
#'   }
#'   
#' @export
 
count_judgments <- function(all = NULL, legalBase = NULL,
                            referencedRegulation = NULL, keyword = NULL,
                            courtName = NULL, judgeName = NULL,
                            judgmentDateFrom = NULL, judgmentDateTo = NULL){
  query <- list(all = all, legalBase = legalBase, 
                referencedRegulation = referencedRegulation, keyword = keyword,
                courtName = courtName, judgeName = judgeName,
                judgmentDateFrom = judgmentDateFrom, 
                judgmentDateTo = judgmentDateTo, pageSize = 1)

  url <- "https://saos-test.icm.edu.pl/api/search/judgments"
  query <- c(query, pageSize = 1)
  response <- get_response(url, query = query)
  extract_total(response)
}



extract_total <- function(response){
  response$info$totalResults
}