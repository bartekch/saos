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
#'  count_judgments(list(judgmentDateFrom="2014-01-01"))
#'  count_judgments(list(judgeName="Maria Tyszel", judgmentDateTo="2014-06-30"))
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
                judgmentDateTo = judgmentDateTo)
  query <- paste_query(query)
  url <- "https://saos-test.icm.edu.pl/api/search/judgments"
  link <- paste0(url, "?pageSize=1&", query)
  response <- get_response(link)
  extract_total(response)
}



extract_total <- function(response){
  response$info$totalResults
}