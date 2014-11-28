#' Count number of results for a given search
#'
#' Count the number of results for a given search. Useful to check before
#'  attempting to download a huge number of records, which could be very
#'  memory and time consuming.
#'     
#' @param query Search pattern as a named list; pairs name-value are treated
#'   as query parameters with corresponding values, see Details.
#' 
#' @template query
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
 
count_judgments <- function(query = NULL){
  query <- paste_query(query)
  url <- "https://saos-test.icm.edu.pl/api/search/judgments"
  link <- paste0(url, "?pageSize=1&", query)
  response <- get_response(link)
  extract_total(response)
}



extract_total <- function(response){
  response$info$totalResults
}