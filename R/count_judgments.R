#' Count number of results for a given search
#'
#' Count the number of results for a given search. Useful to check before
#'  attempting to download a huge number of records, which could be very
#'  memory and time consuming.
#'     
#' @param query Search pattern as a named list; pairs name-value are treated
#'   as query parameters with corresponding values, see Details.
#' 
#' @details The available query parameters:
#' 
#' @return the number of results (integer)
#' 
#' @seealso \code{\link[saos]{search_judgments}}
#' 
#' @examples \dontrun{
#'  count_judgments(list(dateFrom="01-11-2014"))
#'  count_judgments(list(judgeName="Maria+Tyszel", dateTo="01-11-1995"))
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