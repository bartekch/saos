#' Search for judgments
#' 
#' Search for judgments matching the given query
#' 
#' @param query Search pattern as a named list; pairs name-value are treated
#'   as query parameters with corresponding values, see Details.
#' @param limit Limit the number of search results.
#' 
#' @details The available query parameters:
#' 
#' @return data.frame with rows corresponding to judgments, or NULL if none
#'  judgment is available
#'  
#' @examples
#' search_judgments(list(dateFrom = "10-11-2014"))
#'  
#' @export

search_judgments <- function(query = NULL, limit = 100){
  query <- paste_query(query)
  url <- "https://saos-test.icm.edu.pl/api/search/judgments/?pageSize=100&"
  link <- paste0(url, query)
  response <- get_response(paste0(url, query))
  judgments <- extract_judgments(response)
#   next_page <- extract_link(response)
#   while (!is.null(next_page)){
#     response <- get_response(next_page)
#     judgments <- rbind(judgments, extract_courts(response))
#     next_page <- extract_link(response)
#   }
  judgments
}
