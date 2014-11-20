#' Search for judgments
#' 
#' Search for judgments matching the given query
#' 
#' @param query Search pattern as a named list; pairs name-value are treated
#'   as query parameters with corresponding values, see Details.
#' @param limit Limit the number of search results.
#' @param force If TRUE, force search request even if it seems extreme.
#' 
#' @details The available query parameters:
#' 
#' @return data.frame with rows corresponding to judgments, or NULL if none
#'  judgment is available
#'  
#' @examples \dontrun{
#' search_judgments(list(dateFrom = "10-11-2014"))
#'  }
#'  
#' @export

search_judgments <- function(query = NULL, limit = 200, force = FALSE){
  
  count <- count_judgments(query)
  if (limit > count) limit <- count
    
  if ((limit > 200) & !force){
    limit <- 200
    warning(sprintf("Pulling down only 200 out of expected %s results. If you
are sure to pull down everything use force = TRUE", limit))
  }
  
  query <- paste_query(query)
  url <- "https://saos-test.icm.edu.pl/api/search/judgments/?pageSize=100&"
  link <- paste0(url, query)
  response <- get_response(paste0(url, query))
  judgments <- extract_judgments(response)
  number <- nrow(judgments)
  next_page <- extract_link(response)
  while (!is.null(next_page) & (number < limit)){
    response <- get_response(next_page)
    judgments <- rbind(judgments, extract_judgments(response))
    number <- nrow(judgments)
    next_page <- extract_link(response)
  }
  if (number > limit){
    judgments <- judgments[1:limit, ]
  }
  judgments
}
