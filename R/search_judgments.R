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
#' 
#' # search with no query, various limit options
#' search_judgments()
#' search_judgments(limit = 50)
#' search_judgments(limit = 300)
#' search_judgments(limit = 300, force = TRUE)
#'  }
#'  
#' @export

search_judgments <- function(query = NULL, limit = 200, force = FALSE){
  # count expected number of results 
  count <- count_judgments(query)
  
  # check number of results  
  if (count == 0){
    message("No search results.")
    return(NULL)
  }
  
  # set limit
  if ((limit > count) | is.null(limit)) limit <- count
  
  # check limit size
  if (limit < 0) stop("Limit should be non-negative")
  if (limit == 0){
    message("Limit is set to 0, no results.")
    return(NULL)
  }
  
  # check for extreme number of results    
  if ((limit > 200) & !force){
    message(sprintf("Pulling down only 200 out of expected %s results. If you
are sure to pull down everything use force = TRUE", limit))
    limit <- 200
  }
  
  # get results 
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
  
  # reduce number of results to limit
  if (number > limit){
    judgments <- judgments[1:limit, ]
  }
  
  message(sprintf("Number of records downloaded: %s.", nrow(judgments)))
  message(sprintf("Number of records expected: %s.", count))
  
  judgments
}
