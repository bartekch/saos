#' Get all enrichments tags (EXPERIMENTAL)
#' 
#' Get the list of all tags with extra information abour judgments
#' 
#' @param simplify Logical. If \code{TRUE} results will be returned as 
#'   \code{data.frame}.
#'   
#' @return List of enrichment tags as returned from API.
#'  
#' @export

get_dump_enrichments <- function(simplify = FALSE){
  url <- "https://saos-test.icm.edu.pl/api/dump/enrichments"
  
  # prepare query to API
  query <- list(pageSize = 100)
  
  # get results
  response <- get_response(url, query = query, simplify = simplify)
  tags <- get_all_items(response, simplify = simplify)
  
  tags
}
