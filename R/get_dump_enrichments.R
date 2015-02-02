#' Get all enrichments tags (EXPERIMENTAL)
#' 
#' Get the list of all tags with extra information abour judgments
#' 
#' @return List of enrichment tags as returned from API.
#'  
#' @export

get_dump_enrichments <- function(){
  url <- "https://saos-test.icm.edu.pl/api/dump/enrichments"
  
  # prepare query to API
  query <- list(pageSize = 100)
  
  # get results
  response <- get_response(url, query = query, simplify = FALSE)
  tags <- get_all_items(response)
  
  tags
}
