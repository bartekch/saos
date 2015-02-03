#' Get all enrichments tags (EXPERIMENTAL)
#' 
#' Get the list of all tags with extra information abour judgments
#' 
#' @template dump_param
#'   
#' @return List of enrichment tags as returned from API.
#'  
#' @export

get_dump_enrichments <- function(simplify = FALSE){
  url <- "https://saos-test.icm.edu.pl/api/dump/enrichments"
  
  # get results
  tags <- get_all_items(url, query = list(pageSize = 100), simplify = simplify)
  
  tags
}
