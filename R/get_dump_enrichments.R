#' Download all enrichments tags (EXPERIMENTAL)
#' 
#' Download the list of all tags with extra information abour judgments
#' 
#' @template dump_param
#'   
#' @return If \code{simplify = FALSE} the list of enrichment tags as returned 
#'   from API. Every element of the list represents one tag and has the 
#'   following, illustrative structure: \cr
#'  List of 4 
#'  $ id        : int 204 \cr
#'  $ judgmentId: int 31345 \cr
#'  $ tagType   : chr "REFERENCED_CASE_NUMBERS" \cr
#'  $ value     :List of 2 \cr
#'  ..$ referencedCaseNumber: chr "III CK 430/03" \cr
#'  ..$ referencedIds       :List of 1 \cr
#'  .. ..$ : int 1728 \cr 
#'  
#'  Detailed description of the meaning of all elements TODO.
#'    
#'  If \code{simplify = TRUE} a \code{data.frame} described in TODO is returned.
#'    
#' @export

get_dump_enrichments <- function(simplify = FALSE){
  url <- "https://saos-test.icm.edu.pl/api/dump/enrichments"
  
  # get results
  tags <- get_all_items(url, query = list(pageSize = 100), simplify = simplify)
  
  tags
}
