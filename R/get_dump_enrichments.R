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
#'  Detailed description of the meaning of all elements could be found below.
#'    
#'  If \code{simplify = TRUE} a \code{data.frame} with following columns:
#' \tabular{rlll}{
#' n \tab name \tab class \tab description \cr
#' [,1] \tab id \tab integer \tab ID of a tag \cr
#' [,2] \tab judgmentId \tab integer \tab ID of tagged judgment \cr
#' [,3] \tab tagType \tab character \tab type of tag \cr
#' [,4] \tab value \tab list \tab tag's details, depend on type of a tag \cr
#' }
#' 
#' There are a few types of tags listed below:
#'  TODO
#'
#' @export

get_dump_enrichments <- function(simplify = FALSE){
  url <- "https://saos-test.icm.edu.pl/api/dump/enrichments"
  
  # get results
  tags <- get_all_items(url, query = list(pageSize = 100))
  if (simplify) tags <- simplify_tags(tags)
  tags
}



#### utility functions ####

# simplifying to data frame
simplify_tags <- function(tags){
  res <- data.frame(id = sapply(tags, `[[`, "id"), 
                    judgmentId = sapply(tags, `[[`, "judgmentId"), 
                    tagType = sapply(tags, `[[`, "tagType"), 
                    stringsAsFactors = FALSE)
  res$value <- lapply(tags, `[[`, "value")
  res
}