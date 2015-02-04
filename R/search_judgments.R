#' Search for judgments
#' 
#' Search for judgments matching the given query
#'
#' @template query_param 
#' @param limit Limit the number of search results.
#' @param force If TRUE, force search request even if it seems extreme.
#' 
#' @return List of judgments as returned from API.
#' 
# @return data.frame with rows corresponding to judgments, or \code{NULL} if 
#  none judgment is available, and following columns (* means that column is 
#  a list with elements of given type, if impossible to unlist)
# \tabular{rlll}{
# n \tab name \tab type \tab description \cr
# [,1] \tab id \tab integer \tab judgment ID in the repository \cr
# [,2] \tab href \tab character \tab link to judgment's page in API \cr
# [,3] \tab courtCases \tab character \tab case signature, indicates division
#   of the trial court, a case number and a year of the judgment \cr
# [,4] \tab judgmentType \tab character \tab one from "DECISION", "RESOLUTION",
#   "SENTENCE", "REGULATION", "REASONS" \cr
# [,5] \tab judges \tab data.frame* \tab two columns with judges names ("name")
#   and their role ("specialRoles") \cr
# [,6] \tab textContent \tab character \tab shortened version of full text of 
#   the judgment \cr
# [,7] \tab keywords \tab character* \tab keywords associated with the given
#   judgment \cr
# [,8] \tab division \tab data.frame* \tab information about the court (columns
#   "court.name", "court.code", "court.href") and its division (columns "name",
#   "code", "href"), see \link{get_courts} for details \cr
# [,9] \tab judgmentDate \tab character \tab date of the judgment in format
#   YYYY-MM-DD \cr
# } 
#'  
#' @examples \donttest{
#' search_judgments(judgmentDateFrom = "2014-11-20")
#' search_judgments(judgeName="Maria Tyszel", judgmentDateTo="2014-06-30")
#' 
#' # search with no query, various limit options
#' search_judgments()
#' search_judgments(limit = 50)
#' search_judgments(limit = 300)
#' search_judgments(limit = 300, force = TRUE)
#'  }
#'  
#' @export

search_judgments <- function(all  = NULL, legalBase  = NULL,
                             referencedRegulation  = NULL, judgeName  = NULL, 
                             caseNumber  = NULL, courtType  = NULL,
                             ccCourtType  = NULL, ccCourtId  = NULL, 
                             ccCourtCode  = NULL, ccCourtName  = NULL,
                             ccDivisionId  = NULL, ccDivisionCode  = NULL, 
                             ccDivisionName  = NULL, scPersonnelType  = NULL, 
                             scChamberId  = NULL, scChamberName  = NULL, 
                             scDivisionId  = NULL, scDivisionName  = NULL, 
                             judgmentTypes  = NULL, keywords  = NULL, 
                             judgmentDateFrom  = NULL, judgmentDateTo  = NULL,
                             sortingField = "DATABASE_ID", 
                             sortingDirection = "ASC",
                             limit = 200, force = FALSE){
  
  query <- list(all  =  all, 
                legalBase  =  legalBase, 
                referencedRegulation  =  referencedRegulation, 
                judgeName  =  judgeName, 
                caseNumber  =  caseNumber, 
                courtType  =  courtType, 
                ccCourtType  =  ccCourtType, 
                ccCourtId  =  ccCourtId, 
                ccCourtCode  =  ccCourtCode, 
                ccCourtName  =  ccCourtName, 
                ccDivisionId  =  ccDivisionId, 
                ccDivisionCode  =  ccDivisionCode, 
                ccDivisionName  =  ccDivisionName, 
                scPersonnelType  =  scPersonnelType, 
                scChamberId  =  scChamberId, 
                scChamberName  =  scChamberName, 
                scDivisionId  =  scDivisionId, 
                scDivisionName  =  scDivisionName, 
                judgmentTypes  =  judgmentTypes, 
                keywords  =  keywords, 
                judgmentDateFrom  =  judgmentDateFrom, 
                judgmentDateTo  =  judgmentDateTo,
                sortingField = sortingField,
                sortingDirection = sortingDirection)

#   query <- check_query(query)
  
  # count expected number of results 
  count <- do.call(count_judgments, query)
  
  # check number of results  
  if (count == 0){
    message("No search results.")
    return(NULL)
  }
  
  # set limit
  if (is.null(limit)) limit <- count
  if (limit > count) limit <- count
  
  # check limit size
  if (limit < 0) stop("Limit should be non-negative.")
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
  
  # prepare link to API
  pagesize <- if (limit > 100) { 100 } else { limit }
  query <- c(query, pageSize = pagesize)
  url <- "https://saos-test.icm.edu.pl/api/search/judgments"
  
  # get results
  response <- get_response(url, query = query, simplify = FALSE)
  # judgments <- extract_judgments(response)
  judgments <- response$items
  #number <- nrow(judgments)
  number <- length(judgments)
  next_page <- extract_link(response)
  while (!is.null(next_page) & (number < limit)){
    response <- get_response(next_page, simplify = FALSE)
    #judgments <- rbind(judgments, extract_judgments(response))
    judgments <- c(judgments, response$items)
    number <- length(judgments)
    next_page <- extract_link(response)
  }
  
  #id <- as.integer(sapply(strsplit(judgments$href, "/"), function(x) tail(x, 1)))
  #judgments <- cbind(id, judgments)
  
  # reduce number of results to limit
  if (number > limit){
    #judgments <- judgments[1:limit, ]
    judgments <- judgments[1:limit]
  }
  
  message(sprintf("Number of records downloaded: %s.", length(judgments)))
  message(sprintf("Number of records expected: %s.", count))
  
  judgments
}
