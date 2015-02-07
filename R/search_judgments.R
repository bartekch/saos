#' Search for judgments
#' 
#' Search for judgments matching the given query. By default set of results is
#'   limited to 200. If you want to download more results, use 
#'   \code{force = TRUE}. However be aware that downloading huge number of 
#'   results could be time- and memory-consuming.
#' 
#'
#' @template query_param 
#' @param limit Limit the number of search results.
#' @param force If TRUE, force search request even if it seems extreme.
#' @param progress Logical. If \code{TRUE} a progress bar shows up.
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
#' sj1 <- search_judgments(judgmentDateFrom = "2014-11-20")
#' sj2 <- search_judgments(judgeName="Maria Tyszel", judgmentDateTo="2014-06-30")
#'
#'  
#' ## Using limit and force arguments
#' 
#' # Deafult values, downloadig 200 results
#' s1 <- search_judgments()
#' 
#' # Limit set to 50, downloading 50 results
#' s2 <- search_judgments(limit = 50)
#' 
#' # Limit set to 300, but force remains FALSE, downloading 200 results
#' s3 <- search_judgments(limit = 300)
#' 
#' # Limit set to 300, force is TRUE, downloading 300 results
#' s4 <- search_judgments(limit = 300, force = TRUE)
#' 
#' length(s1); length(s2); length(s3); length(s4)
#' 
#' 
#' ## Examples of query operators, starting from most general query.
#'  
#'  # any of two words
#'  s1 <- search_judgments(all = "dobra OR osobiste")
#'  
#'  # both words
#'  s2 <- search_judgments(all = "dobra osobiste")
#'  
#'  # exactly given phrase
#'  s3 <- search_judgments(all = "\"dobra osobiste\"")
#'  
#'  # one word but not the other
#'  s4 <- search_judgments(all = "dobra -osobiste")
#'  s5 <- search_judgments(all = "-dobra osobiste")
#'  
#'  #'  ## Mixing operators
#'  # in one string
#'  s6 <- search_judgments(all = "\"dobra osobiste\" -\"dobra publiczne\"")
#'  
#'  # as a list
#'  s7 <- search_judgments(all = list(include = "dobra osobiste",
#'                             exclude = "dobra publiczne"))
#'  identical(s6, s7)
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
                             limit = NULL, force = FALSE, progress = TRUE){
  
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

  query <- check_query(query)
  
  # count expected number of results 
  count <- count_judgments_(query)
  
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
    message("Pulling down only 200 out of expected ", limit, " results. If you ",
            "are sure to pull down everything use force = TRUE")
    limit <- 200
  }
  
  # prepare link to API
  query <- c(query, pageSize = 100)
  url <- "https://saos-test.icm.edu.pl/api/search/judgments"
  
  # get results
  message("Number of records expected: ", count)

  judgments <- get_limited_items(url, query = query, limit = limit, 
                                 progress = progress)
  
  message("Number of records downloaded: ", length(judgments))
  
  judgments
}
