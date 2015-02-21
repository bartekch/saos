#' Search for judgments
#' 
#' Search for judgments matching the given query. By default set of results is
#'   limited to 200. If you want to download more results, use 
#'   \code{limit = NULL, force = TRUE}. However be aware that downloading 
#'   huge number of results could be time- and memory-consuming. You could
#'   always check number of results with \code{\link[saos]{count_judgments}}.
#' 
#'
#' @template query_param 
#' @param limit Non-negative integer or NULL. Limit the number of search results.
#' @param force Logical. If \code{TRUE} all search results will be downloaded.
#' @param verbose Logical. If \code{TRUE} details of searching are printed to 
#'   the screen.
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
                             sortingField = NULL, 
                             sortingDirection = NULL,
                             limit = NULL, force = FALSE, verbose = TRUE){
  
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

  # print final version of query
  if (verbose) message("Version of query sent to API:\n", print_query_(query))
  
  # count expected number of results 
  count <- count_judgments_(query)
  
  # check number of results  
  if (count == 0){
    if (verbose) message("No search results.")
    return(empty_search_result())
  }
  
  # set limit
  if (is.null(limit)) limit <- count
  if (limit > count) limit <- count
  
  # check limit size
  if (limit < 0) stop("Limit should be non-negative.")
  if (limit == 0){
    if (verbose) message("Limit is set to 0, no results.")
    return(empty_search_result())
  }
  
  # check for extreme number of results    
  if ((limit > 200) & !force){
    warning("Pulling down only 200 out of expected ", limit, " results. If you ",
            "are sure to pull down everything use force = TRUE.", call. = FALSE)
    limit <- 200
  }
  
  # prepare link to API
  query <- c(query, pageSize = 100)
  url <- "https://saos-test.icm.edu.pl/api/search/judgments"
  
  # get results
  if (verbose) message("Number of records matching query: ", count,
                       "\nNumber of records to download: ", limit)

  judgments <- get_limited_items(url, query = query, limit = limit, 
                                 verbose = verbose)
  
  class(judgments) <- c("saos_search", class(judgments))
  judgments
}


# method for concatenating search results
#' @export
c.saos_search <- function(..., recursive = FALSE) {
  res <- unlist(list(...), recursive = FALSE)
  class(res) <- c("saos_search", "list")
  res
}

# method for subsetting
#' @export
`[.saos_search` <- function(x, ind) {
  res <- unclass(x)[ind]
  class(res) <- c("saos_search", "list")
  res
} 



#### utility functions ####

# function returning empty list with proper class attribute
empty_search_result <- function() {
  res <- list()
  class(res) <- c("saos_search", "list")
  res
}