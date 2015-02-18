#' Count number of results for a given search
#'
#' Count the number of results for a given search. Useful to check before
#'  attempting to download a huge number of records, which could be very
#'  memory and time consuming.
#'     
#' @template query_param
#' 
#' @return the number of results (integer)
#' 
#' @seealso \code{\link[saos]{search_judgments}}
#' 
#' @examples \donttest{
#'  count_judgments(judgmentDateFrom="2014-01-01")
#'  count_judgments(judgeName="Maria Tyszel", judgmentDateTo="2014-06-30")
#'  
#'  
#'  ## Examples of query operators, starting from most general query.
#'  
#'  # any of two words
#'  AorB <- count_judgments(all = "dobra OR osobiste")
#'  
#'  # both words
#'  AandB <- count_judgments(all = "dobra osobiste")
#'  
#'  # exactly given phrase
#'  count_judgments(all = "\"dobra osobiste\"")
#'  
#'  # one word but not the other
#'  onlyA <- count_judgments(all = "dobra -osobiste")
#'  onlyB <- count_judgments(all = "-dobra osobiste")
#'  AorB == AandB + onlyA + onlyB
#'  
#'  ## Mixing operators
#'  # in one string
#'  count_judgments(all = "\"dobra osobiste\" -\"dobra publiczne\"")
#'  
#'  # as a list
#'  count_judgments(all = list(include = "dobra osobiste",
#'                             exclude = "dobra publiczne"))
#'   }
#'   
#' @export
 
count_judgments <- function(all  = NULL, legalBase  = NULL,
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
                            sortingDirection = NULL){
  
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
  count_judgments_(query)
}



count_judgments_ <- function(query){
  url <- "https://saos-test.icm.edu.pl/api/search/judgments"
  query <- c(query, pageSize = 1)
  response <- get_response(url, query = query)
  response$info$totalResults
}
