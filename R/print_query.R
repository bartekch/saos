#' Print version of query sent to API
#'
#' Print the final version of search query after potential reformatting to check
#'  if this is what you expected before starting actual download.
#'     
#' @template query_param
#' 
#' @return Search query as single string.
#' 
#' @seealso \code{\link[saos]{search_judgments}}
#' 
#' @examples 
#' print_query()
#' print_query(judgmentDateFrom="2014-01-01")
#' print_query(judgeName="Maria Tyszel", judgmentDateTo="2014-06-30")
#' print_query(all = "\"dobra osobiste\" -\"dobra publiczne\"")
#' print_query(all = c("dobra osobiste", "kodeks cywilny"))
#' print_query(all = list(include = "dobra osobiste",
#'                        exclude = "dobra publiczne"))
#' print_query(judgmentTypes = c("SENTENCE", "DECISION"))
#'   
#' @export

print_query <- function(all  = NULL, legalBase  = NULL,
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
  print_query_(query)
}



print_query_ <- function(query){
  if (length(query) > 0) {
    paste(paste(names(query), query, sep = "="), collapse = "&")
  } else {
    "<empty>"
  }
}
