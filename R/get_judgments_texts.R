#' Get full texts of given judgments
#' 
#' Get full texts, if available, of judgments with IDs from a given vector
#' 
#' @param idlist integer vector with judgments' IDs
#' 
#' @return named character vector with judgments
#'  
#' @examples \dontrun{
#' get_judgment_texts(128334)
#' get_judgment_texts(c(128334, 77354))
#'  }
#'  
#' @export

get_judgment_texts <- function(idlist){
  judgments <- get_judgments(idlist)
  texts <- judgments$textContent
  # these texts are in HTML, we need pure text as in search/API, but full
  names(texts) <- idlist
  texts
}