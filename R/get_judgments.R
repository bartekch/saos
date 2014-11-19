#' Get judgments with given IDs
#' 
#' Get all judgments, if available, with IDs from a given vector
#' 
#' @param idlist integer vector with judgments' IDs
#' 
#' @return data.frame with rows corresponding to judgments, or NULL if none
#'  judgment is available
#'  
#' @examples
#' get_judgments(128334)
#' get_judgments(c(128334, 77354))
#'  
#' @export

get_judgments <- function(idlist){
  url <- "https://saos-test.icm.edu.pl/api/judgments/"
  links <- paste0(url, idlist)
  result <- lapply(links, function(link){
    response <- get_response(link)
    data <- response$data
    # convert NULLs to NA
    data <- lapply(data, function(x) if (length(x) == 0){ NA } else { x })
    data
  })
  result <- as.data.frame(do.call(rbind, result))
  for (i in seq_along(result)){
    if (all(sapply(result[, i], length) <= 1)){
      result[, i] <- unlist(result[, i])
    }
  }
  result
}



#' Get full texts of given judgments
#' 
#' Get full texts, if available, of judgments with IDs from a given vector
#' 
#' @param idlist integer vector with judgments' IDs
#' 
#' @value named character vector with judgments
#'  
#' @examples
#' get_judgment_texts(128334)
#' get_judgment_texts(c(128334, 77354))
#'  
#' @export

get_judgment_texts <- function(idlist){
  judgments <- get_judgments(idlist)
  texts <- judgments$textContent
  # these texts are in HTML, we need pure text as in search/API, but full
  names(texts) <- idlist
  texts
}