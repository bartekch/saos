#' Get judgments with given IDs
#' 
#' Get all judgments, if available, with IDs from a given vector
#' 
#' @param idlist integer vector with judgments' IDs
#' 
#' @return data.frame with rows corresponding to judgments, or NA if none
#'  judgment is available
#'  
#' @examples \donttest{
#' # single ID
#' get_judgments(128334)
#' 
#' # vector of IDs 
#' get_judgments(c(128334, 77354))
#' 
#' # vector of IDs with non-existent judgment
#' get_judgments(c(128334, 1, 77354))
#'  }
#'  
#' @export

get_judgments <- function(idlist){
  idlist <- sort(unique(idlist))
  url <- "https://saos-test.icm.edu.pl/api/judgments/"
  links <- paste0(url, idlist)
  result <- lapply(links, function(link){
    response <- get_response_if_available(link)
    if (is.null(response)){
      NA
    } else {
      extract_judgments(response)
    }
  })
  result <- do.call(rbind, result)
  result$id <- idlist
  result
}



get_response_if_available <- function(link){
  tryCatch(get_response(link), http_404 = function(x) NULL)
}
