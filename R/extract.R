#' Extract information from search results
#' 
#' Extrac specific elements 
#'
#' @param results list, result of searching
#' @param element Piece of information to extract
#'
#' @details Elements:
#' id
#' caseNumber
#' judges
#' judgmentType
#' textContent
#' keywords
#' division
#' judgmentDate
#'   
#' @return data.frame with specific information
#'  
#' @export
extract <- function(results, element = c("id", "caseNumber", "judges", "judgmentType",
                                         "textContent", "keywords", "division", 
                                         "judgmentDate")){
  element <- match.arg(element)
  ids <- extract_id(results)
  
  if (element == "id") return(ids)
  
  if (element == "caseNumber"){
    el <- lapply(results, function(x) x$courtCases[[1]]$caseNumber)
    times <- sapply(el, length)
    el <- data.frame(caseNumber = unlist(el))
  } else if (element == "judges"){
    el <- lapply(results, function(x) {
      if (length(x$judges) == 0){
        NA
      } else {
        as.data.frame(t(simplify2array(x$judges)))
      }})
    times <- sapply(el, function(x) if (!is.data.frame(x)) { 1 } else { nrow(x) })
    el <- do.call(rbind, el)
  } else if (element == "keywords"){
    el <- lapply(results, function(x) unlist(x[["keywords"]]))
    times <- sapply(el, length)
    el <- data.frame(keywords = unlist(el))
  } else if (element == "division"){
    el <- lapply(results, function(x) as.data.frame(t(simplify2array(x$division))))
    times <- sapply(el, function(x) if (!is.data.frame(x)) { 1 } else { nrow(x) })
    el <- do.call(rbind, el)
  } else {
    el <- lapply(results, function(x) x[[element]])
    times <- sapply(el, length)
    el <- data.frame(unlist(el))
    names(el) <- element
  }
  
  res <- cbind(data.frame(id = rep(ids, times = times)), el)
  res
}


extract_id <- function(results){
  as.numeric(sapply(results, function(x) tail(strsplit(x$href, "/")[[1]], 1)))
}
