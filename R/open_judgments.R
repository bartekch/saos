#' Open SAOS judgments' pages
#' 
#' Open in web browser SAOS pages for given judgments.
#' 
#' @param x An object used to select a method.
#' @param limit Integer. Limit the number of opened pages.
#' 
#' @return (Invisibly) Vector of character strings with URLs of abstracts opened.
#'  
#' @examples \donttest{
#' 
#' open_judgments(1:3)
#' 
#' judgments <- search_judgments(limit = 5, verbose = FALSE)
#' open_judgments(judgments)
#'  }
#'  
#' @export
#' 

open_judgments <- function(x, limit = 5) UseMethod("open_judgments")

# default method
#' @export
open_judgments.default <- function(x){
  stop("open_judgments accept arguments of class 'saos_search' or numeric vectors.")
}


#' @describeIn open_judgments Open in web browser SAOS pages for judgments with
#'   given IDs.
#' @export
open_judgments.numeric <- function(x, limit = 5){
  stopifnot(limit >= 1)
  links <- paste("https://saos-test.icm.edu.pl/judgments/", x, sep = "")
    
  if (length(links) > limit) {
    warning("More pages (", length(links), ") than maximum to be opened (", 
            limit, ").")
    links <- links[1:limit]
  }
  
  for (link in links) {
    utils::browseURL(link)
  }
  invisible(links)
}



#' @describeIn open_judgments Open in web browser SAOS pages for judgments with
#'   from the given search results.
#' @export
open_judgments.saos_search <- function(x, limit = 5){
  stopifnot(limit >= 1)
  links <- sapply(x, `[[`, "href")
  links <- gsub("api/", "", links)
  
  if (length(links) > limit) {
    warning("More pages (", length(links), ") than maximum to be opened (", 
            limit, ").")
    links <- links[1:limit]
  }
  
  for (link in links) {
    utils::browseURL(link)
  }
  invisible(links)
}
