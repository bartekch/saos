#' Open SAOS judgments' pages
#' 
#' Open in web browser SAOS pages for given judgments.
#' 
#' @param judgments Data frame of search results, as returned from 
#'   \code{\link{search_judgments}} or \code{\link{get_judgments}}.
#' @param limit Limit the number of opened pages.
#' 
#' @return (Invisibly) Vector of character strings with URLs of abstracts opened.
#'  
#' @examples \donttest{
#' judgments <- search_judgments()
#' open_judgments(judgments)
#'  }
#'  
#' @export
#' 
open_judgments <- function(judgments, limit = 5){
  stopifnot(limit >= 1)
  links <- paste("https://saos-test.icm.edu.pl/judgments/", judgments$id, 
                 sep = "")
  links <- links[!is.na(judgments$courtCases)]
  
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

