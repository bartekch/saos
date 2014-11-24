#' Download all Supreme Court chambers
#'
#' Download information about all chambers of the Supreme Court of Poland.
#' 
#' @param flatten logical, whether flatten information about divisions of every
#'  chamber
#'
#' @return If \code{flatten = FALSE} (default) data.frame with rows 
#'   corresponding to chambers and three columns
#' \tabular{rlll}{
#' n \tab name \tab type \tab description \cr
#' [,1] \tab id \tab integer \tab ID in the repository \cr
#' [,2] \tab name \tab character \tab full name of the chamber \cr
#' [,3] \tab divisions \tab list \tab details of the chamber's divisions, see 
#'   details below \cr
#' }
#' 
#' Information about divisions is stored in dataframes with following columns.
#' \tabular{rlll}{
#' n \tab name \tab type \tab description \cr
#' [,1] \tab id \tab integer \tab uniue ID \cr
#' [,2] \tab name \tab character \tab name of the division \cr
#' [,3] \tab fullName \tab character \tab full name of a division, composed from
#'   names of both chamber and its division \cr
#' }
#' 
#' If \code{flatten = TRUE} data.frame with rows corresponding to divisions
#'   and five columns: information about chamber (\code{id, name}) and
#'   information about division (\code{div_id, div_name, div_fullName}). 
#'
#' @seealso \code{\link[saos]{get_courts}}
#' 
#' @export

get_scChambers <- function(flatten = FALSE){
  #   tmp <- GET("https://saos-test.icm.edu.pl/", path = "api/dump/courts",
  #              query = list(pageSize = 100))
  url <- "https://saos-test.icm.edu.pl/api/dump/scChambers"
  response <- get_response(url)
  chambers <- extract_chambers(response)
  next_page <- extract_link(response)
  while (!is.null(next_page)){
    response <- get_response(next_page)
    chambers <- rbind(chambers, extract_chambers(response))
    next_page <- extract_link(response)
  }
  if (flatten){
    l <- sapply(chambers$divisions, nrow)
    ind <- rep(seq(nrow(chambers)), times = l)
    divisions <- do.call(rbind, chambers$divisions)
    names(divisions) <- paste("div", names(divisions), sep = "_")
    chambers <- cbind(chambers[ind, 1:2], divisions)
  }
  chambers
}


extract_chambers <- function(response){
  chambers <- response$items
  chambers
}
