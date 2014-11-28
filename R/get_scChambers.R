#' Download all Supreme Court chambers
#'
#' Download information about all chambers of the Supreme Court of Poland.
#' 
#' @param flatten logical, whether flatten information about divisions of every
#'  chamber
#'
#' @return If \code{flatten = FALSE} (default) data.frame as described in 
#'   \code{\link[saos]{scchambers}}.  
#' If \code{flatten = TRUE} data.frame with rows corresponding to divisions
#'   and five columns: information about chamber (\code{id, name}) and
#'   information about division (\code{div_id, div_name, div_fullName}). 
#'
#' @seealso \code{\link[saos]{scchambers}}, \code{\link[saos]{get_courts}}
#' 
#' @export

get_scChambers <- function(flatten = FALSE){
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
