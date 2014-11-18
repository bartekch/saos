#' Download all Supreme Court chambers
#'
#' Download information about all chambers of the Supreme Court of Poland.
#' 
#' @param flatten logical, whether flatten information about divisions of every
#'  chamber
#'
#' @return data.frame with rows corresponding to chambers or, if flatten == TRUE,
#'  to their divisions
#' 
#' @seealso \code{\link[soasAPI]{get_courts}}
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
