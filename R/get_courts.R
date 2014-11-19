#' Download all courts
#'
#' Download information about all courts in the repository.
#'     
#' @return data.frame with rows corresponding to courts and columns TO DO
#' 
#' @seealso \code{\link[saos]{get_scChambers}}
#' 
#' @export

get_courts <- function(){
  #   tmp <- GET("https://saos-test.icm.edu.pl/", path = "api/dump/courts",
  #              query = list(pageSize = 100))
  url <- "https://saos-test.icm.edu.pl/api/dump/courts/?pageSize=100"
  response <- get_response(url)
  courts <- extract_courts(response)
  next_page <- extract_link(response)
  while (!is.null(next_page)){
    response <- get_response(next_page)
    courts <- rbind(courts, extract_courts(response))
    next_page <- extract_link(response)
  }
  courts
}


extract_courts <- function(response){
  courts <- response$items
  courts$parentCourt <- courts$parentCourt[,1]
  courts
}