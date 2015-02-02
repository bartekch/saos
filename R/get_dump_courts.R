#' Download all common courts
#'
#' Download information about all common courts in the repository.
#'
#' @return The list of all common courts as returned from API.
# @return data.frame as described in \code{\link[saos]{courts}}
#' 
#' @seealso \code{\link[saos]{courts}}, \code{\link[saos]{get_dump_scChambers}}
#' 
#' @export

get_dump_courts <- function(){
  url <- "https://saos-test.icm.edu.pl/api/dump/courts"
  response <- get_response(url, query = list(pageSize = 100))
  courts <- get_all_items(response)
#   courts <- extract_courts(response)
#   next_page <- extract_link(response)
#   while (!is.null(next_page)){
#     response <- get_response(next_page)
#     courts <- rbind(courts, extract_courts(response))
#     next_page <- extract_link(response)
#   }
  courts
}

# 
# extract_courts <- function(response){
#   courts <- response$items
#   courts$parentCourt <- courts$parentCourt[,1]
#   courts
# }