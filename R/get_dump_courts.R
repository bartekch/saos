#' Download all common courts
#'
#' Download information about all common courts in the repository.
#'
#' @template dump_param
#' 
#' @return The list of all common courts as returned from API.
# @return data.frame as described in \code{\link[saos]{courts}}
#' 
#' @seealso \code{\link[saos]{courts}}, \code{\link[saos]{get_dump_scChambers}}
#' 
#' @export

get_dump_courts <- function(simplify = FALSE){
  url <- "https://saos-test.icm.edu.pl/api/dump/courts"
  courts <- get_all_items(url, query = list(pageSize = 100),
                          simplify = simplify, simp_fun = simp_courts)
#   courts <- extract_courts(response)
#   next_page <- extract_link(response)
#   while (!is.null(next_page)){
#     response <- get_response(next_page)
#     courts <- rbind(courts, extract_courts(response))
#     next_page <- extract_link(response)
#   }
  courts
}

 
simp_courts <- function(items){
  items$parentCourt <- items$parentCourt[,1]
  items
}