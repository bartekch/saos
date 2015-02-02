#' Get all judgments
#' 
#' Get the list of all judgments within (and including) given dates.
#' 
#' @param start_date An object of class \code{Date} or character in format 
#'   "YYYY-MM-DD". Represents the earliest allowed judgment's date on the list.
#'   If missing, no time limit is set.
#' @param end_date An object of class \code{Date} or character in format 
#'   "YYYY-MM-DD". Represents the latest allowed judgment's date on the list.
#'   If missing, no time limit is set.
#' 
#' @return List of judgments as returned from API.
#'  
#' @examples \donttest{
#' dump <- get_judgments_dump(start_date = "2015-01-15", end_date = "2015-01-20")
#'  }
#'  
#' @export

get_judgments_dump <- function(start_date = NULL, end_date = NULL){
  url <- "https://saos-test.icm.edu.pl/api/dump/judgments/"
  
  # check arguments
  start_date <- check_date(start_date)
  end_date <- check_date(end_date)
  
  # prepare link to API
  query <- list(pageSize = 100, judgmentStartDate = start_date, 
             judgmentEndDate = end_date)
  
  # get results
  response <- get_response(url, query = query, simplify = FALSE)
  judgments <- response$items
  next_page <- extract_link(response)
  while (!is.null(next_page)){
    response <- get_response(next_page, simplify = FALSE)
    judgments <- c(judgments, response$items)
    next_page <- extract_link(response)
  }
  
  judgments
}
