#' Get all judgments
#' 
#' Get the list of all judgments within (and including) given dates.
#' 
#' @param start_date Any date/time object that could be properly converted with 
#'   \code{as.POSIXct}, or a string in a format "\%Y-\%m-\%d". 
#'   Represents the earliest allowed judgment's date on the list. 
#'   If missing, no time limit is set.
#' @param end_date Any date/time object that could be properly converted with 
#'   \code{as.POSIXct}, or a string in a format "\%Y-\%m-\%d". 
#'   Represents the latest allowed judgment's date on the list. 
#'   If missing, no time limit is set.
#' @param modification_date Any date/time object that could be properly converted with 
#'   \code{as.POSIXct}, or a string in format "\%Y-\%m-\%dT\%H:\%M:\%S". 
#'   Allows to select judgments which were modified later than the specified 
#'   time. If missing, no time limit is set.
#' 
#' @return List of judgments as returned from API.
#'  
#' @examples 
#' \dontrun{
#' full <- get_judgments_dump()
#' lastchanges <- get_judgments_dump(modification_date = Sys.Date() - 7)
#' }
#' \donttest{
#' # judgments from last week
#' lastweek <- get_judgments_dump(start_date = Sys.Date() - 7, 
#'                                end_date = Sys.Date())
#'  }
#'  
#' @export

get_judgments_dump <- function(start_date = NULL, end_date = NULL,
                               modification_date = NULL){
  url <- "https://saos-test.icm.edu.pl/api/dump/judgments/"
  
  # check arguments
  start_date <- check_date(start_date)
  end_date <- check_date(end_date)
  modification_date <- check_date(modification_date, 
                                  format = "%Y-%m-%dT%H:%M:%S")
  if (!is.null(modification_date))
    modification_date <- paste0(modification_date, ".000")
  
  # prepare link to API
  query <- list(pageSize = 100, judgmentStartDate = start_date, 
             judgmentEndDate = end_date,
             sinceModificationDate = modification_date)
  
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
