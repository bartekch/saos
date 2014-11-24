#' Download all courts
#'
#' Download information about all courts in the repository.
#'     
#' @return data.frame with rows corresponding to courts and following columns
#' \tabular{rlll}{
#' n \tab name \tab type \tab description \cr
#' [,1] \tab id \tab integer \tab court ID in the repository \cr
#' [,2] \tab name \tab character \tab full name of the court \cr
#' [,3] \tab type \tab character \tab type of the court; district, regional or 
#'   appeal \cr
#' [,4] \tab code \tab character \tab court's code, see details below \cr
#' [,5] \tab parentCourt \tab integer \tab ID of the superior court \cr
#' [,6] \tab divisions \tab list \tab details of the court's divisions, see 
#'   details below   \cr
#' }
#' Court's code represents nested structure. It is in format "15BBCCDD" where 
#'   "BB" - code of the coresponding appeal court, 
#'   "CC" - code of the corresponding regional court ("00" for appeal courts),
#'   "DD" - code of the district court ("00" for appeal and regional courts).
#'  All codes are multiples of five.     
#'
#' Information about divisions is stored in dataframes with following columns.
#' \tabular{rlll}{
#' n \tab name \tab type \tab description \cr
#' [,1] \tab id \tab integer \tab unique ID \cr
#' [,2] \tab name \tab character \tab full name of the division \cr
#' [,3] \tab code \tab character \tab court's code, see details below \cr
#' [,4] \tab type \tab character \tab type of the division \cr
#' }
#' Code of the division is in the format "AABCCDD", where
#'   "AA" - usually  "00", or number of affiliate division,
#'   "B" - usually "0", in case of affiliate divisions "1" or "2",
#'   "CC" - number of division in the given court (multiples of five),
#'   "DD" - code corresponding to the type of the division.
#' 
#' Information in the repository should be thorough (no missing data).
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