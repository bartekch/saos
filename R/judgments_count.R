#' Count number of results for a given search
#'
#' Count the number of results for a given search. Useful to check before
#'  attempting to download a huge number of records, which could be very
#'  memory and time consuming.
#'     
#' @param query Search pattern as a named list; pairs name-value are treated
#'   as query parameters with corresponding values, see Details.
#' 
#' @details The available query parameters:
#' 
#' @return the number of results (integer)
#' 
#' @seealso \code{\link[soasAPI]{search_judgments}}
#' 
#' @example
#'  count_judgments(list(dateFrom="01-11-2014"))
#'  count_judgments(list(judgeName="Maria+Tyszel", dateTo="01-11-1995"))
#'   
#' @export
 
count_judgments <- function(query = NULL){
  query <- paste_query(query)
  url <- "https://saos-test.icm.edu.pl/results"
  tmp <- RCurl::getURL(paste0(url, "?size=1&", query))
  pos <- gregexpr("Wynik wyszukiwania", tmp)[[1]]
  str <- substr(tmp, pos + 24, pos + 40)
  str <- strsplit(str, "orzecz")[[1]][1]
  str <- suppressWarnings(as.numeric(strsplit(str, "")[[1]]))
  str <- as.numeric(paste(na.omit(str), collapse = ""))
  str
}
