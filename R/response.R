
# function for accessing API and processing a response
get_response <- function(link, simplify = TRUE){
  # download response directly from link
  res <- httr::GET(link)
  
  # TO DO error handling!
  
  res <- httr::content(res, as = "text")
  res <- jsonlite::fromJSON(res, simplifyVector = simplify)
  res
}