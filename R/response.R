
# function for accessing API and processing a response
get_response <- function(link, simplify = TRUE){
  # download response directly from link
  res <- httr::GET(link)
  
  # TO DO error handling!
  
  res <- httr::content(res, as = "text")
  res <- jsonlite::fromJSON(res, simplifyVector = simplify)
  res
}


# function extractring link for the next page from current response
extract_link <- function(response){
  links <- response$links
  if (any(links$rel == "next")){
    res <- links[links$rel == "next", "href"]
  } else res <- NULL
  res
}