
# function for accessing API and processing a response
get_response <- function(link, simplify = TRUE){
  # download response directly from link
  res <- httr::GET(link)
  
  # TO DO error handling!
  
  res <- httr::content(res, as = "text")
  res <- jsonlite::fromJSON(res, simplifyVector = simplify)
  res
}


# function extracting link for the next page from current response
extract_link <- function(response){
  links <- response$links
  if (any(links$rel == "next")){
    res <- links[links$rel == "next", "href"]
  } else res <- NULL
  res
}


# function extracting information about judgments in proper structure
extract_judgments <- function(response){
  judg <- response$items
  judg$division <- lapply(1:nrow(judg), function(i) judg$division[i,])
  jl <- sapply(judg, is.list)
  
  # replace NULLs with NAs
  judg[, jl] <- lapply(judg[, jl], function(col)
    lapply(col, function(x) if (length(x) == 0) { NA } else { x }))
  
  # unlist columns where possible
  judg[, jl] <- lapply(judg[,jl], function(x)
    if (all(sapply(x, length) == 1)) { unlist(x) } else { x })
  judg
}