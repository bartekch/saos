
# function for accessing API and processing a response
get_response <- function(link, simplify = TRUE){
  # download response directly from link
  res <- httr::GET(link)
  
  httr::stop_for_status(res)
  
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
  if (any(names(response) == "items")){
    judg <- response$items
    judg$division <- lapply(1:nrow(judg), function(i) judg$division[i,])
  } else if (any(names(response) == "data")){
    judg <- response$data
    judg <- as.data.frame(t(as.matrix(judg)))  # find some better way to do it!
  } else {
    stop("No content")
  }
  
  jl <- sapply(judg, is.list)
  
  # replace NULLs with NAs
  judg[, jl] <- lapply(judg[, jl], function(col)
    lapply(col, function(x) if (length(x) == 0) { NA } else { x }))
  
  # unlist columns where possible
  judg <- unlist_columns(judg)
  judg
}



# unlisting columns if possible
unlist_columns <- function(df){
  dl <- sapply(df, is.list)
  df[, dl] <- lapply(df[, dl], function(x){
    if (is.null(dim(x)) & all(sapply(x, length) == 1)){
      unlist(x)
    } else {
      x
    }})
  df
}