
# function for accessing API and processing a response
get_response <- function(url, query = NULL, simplify = TRUE){
  
  # if query is NULL we probably have DIRECT link, so we do not want to
  # override it with NULL  
  if (is.null(query)){
    res <- httr::GET(url)
  } else {
    res <- httr::GET(url, query = query)
  }
  
  httr::stop_for_status(res)
  
  res <- httr::content(res, as = "text")
  res <- jsonlite::fromJSON(res, simplifyVector = simplify)
  res
}

# function downloading all possible pages for a given response
get_all_items <- function(response, simplify = FALSE, simp_fun = NULL){
  if (is.null(simp_fun)) simp_fun <- base::identity
  
  items <- simp_fun(response$items)
  next_page <- extract_link(response)
  while (!is.null(next_page)){
    response <- get_response(next_page, simplify = simplify)
    if (simplify){
      items <- rbind(items, simp_fun(response$items))
    } else {
      items <- c(items, response$items)
    }
    next_page <- extract_link(response)
  }
  items
}

# function extracting link for the next page from current response
extract_link <- function(response){
  links <- response$links
  if (is.data.frame(links)){
    if (any(links$rel == "next")){
      res <- links[links$rel == "next", "href"]
    } else res <- NULL
  } else {
    pos <- which(sapply(links, function(x) x$rel) == "next")
    if (length(pos) == 0){
      res <- NULL
    } else {
      res <- links[[pos]]$href
    }
  }
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