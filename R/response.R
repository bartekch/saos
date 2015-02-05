
# function for accessing API and processing a response
get_response <- function(url, query = NULL, simplify = FALSE){
  
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


# function downloading exact number of pages for a given response
get_limited_items <- function(url, limit = NULL, query = NULL, 
                              simplify = FALSE, flatten = FALSE, 
                              simp_fun = NULL){
  if (is.null(simp_fun)) simp_fun <- base::identity
  
  response <- get_response(url, query, simplify)
  items <- simp_fun(response$items)
  next_page <- extract_link(response)
  if (is.null(limit)){
    while (!is.null(next_page)){
      response <- get_response(next_page, simplify = simplify)
      if (simplify){
        items <- rbind(items, simp_fun(response$items))
      } else {
        items <- c(items, response$items)
      }
      next_page <- extract_link(response)
    }
  } else {
    pb <- txtProgressBar(style = 3)
    number <- length(items)
    setTxtProgressBar(pb, number / limit)
    while (!is.null(next_page) & (number < limit)){
      response <- get_response(next_page, simplify = simplify)
      if (simplify){
        items <- rbind(items, simp_fun(response$items))
      } else {
        items <- c(items, response$items)
      }
      number <- length(items)
      setTxtProgressBar(pb, number / limit)
      next_page <- extract_link(response)
    }
    # reduce number of results to limit
    if (number > limit){
      items <- items[1:limit]
    }
    close(pb)
  }

  if (simplify & flatten) items <- jsonlite::flatten(items)
  items
}



# function downloading all possible pages for a given response
get_all_items <- function(url, query = NULL, simplify = FALSE, flatten = FALSE,
                          simp_fun = NULL){
  get_limited_items(url, limit = NULL, query = query, 
                    simplify = simplify, flatten = flatten, 
                    simp_fun = simp_fun)
#   if (is.null(simp_fun)) simp_fun <- base::identity
#   
#   response <- get_response(url, query, simplify)
#   
#   items <- simp_fun(response$items)
#   next_page <- extract_link(response)
#   while (!is.null(next_page)){
#     response <- get_response(next_page, simplify = simplify)
#     if (simplify){
#       items <- rbind(items, simp_fun(response$items))
#     } else {
#       items <- c(items, response$items)
#     }
#     next_page <- extract_link(response)
#   }
#   if (simplify & flatten) items <- jsonlite::flatten(items)
#   items
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