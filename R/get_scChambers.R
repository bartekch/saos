library(jsonlite)
library(httr)


get_response <- function(link){
  res <- GET(link)
  res <- content(res, as = "text")
  res <- fromJSON(res, simplifyVector = TRUE)
  res
}

extract_chambers <- function(response){
  chambers <- response$items
  chambers
}

extract_link <- function(response){
  links <- response$links
  if (any(links$rel == "next")){
    res <- links[links$rel == "next", "href"]
    res <- sub("http:", "https:", res)
  } else res <- NULL
  res
}

get_scChambers <- function(flatten = FALSE){
  #   tmp <- GET("https://saos-test.icm.edu.pl/", path = "api/dump/courts",
  #              query = list(pageSize = 100))
  url <- "https://saos-test.icm.edu.pl/api/dump/scChambers"
  response <- get_response(url)
  chambers <- extract_chambers(response)
  next_page <- extract_link(response)
  while (!is.null(next_page)){
    response <- get_response(next_page)
    chambers <- rbind(chambers, extract_chambers(response))
    next_page <- extract_link(response)
  }
  if (flatten){
    l <- sapply(chambers$divisions, nrow)
    ind <- rep(seq(nrow(chambers)), times = l)
    divisions <- do.call(rbind, chambers$divisions)
    names(divisions) <- paste("div", names(divisions), sep = "_")
    chambers <- cbind(chambers[ind, 1:2], divisions)
  }
  chambers
}

