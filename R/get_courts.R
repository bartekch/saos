library(jsonlite)
library(httr)


get_response <- function(link){
  res <- GET(link)
  res <- content(res, as = "text")
  res <- fromJSON(res, simplifyVector = TRUE)
  res
}

extract_courts <- function(response){
  courts <- response$items
  courts$parentCourt <- courts$parentCourt[,1]
  courts
}

extract_link <- function(response){
  links <- response$links
  if (any(links$rel == "next")){
    res <- links[links$rel == "next", "href"]
  } else res <- NULL
  res
}

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
