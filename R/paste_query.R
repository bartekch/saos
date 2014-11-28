paste_query <- function(query){
  if (is.null(query))
    return(query)
  #   url$query <- compact(url$query)  # removing NULL see httr::build_url
  params <- c("all", "legalBase", "referencedRegulation", "keyword", "courtName",
              "judgeName", "judgmentDateFrom", "judgmentDateTo")
  names <- RCurl::curlEscape(names(query))
  values <- RCurl::curlEscape(query)
  query <- paste0(names, "=", values, collapse = "&")
  query  
}


