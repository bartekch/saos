paste_query <- function(query){
  if (is.null(query))
    return(query)
  query <- query[!vapply(query, is.null, logical(1))]  # removing NULLs
  params <- c("all", "legalBase", "referencedRegulation", "keyword", "courtName",
              "judgeName", "judgmentDateFrom", "judgmentDateTo")
  names <- RCurl::curlEscape(names(query))
  values <- RCurl::curlEscape(query)
  query <- paste0(names, "=", values, collapse = "&")
  query  
}


