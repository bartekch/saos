# checking correctness of query
check_query <- function(query){
  # remove NULLs
  query <- query[!vapply(query, is.null, logical(1))]
  
  # check type
  if (!all(sapply(query, is.character)))
    stop("All parameters should be of type 'character'")
  
  # check length
  if (any(sapply(query, length) > 1))
    stop("All parameters should be of length 1.")
  
  query$judgmentDateFrom <- check_date(query$judgmentDateFrom)
  query$judgmentDateTo <- check_date(query$judgmentDateTo)
  
  return(query)
}

check_date <- function(date){
  if (is.null(date)) return(date)
  
  d <- as.Date(date, format = "%Y-%m-%d")
  if (is.na(d))
    stop("Date should be given in format 'YYYY-MM-DD'")
  return(date)
}