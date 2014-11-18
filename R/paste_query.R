paste_query <- function(query){
  if (is.null(query))
    return(query)
  paste(paste(names(query), sapply(query, as.character), sep = "="), 
        collapse = "&")
}


# TO DECIDE - strings (like in aRxiv) or lists (like in ?GET query processing)
# or simple character vector
# signature - appropiate string
# dateFrom - date in format dd-mm-yyyy
# dateTo - date in format dd-mm-yyyy
# judgmentType - a character vector with any subset of "decision", "resolution",
#   "sentence", "regulation", "reasons"
# judgeName - string
# legalBase - ??
# referencedRegulation - 
# courtType - 
# commonCourtId - 
# keyword - 
# supremeChamberId - 

# size - integer
# sort - string, example "JUDGMENT_DATE"
