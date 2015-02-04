# checking correctness of query
check_query <- function(query){
  # remove NULLs
  #query <- query[!vapply(query, is.null, logical(1))]
  # 
  # checking dates
  query$judgmentDateFrom <- check_date(query$judgmentDateFrom)
  query$judgmentDateTo <- check_date(query$judgmentDateTo)
  
  # checking natural values
  query$ccCourtId <- check_natural(query$ccCourtId)
  query$ccDivisionId <- check_natural(query$ccDivisionId)
  query$scChamberId <- check_natural(query$scChamberId)
  query$scDivisionId <- check_natural(query$scDivisionId)
  
  # checking sort
  query$sortingField <- match.arg(toupper(query$sortingField),
                                  c("DATABASE_ID", "JUDGMENT_DATE", "CASE_NUMBER",
                                    "CC_COURT_TYPE", "CC_COURT_ID", "CC_COURT_CODE",
                                    "CC_COURT_NAME", "CC_COURT_DIVISION_ID",
                                    "CC_COURT_DIVISION_CODE", "CC_COURT_DIVISION_NAME",
                                    "SC_JUDGMENT_FORM", "SC_PERSONNEL_TYPE", 
                                    "SC_COURT_DIVISION_ID", "SC_COURT_DIVISION_NAME",
                                    "SC_COURT_DIVISIONS_CHAMBER_ID",
                                    "SC_COURT_DIVISIONS_CHAMBER_NAME"))
  query$sortingDirection <- match.arg(toupper(query$sortingDirection), c("ASC", "DESC"))
  
  # check arguments with predefined list of values and single value allowed only
  if (!is.null(query$courtType)){
    query$courtType <- match.arg(toupper(query$courtType),
                               c("COMMON", "SUPREME","ADMINISTRATIVE",
                                 "CONSTITUTIONAL_TRIBUNAL", 
                                 "NATIONAL_APPEAL_CHAMBER"))
  }
  if (!is.null(query$ccCourtType)){
    query$ccCourtType <- match.arg(toupper(query$ccCourtType),
                                   c("APPEAL", "REGIONAL", "DISTRICT"))
  }
  if (!is.null(query$scPersonnelType)){
    query$scPersonnelType <- match.arg(toupper(query$scPersonnelType),
                                       c("ONE_PERSON", "THREE_PERSON", "FIVE_PERSON",
                                         "SEVEN_PERSON", "ALL_COURT", "ALL_CHAMBER",
                                         "JOINED_CHAMBERS"))
  }
  if (!is.null(query$judgmentTypes)){
    query$judgmentTypes <- match.arg(toupper(query$judgmentTypes),
                                     c("DECISION", "RESOLUTION", "SENTENCE",
                                       "REGULATION", "REASONS"))
  }
  
  # check the rest of arguments - in future query language must be implemened somehow
  query$all <- check_arg(query$all)
  query$legalBase <- check_arg(query$legalBase)
  query$referencedRegulation <- check_arg(query$referencedRegulation)
  query$judgeName <- check_arg(query$judgeName)
  query$caseNumber <- check_arg(query$caseNumber)
  query$ccCourtCode <- check_arg(query$ccCourtCode)
  query$ccCourtName <- check_arg(query$ccCourtName)
  query$ccDivisionCode <- check_arg(query$ccDivisionCode)
  query$ccDivisionName <- check_arg(query$ccDivisionName)
  query$scChamberName <- check_arg(query$scChamberName)
  query$scDivisionName <- check_arg(query$scDivisionName)
  query$keywords <- check_arg(query$keywords)
  
  return(query)
}


check_date <- function(date, format = "%Y-%m-%d"){
  if (is.null(date)) return(date)
  
  if (length(date) > 1) stop("Date has to be length one.")
  
  if (is.character(date)) {
    date <- as.POSIXct(date, format = format)
    if (is.na(date))
      stop(sprintf("Wrong date format, should be %s.", format))
  } else {
    date <- as.POSIXct(date)
  }
  date <- format(date, format = format)
  
  return(date)
}


check_natural <- function(x){
  if (is.null(x)) return(x)
  
  if (length(x) > 1) stop("Arg has to be length one.")
  
  if (!is.numeric(x)) stop("Arg has to be numeric")
  
  if (x <= 0) stop("Arg has to be positive")
  if (abs(x - round(x)) > .Machine$double.eps^0.5) stop("Arg has to be natural number")
  x
}


check_arg <- function(x){
  if (is.null(x)) return(x)
  
  if (length(x) > 1) stop("Arg has to be length one.")
  
  if (!is.character(x)) stop("Arg has to be character.")
  x
}