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
  
  # parse judgmentType
  if (!is.null(query$judgmentTypes)){
    tmp <- sapply(query$judgmentTypes, match.arg, 
                  c("DECISION", "RESOLUTION", "SENTENCE", "REGULATION", "REASONS"))
    query$judgmentTypes <- paste(tmp, collapse = ",")
  }
  
  # check arguments with query language
  query$all <- parse_query(query$all)
  query$legalBase <- parse_query(query$legalBase)
  query$referencedRegulation <- parse_query(query$referencedRegulation)
  
  # check the rest of arguments
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
  argname <- tail(strsplit(deparse(substitute(date)), "\\$")[[1]], 1)
  
  if (is.null(date)) return(date)
  if (is.na(date)) stop(argname, " cannot be NA", call. = FALSE)
  
  if (length(date) > 1) 
    stop("Argument ", argname, " has to be of length one.", call. = FALSE)
  
  if (is.character(date)) {
    date <- as.POSIXct(date, format = format)
    
    if (is.na(date))
      stop("Wrong ", argname, " format, should be ", format, call. = FALSE)
    
  } else {
    date <- try(as.POSIXct(date, format = format), silent = TRUE)
    
    if (inherits(date, "try-error"))
      stop(argname, " cannot be coerced to POSIXt.", call. = FALSE)
  }
  
  date <- format(date, format = format)
  
  return(date)
}


check_natural <- function(x){
  argname <- tail(strsplit(deparse(substitute(x)), "\\$")[[1]], 1)
  
  if (is.null(x)) return(x)
  
  if (length(x) > 1) 
    stop("Argument ", argname, " has to be of length one.", call. = FALSE)
  
  if (!is.numeric(x)) 
    stop("Argument ", argname, " has to be numeric.", call. = FALSE)
  
  if (x <= 0) 
    stop("Argument ", argname, " has to be positive.", call. = FALSE)
  
  if (abs(x - round(x)) > .Machine$double.eps^0.5) 
    stop("Argument ", argname, " has to be natural number.", call. = FALSE)
  x
}


check_arg <- function(x){
  argname <- tail(strsplit(deparse(substitute(x)), "\\$")[[1]], 1)
  
  if (is.null(x)) return(x)
  
  if (length(x) > 1) stop("Argument ", argname, " has to be of length one.",
                          call. = FALSE)
  
  if (!is.character(x)) stop("Argument ", argname, " has to be character.",
                             call. = FALSE)
  x
}


# parsing query accordign to query language
parse_query <- function(x) UseMethod("parse_query")

parse_query.default <- function(x){
  if (is.null(x)) return(x)
  argname <- tail(strsplit(deparse(substitute(x)), "\\$")[[1]], 1)
  stop(argname, " has to be a character vector or a list with one of two fields",
       "'include' and 'exclude'.", call. = FALSE)
}

parse_query.character <- function(x){
  paste(x, collapse = " OR ")
}

parse_query.list <- function(x){
  argname <- tail(strsplit(deparse(substitute(x)), "\\$")[[1]], 1)
  
  inc <- x$include
  exc <- x$exclude
  
  if (is.null(inc) & is.null(exc)){
    warning(argname, " doesn't contain 'include' and 'exlude' fields, using NULL.",
            call. = FALSE)
    return(NULL)
  }
  
  
  if (any((!is.character(inc) & !is.null(inc)) |
          (!is.character(exc) & !is.null(exc))))
    stop("If ", argname, " is a list, fields 'include' and 'exclude' have to be",
         "character vectors or NULLs", call. = FALSE)
  
  inc <- paste(paste0("\"", inc, "\""), collapse = " OR ")
  exc <- if (is.null(exc)) { NULL } else { paste(paste0("-\"", exc, "\""), collapse = " ") }
  paste(inc, exc)
}

