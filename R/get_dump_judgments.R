#' Download all judgments
#' 
#' Download the list of all judgments within (and including) given dates.
#'  
#' @param judgmentStartDate Any date/time object that could be properly converted with 
#'   \code{as.POSIXct}, or a string in a format "\%Y-\%m-\%d". 
#'   Represents the earliest allowed judgment's date on the list. 
#'   If missing, no time limit is set.
#' @param judgmentEndDate Any date/time object that could be properly converted with 
#'   \code{as.POSIXct}, or a string in a format "\%Y-\%m-\%d". 
#'   Represents the latest allowed judgment's date on the list. 
#'   If missing, no time limit is set.
#' @param sinceModificationDate Any date/time object that could be properly converted with 
#'   \code{as.POSIXct}, or a string in format "\%Y-\%m-\%dT\%H:\%M:\%S". 
#'   Allows to select judgments which were modified later than the specified 
#'   time. If missing, no time limit is set.
#' @param verbose Logical. Whether or not display progress bar.
#'   
#' @encoding UTF-8
#' 
#' @return The list of judgments as returned from API. 
#'   Every element of the list represents one judgment and has the 
#'   following, illustrative structure: \cr
#'  List of 18 \cr
#'  $ id                   : int 76452 \cr
#'  $ courtType            : chr "SUPREME" \cr
#'  $ courtCases           :List of 1 \cr
#'  ..$ :List of 1 \cr
#'  .. ..$ caseNumber: chr "III ARN 5/94" \cr
#'  $ judgmentType         : chr "SENTENCE" \cr
#'  $ judges               :List of 5 \cr
#'  ..$ :List of 3 \cr
#'  .. ..$ name        : chr "Jerzy Kwaśniewski" \cr
#'  .. ..$ function    : NULL \cr
#'  .. ..$ specialRoles:List of 1 \cr
#'  .. .. ..$ : chr "PRESIDING_JUDGE" \cr
#'  ..$ :List of 3 \cr
#'  .. ..$ name        : chr "Maria Tyszel" \cr
#'  .. ..$ function    : NULL \cr
#'  .. ..$ specialRoles:List of 2 \cr
#'  .. .. ..$ : chr "REPORTING_JUDGE" \cr
#'  .. .. ..$ : chr "REASONS_FOR_JUDGMENT_AUTHOR" \cr
#'  .. ... \cr
#'  $ source               :List of 6 \cr
#'  ..$ code           : chr "SUPREME_COURT" \cr
#'  ..$ judgmentUrl    : chr "http://www.sn.pl/orzecznictwo" __truncated__ \cr
#'  ..$ judgmentId     : chr "447e19d3e677e04362b53a63ef85da90" \cr
#'  ..$ publisher      : NULL \cr
#'  ..$ reviser        : NULL \cr
#'  ..$ publicationDate: chr "2014-02-11" \cr
#'  $ courtReporters       : list() \cr
#'  $ decision             : NULL \cr
#'  $ summary              : NULL \cr
#'  $ textContent          : chr "17.Wyrok z dnia 20 stycznia" __truncated__ \cr
#'  $ legalBases           : list() \cr
#'  $ referencedRegulations: list() \cr
#'  $ keywords             : list() \cr
#'  $ personnelType        : chr "FIVE_PERSON" \cr
#'  $ form                 :List of 1 \cr
#'  ..$ name: chr "wyrok SN" \cr
#'  $ division             :List of 1 \cr
#'  ..$ id: int 11 \cr
#'  $ chambers             :List of 1 \cr
#'  ..$ :List of 1 \cr
#'  .. ..$ id: int 3 \cr
#'  $ judgmentDate         : chr "1994-01-20" \cr
#'  
#'  Detailed description of the meaning of all elements could be found in TODO
#'    
#' @examples 
#' \dontrun{
#' full <- get_dump_judgments()
#' lastchanges <- get_dump_judgments(sinceModificationDate = Sys.Date() - 7)
#' }
#' \donttest{
#' # Download judgments from last month
#' lastmonth <- get_dump_judgments(judgmentStartDate = Sys.Date() - 30, 
#'                                 judgmentEndDate = Sys.Date())
#'                                      
#' class(lastmonth)
#' length(lastmonth)
#'  }
#'  
#' @export

get_dump_judgments <- function(judgmentStartDate = NULL, 
                               judgmentEndDate = NULL,
                               sinceModificationDate = NULL, 
                               verbose = TRUE){
  url <- "https://saos-test.icm.edu.pl/api/dump/judgments/"
  
  # check arguments
  judgmentStartDate <- check_date(judgmentStartDate)
  judgmentEndDate <- check_date(judgmentEndDate)
  sinceModificationDate <- check_date(sinceModificationDate, 
                                  format = "%Y-%m-%dT%H:%M:%S")
  if (!is.null(sinceModificationDate)) {
    sinceModificationDate <- paste0(sinceModificationDate, ".000")
    if (verbose) {
      message("Cannot estimate the size of results set when modification_date is given")
      verbose <- FALSE
    }
  }
  
  number <- if (verbose)  { 
    count_judgments(judgmentDateFrom = judgmentStartDate, 
                    judgmentDateTo = judgmentEndDate)
  } else { NULL }
    
  # prepare link to API
  query <- list(pageSize = 100, judgmentStartDate = judgmentStartDate, 
             judgmentEndDate = judgmentEndDate,
             sinceModificationDate = sinceModificationDate)
  
  # get results
  if (verbose) message(number, " judgments to download.\n")
  judgments <- get_all_items(url, query = query, verbose = verbose, 
                             number = number)
  
  class(judgments) <- c("saos_judgments_dump", "list")
  judgments
}


# method for concatenating judgments list
#' @export
c.saos_judgments_dump <- function(..., recursive = FALSE) {
  res <- unlist(list(...), recursive = FALSE)
  class(res) <- c("saos_judgments_dump", "list")
  res
}

# method for subsetting
#' @export
`[.saos_judgments_dump` <- function(x, ind) {
  res <- unclass(x)[ind]
  class(res) <- c("saos_judgments_dump", "list")
  res
} 

#### utility functions ####

# function returning empty list with proper class attribute
empty_dump_result <- function() {
  res <- list()
  class(res) <- c("saos_judgments_dump", "list")
  res
}

