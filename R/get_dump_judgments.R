#' Download all judgments
#' 
#' Download the list of all judgments within (and including) given dates.
#'  
#' @param start_date Any date/time object that could be properly converted with 
#'   \code{as.POSIXct}, or a string in a format "\%Y-\%m-\%d". 
#'   Represents the earliest allowed judgment's date on the list. 
#'   If missing, no time limit is set.
#' @param end_date Any date/time object that could be properly converted with 
#'   \code{as.POSIXct}, or a string in a format "\%Y-\%m-\%d". 
#'   Represents the latest allowed judgment's date on the list. 
#'   If missing, no time limit is set.
#' @param modification_date Any date/time object that could be properly converted with 
#'   \code{as.POSIXct}, or a string in format "\%Y-\%m-\%dT\%H:\%M:\%S". 
#'   Allows to select judgments which were modified later than the specified 
#'   time. If missing, no time limit is set.
#' @template dump_param
#' @param flatten Logical, works only if \code{simplify = TRUE}. If \code{TRUE}
#'   resulting \code{data.frame} will be flattened (with \code{jsonlite::flatten}).
#' @param verbose Logical. Whether or not display progress bar.
#'   
#' @return If \code{simplify = FALSE} the list of judgments as returned 
#'   from API. Every element of the list represents one judgment and has the 
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
#'  If \code{simplify = TRUE} a \code{data.frame} as described in 
#'    TODO is returned. If also \code{flatten = TRUE}, a \code{data.frame} will
#'    be flattened.
#'    
#' @examples 
#' \dontrun{
#' full <- get_dump_judgments()
#' lastchanges <- get_dump_judgments(modification_date = Sys.Date() - 7)
#' }
#' \donttest{
#' # Download judgments from last month as a:
#' # - list
#' lastmonth_l <- get_dump_judgments(start_date = Sys.Date() - 30, 
#'                                   end_date = Sys.Date())
#' # - data.frame                                 
#' lastmonth_df <- get_dump_judgments(start_date = Sys.Date() - 30, 
#'                                    end_date = Sys.Date(),
#'                                    simplify = TRUE, flatten = FALSE)
#' # - flattened data.frame                                    
#' lastmonth_df_f <- get_dump_judgments(start_date = Sys.Date() - 30, 
#'                                      end_date = Sys.Date(),
#'                                      simplify = TRUE)
#'                                      
#' class(lastmonth_l)
#' class(lastmonth_df)                           
#' class(lastmonth_df_f)
#' length(lastmonth_l)
#' dim(lastmonth_df)
#' dim(lastmonth_df_f)
#'  }
#'  
#' @export

get_dump_judgments <- function(start_date = NULL, end_date = NULL,
                               modification_date = NULL, simplify = FALSE,
                               flatten = simplify, verbose = TRUE){
  url <- "https://saos-test.icm.edu.pl/api/dump/judgments/"
  
  # check arguments
  start_date <- check_date(start_date)
  end_date <- check_date(end_date)
  modification_date <- check_date(modification_date, 
                                  format = "%Y-%m-%dT%H:%M:%S")
  if (!is.null(modification_date)) {
    modification_date <- paste0(modification_date, ".000")
    if (verbose) {
      warning("Cannot estimate the size of results set when modification_date is given",
              call. = FALSE)
      verbose <- FALSE
    }
  }
  
  number <- if (verbose)  { 
    count_judgments(judgmentDateFrom = start_date, judgmentDateTo = end_date)
  } else { NULL }
    
  # prepare link to API
  query <- list(pageSize = 100, judgmentStartDate = start_date, 
             judgmentEndDate = end_date,
             sinceModificationDate = modification_date)
  
  # get results
  if (verbose) message(number, " judgments to download.\n")
  judgments <- get_all_items(url, query = query, simplify = simplify,
                             flatten = flatten, verbose = verbose, 
                             number = number)
  
  # simplify courtcases
  if (simplify) judgments$courtCases <- unlist(judgments$courtCases)
  
  judgments
}
