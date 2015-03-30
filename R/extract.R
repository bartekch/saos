#' Extract information from judgments' list
#' 
#' Extract as a data frame specific elements from judgements' list as returned
#'   from \code{\link{search_judgments}}, \code{\link{get_judgments}} or
#'   \code{\link{get_dump_judgments}}.
#'
#' @param x An object used to select a method.
#' @param element Name of element to extract, see Details.
#'
#' @details Available elements for all methods are:
#' \itemize{
#'  \item id,
#'  \item courtCases,
#'  \item judgmentType,
#'  \item judges,
#'  \item textContent,
#'  \item keywords,
#'  \item division,
#'  \item judgmentDate.
#'  }
#'  Additionally for classes \code{saos_judgments} and \code{saos_judgments_dump}
#'    there are more elements. NOTE - some fields are specific to single court type,
#'    thus are not always present, it is indicated in parentheses.
#'  \itemize{
#'  \item courtType,
#'  \item source, 
#'  \item courtReporters,
#'  \item decision,
#'  \item summary, 
#'  \item legalBases,
#'  \item referencedRegulations,
#'  \item referencedCourtCases,
#'  \item judgmentResult,
#'  \item receiptDate,
#'  \item meansOfAppeal,
#'  \item lowerCourtJudgments,
#'  \item judgmentForm (\code{saos_judgments}) or form (\code{saos_judgments_dump}),
#'    (only SUPREME COURT),
#'  \item personnelType (only SUPREME COURT),
#'  \item chambers (only SUPREME COURT).
#'  }
#'  For class \code{saos_judgments} and \code{saos_search} there is also 
#'    element "href".
#'  
#' Data frame with data for all given judgments is always returned. If there is
#'   no data, a row with only \code{NA} (except for id) is insertes in the data
#'   frame. Data frames are always in long format, for example when one judge
#'   has a few roles, his name is multiplied accordingly. 
#'   
#' @return A data.frame with first column indicating ID of judgment and the rest
#'   depending on which element was extracted, see Details.
#'  
#' @examples \donttest{
#' saos_search <- search_judgments(limit = 50, verbose = FALSE)
#' saos_judgments <- get_judgments(saos_search, verbose = FALSE)
#' 
#' # for class "saos_search"
#' judges <- extract(saos_search, "judges")
#' dim(judges)
#' names(judges)
#' 
#' divisions <- extract(saos_search, "division")
#' dim(divisions)
#' names(divisions)
#' 
#' 
#' # for class "saos_judgments"
#' sources <- extract(saos_judgments, "source")
#' dim(sources)
#' names(sources)
#' 
#' ref_reg <- extract(saos_judgments, "referencedRegulations")
#' dim(ref_reg)
#' names(ref_reg)
#' 
#' 
#' ## for class "saos_judgments_dump"
#' judgments <- get_dump_judgments(judgmentStartDate = "2015-01-01", 
#'                                 judgmentEndDate = "2015-10-01", 
#'                                 verbose = FALSE)
#' court <- extract(judgments, "courtType")
#' dim(court)
#' names(court)                                
#' }
#' 
#' @export

extract <- function(x, element) UseMethod("extract")

# default function
#' @export
extract.default <- function(x, element) {
  stop("extract accepts arguments of class saos_search, saos_judgments or saos_judgments_dump.",
       call. = FALSE)
}


#' @describeIn extract Extract data from list of results from 
#'   \code{\link{search_judgments}}.
#' 
#' @export
extract.saos_search <- function(x, element) {
  element <- match.arg(element, c("id", "href", "courtCases", "judgmentType",
                                  "judges", "textContent", "keywords", 
                                  "division", "judgmentDate"))
  ids <- extract_id(x)
  
  if (element == "id") return(data.frame(id = ids))
  
  # extract chosen element
  # in first column there are always IDs numbered within given search (1,2,...)
  info <- switch(element, 
                 href = extract_href(x),
                 courtCases = extract_courtcases(x),
                 judgmentType = extract_judgmenttype(x),
                 judges = extract_judges(x),
                 textContent = extract_text(x),
                 keywords = extract_keywords(x),
                 division = extract_division(x),
                 judgmentDate = extract_judgmentdate(x))
  
  # convert IDs to true values 
  info$id <- ids[info$id]
  info
}



#' @describeIn extract Extract data from list of judgments from 
#'   \code{\link{get_judgments}}.
#' 
#' @export
extract.saos_judgments <- function(x, element) {
  element <- match.arg(element, c("id", "courtType", "href", "courtCases", 
                                  "judgmentType", "judgmentDate", "judges", 
                                  "source", "courtReporters", "decision", 
                                  "summary", "textContent", "legalBases",
                                  "referencedRegulations", "keywords",
                                  "referencedCourtCases", "division",
                                  "personnelType", "judgmentForm", "chambers",
                                  "judgmentResult", "receiptDate", 
                                  "meansOfAppeal", "lowerCourtJudgments"))
  ids <- sapply(x, `[[`, "id")
  if (length(ids) == 0) ids <- integer()
  if (element == "id") return(data.frame(id = ids))
  
  # extract chosen element
  # in first column there are always IDs numbered within given search (1,2,...)
  info <- switch(element,
                 courtType = extract_courttype(x),
                 href = extract_href(x),
                 courtCases = extract_courtcases(x),
                 judgmentType = extract_judgmenttype(x),
                 judgmentDate = extract_judgmentdate(x),
                 judges = extract_judges(x),
                 source = extract_source(x),
                 courtReporters = extract_courtreporters(x),
                 decision = extract_decision(x),
                 summary = extract_summary(x),
                 textContent = extract_text(x),
                 legalBases = extract_legalbases(x),
                 referencedRegulations = extract_refreg(x),
                 referencedCourtCases = extract_refcc(x),
                 keywords = extract_keywords(x),
                 division = extract_division(x),
                 personnelType = extract_personnel(x),
                 judgmentForm = extract_form(x, "judgmentForm"),
                 chambers = extract_chambers(x),
                 judgmentResult = extract_result(x),
                 receiptDate = extract_receiptdate(x),
                 meansOfAppeal = extract_means(x),
                 lowerCourtJudgments = extract_lowercourt(x))
  
  # convert IDs to true values 
  info$id <- ids[info$id]
  info
}





#' @describeIn extract Extract data from list of judgments from 
#'   \code{\link{get_dump_judgments}}.
#' 
#' @export
extract.saos_judgments_dump <- function(x, element) {
  element <- match.arg(element, c("id", "courtType", "courtCases", 
                                  "judgmentType", "judgmentDate", "judges", 
                                  "source", "courtReporters", "decision", 
                                  "summary", "textContent", "legalBases",
                                  "referencedRegulations", "keywords",
                                  "personnelType", "form", "chambers",
                                  "referencedCourtCases", "division", 
                                  "judgmentResult", "receiptDate",
                                  "meansOfAppeal", "lowerCourtJudgments"))
  ids <- sapply(x, `[[`, "id")
  if (length(ids) == 0) ids <- integer()
  if (element == "id") return(data.frame(id = ids))
  
  # extract chosen element
  # in first column there are always IDs numbered within given search (1,2,...)
  info <- switch(element,
                 courtType = extract_courttype(x),
                 courtCases = extract_courtcases(x),
                 judgmentType = extract_judgmenttype(x),
                 judgmentDate = extract_judgmentdate(x),
                 judges = extract_judges(x),
                 source = extract_source(x),
                 courtReporters = extract_courtreporters(x),
                 decision = extract_decision(x),
                 summary = extract_summary(x),
                 textContent = extract_text(x),
                 legalBases = extract_legalbases(x),
                 referencedRegulations = extract_refreg(x),
                 referencedCourtCases = extract_refcc(x),
                 keywords = extract_keywords(x),
                 division = extract_division(x),
                 personnelType = extract_personnel(x),
                 form = extract_form(x, "form"),
                 chambers = extract_chambers(x),
                 judgmentResult = extract_result(x),
                 receiptDate = extract_receiptdate(x),
                 meansOfAppeal = extract_means(x),
                 lowerCourtJudgments = extract_lowercourt(x))
  
  # convert IDs to true values 
  info$id <- ids[info$id]
  info
}



### functions with actual extracting

# problem: some fields are very often empty; it could happen than in given
#  judgments all fields of chosen type are empty. In such case we return data
#  frame with NA in all rows and columns corresponding to this field, instead
#  of a data frame with only IDs. It must be coherent - for given method
#  always return the same data frame, even if there is no data in it. 
#  Therefore we need an empty template of a data frame. 
# What is more, sometimes the same fields have different structure for different
#  judgments (difference between supreme and common courts). The solution is to
#  include always all possible columns. 
# Another problem is difference in structure between classes. Solution - 
#  declaring NA template inside method, not in 'extract_' functions. 

extract_id <- function(judgments){
  as.integer(sapply(judgments, function(x) tail(strsplit(x$href, "/")[[1]], 1)))
}

extract_href <- function(x){
  extractor_single(x, "href")
}

extract_courtcases <- function(x){
  extractor_dflist(x, "courtCases", data.frame(caseNumber = character(),
                                               stringsAsFactors = FALSE))
}

extract_judgmenttype <- function(x){
  extractor_single(x, "judgmentType")
}

extract_judges <- function(results) {
  df_template <- data.frame(name = character(), "function" = character(),
                            specialRoles = character(), stringsAsFactors = FALSE)
  if (length(results) == 0) {
    return(data.frame(id = integer(), df_template))
  }
  
  df_na <- df_template[NA_character_,, drop = FALSE]
  el <- lapply(results, function(x) {
    if (length(x$judges) == 0){
      df_na
    } else {
      dplyr::bind_rows(lapply(x$judges, function(judge) {
        judge <- lapply(judge, function(f) if (is.null(f)) { NA } else { f })
        l <- length(judge$specialRoles)
        l2 <- max(1, l)
        data.frame(name = rep(judge$name, l2), 
                   "function" = rep(judge$"function", l2),
                   specialRoles = if (l == 0) { NA } else { unlist(judge$specialRoles) },
                   stringsAsFactors = FALSE)
      }))
    }})
  times <- sapply(el, function(x) ifelse(is.data.frame(x), nrow(x), 1))
  el <- data.frame(id = rep(seq_along(times), times = times),
                   dplyr::bind_rows(el))
  el
}

extract_text <- function(x) {
  extractor_single(x, "textContent")
}

extract_keywords <- function(x) {
  extractor_list(x, "keywords")
}

extract_division <- function(x) {
  df_temp <- data.frame(id = integer(), name = character(),
                        href = character(), stringsAsFactors = FALSE)
  result <- extractor_df(x, "division", df_temp)
  names(result)[which(names(result) == "id.1")] <- "division.id"
  result
}

extract_judgmentdate <- function(x){
  extractor_single(x, "judgmentDate")
}

extract_courttype <- function(x){
  extractor_single(x, "courtType")
}

extract_source <- function(x) {
  df_temp <- data.frame(code = character(), judgmentUrl = character(), 
                        judgmentId = character(),publisher = character(),
                        reviser = character(), publicationDate = character(),
                        stringsAsFactors = FALSE)
  extractor_df(x, "source", df_temp)
}

extract_courtreporters <- function(x) {
  extractor_list(x, "courtReporters")
}

extract_decision <- function(x){
  extractor_single(x, "decision")
}

extract_summary <- function(x){
  extractor_single(x, "summary")
}

extract_legalbases <- function(x){
  extractor_list(x, "legalBases")
}

extract_refreg <- function(x) {
  df_temp <- data.frame(journalTitle = character(), journalNo = integer(),
                        journalYear = integer(), journalEntry = integer(),
                        text = character(), stringsAsFactors = FALSE)
  extractor_dflist(x, "referencedRegulations", df_temp)
}

extract_refcc <- function(x) {
  extractor_list(x, "referencedCourtCases")
}

extract_personnel <- function(x) {
  extractor_single(x, "personnelType")
}

extract_form <- function(x, name) {
  extractor_list(x, name)
}

extract_chambers <- function(x) {
  df_temp <- data.frame(id = integer())
  result <- extractor_df(x, "chambers", df_temp)
  names(result)[which(names(result) == "id.1")] <- "chambers.id"
  result
}

extract_result <- function(x) {
  extractor_single(x, "judgmentResult")
}

extract_receiptdate <- function(x) {
  extractor_single(x, "receiptDate")
}

extract_means <- function(x) {
  extractor_single(x, "meansOfAppeal")
}

extract_lowercourt <- function(x) {
  extractor_list(x, "lowerCourtJudgments")
}

# extractors
extractor_single <- function(judgments, field_name) {
  if (length(judgments) == 0) {
    tmp <- data.frame(id = integer(), var = character(), stringsAsFactors = FALSE)
    names(tmp)[2] <- field_name
    return(tmp)
  }
  el <- lapply(judgments, `[[`, field_name)
  el <- sapply(el, function(x) if (is.null(x)) { NA } else { x })
  el <- data.frame(seq_along(el), el, stringsAsFactors = FALSE)
  colnames(el) <- c("id", field_name)
  el
}

extractor_list <- function(judgments, field_name) {
  if (length(judgments) == 0) {
    tmp <- data.frame(id = integer(), var = character(), stringsAsFactors = FALSE)
    names(tmp)[2] <- field_name
    return(tmp)
  }
  el <- lapply(judgments, function(x) {
    if (length(x[[field_name]]) == 0) {
      NA
    } else {
      x[[field_name]]
    }
  })
  times <- sapply(el, length)
  el <- data.frame(rep(seq_along(times), times = times), unlist(el),
                   stringsAsFactors = FALSE)
  colnames(el) <- c("id", field_name)
  el
}

extractor_df <- function(judgments, field_name, df_template) {
  if (length(judgments) == 0) {
    return(data.frame(id = integer(), df_template))
  }
  df_na <- df_template[NA_character_,, drop = FALSE]
  el <- lapply(judgments, function(x) {
    if (length(x[[field_name]]) == 0){
      df_na
    } else {
      tmp <- lapply(x[[field_name]], function(f) if (is.null(f)) { NA } else { f })
      as.data.frame(tmp, stringsAsFactors = FALSE)
    }})
  
  times <- sapply(el, nrow)
  el <- data.frame(id = rep(seq_along(times), times = times),
                   dplyr::bind_rows(el))
  el
}

extractor_dflist <- function(judgments, field_name, df_template) {
  if (length(judgments) == 0) {
    return(data.frame(id = integer(), df_template))
  }
  
  df_na <- df_template[NA_character_,, drop = FALSE]
  el <- lapply(judgments, function(x) {
    if (length(x[[field_name]]) == 0){
      df_na
    } else {
      dplyr::bind_rows(lapply(x[[field_name]], dplyr::as_data_frame))
    }})
  
  times <- sapply(el, nrow)
  el <- data.frame(id = rep(seq_along(times), times = times),
                   dplyr::bind_rows(el))
  el
}