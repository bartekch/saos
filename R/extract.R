#' Extract information from judgments' list
#' 
#' Extract as a data frame specific elements from judgements' list as returned
#'   from \code{\link{search_judgments}} or \code{\link{get_judgments}}.
#'
#' @param x An object used to select a method.
#' @param element Name of element to extract, see Details.
#'
#' @details Available elements for all methods are:
#' \itemize{
#'  \item id,
#'  \item href,
#'  \item courtCases,
#'  \item judgmentType,
#'  \item judges,
#'  \item textContent,
#'  \item keywords,
#'  \item division,
#'  \item judgmentDate.
#'  }
#'  Additionally for class \code{saos_judgments} there are more elements:
#'  \itemize{
#'  \item courtType,
#'  \item source, 
#'  \item courtReporters,
#'  \item decision,
#'  \item summary, 
#'  \item legalBases,
#'  \item referencedRegulations,
#'  \item referencedCourtCases.
#'  }
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
#' saos_search <- search_judgments(limit = 150, verbose = FALSE)
#' saos_judgments <- get_judgments(saos_search)
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
#' sources < extract(saos_judgments, "source")
#' dim(sources)
#' names(sources)
#' 
#' ref_reg <- extract(saos_judgments, "referencedRegulations")
#' dim(ref_reg)
#' names(ref_reg)
#' 
#' }
#' 
#' @export

extract <- function(x, element) UseMethod("extract")

# default function
#' @export
extract.default <- function(x, element) {
  stop("extract accepts arguments of class saos_search and saos_judgments.",
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
                                  "referencedCourtCases", "division"))
  ids <- sapply(x, `[[`, "id")
  
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
                 division = extract_division(x))
  
  # convert IDs to true values 
  info$id <- ids[info$id]
  info
}




### functions with actual extracting

# problem: some fields are very often empty; it could happen than in given
#  judgments all fields of chosen type are empty. In such case we return data
#  frame with NA in all rows and columns corresponding to this field, instead
#  of a data frame with only IDs. It must be coherent - always returns the same
#  data frame, even if there is no data in it. Therefore we need an empty 
#  template of a data frame.

extract_id <- function(judgments){
  as.numeric(sapply(judgments, function(x) tail(strsplit(x$href, "/")[[1]], 1)))
}

extract_href <- function(x){
  extractor_single(x, "href")
}

extract_courtcases <- function(x){
  extractor_dflist(x, "courtCases", data.frame(caseNumber = NA))
}

extract_judgmenttype <- function(x){
  extractor_single(x, "judgmentType")
}

extract_judges <- function(results) {
  df_na <- data.frame(name = NA, "function" = NA, specialRoles = NA)
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
  df_na <- data.frame(name = NA, href = NA, code = NA, type = NA)
  result <- extractor_df(x, "division", df_na)
  names(result)[2] <- "division.id"
  result
}

extract_judgmentdate <- function(x){
  extractor_single(x, "judgmentDate")
}

extract_courttype <- function(x){
  extractor_single(x, "courtType")
}

extract_source <- function(x) {
  df_na <- data.frame(code = NA, judgmentUrl = NA, judgmentId = NA,
                      publisher = NA, reviser = NA, publicationDate = NA)
  extractor_df(x, "source", df_na)
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
  df_na <- data.frame(journalTitle = NA, journalNo = NA, journalYear = NA,
                      journalEntry = NA, text = NA)
  extractor_dflist(x, "referencedRegulations", df_na)
}

extract_refcc <- function(x) {
  extractor_list(x, "referencedCourtCases")
}




# extractors
extractor_single <- function(judgments, field_name) {
  el <- lapply(judgments, `[[`, field_name)
  el <- sapply(el, function(x) if (is.null(x)) { NA } else { x })
  el <- data.frame(seq_along(el), el, stringsAsFactors = FALSE)
  colnames(el) <- c("id", field_name)
  el
}

extractor_list <- function(judgments, field_name) {
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
  el <- lapply(judgments, function(x) {
    if (length(x[[field_name]]) == 0){
      df_template
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
  el <- lapply(judgments, function(x) {
    if (length(x[[field_name]]) == 0){
      df_template
    } else {
      dplyr::bind_rows(lapply(x[[field_name]], dplyr::as_data_frame))
    }})
  
  times <- sapply(el, nrow)
  el <- data.frame(id = rep(seq_along(times), times = times),
                   dplyr::bind_rows(el))
  el
}