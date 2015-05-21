#' Get all data about judgments
#' 
#' Get_judgments will download all information available about given judgments.
#' 
#' @param x An object used to select a method.
#' @param verbose Logical. Whether or not display progress bar.
#' @param updateProgress Function for updating progress (i.e. in Shiny).
#' 
#' @return A list with elements corresponding to judgments, as returned from API.
#' 
#' @examples \donttest{
#' ## From list of IDs
#' # single ID
#' judgments1 <- get_judgments(1)
#' 
#' # vector of IDs 
#' judgments2 <- get_judgments(c(1, 100))
#' 
#' # vector of IDs with non-existent judgment
#' judgments3 <- get_judgments(c(128334, 1, 100))
#' 
#' 
#' ## From search results
#' search <- search_judgments(limit = 10, verbose = FALSE)
#' judgments4 <- get_judgments(search)
#'  }
#'  
#' @export

get_judgments <- function(x, verbose = TRUE, updateProgress = NULL) UseMethod("get_judgments")


# default method
#' @export
get_judgments.default <- function(x, verbose, updateProgress){
  stop("get_judgments accept arguments of class 'saos_search' or numeric vectors.")
}






#' @describeIn get_judgments Download judgments, if available, with IDs from a 
#'   given vector. If judgment is not available (meaning it doesn't exist in
#'   database) it will be skipped and warning will be generated.
#' 
#' @export

get_judgments.numeric <- function(x, verbose = TRUE, updateProgress = NULL){
  x <- check_idlist(x)
  idlist <- sort(unique(x))
  url <- "https://saos-test.icm.edu.pl/api/judgments/"
  links <- paste0(url, idlist)
  
  l <- length(x)
  result <- vector("list", l)
  nulls <- logical(l)
  
  if (verbose) pb <- txtProgressBar(style = 3)
  
  for (i in seq(l)) {
    response <- get_response_if_available(links[i])
    if (is.null(response)){
      result[[i]] <- NULL
      nulls[i] <- TRUE
    } else {
      result[[i]] <- response$data
    }
    if (verbose) setTxtProgressBar(pb, i / l)
    if (is.function(updateProgress)) {
      updateProgress(value = i/l)
    }
  }
  
  if (verbose) close(pb)
  
  # message about NULLs
  if (any(nulls)) {
    message("Following IDs don't exist: \n", 
            paste(idlist[nulls], collapse = ", "))
  }
  
  class(result) <- c("saos_judgments", "list")
  result
}






#' @describeIn get_judgments Get details of all judgments in given search 
#'   results.
#' 
#' @export

get_judgments.saos_search <- function(x, verbose = TRUE, updateProgress = NULL){
  links <- sapply(x, `[[`, "href")
  l <- length(x)
  result <- vector("list", l)
  
  if (verbose) pb <- txtProgressBar(style = 3)
  
  for (i in seq_along(x)) {
    response <- get_response(links[i])
    result[[i]] <- response$data
    if (verbose) setTxtProgressBar(pb, i / l)
    if (is.function(updateProgress)) {
      updateProgress(value = i/l)
    }
  }
  
  if (verbose) close(pb)
  
  class(result) <- c("saos_judgments", "list")
  result
}


# method for concatenating judgments list
#' @export
c.saos_judgments <- function(..., recursive = FALSE) {
  res <- unlist(list(...), recursive = FALSE)
  class(res) <- c("saos_judgments", "list")
  res
}

# method for subsetting
#' @export
`[.saos_judgments` <- function(x, ind) {
  res <- unclass(x)[ind]
  class(res) <- c("saos_judgments", "list")
  res
} 





#### utility functions ####

# function returning empty list with proper class attribute
empty_get_result <- function() {
  res <- list()
  class(res) <- c("saos_judgments", "list")
  res
}


get_response_if_available <- function(link){
  tryCatch(get_response(link), http_404 = function(x) NULL)
}


check_idlist <- function(x) {
  if (any(x <= 0)) 
    stop("All id's have to be positive integers.", call. = FALSE)
  
  if (any(abs(x - round(x)) > .Machine$double.eps^0.5))
    stop("All id's have to be positive integers.", call. = FALSE)
  x
}

