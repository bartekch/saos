#' Get all data about judgments
#' 
#' Get_judgments will download all information available about given judgments.
#' 
#' @param x An object used to select a method.
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
#' search <- search_judgments(limit = 10, progress = FALSE)
#' judgments4 <- get_judgments(search)
#'  }
#'  
#' @export

get_judgments <- function(x) UseMethod("get_judgments")


# default method
#' @export
get_judgments.default <- function(x){
  stop("get_judgments accept arguments of class 'saos_search' or numeric vectors.")
}

#' @describeIn get_judgments Download judgments, if available, with IDs from a 
#'   given vector. If judgment is not available (meaning it doesn't exist in
#'   database) it will be skipped and warning will be generated.
#' 
#' @export

get_judgments.numeric <- function(x){
  idlist <- sort(unique(x))
  url <- "https://saos-test.icm.edu.pl/api/judgments/"
  links <- paste0(url, idlist)
  result <- lapply(links, get_response_if_available)
  
  # skip NULLs
  nulls <- sapply(result, is.null)
  message("Following ID don't exist: \n", idlist[nulls])
  result <- result[!nulls]
  class(result) <- c("saos_judgments", "list")
  result
}


#' @describeIn get_judgments Get details of all judgments in given search 
#'   results.
#' 
#' @export

get_judgments.saos_search <- function(x){
  links <- sapply(x, `[[`, "href")
  result <- lapply(links, get_response)
  class(result) <- c("saos_judgments", "list")
  result
}






#########

get_response_if_available <- function(link){
  tryCatch(get_response(link), http_404 = function(x) NULL)
}
