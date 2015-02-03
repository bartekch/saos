#' Download all Supreme Court chambers
#'
#' Download information about all chambers of the Supreme Court of Poland.
#' 
#' @template dump_param
#'   
#' @return The list of all Supreme Courts chambers as returned from API.
#' 
# @param flatten logical, whether flatten information about divisions of every
#  chamber
# 
# @return If \code{flatten = FALSE} (default) data.frame as described in 
#   \code{\link[saos]{scchambers}}.  
# If \code{flatten = TRUE} data.frame with rows corresponding to divisions
#   and five columns: information about chamber (\code{id, name}) and
#   information about division (\code{div_id, div_name, div_fullName}). 
#'
#' @seealso \code{\link[saos]{scchambers}}, \code{\link[saos]{get_dump_courts}}
#' 
#' @export

get_dump_scChambers <- function(simplify = FALSE){
  url <- "https://saos-test.icm.edu.pl/api/dump/scChambers"
  chambers <- get_all_items(url, simplify = simplify)
#   }
#   if (flatten){
#     l <- sapply(chambers$divisions, nrow)
#     ind <- rep(seq(nrow(chambers)), times = l)
#     divisions <- do.call(rbind, chambers$divisions)
#     names(divisions) <- paste("div", names(divisions), sep = "_")
#     chambers <- cbind(chambers[ind, 1:2], divisions)
#   }
  chambers
}
