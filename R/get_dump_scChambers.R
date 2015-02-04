#' Download all Supreme Court chambers
#'
#' Download information about all chambers of the Supreme Court of Poland.
#' 
#' @template dump_param
#'   
#' @return If \code{simplify = FALSE} the list of all Supreme Courts chambers 
#'   as returned from API. Every element of the list represents one chamber and 
#'   has the following, illustrative structure: \cr
#'   List of 3 \cr
#'   $ id       : int 1 \cr
#'   $ name     : chr "Izba Karna" \cr
#'   $ divisions:List of 6 \cr
#'   ..$ :List of 3 \cr
#'   .. ..$ id      : int 1 \cr
#'   .. ..$ name    : chr "Wydział VI" \cr
#'   .. ..$ fullName: chr "Izba Karna Wydział VI" \cr
#'   ..$ :List of 3 \cr
#'   .. ..$ id      : int 4 \cr
#'   .. ..$ name    : chr "Wydział I" \cr
#'   .. ..$ fullName: chr "Izba Karna Wydział I" \cr
#'   .. ... \cr
#'    
#'    Detailed description of the meaning of all elements could be found in
#'    \code{\link[saos]{scchambers}}.
#'    
#'    If \code{simplify = TRUE} a \code{data.frame} as described in 
#'    \code{\link[saos]{scchambers}} is returned.
#'    
#' @seealso \code{\link[saos]{scchambers}}, \code{\link[saos]{get_dump_courts}}
#' 
#' @export

get_dump_scChambers <- function(simplify = FALSE){
  url <- "https://saos-test.icm.edu.pl/api/dump/scChambers"
  chambers <- get_all_items(url, simplify = simplify)
  chambers
}
