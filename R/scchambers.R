#' Information about all Supreme Court chambers
#'
#' A dataset containing basic information about chambers of the Supreme Court of
#'   Poland and their organisational structure. Up-to-date version of this 
#'   dataset could be obtained with \code{\link{get_scChambers}}.
#'
#' @format A data frame with 4 rows and 3 variables:
#' \tabular{rlll}{
#' n \tab name \tab class \tab description \cr
#' [,1] \tab id \tab integer \tab ID in the repository \cr
#' [,2] \tab name \tab character \tab full name of the chamber \cr
#' [,3] \tab divisions \tab list \tab details of the chamber's divisions, see 
#'   details below \cr
#' }
#' 
#' Information about divisions is stored in dataframes with following columns.
#' \tabular{rlll}{
#' n \tab name \tab class \tab description \cr
#' [,1] \tab id \tab integer \tab uniue ID \cr
#' [,2] \tab name \tab character \tab name of the division \cr
#' [,3] \tab fullName \tab character \tab full name of a division, composed from
#'   names of both chamber and its division \cr
#' }
#' 
#' There is no missing data.
#' 
"scchambers"