#' @param all Character. Search everywhere for given phrase.
#' @param legalBase Character. Search for legal basis of judgments.
#' @param referencedRegulation Character. Search for regulations referenced 
#'   in judgments.
#' @param judgeName Character. Search for name of any involved judge.
#' @param caseNumber Character. Search for judgments with given signature.
#' @param courtType Character, one from COMMON, SUPREME, ADMINISTRATIVE, 
#'   CONSTITUTIONAL_TRIBUNAL, NATIONAL_APPEAL_CHAMBER. Search for type of court.
#' @param ccCourtType Character, one from APPEAL, REGIONAL, DISTRICT. Search for
#'   type of common court.
#' @param ccCourtId Positive integer. Search for the given common court ID.
#' @param ccCourtCode Character. Search for the given common court code.
#' @param ccCourtName Character. Search for the given common court name.
#' @param ccDivisionId Positive integer. Search for given common court division ID.
#' @param ccDivisionCode Character. Search for given common court division code.
#' @param ccDivisionName Character. Search for given common court division name.
#' @param scPersonnelType Character, one from ONE_PERSON, THREE_PERSON, 
#'   FIVE_PERSON, SEVEN_PERSON, ALL_COURT, ALL_CHAMBER, JOINED_CHAMBERS. Search
#'   for supreme court judgment's personnel type.
#' @param scChamberId Positive integer. Search for supreme court chamber ID.
#' @param scChamberName Character. Search for supreme court chamber name.
#' @param scDivisionId Positive integer. Search for supreme court chamber division ID.
#' @param scDivisionName Character. Search for supreme court chamber division name.
#' @param judgmentTypes Character, one from DECISION, RESOLUTION, SENTENCE, 
#'   REGULATION, REASONS. Search for judgments type.
#' @param keywords Character. Search for keywords of judgments.
#' @param judgmentDateFrom Any date/time object that could be properly converted
#'   with \code{as.POSIXct}, or a string in a format "\%Y-\%m-\%d".Represents 
#'   the earliest allowed judgment's date on the list.
#' @param judgmentDateTo Any date/time object that could be properly converted
#'   with \code{as.POSIXct}, or a string in a format "\%Y-\%m-\%d".Represents 
#'   the latest allowed judgment's date on the list.
#' @param sortingField Character. Represents the field by which you want to sort a list 
#'   of items, one from: DATABASE_ID, JUDGMENT_DATE, CASE_NUMBER, CC_COURT_TYPE,
#'   CC_COURT_ID, CC_COURT_CODE, CC_COURT_NAME, CC_COURT_DIVISION_ID, 
#'   CC_COURT_DIVISION_CODE, CC_COURT_DIVISION_NAME, SC_JUDGMENT_FORM, 
#'   SC_PERSONNEL_TYPE, SC_COURT_DIVISION_ID, SC_COURT_DIVISION_NAME, 
#'   SC_COURT_DIVISIONS_CHAMBER_ID, SC_COURT_DIVISIONS_CHAMBER_NAME.
#' @param sortingDirection Character, one from ASC, DESC. Indicates whether sort
#'   should be increasing or decreasing.
#'   
#' @section Query details:
#' All parameters are case insensitive.
#' A few parameters (\code{"all", "legalBase", "referencedRegulation"}) support 
#'   advanced query parsing:
#'   \itemize{
#'   \item when query contains multiple words then search for judgments that 
#'     contains all of that words, e. g. \code{"dobra osobiste"}
#'   \item operator "OR" - search for judgments that contains one of the words 
#'     in a query, e.g. \code{"dobra OR osobiste"},
#'   \item quote - search for judgments that contains whole phrase, 
#'     e.g. \code{"\\"dobra osobiste\\""},
#'   \item minus sign - search for judgments that doesn't contain this word, 
#'     e.g. \code{"dobra -osobiste"}.
#'     }
#'  Operators could be freely mixed, e.g \code{"\"dobra osobiste\" OR -kodeks"}.
#'  Phrase \code{"word1 OR -word2"} is equivalent to \code{"word1 -word2"}.
#'  A parameter has to remain a single string. Operators could be passed
#'  to other (character) arguments without an error but it will probably 
#'  return meaningless results.
#'  

