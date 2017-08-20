
#' Reference Values
#'
#' @param assessmentKey
#'
#' @return to do
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
#'
reference_vals <- function(assessmentKey){
  ref_tab <- icesSAG::getFishStockReferencePoints(assessmentKey = assessmentKey)[[1]] %>%
    select(AssessmentYear, Fpa, Bpa, FLim, Blim, FMSY,
           MSYBtrigger)
  return(ref_tab)
}

#' Summary Values
#'
#' @param assessmentKey
#'
#' @return todo
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
summary_vals <- function(assessmentKey){
  sum_tab <- icesSAG::getSummaryTable(assessmentKey = assessmentKey)[[1]] %>%
    select(Year, low_SSB, high_SSB, SSB, catches, landings, discards, low_F, F, high_F,
           fishstock, AssessmentYear)
  return(sum_tab)
}
