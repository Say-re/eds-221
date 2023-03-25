#' Function to calculate estimated storm runoff volume using the Simple Method
#'
#' @param imp_frac - impervious fraction used to calculate the runoff coefficient
#' @param wtrshed_area - Total watershed area
#'
#' @return - Estimated storm runoff volume
#' @export
#'
#' @examples
#'   get_predicted_runoff(imp_frac = 0.28, wtrshed_area = 33000)
get_predicted_runoff <-function(imp_frac, wtrshed_area) {
  runoff_coef <- 0.05 + 0.09 * imp_frac
  pred_runoff <- 3630 * 1 * runoff_coef * wtrshed_area
  return(pred_runoff)
}
