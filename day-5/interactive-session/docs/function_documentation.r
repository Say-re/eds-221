# Example of producing important documentation for functions
#' Calculating maximum sustainable harvest for a fishery
#'
#' @param K = Fishery carrying capacity
#' @param r = intrinsic growth rate
#'
#' @return H = maximum sustainable harvest
#' @export
#'
#' @examples
#' max_sustainable_yield(K = 392000, r = 0.33)
max_sustainable_harvest <- function (K, r) {
  harvest <- (K * r) / 4
  return(harvest)
}

# Sample execution
max_sustainable_harvest(42000, 0.45)
