# Create custom function for displaying current state of climate crisis

#' Retrieve current status of global effort to combat climate change
#'
#' @param temp - Average global temperature in degrees F
#' @param investment - Total global investment in climate change
#'  scale of ("minimal", "decent", "fully vested")
#' @param support - global population support to combat climate change ("low", "medium", "high")
#'
#' @return - Message on the status of combating climate change
#' @export
#'
#' @examples
#'   get_climate_status(88, "minimal", "low")
get_climate_status <- function(temp, investment, support) {
  if (temp > 88 && investment == "minimal" && support == "low") {
    return("We're doomed! It's too late now!!")
  } else {
    return("We still have a chance. Let's do this!")
  }
}
