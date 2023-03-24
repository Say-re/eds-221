library(testthat)
# import function from the sourcing_functions script file
source(here::here("interactive-session", "docs", "sourcing_functions.r"))
source(here::here("interactive-session", "docs", "cork_oak_growth.r"))

mean_range <- function(df) {
  # Return column means as a vector
  col_means <- apply(X = df, MARGIN = 2, FUN = mean, na.rm = TRUE)

  # Get max & min value in vector
  col_mean_max <- max(col_means)
  col_mean_min <- min(col_means)

  # Return max & min values
  return(c(col_mean_min, col_mean_max))
}

name_cart("pear", "lemur")
height_t2(100, 2, 10)

# ------ UNIT TESTS START --------
expect_length(mean_range(mtcars), 2)
expect_true(mean_range(mtcars)[1] < mean_range(mtcars)[2])

