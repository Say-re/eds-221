# *** INTERACTIVE SESSION DAY 9 *** #

# Import required packages
library(tidyverse)
library(palmerpenguins)
library(lubridate)
library(kableExtra)
library(janitor)

# PART 2: A Few New Wrangling Tools

# Janitor get_dupes() checks for duplicates
dupes <- get_dupes(starwars) # Across all variables
dupes

# Check for duplicate values in homeworld
dupes_2 <- starwars |>
  get_dupes(homeworld)
dupes_2

# Check for duplicates in the homeworld and species column
dupes_3 <- starwars |>
  get_dupes(homeworld, species)
dupes_3

# dplyr::across_columns() performs operations across columns

# Simple conversion of character columns to lowercase
starwars |>
  mutate(across(where(is.character), tolower))

# Using with group_by() & summarize()
starwars |>
  group_by(homeworld) |>
  summarize(across(where(is.numeric), mean, na.rm = TRUE), count = n())

# Tables with {kable} & {kableExtra}
penguins |>
  group_by(species, island) |>
  summarize(mean_mass = mean(body_mass_g, na.rm = TRUE)) |>
  kable(col.names = c("Species", "Island", "Body mass (g)")) |>
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
