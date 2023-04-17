library(tidyverse)
library(palmerpenguins)
library(lubridate)
library(kableExtra)

#view(penguins)

# Task 1: Data Wrangling Refresher

# Penguins Wrangled #1
penguins_wrangled <- penguins |>
  dplyr::filter(island == "Bisco" | island == "Dream") |> # Only show penguins from the islands Bisco and Dream
  dplyr::select(species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) |> # Remove year and sex columns from frame
  dplyr::mutate(body_mass_kg = body_mass_g * 1000) |> # Add new column that shows penguins mass in kg
  dplyr::rename("location" = island) # Rename island column to location

# view(penguins_wrangled)

# Penguins Wrangled #2
penguins_wrangled_2 <- penguins |>
  filter(species == "Adelie" & !is.na(flipper_length_mm)) |>
  group_by(sex) |>
  summarise(flipper_length_mean = mean(flipper_length_mm, na.rm = TRUE), flipper_length_sd = sd(flipper_length_mm, na.rm = TRUE), total_count = n())

# view(penguins_wrangled_2)

# Part 2: Wrangling Continued - Joins of different flavors
animals <- tibble::tribble(
  ~location,   ~species,  ~maturity,
   "lagoon",   "bobcat",    "adult",
    "bluff",   "coyote", "juvenile",
    "creek",      "fox",    "adult",
     "oaks", "squirrel", "juvenile",
    "bluff",   "bobcat",    "adult"
  )

sites <- tibble::tribble(
           ~location,      ~full_site_name, ~jurisdiction,
             "beach",       "Goleta Beach",     "SB City",
            "lagoon",        "UCSB Lagoon",        "UCSB",
             "bluff",       "Ellwood Mesa",     "SB City",
              "oaks", "Fremont Campground",        "USFS"
           )

# Full join sites and animals together
full_join_example <- full_join(animals, sites)
head(full_join_example)

# Left join animals and sites together
left_join_example <- left_join(animals, sites)
head(left_join_example)

# Right join animals and sites together
right_join_example <- right_join(animals, sites)
head(right_join_example)

# Inner join animals and sites together
inner_join_example <- inner_join(animals, sites)
head(inner_join_example)

# FILTERING JOINS

# Semi join - Only keep entries from x that are in y. Does not combine the 2 data frames
semi_join_example <- semi_join(animals, sites)
head(semi_join_example)

anti_join_example <- anti_join(animals, sites)
head(anti_join_example)


# *** PART 3: Dates and Times with {lubridate} *** #

# Example of using lubridate to format dates in a single format
## Output of lubridate is in ISO 8601 format *** YYYY-MM-DD ***
some_date <- "04-24-1994"
lubridate::mdy(some_date)

# Fail Case 1: Format that doesn't match function being used
lubridate::mdy("1934-08-30")

# Converting date with time to constant format UTC

time <- "2019-12-31 19:32"
time <- ymd_hm(time)
time

# Determine class of time object
class(time)

# Convert to PDT
time <- with_tz(time, "America/Los_Angeles")

# Pull out the different elements of the date
new_time <- lubridate::ymd_hms(time)

# Week number in year
week(new_time)

# Day
day(new_time)

# Hour
hour(new_time)

# Minute
minute(new_time)

# Second
second(new_time)

# Get System Time
Sys.time()

# Example of using lubridate to extract individual elements of dates into new columns

urchin_counts <- tribble(
  ~date, ~species, ~size_mm,
  "10/5/2020", "purple", 55,
  "10/8/2020", "red", 48,
  "11/17/2020", "red", 67
)

urchin_counts_ymd <- urchin_counts |>
  mutate(date = lubridate::mdy(date)) |>
  mutate(year = year(date),
         month = month(date),
         day = day(date))

urchin_counts_ymd

# Find durations of times
day_1 <- lubridate::ymd("2020-01-06")
day_2 <- lubridate::ymd("2020-05-18")
day_3 <- lubridate::ymd("2020-05-19")

# Create a time interval
time_interval <- interval(day_1, day_2)

# Check the length in weeks
time_length(time_interval, "week")

# Check the length in years
time_length(time_interval, "year")

# *** PART 4: WRANGLING STRINGS WITH {stringr} *** #

# str_detect() returns TRUE or FALSE based on if pattern is detected
sample_string <- "This is a sample string to test detection."

# Expect TRUE
str_detect(sample_string, "sample")

# Using str_detect() to filter columns based on a value within a string

skywalkers <- starwars |>
  filter(str_detect(name, "Skywalker"))

skywalkers

# Using str_replace to replace matching string patterns with a new value

skywalker_new_name <- starwars |>
  mutate(name = str_replace(name, pattern = "Sky", replacement = "Fire")) |>
  filter(str_detect(name, "Fire"))

skywalker_new_name










