# Import required libraries
library(tidyverse)
library(here)
library(janitor)

# Read in data files
meteor_data <- read_csv(here::here("activity", "data", "meteorological_madison.csv"), na = c("-999"))
lake_data <- read_csv(here::here("activity", "data", "ntl_lter_madison.csv"), na = c("-999"))

# view(meteor_data)
# view(lake_data)

lake_sum <- lake_data |>
  group_by(lakeid) |>
  summarize(mean_duration = mean(ice_duration, na.rm = TRUE), ice_duration = ice_duration)

# view(lake_sum)

ggplot(data = lake_sum, aes(x = lakeid, y = ice_duration, color = lakeid, fill = lakeid)) +
  geom_boxplot(width = 0.75) +
  geom_jitter(width = 0.3) +
  labs(
    x = "\nLake",
    y = "\nIce duration (days)\n",
    title = "Ice duration within Madison Area in Wisconsin at 3 lake sites",
    subtitle = "(Lake Mendota (ME), Lake Monona (MO), and Lake Wingra (WI))\n",
    caption = "\nData source: University of Wisconsin edi portal repository\n"
  ) +
  scale_fill_manual(values = c("#9DBA463a", "#469DBA3a", "#BA469D3a")) +
  scale_color_manual(values = c("#9DBA46", "#469DBA", "#BA469D")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"),
  )

lake_sum_year <- lake_data |>
  group_by(year4) |>
  summarise(mean_duration = mean(ice_duration, na.rm = TRUE))

# view(lake_sum_year)

ggplot(data = lake_sum_year, aes(x = year4, y = mean_duration, color = mean_duration)) +
  geom_point() +
  scale_y_continuous(breaks = c(50, 80, 100, 120, 140, 160)) +
  scale_color_gradient(low = "#9DBA46", high = "#BA469D") +
  geom_smooth(method = "lm", color = "#469DBA") +
  labs(
    x = "Year",
    y = "Ice duration (days)",
    title = "Average ice duration per year at Madison Area",
    caption = "\nBetween 1852 and 2018 the Madison Lake Area saw a downward trend in the total number of days that the lakes were covered in ice."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 10)
  )

winter_mean_temps <- meteor_data |>
  filter(month %in% c(12, 1, 2)) |> # Only include data from December, January, and February
  group_by(year4) |> # Group by year
  summarise(mean_air_temp = mean(ave_air_temp_adjusted)) # Determine average air temperature each year

# Confirm wrangled data
# view(winter_mean_temps)


ggplot(data = winter_mean_temps, aes(x = year4, y = mean_air_temp, color = mean_air_temp)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#469DBA") +
  scale_color_gradient(low = "#9DBA46", high = "#BA469D") +
  labs(
    caption = "\nBetween 1852 and 2018 the Madison Lake Area saw an upward trend in the average air temperature.",
    title = "Average air temperature with the Madison Lake Area from 1859 to 2018",
    x = "Year",
    y = "Mean air temperature (C)"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
  )

mean_temp_ice_join <- winter_mean_temps |>
  inner_join(lake_sum_year, by = "year4")

# Verify data join
# view(mean_temp_ice_join)

ggplot(data = mean_temp_ice_join, aes(x = mean_air_temp, y = mean_duration)) +
  geom_point(color = "#9DBA46") +
  geom_smooth(method = "lm", color = "#BA469D") +
  labs(
    title = "",
    x = "Mean Air Temperature (C)",
    y = "Mean Ice Duration (days)",
  ) +
    theme_minimal()
