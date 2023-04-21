# Read in required libraries
library(tidyverse)
library(janitor)
library(ggridges)
library(here)
library(paletteer)
library(ggrepel)
library(patchwork)

# Read in space launches data file
space_launches <- read_csv(here::here("activity", "data", "space_launches.csv"))

# Review file
# view(space_launches)

# Variables to graph *** launch_year, type, agency ***

ggplot(data = space_launches, aes(x = launch_year, y = agency)) +
  geom_density_ridges(
    stat = "binline",
    bins = 20,
    scale = 0.95,
    draw_baseline = FALSE,
    show.legend = FALSE,
    aes(color = type, fill = type)) +
  scale_x_continuous(n.breaks = 20) +
  labs(
    x = "Launch\n\n\nYEAR",
    y = "Agency",
    title = "SPACE LAUNCHES BY agency"
  ) +
  theme(
    axis.text.x = element_text(angle = 75),
    axis.text.y = element_text(angle = -10),
    axis.title.y = element_text(size = 100, color = "goldenrod"),
    axis.title.x = element_text(color = "aquamarine"),
    panel.background = element_rect(fill = "cyan4"),
    plot.title = element_text(family = "mono", hjust = 0.75, color = "yellow", size = 10),
    plot.background = element_rect(color = "orangered", fill = "blue", size = 5)
  )

# *** TASK 2: WRANGLING SF GREENHOUSE GAS DATA *** #

# Read in SF GHG data
sf_ghg_data <- read_csv(here::here("activity", "data", "sf_ghg.csv"))

# Review read-in data
# view(sf_ghg_data)

sf_subset_sector_general <- sf_ghg_data |>
  group_by(Sector_General) |>
  summarise(total_emissions = sum(Emissions_mtCO2e))

# Confirm summarized data
# view(sf_subset_sector_general)

sf_subset_yr_commodity <- sf_ghg_data |>
  group_by(Commodity_Type, Calendar_Year) |>
  summarise(total_emissions = sum(Emissions_mtCO2e))

# Confirm summarized data
# view(sf_subset_yr_commodity)

# Create subset data from 2005, & section_detail2 contains "PG&E"
# Only have the following columns --- (Calendar_Year, Sector_Detail2, Emissions_mtCO2e)
subset_filtered <- sf_ghg_data |>
  filter(Calendar_Year == 2005, str_detect(Sector_Detail2, "PG&E")) |>
  select(Calendar_Year, Sector_Detail2, Emissions_mtCO2e)

# Confirm subset data
# view(subset_filtered)

# TASK 3: DO YOUR DATA VIZ BEST
# Year 1990 & 2018, grouped by sector general, total emissions
# Yearly, summarize total emissions

tot_emm_1990_2018 <- sf_ghg_data |>
  group_by(Sector_General, Calendar_Year) |>
  summarize(total_emissions = sum(Emissions_mtCO2e)) |>
  filter(Calendar_Year == 1990 | Calendar_Year == 2018)

# view(tot_emm_1990_2018)

total_emm_comp_1990_2018 <- ggplot(data = tot_emm_1990_2018, aes(x = Sector_General, y = total_emissions, group = factor(Calendar_Year))) +
  geom_col(aes(fill = factor(Calendar_Year)), position = "dodge") +
  scale_y_continuous(labels = scales::label_comma(), n.breaks = 6, limits = c(0, 5000000)) +
  labs(
    x = "\nSector",
    y = "\nTotal emissions (mtCO2)\n",
    title = "San Francisco Total GHG emissions by sector\n(1990 & 2018)\n",
    caption = "\nData source: San Francisco government data portal\n",
    fill = "Year"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("1990" = "#F08C78", "2018" = "#8070D1")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = 8, face = "italic"),
    panel.grid.major = element_line(color = "grey70", size = 0.25),
    panel.grid.minor = element_blank(),
    legend.position = c(0.45, 0.84),
    legend.background = element_rect(fill = "white", color = "grey70", size = 0.25)
  )

total_emm_comp_1990_2018

label_data <- sf_ghg_data |>
  group_by(Sector_General, Calendar_Year) |>
  summarize(total_emm = sum(Emissions_mtCO2e)) |>
  filter(Calendar_Year == 2017)

total_emm <- sf_ghg_data |>
  group_by(Sector_General, Calendar_Year) |>
  summarize(total_emm = sum(Emissions_mtCO2e)) |>
  ggplot(aes(x = Calendar_Year, y = total_emm, group = factor(Sector_General))) +
  geom_line(aes(color = factor(Sector_General)), show.legend = FALSE, size = 0.7) +
  scale_y_continuous(labels = scales::label_comma(), n.breaks = 6) +
  scale_color_manual(values = c("grey90", "#F08C78", "grey80", "grey70", "#8070D1", "grey50")) +
  labs(
    color = "Sector",
    title = "San Francisco Total GHG emissions by sector\n(1990 - 2018)\n",
    y = "Total emissions (mtCO2)\n",
    x = "Year",
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.86, 0.85),
    legend.background = element_rect(fill = "white", color = "grey70"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
  ) +
  geom_label_repel(aes(x = Calendar_Year, y = total_emm, label = factor(Sector_General)),
                   data = label_data, size = 3, nudge_x = 5)
total_emm

(total_emm_comp_1990_2018 | total_emm)

ggsave("sf_ghg_summary_1990-2018.png", width = 15, height = 3.5)



