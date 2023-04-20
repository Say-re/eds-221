# Read in required libraries
library(tidyverse)
library(janitor)
library(ggridges)
library(here)

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
    x = "Launch\n\n\nYEAR"
  ) +
  theme(
    axis.text.x = element_text(angle = 35),
    axis.text.y = element_text(angle = -35),
    panel.background = element_rect(fill = "cyan4")
  )
