# To further review the workshop this work is based on go to the following URL:
## https://allisonhorst.github.io/rice-data-viz/#1_Welcome

# General Packages
library(tidyverse)
library(here)
library(janitor)

# Packages for Plots
library(patchwork)
library(ggrepel)
library(gghighlight)
library(paletteer)
library(ggExtra)
library(ggbeeswarm)

# Spatial Data Packages
library(sf)

# For data set exploration
library(gapminder)


# Read in Lizard Data
lizards <- read_csv(here::here("comp-session", "data", "lizard_pitfall_data.csv"))

# 4.1 ggplot basics

# 3 ways to read in data:
# *** 1 *** #
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point()

# *** 2 *** #
ggplot(data = lizards) +
  geom_point(aes(x = total_length, y = weight))

# *** 3 *** #
ggplot() +
  geom_point(data = lizards, aes(x = total_length, y = weight))

# Some graphs only require 1 variable
ggplot(data = lizards, aes(x = weight)) +
  geom_histogram()

# Important to consider what type of data you are visualizing
ggplot(data = lizards, aes(y = spp, x = weight)) +
  geom_jitter()

# *** 4.2 Aesthetic Mapping *** #

# When updating based on a constant do not use aes()
ggplot(data = lizards, aes(x = weight)) +
  geom_histogram(color = "red",
                 fill = "green",
                 size = 3,
                 linetype = "dotted")

# Example of graph with fill and color aesthetic
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(
    color = "cyan4",
    fill = "yellow",
    shape = 22,
    size = 3,
    alpha = 0.4
  )

# Updating aesthetic based on variable, aes()
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = spp, size = total_length))

# Updating based on constant and variable through aes()
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = spp), alpha = 0.4)

# Getting things in order
lizard_counts <- lizards |>
  count(spp)

ggplot(data = lizard_counts, aes(y = fct_reorder(spp, n), x = n)) +
  geom_col()

# Synthesis Example
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = spp, shape = spp),
             fill = "black",
             size = 2) +
  theme_minimal() +
  labs(x = "Total length (mm)",
       y = "Weight (g)",
       color = "Lizard species") +
  facet_wrap(~spp, scales = "free")

# Synthesis Example 2
ggplot(data = lizards, aes(y = fct_infreq(spp))) +
  geom_bar(aes(fill = site)) +
  theme_bw() +
  labs(x = "Lizard counts",
       y = "Species (common name)") +
  facet_grid(sex ~ tail)
