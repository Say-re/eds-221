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

# *** Section 5 Advanced ggplot2 Customization *** #

# Updating Breaks and labels
## For Dates: scale_*_date()
## For continuous variables: scale_*_continuous()
## For discrete variables: scale_*_discrete()

# Example
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point() +
  scale_x_continuous(breaks = c(0, 250, 500),
                     limits = c(0, 500)) +
  scale_y_continuous(breaks = seq(from = 0, to = 70, by = 10),
                     limits = c(0, 70)) +
  theme_light()

# Updating x to a log scale
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point() +
  scale_x_log10()


# Determine lizard counts by date
lizard_counts <- lizards |>
  count(date)

# Plot by date and use scale package to determine scales
ggplot(data = lizard_counts, aes(x = date, y = n)) +
  geom_line() +
  scale_x_date(breaks = scales::breaks_width("3 years"),
               labels = scales::label_date("'%y")) +
  scale_y_log10(labels = scales::label_scientific())

# Prettify x breaks
ggplot(data = lizard_counts, aes(x = date, y = n)) +
  geom_line() +
  scale_x_date(breaks = scales::breaks_pretty())

# Custom aesthetics with scale_ functions

# Sample gradient with 2 colors
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = weight)) +
  scale_color_gradient(low = "orange", high = "navy")

# Sample gradient with n colors
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = weight)) +
  scale_color_gradientn(colors = c("orange", "red", "pink", "purple", "navy"))

# Sample Binned colors
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = weight)) +
  scale_color_steps(low = "red", high = "aquamarine")

# Creating divergent binned scales
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = total_length)) +
  scale_color_steps2(low = "green",
                     mid = "navy",
                     high = "maroon",
                     midpoint = 150,
                     breaks = c(50, 75, 150, 180, 220, 280))

# Creating custom divergent binned scales
ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = weight)) +
  scale_color_stepsn(colors = c("cyan4", "yellow", "purple"),
                     breaks = seq(from = 10, to = 60, by = 10))

# Creating custom discrete color scheme
ggplot(data = lizards, aes(x = spp, y = total_length)) +
  geom_boxplot(aes(fill = spp), color = "black", show.legend = FALSE) +
  theme_minimal() +
  coord_flip()

# These are unordered so let's put them in order
lizards_mean <- lizards |>
  mutate(spp = fct_reorder(spp, total_length, .fun = median))

# Make a new graph and utilize paleteer for plotting color scheme
ggplot(data = lizards_mean, aes(y = spp, x = total_length)) +
  geom_boxplot(aes(fill = spp), show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 500)) +
  scale_fill_paletteer_d(palette = "ggsci::default_gsea") +
  labs(y = "Lizard species",
       x = "Total length (mm)") +
  theme_minimal()

# Find total counts by lizard species
lizards_n <- lizards |>
  count(spp) |>
  mutate(spp = fct_reorder(spp, n)) # Reorder by count

ggplot(data = lizards_n, aes(y = spp, x = n)) +
  geom_col(aes(fill = spp), show.legend = FALSE) +
  scale_fill_paletteer_d("tidyquant::tq_dark") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 50)) +
  theme_bw() +
  labs(x = "Species",
       y = "Total count") +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )

# Gridlines & Panel colors
p <- ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point()

sample_1 <- p +
  theme(
    panel.grid = element_blank() # Remove all grid lines (major & minor)
  )

sample_2 <- p +
  theme(
    panel.grid.minor = element_blank(), # Remove all minor grid lines
    panel.grid.major = element_line(color = "red") # Render major grid lines
  )

# More expansive theme customizations
sample_3 <- p +
  theme(
    panel.background = element_rect(color = "navy", size = 2, fill = "orangered"),
    panel.grid.major.y = element_line(color = "cyan2"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(color = "yellow"),
    axis.text.y = element_text(color = "yellow"),
    axis.title.x = element_text(color = "navy"),
    axis.title.y = element_text(color = "navy"),
    text = element_text(size = 12, family = "serif"),
    plot.background = element_rect(fill = "aquamarine")
  )

sample_1
sample_2


# Direct annotation and thresholds
sample_4 <- p +
  annotate("text", x = 100, y = 50, label = "Interesting", color = "maroon") +
  geom_hline(yintercept = 40, linetype = "dotted", color = "navy") +
  geom_vline(xintercept = 20, linetype = "solid", color = "pink") +
  theme_minimal()

sample_4

# Thresholds can also be based on a variable
mean(lizards$weight)
sample_5 <- p +
  geom_hline(yintercept = mean(lizards$weight, na.rm = TRUE), linetype = "dashed", color = "purple") +
  annotate("text", x = 350, y = mean(lizards$weight, na.rm = TRUE) + 2, label = "Mean weight", color = "red")

sample_5

# Apply above concepts to a new data set from Mono Basin Clearinghouse

# Read in data:
mono <- read_csv(here::here("comp-session", "data", "lake_levels.csv"))

ggplot(data = mono, aes(x = year, y = lake)) +
  geom_rect(aes(
    xmin = 1941,
    xmax = 1983,
    ymin = 6350,
    ymax = 6440
  ),
  fill = "gray90") +
  geom_line() +
  labs(
    x = "\nYear",
    y = "Lake surface level\n(feet avone sea level)\n",
    title = "Mono Lake Levels (1850 - 2017)\n",
    caption = "Data: Mono Basin Clearinghouse"
  ) +
  scale_x_continuous(limits = c(1850, 2020),
                     expand = c(0, 0),
                     breaks = seq(1850, 2010, by = 20)) +
  scale_y_continuous(limits = c(6350, 6440),
                     breaks = c(6370, 6400, 6430),
                     expand = c(0, 0),
                     labels = scales::label_comma()) +
  annotate("text", x = 1962, y = 6425,
           label = "unrestricted diversions\n(1941 - 1983)",
           size = 3) +
  theme_light() +
  theme(
    plot.title.position = "plot",
    axis.text.y = element_text(face = "italic")
  ) +
  geom_hline(yintercept = 6360, linetype = "dashed") +
  annotate("text",
           x = 1910,
           y = 6367,
           label = "Decreased shrimp abundance expected\n(6,360 feet above sea level)",
           size = 3)

# Quick notes on making better legends
two_lizards <- lizards |>
  filter(spp %in% c("SCUN", "PHCO"))

ggplot(data = two_lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = spp,
                 shape = spp),
                 size = 2) +
  scale_color_manual(name = "Lizard species:",
                     values = c("purple", "yellow"),
                     labels = c("SCUN", "PHCO")) +
  scale_shape_discrete(name = "Lizard species:",
                       labels = c("SCUN", "PHCO")) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_blank())

# Repulsive labels using ggrepel
wcc_lizards <- lizards |>
  filter(spp == "CNTI", site == "SAND")

ggplot(data = wcc_lizards, aes(x = total_length, y = weight)) +
  geom_point() +
  geom_text_repel(aes(label = toe_num), size = 3, max.overlaps = 20, show.legend = FALSE)

gapminder |>
  filter(year == 2002, continent == "Europe") |>
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3)

# Highlighting specific data points with gghighlight
highlight_ex <- p +
  gghighlight(toe_num == 250, label_key = toe_num)

highlight_ex

highlight_ex_2 <- p +
  aes(color = site) + # This would normally be placed in geom_line
  gghighlight(site %in% c("CALI", "GRAV"))
highlight_ex_2

# Last highlighting example
q <- ggplot(data = lizards, aes(x = total_length, y = weight, group = spp)) +
  geom_line(aes(color = spp)) +
  gghighlight(max(weight, na.rm = TRUE) > 30)
q

# *** PART 6: Using patchwork to combine multiple figures into a single graphic *** #
(p | q) &
  theme_minimal()

# Add a third figure to the graphic
z <- ggplot(data = lizards, aes(y = site, x = weight)) +
  geom_boxplot(aes(fill = site), show.legend = FALSE)

# Structure them together under PEMDAS structure
((p | q) / z) &
  theme_light()

# *** PART 7: New Graph Types *** #

# Marginal Plots
some_species <- lizards |>
  filter(spp == "SCUN") |>
  drop_na(total_length, weight)

# An issue with rug plots
ggplot(data = some_species, aes(x = total_length, y = weight)) +
  geom_point() +
  geom_rug()

p <- ggplot(data = some_species, aes(x = total_length, y = weight)) +
  geom_point(aes(color = sex), size = 2) +
  theme_minimal() +
  scale_color_manual(values = c("cyan4", "goldenrod", "navy"),
                     name = "Sex:",
                     labels = c("female", "juvenile", "male")) +
  theme(legend.position = "bottom") +
  labs(x = "Total length (mm)",
       y = "Weight (grams)")

# Example 1: A histogram
ggMarginal(p, type = "histogram", fill = "gray60", color = NA)

# Example 2: A boxplot, grouped by sex (as in the plot)
ggMarginal(p, type = "boxplot", groupColour = TRUE)

# Beeswarm plots using ggbeeswarm
ggplot(data = some_species, aes(x = sex, y = weight)) +
  geom_beeswarm(size = 1) +
  geom_boxplot(fill = NA) +
  scale_x_discrete(labels = c("female", "juvenile", "male")) +
  theme_minimal()

# Heatmaps with geom_tile
# Get counts
lizard_counts <- lizards |>
  count(year = lubridate::year(date), spp) |>
  drop_na()

# Make heatmap of counts
ggplot(data = lizard_counts, aes( x = year, y = spp)) +
  geom_tile(aes(fill = n), show.legend = FALSE) +
  geom_text(aes(label = n), color = "white", size = 3) +
  scale_fill_gradientn(colors = c("navy", "red", "orangered")) +
  theme_minimal() +
  labs(x = "Year",
       y = "Lizard species")

# Make a map!
# Read in Jornada Basin Vegetation data:
jordana_veg <- read_sf(here("comp-session", "data", "spatial_vegetation", "doc.kml")) |>
  select(Name) |>
  clean_names()

# Initial exploratory plot
plot(jordana_veg)

# Paletteer palettes are available to view:
# View(palettes_c_names)
# View(palettes_d_names)

ggplot() +
  geom_sf(data = jordana_veg,
          aes(fill = name),
          color = NA) +
  theme_minimal() +
  scale_fill_paletteer_d(palette = "ggthemes::manyeys") +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Dominant vegetation",
    title = "Jornada Basin vegetation",
    caption = "Data source: Jornada Basin LTER"
  ) +
  theme(legend.position = "right",
         plot.title.position = "plot",
         plot.caption.position = "plot",
         plot.caption = element_text(face = "italic", color = "gray30"),
         axis.text = element_text(size = 5))

# Default with ggsave is to save the last plot rendered
ggsave("jornada_basin_map.png", width = 6, height = 4)
