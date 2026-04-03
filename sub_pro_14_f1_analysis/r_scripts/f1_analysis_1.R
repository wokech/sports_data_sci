# Formula 1 analysis in R
# Laureano Gallardo / R Programming Books
# https://rprogrammingbooks.com/formula-1-analysis-r-f1datar/
# https://github.com/SCasanova/f1dataR

# A) Load the required libraries and datasets

# install.packages(c(
#   "f1dataR",
#   "tidyverse",
#   "lubridate",
#   "janitor",
#   "scales",
#   "slider",
#   "broom",
#   "tidymodels",
#   "gt",
#   "patchwork"
# ))

library(f1dataR)
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(slider)
library(broom)
library(tidymodels)
library(gt)
library(patchwork)


####
# If you want to work with official session-level timing data, 
# it is also a good idea to configure FastF1 support and define 
# a local cache.

# setup_fastf1() sets up using reticulate, uv, and Python

####

# setup_fastf1()
# 
# options(f1dataR.cache = "f1_cache")
# dir.create("sub_pro_14_f1_analysis/f1_cache", showWarnings = FALSE)

# B) Start with race results

# Before diving into laps and strategy, start with historical race results. 
# They provide the backbone for season summaries, driver comparisons, 
# constructor trends, and predictive features.

results_2024 <- load_results(season = 2024)

results_2024 %>%
  clean_names() %>%
  select(driver_id, constructor_id, grid, position, points, status) %>%
  glimpse()

# Build a season summary table

season_table <- results_2024 %>%
  clean_names() %>%
  group_by(driver_id, constructor_id) %>%
  summarise(
    races = n(),
    wins = sum(position == 1, na.rm = TRUE),
    podiums = sum(position <= 3, na.rm = TRUE),
    avg_finish = mean(position, na.rm = TRUE),
    avg_grid = mean(grid, na.rm = TRUE),
    points = sum(points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(points), avg_finish)

season_table
View(season_table)