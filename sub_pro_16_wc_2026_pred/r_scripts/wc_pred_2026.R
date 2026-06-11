# WC 2026 Prediction

# Load the required libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(janitor)

# Read in the data

wc_2026_pred <- read_excel(here::here("sub_pro_16_wc_2026_pred", 
                                       "datasets", "wc_2026_prediction.xlsx"))

# Clean the names

wc_2026_pred_clean <- wc_2026_pred |>
  clean_names()

# Create new normalized columns

wc_2026_pred_clean <- wc_2026_pred_clean |>
  mutate(inverse_ranking = 1/fifa_ranking) 

wc_2026_pred_clean_scale_norm <- wc_2026_pred_clean %>%
  mutate(across(where(is.numeric), ~as.vector(scale(.))))

# Add new column for Team Strength Index (tsi)

wc_2026_pred_clean_scale_norm_tsi_1 <- wc_2026_pred_clean_scale_norm |>
  mutate(tsi = (0.15*fifa_ranking_points) + (0.15*elo_rating) + (0.05*average_yearly_temp) +
               (0.35*team_value_euros) + (0.05*percent_foreigners) + (0.1*gdp_per_capita) + 
               (0.05*population) + (0.05*kalshi_odds_percent) + (0.05*polymarket_odds_percent))

wc_2026_pred_clean_scale_norm_tsi_2 <- wc_2026_pred_clean_scale_norm |>
  mutate(tsi = (0.25*fifa_ranking_points) + (0.25*elo_rating) + (0.05*average_yearly_temp) +
           (0.2*team_value_euros) + (0.05*percent_foreigners) + (0.05*gdp_per_capita) + 
           (0.05*population) + (0.05*kalshi_odds_percent) + (0.05*polymarket_odds_percent))

wc_2026_pred_clean_scale_norm_tsi_3 <- wc_2026_pred_clean_scale_norm |>
  mutate(tsi = (0.1*fifa_ranking_points) + (0.1*elo_rating) + (0.1*average_yearly_temp) +
           (0.2*team_value_euros) + (0.1*percent_foreigners) + (0.1*gdp_per_capita) + 
           (0.1*population) + (0.1*kalshi_odds_percent) + (0.1*polymarket_odds_percent))


tsi_1 <- wc_2026_pred_clean_scale_norm_tsi_1 |> 
  select(country, group, tsi)
tsi_2 <- wc_2026_pred_clean_scale_norm_tsi_2 |> 
  select(country, group, tsi)
tsi_3 <- wc_2026_pred_clean_scale_norm_tsi_3 |> 
  select(country, group, tsi)

write_xlsx(tsi_1, here::here("sub_pro_16_wc_2026_pred",
                             "datasets", "tsi_1_prediction.xlsx"))

write_xlsx(tsi_2, here::here("sub_pro_16_wc_2026_pred",
                             "datasets", "tsi_2_prediction.xlsx"))

write_xlsx(tsi_3, here::here("sub_pro_16_wc_2026_pred",
                             "datasets", "tsi_3_prediction.xlsx"))