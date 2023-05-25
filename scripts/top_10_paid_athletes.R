## Forbes Top 10 Highest-Paid Athletes

library(readr)
library(readxl)
library(janitor)
library(tidyverse)

dataset <- read_excel('datasets/top_10_paid_2023.xlsx')

dataset_clean <- dataset %>%
  clean_names()

dataset_clean <- dataset_clean %>%
  mutate(ratio_on_off = on_field_earnings/off_field_earnings)
