## Forbes Top 10 Highest-Paid Athletes

library(readr)
library(readxl)
library(janitor)
library(tidyverse)
library(forcats)

dataset <- read_excel('datasets/top_10_paid_2023.xlsx')

dataset_clean <- dataset %>%
  clean_names()

dataset_clean <- dataset_clean %>%
  mutate(ratio_on_off = on_field_earnings/off_field_earnings)

dataset_clean_wide <- dataset_clean %>%
  select(name, on_field_earnings, off_field_earnings)

dataset_clean_long <- dataset_clean_wide %>%
  pivot_longer(!name, names_to = "earning_type", values_to = "amount") %>%
  mutate(earning_type = as.factor(earning_type))

str(dataset_clean_long)

ggplot(dataset_clean_long, aes(name, amount, fill = reorder(earning_type, amount))) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip()

  