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
  select(name, on_field_earnings, off_field_earnings, total_earnings) %>%
  mutate(rank = as.integer(rank(total_earnings))) 

dataset_clean_long <- dataset_clean_wide %>%
  pivot_longer(!c(name, total_earnings, rank), names_to = "earning_type", values_to = "amount") %>%
  mutate(earning_type = as.factor(earning_type))

str(dataset_clean_long)

ggplot(dataset_clean_long, aes(reorder(name, rank), amount, fill = earning_type)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() + 
  scale_fill_manual(values = c("goldenrod2", "darkgreen")) +
  theme_minimal()

# Source forbes... / 
  