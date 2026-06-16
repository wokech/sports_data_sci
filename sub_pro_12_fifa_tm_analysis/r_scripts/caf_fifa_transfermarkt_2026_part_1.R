# CAF World Rankings and Market Value
# January 2026 Ranking and March 2026 Values

# Data: https://www.transfermarkt.com/wettbewerbe/fifa
# Source: https://www.datakwery.com/post/2020-07-11-scientific-notation-in-r/

# Load the required libraries
library(tidyverse)
library(tidyr)
library(janitor)
library(tidyr)
library(readxl)
library(scales)
library(devtools)
#devtools::install_github('bbc/bbplot')
#library(bbplot)
#install.packages("wordcloud")
library(wordcloud)
# install.packages("ggwordcloud")
library(ggwordcloud)
# install.packages("treemapify")
library(treemapify)
# install.packages("ggrepel")
library(ggrepel)
library(patchwork)
library(readxl)
library(stringr)
library(writexl)

# Load the required data

caf_fifa_ranking <- read_excel("sub_pro_12_fifa_tm_analysis/datasets/caf_fifa_ranking_jan_2026.xlsx")
caf_fifa_tm_value <- read_excel("sub_pro_12_fifa_tm_analysis/datasets/caf_squad_value_mar_2026.xlsx")

# Group data together

caf_fifa_ranking_tm_value <- caf_fifa_ranking %>%
  inner_join(caf_fifa_tm_value, by = c("Ranking", "Team", "Confederation", "Region"))

caf_fifa_tm_clean <- caf_fifa_ranking_tm_value %>%
  clean_names()

caf_fifa_tm_clean$market_value <- gsub("€", "", caf_fifa_tm_clean$market_value)
caf_fifa_tm_clean$o_market_value_of_players <- gsub("€", "", caf_fifa_tm_clean$o_market_value_of_players)
caf_fifa_tm_clean$value_of_player <- gsub("€", "", caf_fifa_tm_clean$value_of_player)


# Create a new column for the integers
caf_fifa_tm_clean$team_market_value <- str_extract(caf_fifa_tm_clean$market_value, "\\d+\\.?\\d*")
caf_fifa_tm_clean$avg_player_market_value <- str_extract(caf_fifa_tm_clean$o_market_value_of_players, "\\d+\\.?\\d*")
caf_fifa_tm_clean$top_player_value <- str_extract(caf_fifa_tm_clean$value_of_player, "\\d+\\.?\\d*")

# Create a new column for the letters
caf_fifa_tm_clean$units_team_market_value <- str_extract(caf_fifa_tm_clean$market_value, "[A-Za-z]+")
caf_fifa_tm_clean$units_avg_player_market_value <- str_extract(caf_fifa_tm_clean$o_market_value_of_players, "[A-Za-z]+")
caf_fifa_tm_clean$units_top_player_value <- str_extract(caf_fifa_tm_clean$value_of_player, "[A-Za-z]+")

# Add the multiplier column
caf_fifa_tm_clean$multiplier_team_market_value <- ifelse(caf_fifa_tm_clean$units_team_market_value == "m", 1000000, 
                                                         ifelse(caf_fifa_tm_clean$units_team_market_value == "k", 1000, 
                                                                ifelse(caf_fifa_tm_clean$units_team_market_value == "bn", 1000000000, NA)))

caf_fifa_tm_clean$multiplier_avg_player_market_value <- ifelse(caf_fifa_tm_clean$units_avg_player_market_value == "m", 1000000, 
                                                               ifelse(caf_fifa_tm_clean$units_avg_player_market_value == "k", 1000, 
                                                                      ifelse(caf_fifa_tm_clean$units_avg_player_market_value == "bn", 1000000000, NA)))

caf_fifa_tm_clean$multiplier_top_player_value <- ifelse(caf_fifa_tm_clean$units_top_player_value == "m", 1000000, 
                                                        ifelse(caf_fifa_tm_clean$units_top_player_value == "k", 1000, 
                                                               ifelse(caf_fifa_tm_clean$units_top_player_value == "bn", 1000000000, NA)))

# Create new column and convert to integers
caf_fifa_tm_clean <- caf_fifa_tm_clean %>%
  mutate(team_market_value_euros_numeric = as.numeric(team_market_value) * as.numeric(multiplier_team_market_value)) %>%
  mutate(avg_player_market_value_euros_numeric = as.numeric(avg_player_market_value) * as.numeric(multiplier_avg_player_market_value)) %>%
  mutate(top_player_value_euros_numeric = as.numeric(top_player_value) * as.numeric(multiplier_top_player_value)) %>%
  mutate(across(where(is.character), str_trim))

str(caf_fifa_tm_clean)

# Select the relevant columns for analysis and visualization

caf_fifa_tm_clean_analysis <- caf_fifa_tm_clean %>%
  mutate(ranking = as.numeric(ranking)) %>%
  mutate(squad_size = as.numeric(squad_size)) %>%
  mutate(o_age = as.numeric(o_age)) %>%
  select(ranking, team, confederation, region, points, squad_size, o_age,
         most_valuable_player, team_market_value_euros_numeric,
         avg_player_market_value_euros_numeric, 
         top_player_value_euros_numeric, players_playing_abroad_percent)

write_xlsx(caf_fifa_tm_clean_analysis,
                "sub_pro_12_fifa_tm_analysis/datasets/processed_2026/caf_fifa_tm_clean_analysis.xlsx")
