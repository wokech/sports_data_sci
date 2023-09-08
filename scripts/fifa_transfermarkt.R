# FIFA World Rankings and Market Value
# April 6, 2023 Ranking

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
library(bbplot)
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

# Load the required data

fifa_tm <- read_excel("datasets/FIFA_Ranking_Apr_6_2023.xlsx")

fifa_tm_clean <- fifa_tm %>%
  clean_names()

fifa_tm_clean$total_value_euros <- gsub("€", "", fifa_tm_clean$total_value_euros)

library(stringr)

# Create a new column for the integers
fifa_tm_clean$value <- str_extract(fifa_tm_clean$total_value_euros, "\\d+\\.?\\d*")

# Create a new column for the letters
fifa_tm_clean$units <- str_extract(fifa_tm_clean$total_value_euros, "[A-Za-z]+")

# Add the multiplier column
fifa_tm_clean$multiplier <- ifelse(fifa_tm_clean$units == "m", 1000000, 
                                   ifelse(fifa_tm_clean$units == "k", 1000, 
                                          ifelse(fifa_tm_clean$units == "bn", 1000000000, NA)))

fifa_tm_clean$value <- as.numeric(fifa_tm_clean$value)
fifa_tm_clean$multiplier <- as.numeric(fifa_tm_clean$multiplier)
fifa_tm_clean$number <- as.numeric(str_extract(fifa_tm_clean$number, "\\d+"))

# Create new column and convert to integers
fifa_tm_clean <- fifa_tm_clean %>%
  mutate(total_player_cost = value * multiplier) %>%
  mutate(average_player_cost = round(total_player_cost/squad_size)) %>%
  mutate(across(where(is.character), str_trim))

str(fifa_tm_clean)

unique(fifa_tm_clean$confederation)

# All FIFA

fifa_tm_clean <- fifa_tm_clean %>%
  select(number, nation, squad_size, average_age, confederation, points, total_player_cost, average_player_cost)

# Federations

# UEFA

fifa_tm_clean_uefa <- fifa_tm_clean %>%
  filter(confederation == "UEFA")

# CAF

fifa_tm_clean_caf <- fifa_tm_clean %>%
  filter(confederation == "CAF")

# AFC

fifa_tm_clean_afc <- fifa_tm_clean %>%
  filter(confederation == "AFC")
  
# CONCACAF

fifa_tm_clean_concacaf <- fifa_tm_clean %>%
  filter(confederation == "CONCACAF")

# OFC

fifa_tm_clean_ofc <- fifa_tm_clean %>%
  filter(confederation == "OFC")
  
# CONMEBOL

fifa_tm_clean_conmebol <- fifa_tm_clean %>%
  filter(confederation == "CONMEBOL")

# EDA

# Rank and Cost

fifa_tm_clean %>%
  ggplot(aes(number, average_player_cost)) + 
  geom_point(aes(color= confederation), size = 2) +
  geom_text_repel(aes(label = ifelse(number < 5, nation, "")), size = 5) +
  geom_text_repel(aes(label = ifelse(number >= 100 & number < 105, nation, "")), size = 5) +
  geom_text_repel(aes(label = ifelse(number >= 200 & number < 205, nation, "")), size = 5) +
  labs(x = "FIFA Ranking (April 2023)",
       y = "Average Player Value (Euros)",
       title = "Highly ranked soccer teams have the highest average player values",
       subtitle = "Comparing the FIFA rankings and average player values globally",
       caption = "Data Source: transfermarkt\nBy: @willyokech") +
  theme_classic() +
  scale_y_log10(labels  = 
                  label_number(scale = 1e-6, prefix = "$", suffix = "m", accuracy = 0.01)) + 
  theme(axis.title.x =element_text(size = 18, vjust = -2),
        axis.title.y =element_text(size = 18,  vjust = 2),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 24),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 14, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("azure2"),
        legend.position = "right")

ggsave("images/fifa_tm/all_teams_1.png", width = 12, height = 8, dpi = 600)

fifa_tm_clean_uefa %>%
  ggplot(aes(number, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_caf %>%
  ggplot(aes(number, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()


fifa_tm_clean_afc %>%
  ggplot(aes(number, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_concacaf %>%
  ggplot(aes(number, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_ofc %>%
  ggplot(aes(number, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_conmebol %>%
  ggplot(aes(number, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

# Points and Cost

fifa_tm_clean %>%
  ggplot(aes(points, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_uefa %>%
  ggplot(aes(points, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_caf %>%
  ggplot(aes(points, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_afc %>%
  ggplot(aes(points, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_concacaf %>%
  ggplot(aes(points, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_ofc %>%
  ggplot(aes(points, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

fifa_tm_clean_conmebol %>%
  ggplot(aes(points, average_player_cost)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()


# Political Groupings and FIFA Rankings
# Make a vector of groupings

####################
# SOME MISSING DATA - NEEDS CLEANING!!
###################

# Arab Maghreb Union (AMU)
amu <- c("Algeria", "Libya", "Mauritania", "Morocco", "Tunisia")

# Common Market for Eastern and Southern Africa (COMESA)
comesa <- c("Burundi", "Comoros", "Democratic Republic of the Congo", "Djibouti", 
            "Egypt", "Eritrea", "Eswatini", "Ethiopia", "Kenya", "Libya", "Madagascar", 
            "Malawi", "Mauritius", "Rwanda", "Seychelles", "Somalia", "Sudan", 
            "Tunisia", "Uganda", "Zambia", "Zimbabwe")

# Community of Sahel-Saharan States (CEN-SAD)
cen_sad <- c("Benin", "Burkina Faso", "Cape Verde", "Central African Republic", "Chad", 
             "Comoros", "Djibouti", "Egypt", "Eritrea", "Gambia", "Ghana", "Guinea", 
             "Guinea-Bissau", "Ivory Coast", "Kenya", "Liberia", "Libya", "Mali", 
             "Mauritania", "Morocco", "Niger", "Nigeria", "Sao Tome and Principe", 
             "Senegal", "Sierra Leone", "Somalia", "Sudan", "Togo", "Tunisia")

# East African Community (EAC)
eac <- c("Burundi", "Democratic Republic of the Congo", "Kenya", 
         "Rwanda", "South Sudan", "Tanzania", "Uganda")

# Economic Community of Central African States (ECCAS/CEEAC)
eccas <- c("Angola", "Burundi", "Cameroon", "Central African Republic", "Chad", 
           "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", 
           "Republic of Congo", "Rwanda", "Sao Tome and Principe")

# Economic Community of West African States (ECOWAS)
ecowas <- c("Benin", "Burkina Faso", "Cape Verde", "Ivory Coast", "The Gambia", 
            "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", 
            "Nigeria", "Senegal", "Sierra Leone", "Togo")

# Intergovernmental Authority on Development (IGAD)
igad <- c("Djibouti", "Eritrea", "Ethiopia", "Kenya", 
          "Somalia", "South Sudan", "Sudan", "Uganda")

# Southern African Development Community (SADC)
sadc <- c("Angola", "Botswana", "Comoros", "Democratic Republic of the Congo", "Eswatini", 
          "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", 
          "Seychelles", "South Africa", "Tanzania", "Zambia", "Zimbabwe")

####################
# SOME MISSING DATA - NEEDS CLEANING!!
###################

# Arab Maghreb Union (AMU)
fifa_tm_clean_amu <- fifa_tm_clean %>%
  filter(nation %in% amu)  %>%
  mutate(region = "AMU")

# Common Market for Eastern and Southern Africa (COMESA)
fifa_tm_clean_comesa <- fifa_tm_clean %>%
  filter(nation %in% comesa)  %>%
  mutate(region = "COMESA")

# Community of Sahel-Saharan States (CEN-SAD)
fifa_tm_clean_cen_sad <- fifa_tm_clean %>%
  filter(nation %in% cen_sad)  %>%
  mutate(region = "CEN_SAD")

# East African Community (EAC)
fifa_tm_clean_eac <- fifa_tm_clean %>%
  filter(nation %in% eac)  %>%
  mutate(region = "EAC")

# Economic Community of Central African States (ECCAS/CEEAC)
fifa_tm_clean_eccas <- fifa_tm_clean %>%
  filter(nation %in% eccas)  %>%
  mutate(region = "ECCAS")

# Economic Community of West African States (ECOWAS)
fifa_tm_clean_ecowas <- fifa_tm_clean %>%
  filter(nation %in% ecowas) %>%
  mutate(region = "ECOWAS")

# Intergovernmental Authority on Development (IGAD)
fifa_tm_clean_igad <- fifa_tm_clean %>%
  filter(nation %in% igad)  %>%
  mutate(region = "IGAD")

# Southern African Development Community (SADC)
fifa_tm_clean_sadc <- fifa_tm_clean %>%
  filter(nation %in% sadc)  %>%
  mutate(region = "SADC")

## Comparison of EAC and ECOWAS

fifa_tm_clean_eac_ecowas <- rbind(fifa_tm_clean_eac, fifa_tm_clean_ecowas)

fifa_tm_clean_eac_ecowas %>%
  group_by(region) %>%
  summarise(average_region_cost = sum(total_player_cost)/sum(squad_size))

fifa_tm_clean_eac_ecowas %>%
  ggplot(aes(number, average_player_cost)) + 
  geom_point(aes(color= region), size = 3) +
  geom_text_repel(aes(label = ifelse(number < 85, nation, "")), size = 5) +
  geom_text_repel(aes(label = ifelse(number >= 85 & number < 130, nation, "")), size = 5) +
  geom_text_repel(aes(label = ifelse(number >= 130 & number < 205, nation, "")), size = 5) +
  labs(x = "FIFA Ranking (April 2023)",
       y = "Average Player Value (Euros)",
       title = "West Africa dominates the East-West soccer battle",
       subtitle = "Comparing the FIFA rankings and average player values for East and West African soccer teams",
       caption = "Data Source: transfermarkt\nBy: @willyokech") +
  theme_classic() +
  scale_y_log10(labels  = 
                  label_number(scale = 1e-6, prefix = "$", suffix = "m", accuracy = 0.01)) +
  scale_color_discrete(labels = c('East Africa', 'West Africa')) +
  theme(axis.title.x =element_text(size = 18, vjust = -2),
        axis.title.y =element_text(size = 18,  vjust = 2),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 24),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "right")

#ggsave("images/fifa_tm/west_east_1.png", width = 12, height = 8, dpi = 600)
