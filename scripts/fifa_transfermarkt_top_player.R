# Highest value player in each country / region

# FIFA World Rankings and Market Value
# August 2023 Ranking

# Data: https://www.transfermarkt.com/wettbewerbe/fifa
# Reference: https://www.datakwery.com/post/2020-07-11-scientific-notation-in-r/

# (A )Load the required libraries

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

# (B) New method to export images and gifs

# To export the images
# camcorder::gg_record()

library(camcorder)

gg_record(
  dir = 'images/fifa_tm_top_player',
  width = 12,
  height = 12 *3/4 ,
  dpi = 300,
  bg = 'white'
)

# Load the required data

fifa_tm_top_player_value <- read_excel("datasets/FIFA_Ranking_Player_Value_Aug_2023.xlsx")

fifa_tm_top_player_value_clean <- fifa_tm_top_player_value %>%
  clean_names()

fifa_tm_top_player_value_clean_1 <- fifa_tm_top_player_value_clean %>%
  mutate(name_country = paste(name, country, sep = "-"))

str(fifa_tm_top_player_value_clean_1)

# 1) Rank and Player Value

########### Plot 1080 by 1080

fifa_tm_top_player_value_clean_1 %>%
  ggplot(aes(ranking_aug_2023, value_euros)) + 
  geom_point(aes(color= region), size = 5) +
  geom_text_repel(aes(label = ifelse(ranking_aug_2023 < 50, name_country, "")), size = 10, 
                  seed = 42, box.padding = 0.5, segment.length = 2, segment.size = 1) +
  geom_text_repel(aes(label = ifelse(ranking_aug_2023 >= 95 & ranking_aug_2023 < 110, name_country, "")), size = 10,
                  seed = 42, box.padding = 0.5, segment.length = 2, segment.size = 1) +
  geom_text_repel(aes(label = ifelse(ranking_aug_2023 > 140, name_country, "")), size = 10,
                  seed = 42, box.padding = 0.5, segment.length = 2, segment.size = 1) +
  labs(x = "FIFA Ranking (Aug 2023)",
       y = "Market Value (Euros)",
       title = "FIFA ranking correlates with a team's\nhighest player value",
       subtitle = "Comparing FIFA ranking with the market value of the top\nfootballers in East and West Africa",
       caption = "Data Source: transfermarkt     |     By: @afro_dataviz",
       color = "Regions") +
  theme_classic() +
  scale_y_log10(labels  = 
                  label_number(scale = 1e-6, prefix = "$", suffix = "m", accuracy = 0.01)) +
  scale_color_manual(values = c("goldenrod2", "darkgreen"), labels = c('East Africa', 'West Africa')) +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, colour = "#000000", hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 24, vjust = 1, hjust = 0.5),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        legend.background = element_rect("azure2"),
        legend.position = c(0.15,0.15))

ggsave("images/fifa_tm_top_player/rank_value_square.png", width = 12, height = 12, dpi = 72)

########### Plot 1080 by 1920

fifa_tm_top_player_value_clean_1 %>%
  ggplot(aes(ranking_aug_2023, value_euros)) + 
  geom_point(aes(color= region), size = 5) +
  geom_text_repel(aes(label = ifelse(ranking_aug_2023 < 50, name_country, "")), size = 10,
                  seed = 42, box.padding = 0.5, segment.length = 10, segment.size = 1) +
  geom_text_repel(aes(label = ifelse(ranking_aug_2023 >= 95 & ranking_aug_2023 < 110, name_country, "")), size = 10, 
                  seed = 42, box.padding = 0.5, segment.length = 10, segment.size = 1) +
  geom_text_repel(aes(label = ifelse(ranking_aug_2023 > 140, name_country, "")), size = 10, 
                  seed = 42, box.padding = 0.5, segment.length = 10, segment.size = 1) +
  labs(x = "FIFA Ranking (Aug 2023)",
       y = "Market Value (Euros)",
       title = "FIFA ranking correlates with a team's\nhighest player value",
       subtitle = "Comparing FIFA ranking with the market\nvalue of the top footballers in East and\nWest Africa",
       caption = "Data Source: transfermarkt     |     By: @afro_dataviz",
       color = "Regions") +
  theme_classic() +
  scale_y_log10(labels  = 
                  label_number(scale = 1e-6, prefix = "$", suffix = "m", accuracy = 0.01)) +
  scale_color_manual(values = c("goldenrod2", "darkgreen"), labels = c('East Africa', 'West Africa')) +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 24),
        plot.caption = element_text(family = "Helvetica",size = 24, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        legend.background = element_rect("azure2"),
        legend.position = c(0.25,0.15))

ggsave("images/fifa_tm_top_player/rank_value_portrait.png", width = 9, height = 16, dpi = 72)

# 2) Player value of the most valuable player

########### Plot 1080 by 1080

# For this plot I use the surname alone

fifa_tm_top_player_value_clean_split_name <- separate(fifa_tm_top_player_value_clean, name, into = c("first_name", "surname"), sep = " ")

fifa_tm_top_player_value_clean_split_name %>%
  ggplot(aes(reorder(surname, value_euros), value_euros, fill = region)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(limits = c(0, 150000000), breaks = seq(0, 150000000, 25000000), labels = comma) +
  scale_fill_manual(values = c("goldenrod2", "darkgreen"), labels = c('East Africa', 'West Africa')) +
  geom_text(aes(label = paste(surname, country, sep = "-"), hjust = 0), size = 7) +
  coord_flip() +
  theme_minimal() +
  labs(x = "",
       y = "Market Value (Euros)",
       title = "The dominance of West Africa's\nmost valuable football players",
       subtitle = "Market value of the most valuable football players in East and West Africa",
       caption = "Data Source: transfermarkt     |     By: @afro_dataviz",
       fill = "Regions") +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_blank(),
        axis.text.x = element_text(angle = 15, hjust = 0.75, vjust = 0.65, size = 24, face = "bold", color = "black"),
        axis.text.y = element_blank(),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.ticks.x = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.25, "cm"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 24, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 24, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        plot.margin = margin(0, 0, 0, 0, unit = "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        legend.background = element_rect(color = "azure2", fill = "azure2"),
        legend.position = c(0.8, 0.2)) 

ggsave("images/fifa_tm_top_player/market_value_square.png", width = 12, height = 12, dpi = 72)

########### Plot 1080 by 1920

# For this plot I use the surname alone

fifa_tm_top_player_value_clean_split_name <- separate(fifa_tm_top_player_value_clean, name, into = c("first_name", "surname"), sep = " ")

fifa_tm_top_player_value_clean_split_name %>%
  ggplot(aes(reorder(surname, value_euros), value_euros, fill = region)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(limits = c(0, 150000000), breaks = seq(0, 150000000, 25000000), labels = comma) +
  scale_fill_manual(values = c("goldenrod2", "darkgreen"), labels = c('East Africa', 'West Africa')) +
  geom_text(aes(label = paste(surname, country, sep = "-"), hjust = 0.1), size = 8) +
  coord_flip() +
  theme_minimal() +
  labs(x = "",
       y = "Market Value (Euros)",
       title = "The dominance of West Africa's\nmost valuable football players",
       subtitle = "Market value of the most valuable football players\nin East and West Africa",
       caption = "Data Source: transfermarkt     |     By: @afro_dataviz",
       fill = "Regions") +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_blank(),
        axis.text.x = element_text(angle = 15, hjust = 0.75, vjust = 0.65, size = 24, face = "bold", color = "black"),
        axis.text.y = element_blank(),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.ticks.x = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.25, "cm"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 24, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 24, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        plot.margin = margin(0, 0, 0, 0, unit = "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        legend.background = element_rect(color = "azure2", fill = "azure2"),
        legend.position = c(0.8, 0.2))

ggsave("images/fifa_tm_top_player/market_value_portrait.png", width = 9.6, height = 17, dpi = 72)
