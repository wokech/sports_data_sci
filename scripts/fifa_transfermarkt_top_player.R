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
  mutate(name_country = paste(name, country, sep = "\n"))

str(fifa_tm_top_player_value_clean_1)

fifa_tm_top_player_value_clean_1 %>%
  ggplot(aes(ranking_aug_2023, value_euros)) + 
  geom_point(aes(color= region), size = 4) +
  geom_label_repel(aes(label = ifelse(ranking_aug_2023 < 50, name_country, "")), size = 5, box.padding = 0.5, force = 1, segment.color = "black", fill = "azure") +
  geom_label_repel(aes(label = ifelse(ranking_aug_2023 >= 95 & ranking_aug_2023 < 110, name_country, "")), size = 5, box.padding = 0.5, force = 1, segment.color = "black", fill = "azure") +
  geom_label_repel(aes(label = ifelse(ranking_aug_2023 > 140, name_country, "")), size = 5, box.padding = 0.5, force = 1, segment.color = "black", fill = "azure") +
  labs(x = "FIFA Ranking (Aug 2023)",
       y = "Market Value (Euros)",
       title = "FIFA ranking correlates with a team's highest player value",
       subtitle = "Comparing FIFA ranking with the market value of the top footballers in East and West Africa",
       caption = "Data Source: transfermarkt\nBy: @willyokech",
       color = "Regions") +
  theme_classic() +
  scale_y_log10(labels  = 
                  label_number(scale = 1e-6, prefix = "$", suffix = "m", accuracy = 0.01)) +
  scale_color_manual(values = c("darkgreen", "purple"), labels = c('East Africa', 'West Africa')) +
  theme(axis.title.x =element_text(size = 18, vjust = -2),
        axis.title.y =element_text(size = 18,  vjust = 2),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 24),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = c(0.15,0.15))

fifa_tm_top_player_value_clean_1 %>%
  ggplot(aes(reorder(name, value_euros), value_euros, fill = region)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(limits = c(0, 150000000), breaks = seq(0, 150000000, 25000000), labels = comma) +
  scale_fill_manual(values = c("darkgreen", "purple"), labels = c('East Africa', 'West Africa')) +
  geom_text(aes(label = paste(name, country, sep = " - "), hjust = -0.1), size = 5) +
  coord_flip() +
  theme_minimal() +
  labs(x = "",
       y = "Market Value (Euros)",
       title = "The dominance of West Africa's most valuable football players",
       subtitle = "Assessing the market value (Euros) of the most valuable football players in East and West Africa",
       caption = "Data Source: transfermarkt\nBy: @willyokech",
       fill = "Regions") +
  theme(axis.title.x =element_text(size = 18, vjust = -2),
        axis.title.y =element_blank(),
        axis.text.x = element_text(size = 15, color = "black"),
        axis.text.y = element_blank(),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.ticks.x = element_line(size = 1, color="black") , 
        axis.ticks.length = unit(.25, "cm"),
        plot.title = element_text(family="Helvetica", face="bold", size = 24),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.background = element_rect(color = "bisque1", fill = "bisque1"),
        legend.position = c(0.8, 0.2))
