# Big 5 Euro League Analysis (2024-2025)

# Load the required libraries
library(rvest) # webscrape
library(tidyverse)
library(ggtext)
library(readr)
library(readxl)
library(ggrepel)
#install.packages("fmsb")  # for radar charts
library(fmsb)
library(tidyverse)
library(RColorBrewer)
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)
library(ggradar)
library(dplyr)
library(scales)
library(ggplot2)
library(tibble)
library(janitor)
library(fuzzyjoin)

# # Load the required data for 24/25 Season
# 
# page_fb_ref_big_5_24_25 <- read_html("https://fbref.com/en/comps/Big5/2024-2025/nations/2024-2025-Big-5-European-Leagues-Nationalities")
# table_fb_ref_big_5_24_25 <- html_table(page_fb_ref_big_5_24_25)
# table_fb_ref_big_5_24_25 <- as.data.frame(table_fb_ref_big_5_24_25)
# 
# # Clean the data...
# 
# # Save data as csv in datasets
# write_csv(table_fb_ref_big_5_24_25, here::here("sub_pro_9_fb_ref_big_5_euro_league",
#                                      "datasets", "fb_ref_big_5_euro_league.csv"))

# Read in data
fb_ref_big_5_euro_league_24_25 <- read_csv(here::here("sub_pro_9_fb_ref_big_5_euro_league", 
                                                      "datasets", "fb_ref_big_5_euro_league.csv"))

fb_ref_big_5_euro_league_fifa_ranking_july_2025 <- read_excel(here::here("sub_pro_9_fb_ref_big_5_euro_league", 
                                                                         "datasets", "fb_ref_big_5_euro_league_fifa_ranking_july_2025.xlsx"))


# Clean the fbref data
fb_ref_big_5_euro_league_24_25_clean <- fb_ref_big_5_euro_league_24_25 |>
  clean_names()

fb_ref_big_5_euro_league_24_25_clean <- fb_ref_big_5_euro_league_24_25_clean |>
  filter(nation != "Nation") |>
  separate(nation, into = c("code", "nation"), sep = "\\s+", extra = "merge", fill = "right") |> 
  mutate(across(everything(), str_trim))

# List of African Countries
african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
                       "Burundi", "Cape Verde", "Cameroon", "Central African Republic", 
                       "Chad", "Comoros", "Congo", "Democratic Republic of Congo", 
                       "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
                       "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
                       "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", 
                       "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                       "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
                       "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
                       "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
                       "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", 
                       "Uganda", "Zambia", "Zimbabwe")


# Filter out African Countries
fb_ref_big_5_euro_league_24_25_clean_africa <- fb_ref_big_5_euro_league_24_25_clean |>
  mutate(nation = case_when(
    nation == "Côte d'Ivoire" ~ "Ivory Coast",
    nation == "Congo DR" ~ "Democratic Republic of Congo",
    TRUE ~ nation
  )) |>
  filter(nation %in% african_countries) |>
  mutate(x_players = as.integer(x_players)) |>
  mutate(
    min = min |>
      str_replace_all("[^0-9]", "") |>
      na_if("") |>
      as.integer()
  ) |>
  select(nation, x_players, min)

# Use the fuzzy join to complete the merge

fifa_rank_fb_ref <- stringdist_inner_join(fb_ref_big_5_euro_league_24_25_clean_africa, 
                                          fb_ref_big_5_euro_league_fifa_ranking_july_2025, 
                                          by = "nation", max_dist = 1)

# Plot of Country vs Number of Players

fb_ref_big_5_euro_league_24_25_clean_africa |> 
  arrange(desc(x_players)) |>
  ggplot(aes(x = reorder(nation, x_players), y = x_players)) +
  geom_col(fill = "skyblue") + 
  geom_text(aes(label = x_players, y = x_players-0.1),  
            hjust = 1,                           
            color = "black",                        
            size = 8) + 
  geom_text(aes(label = nation, y = x_players+0.3),  
            hjust = 0,                           
            color = "black",                        
            size = 7) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "Number of Players",
       title = "",
       subtitle = "",
       caption = "Data Source: FBRef") +
  scale_y_continuous(breaks = seq(0, 50, by = 10),
                     minor_breaks = seq(0, 50, by = 10),
                     expand = expansion(mult = c(0, 0.2))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.grid.major = element_blank(),  # removes major
        panel.grid.minor = element_blank(),   # removes minor
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_9_fb_ref_big_5_euro_league/images/country_num_players.png", width = 12, height = 12, dpi = 300)

# Plot of Country vs Minutes Played

fb_ref_big_5_euro_league_24_25_clean_africa |> 
  filter(!is.na(min)) |>
  arrange(desc(min)) |>
  ggplot(aes(x = reorder(nation, min), y = min)) +
  geom_col(fill = "skyblue") + 
  geom_text(aes(label = min, y = min+10000),  
            hjust = 1,                           
            color = "black",                        
            size = 8) + 
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "Number of Minutes Played",
       title = "",
       subtitle = "",
       caption = "Data Source: FBRef") +
  scale_y_continuous(breaks = seq(0, 55000, by = 10000),
                     minor_breaks = seq(0, 55000, by = 10000),
                     expand = expansion(mult = c(0, 0.05))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.grid.major = element_blank(),  # removes major
        panel.grid.minor = element_blank(),   # removes minor
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_9_fb_ref_big_5_euro_league/images/country_min_played.png", width = 12, height = 12, dpi = 300)


# Correlation between FIFA ranking and the number of players in the top 5 leagues

# FIFA ranking (July 2025) 

# FIFA ranking vs Number of Players and Minutes Played

# Thresholds
num_players_thresh <- median(fifa_rank_fb_ref$x_players)
min_played_thresh <- median(fifa_rank_fb_ref$min, na.rm = TRUE)
fifa_ranking_thresh <- median(fifa_rank_fb_ref$fifa_ranking_july_2025)

fifa_rank_fb_ref <- fifa_rank_fb_ref |>
  filter(!is.na(min)) |>
  arrange(fifa_ranking_july_2025)

# FIFA ranking and Minutes Played

ggplot(fifa_rank_fb_ref, aes(x = fifa_ranking_july_2025, y = min)) +
  geom_jitter(color = "brown4", size =3) +
  geom_text_repel(data = subset(fifa_rank_fb_ref, fifa_ranking_july_2025 < fifa_ranking_thresh), 
                  aes(label = nation.x), color = "brown4", size = 6) +
  labs(x = "FIFA Ranking (July 2025)", y = "Minutes Played", title = "") +
  annotate("rect", xmin = -Inf, xmax = fifa_ranking_thresh,
           ymin = -Inf, ymax = min_played_thresh, alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = min_played_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = fifa_ranking_thresh, linetype = "dashed", color = "gray") +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"),
        panel.grid.major = element_blank(),  # removes major
        panel.grid.minor = element_blank())   # removes minor

ggsave("sub_pro_9_fb_ref_big_5_euro_league/images/fifa_ranking_min_played.png", width = 12, height = 12, dpi = 300)

# FIFA ranking and Number of Players

ggplot(fifa_rank_fb_ref, aes(x = fifa_ranking_july_2025, y = x_players)) +
  geom_jitter(color = "brown4", size =3) +
  geom_text_repel(data = subset(fifa_rank_fb_ref, fifa_ranking_july_2025 < fifa_ranking_thresh),
                  aes(label = nation.x), color = "brown4", size = 6) +
  labs(x = "FIFA Ranking (July 2025)", y = "Number of Players", title = "") +
  annotate("rect", xmin = -Inf, xmax = fifa_ranking_thresh,
           ymin = -Inf, ymax = num_players_thresh, alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = num_players_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = fifa_ranking_thresh, linetype = "dashed", color = "gray") +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"),
        panel.grid.major = element_blank(),  # removes major
        panel.grid.minor = element_blank())   # removes minor

ggsave("sub_pro_9_fb_ref_big_5_euro_league/images/fifa_ranking_num_players.png", width = 12, height = 12, dpi = 300)

