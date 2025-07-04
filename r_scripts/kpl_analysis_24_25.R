# KPL 2024-2025 League Table Analysis

# Load the required libraries
library(rvest) # webscrape
library(tidyverse)
library(ggtext)

# Load the required data
# 1) Marathon world records
page <- read_html("https://www.espn.com/soccer/standings/_/league/ken.1/season/2024")
tables <- html_table(page)
kpl_1 <- tables[[1]]
kpl_2 <- tables[[2]]

kpl_1 <- kpl_1 |>
  rename(Club = `2024-25 KPL`)

kpl_1 <- kpl_1 %>%
  mutate(
    matches = str_match(Club, "^([0-9]+)([A-Z]{3})(.*)$"),
    code_number = matches[, 2],
    code_letters = matches[, 3],
    team_name = str_trim(matches[, 4])
  ) %>%
  select(-matches)

kpl_merge <- bind_cols(kpl_1, kpl_2) 

kpl_merge <- kpl_merge |>
  select(-Club)

# Plot of Wins and Losses in a Lollipop Chart

kpl_merge_lollipop <- kpl_merge |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(W_PCT = round(W/GP, 3),
         L_PCT = round(L/GP, 3),
         team_name = fct_reorder(team_name, W_PCT)) |>
  select(team_name, W_PCT, L_PCT) 


ggplot(kpl_merge_lollipop) +
  geom_segment(aes(x = L_PCT, xend = W_PCT, y = team_name, yend = team_name), 
               color = "darkolivegreen3", linewidth = 4) +
  geom_point(aes(x = W_PCT, y = team_name), color = "darkgreen", size = 8) +
  geom_point(aes(x = L_PCT, y = team_name), color = "goldenrod2", size = 8) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "",
       title = "<span style='color:darkgreen;'>Wins</span> and 
                <span style='color:goldenrod2;'>Losses</span>") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_text(size = 28),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))