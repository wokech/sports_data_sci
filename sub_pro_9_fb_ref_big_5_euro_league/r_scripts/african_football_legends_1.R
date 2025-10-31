# African Football Legends

# Load the required libraries
library(tidyverse)
library(ggtext)
library(readr)
library(readxl)
library(ggrepel)
library(tidyverse)
library(RColorBrewer)
library(dplyr)
library(scales)
library(ggplot2)
library(tibble)
library(janitor)

# Read in data
african_football_legends <- read_xlsx(here::here("sub_pro_9_fb_ref_big_5_euro_league", 
                                                "datasets", "african_football_legends.xlsx"))

# Clean the fbref data
african_football_legends_clean <- african_football_legends |>
  clean_names()

# Select the correct columns

african_football_legends_clean_select <- african_football_legends_clean |>
  select(player, number_of_90s_minimum_4, best_year_in_top_5_euro_league, era, 
         goals_penalty_kicks, assists, goals_assists_penalty_kicks)

# Select the era

african_football_legends_clean_select_nineties <- african_football_legends_clean_select |>
  filter(era == "Nineties")
african_football_legends_clean_select_noughts <- african_football_legends_clean_select |>
  filter(era == "Twenty-Noughts")
african_football_legends_clean_select_tens <- african_football_legends_clean_select |>
  filter(era == "Twenty-Tens")
african_football_legends_clean_select_twenties <- african_football_legends_clean_select |>
  filter(era == "Twenty-Twenties")

# Nineties

# Thresholds
goals_pk_thresh_nineties <- median(african_football_legends_clean_select_nineties$goals_penalty_kicks)
assists_thresh_nineties <- median(african_football_legends_clean_select_nineties$assists)

# Plot
ggplot(african_football_legends_clean_select_nineties, aes(x = goals_penalty_kicks, y = assists)) +
  geom_jitter(aes(size = number_of_90s_minimum_4), color = "purple3", alpha = 0.75) +
  geom_text_repel(data = subset(african_football_legends_clean_select_nineties),
                  aes(label = paste(player, " (",best_year_in_top_5_euro_league,")", sep = "")), color = "black", size = 6) +
  labs(x = "Goals (minus Penalty Kicks)", y = "Assists", title = "",
       size = "Number of 90 minute\ngames in best season") +
  annotate("rect", xmin = goals_pk_thresh_nineties, xmax = Inf,
           ymin = assists_thresh_nineties, ymax = Inf, alpha = 0.2, fill = "pink3") +
  geom_hline(yintercept = assists_thresh_nineties, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = goals_pk_thresh_nineties, linetype = "dashed", color = "gray") +
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
        legend.position = "bottom",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"),
        panel.grid = element_blank())

ggsave("sub_pro_9_fb_ref_big_5_euro_league/images/african_football_legends_nineties.png",
       width = 12, height = 12, dpi = 300)

# Twenty-Noughts

# Thresholds
goals_pk_thresh_noughts <- median(african_football_legends_clean_select_noughts$goals_penalty_kicks)
assists_thresh_noughts <- median(african_football_legends_clean_select_noughts$assists)

# Plot
ggplot(african_football_legends_clean_select_noughts, aes(x = goals_penalty_kicks, y = assists)) +
  geom_jitter(aes(size = number_of_90s_minimum_4), color = "purple3", alpha = 0.75) +
  geom_text_repel(data = subset(african_football_legends_clean_select_noughts),
                  aes(label = paste(player, " (",best_year_in_top_5_euro_league,")", sep = "")), color = "black", size = 6) +
  labs(x = "Goals (minus Penalty Kicks)", y = "Assists", title = "",
       size = "Number of 90 minute\ngames in best season") +
  annotate("rect", xmin = goals_pk_thresh_noughts, xmax = Inf,
           ymin = assists_thresh_noughts, ymax = Inf, alpha = 0.2, fill = "pink3") +
  geom_hline(yintercept = assists_thresh_noughts, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = goals_pk_thresh_noughts, linetype = "dashed", color = "gray") +
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
        legend.position = "bottom",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"),
        panel.grid = element_blank())

ggsave("sub_pro_9_fb_ref_big_5_euro_league/images/african_football_legends_noughts.png",
       width = 12, height = 12, dpi = 300)

# Twenty-Tens

# Thresholds
goals_pk_thresh_tens <- median(african_football_legends_clean_select_tens$goals_penalty_kicks)
assists_thresh_tens <- median(african_football_legends_clean_select_tens$assists)

# Plot
ggplot(african_football_legends_clean_select_tens, aes(x = goals_penalty_kicks, y = assists)) +
  geom_jitter(aes(size = number_of_90s_minimum_4), color = "purple3", alpha = 0.75) +
  geom_text_repel(data = subset(african_football_legends_clean_select_tens),
                  aes(label = paste(player, " (",best_year_in_top_5_euro_league,")", sep = "")), color = "black", size = 6) +
  labs(x = "Goals (minus Penalty Kicks)", y = "Assists", title = "",
       size = "Number of 90 minute\ngames in best season") +
  annotate("rect", xmin = goals_pk_thresh_tens, xmax = Inf,
           ymin = assists_thresh_tens, ymax = Inf, alpha = 0.2, fill = "pink3") +
  geom_hline(yintercept = assists_thresh_tens, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = goals_pk_thresh_tens, linetype = "dashed", color = "gray") +
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
        legend.position = "bottom",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"),
        panel.grid = element_blank())

ggsave("sub_pro_9_fb_ref_big_5_euro_league/images/african_football_legends_tens.png",
       width = 12, height = 12, dpi = 300)

# Twenty-Twenties

# Thresholds
goals_pk_thresh_twenties <- median(african_football_legends_clean_select_twenties$goals_penalty_kicks)
assists_thresh_twenties <- median(african_football_legends_clean_select_twenties$assists)

# Plot
ggplot(african_football_legends_clean_select_twenties, aes(x = goals_penalty_kicks, y = assists)) +
  geom_jitter(aes(size = number_of_90s_minimum_4), color = "purple3", alpha = 0.75) +
  geom_text_repel(data = subset(african_football_legends_clean_select_twenties),
                  aes(label = paste(player, " (",best_year_in_top_5_euro_league,")", sep = "")), color = "black", size = 6) +
  labs(x = "Goals (minus Penalty Kicks)", y = "Assists", title = "",
       size = "Number of 90 minute\ngames in best season") +
  annotate("rect", xmin = goals_pk_thresh_twenties, xmax = Inf,
           ymin = assists_thresh_twenties, ymax = Inf, alpha = 0.2, fill = "pink3") +
  geom_hline(yintercept = assists_thresh_twenties, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = goals_pk_thresh_twenties, linetype = "dashed", color = "gray") +
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
        legend.position = "bottom",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"),
        panel.grid = element_blank())

ggsave("sub_pro_9_fb_ref_big_5_euro_league/images/african_football_legends_twenties.png",
       width = 12, height = 12, dpi = 300)


# All the eras and players

# Best Overall

# Thresholds
goals_pk_thresh <- median(african_football_legends_clean_select$goals_penalty_kicks)
assists_thresh <- median(african_football_legends_clean_select$assists)

# Plot
ggplot(african_football_legends_clean_select, aes(x = goals_penalty_kicks, y = assists)) +
  geom_jitter(aes(size = number_of_90s_minimum_4), color = "purple3", alpha = 0.75) +
  geom_text_repel(data = subset(african_football_legends_clean_select,
                  goals_penalty_kicks > goals_pk_thresh & assists > assists_thresh),
                  aes(label = paste(player, " (",best_year_in_top_5_euro_league,")", sep = "")), color = "black", size = 6) +
  labs(x = "Goals (minus Penalty Kicks)", y = "Assists", title = "",
       size = "Number of 90 minute\ngames in best season") +
  annotate("rect", xmin = goals_pk_thresh, xmax = Inf,
           ymin = assists_thresh, ymax = Inf, alpha = 0.2, fill = "pink3") +
  geom_hline(yintercept = assists_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = goals_pk_thresh, linetype = "dashed", color = "gray") +
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
        legend.position = "bottom",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"),
        panel.grid = element_blank())

ggsave("sub_pro_9_fb_ref_big_5_euro_league/images/african_football_legends_best_overall.png",
       width = 12, height = 12, dpi = 300)

# Stacked bar plot for the top 10 players in their best season in top 5 league

