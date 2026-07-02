# WC 2026 Prediction

# Load the required libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(janitor)
library(showtext)  # For better font handling
library(scales)
library(ggrepel)


# Read in the data

wc_2026_pred <- read_excel(here::here("sub_pro_16_wc_2026_pred", 
                                      "datasets", "wc_2026_prediction.xlsx"))

# Clean the names

wc_2026_pred_clean <- wc_2026_pred |>
  clean_names()

# Create new normalized columns

wc_2026_pred_clean <- wc_2026_pred_clean |>
  mutate(inverse_ranking = 1/fifa_ranking) 

wc_2026_pred_clean_scale_norm <- wc_2026_pred_clean %>%
  mutate(across(where(is.numeric), ~as.vector(scale(.))))

wc_2026_pred_clean_scale_norm_tsi_1 <- wc_2026_pred_clean_scale_norm |>
  mutate(tsi = (0.15*fifa_ranking_points) + (0.15*elo_rating) + (0.05*average_yearly_temp) +
           (0.35*team_value_euros) + (0.05*percent_foreigners) + (0.1*gdp_per_capita) + 
           (0.05*population) + (0.05*kalshi_odds_percent) + (0.05*polymarket_odds_percent))

africa <- c("South Africa", "Morocco", "Ivory Coast", "Tunisia", "Egypt",
            "Cabo Verde", "Senegal", "Algeria", "DR Congo", "Ghana")

# EDA 

# 1) FIFA ranking points vs ELO rating

wc_2026_pred_clean |>
  filter(country %in% africa) |>
  ggplot(aes(elo_rating, fifa_ranking_points, size = 5)) + 
  geom_jitter(size = 5) +
  geom_smooth(method=lm , color="black", se=FALSE, size = 1, linetype = "dashed") +
  theme_light() +
  labs(x = "Elo Rating (June 2026)",
       y = "FIFA Ranking Points (June 2026)",
       title = "",
       subtitle = "",
       caption = "") +
  geom_text_repel(aes(label = country), size = 8) +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        #plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        #panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = "plot",
        plot.subtitle.position = "plot",
        plot.caption.position = "plot",
        panel.grid= element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        #legend.background = element_rect("bisque1"),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ggsave("sub_pro_16_wc_2026_pred/images/africa/fifa_vs_elo.png", width = 12, height = 12, dpi = 300)

library(ggpmisc)

wc_2026_pred_clean |>
  filter(country %in% africa) |>
  ggplot(aes(elo_rating, fifa_ranking_points)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point()

# 2) Transfermarkt team value

wc_2026_pred_clean_ordered_tm <- wc_2026_pred_clean |>
  mutate(country = fct_reorder(country, team_value_euros)) 

# wc_2026_pred_clean_ordered_tm |>
#   ggplot() + 
#   geom_segment(aes(x = 0, xend = team_value_euros, y = country, yend = country), 
#                color = "salmon", linewidth = 2) +
#   geom_point(aes(x = team_value_euros, y = country), color = "brown4", size = 4) +
#   scale_x_continuous(limits = c(0, 1600000000), 
#                      breaks = seq(0, 1600000000, 400000000), 
#                      labels  = 
#                        label_number(scale = 1e-6, prefix = "$", suffix = "m", accuracy = 1, big.mark = ",")) +
#   labs(x = "Team Market Value (Euros)", y = "",
#        title = "") +
#   theme_classic() + 
#   theme(axis.title.x =element_text(size = 32),
#         axis.title.y =element_text(size = 32, angle = 90),
#         axis.text.x =element_text(size = 24),
#         axis.text.y =element_text(size = 24),
#         axis.line.x = element_line(),
#         axis.ticks.x = element_line(),
#         axis.ticks.length.x = unit(5, "pt"),
#         #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
#         legend.title = element_blank(),
#         plot.caption = element_text(family = "Helvetica",size = 12))



wc_2026_pred_clean_ordered_tm |>
  filter(country %in% africa) |>
  ggplot() + 
  geom_segment(aes(x = 0, xend = team_value_euros, y = country, yend = country), 
               color = "salmon", linewidth = 8) +
  geom_point(aes(x = team_value_euros, y = country), color = "brown4", size = 12) +
  scale_x_log10(labels  = label_number(scale = 1e-6, suffix = "m", accuracy = 1, big.mark = ",")) +
  labs(x = "Team Market Value (Euros, €)", y = "",
       title = "") +
  theme_classic() + 
  theme(axis.title.x =element_text(size = 24, face = "bold"),
        axis.title.y =element_text(size = 24, angle = 90, face = "bold"),
        axis.text.x =element_text(size = 24, face = "bold"),
        axis.text.y =element_text(size = 24, face = "bold"),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12))

ggsave("sub_pro_16_wc_2026_pred/images/africa/transfermarkt.png", width = 12, height = 12, dpi = 300)

# 3) Percentage foreign-based players 

wc_2026_pred_clean_ordered_foreign <- wc_2026_pred_clean |>
  mutate(country = fct_reorder(country, percent_foreigners)) 

wc_2026_pred_clean_ordered_foreign |>
  filter(country %in% africa) |>
  ggplot() + 
  geom_segment(aes(x = 0, xend = percent_foreigners, y = country, yend = country), 
               color = "goldenrod2", linewidth = 8) +
  geom_point(aes(x = percent_foreigners, y = country), color = "goldenrod3", size = 12) +
  scale_x_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 25),
                     labels  = label_number(suffix = "%", accuracy = 1)) +
  labs(x = "Percentage of Foreign-Based Players", y = "",
       title = "") +
  theme_classic() + 
  theme(axis.title.x =element_text(size = 24, face = "bold"),
        axis.title.y =element_text(size = 24, angle = 90, face = "bold"),
        axis.text.x =element_text(size = 24, face = "bold"),
        axis.text.y =element_text(size = 24, face = "bold"),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        panel.grid= element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12))

ggsave("sub_pro_16_wc_2026_pred/images/africa/percent_foreign.png", width = 12, height = 12, dpi = 300)

# 4) Average temperatures

n_groups <- length(unique(wc_2026_pred_clean$group))

wc_2026_pred_clean |>
  filter(country %in% africa) |>
  ggplot(aes(x = group, y = average_yearly_temp, label = country)) +
  geom_vline(
    xintercept = seq(1.5, n_groups - 0.5, by = 1),
    linetype = "dashed",
    color = "grey",
    linewidth = 1
  ) +
  geom_jitter(color = "grey", size = 3, width = 0.2, height = 0, alpha = 0.7) +
  geom_text_repel(position = position_jitter(width = 0.2, height = 0),
            vjust = -0.5,
            size = 3) +
  theme_light() + 
  labs(x = "Group", y = "Average Yearly\nTemperature (°C)",
       title = "") +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20, angle = 90),
        axis.text.x =element_text(size = 16),
        axis.text.y =element_text(size = 16),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        panel.grid= element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12))

ggsave("sub_pro_16_wc_2026_pred/images/africa/avg_yearly_temp.png", width = 8, height = 4, dpi = 300)

# 5) TSI

wc_2026_pred_clean_tsi_ordered <- wc_2026_pred_clean_scale_norm_tsi_1 |>
  mutate(country = fct_reorder(country, tsi)) 

wc_2026_pred_clean_tsi_ordered |>
  filter(country %in% africa) |>
  ggplot() +
  geom_segment(aes(x = 0, xend = tsi, y = country, yend = country), 
               color = "black", linewidth = 12) +
  #geom_point(aes(x = tsi, y = country), color = "black", size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1)), n.breaks = 10) +
  labs(x = "Team Strength Index", y = "",
       title = "") +
  theme_classic() +
  theme(axis.title.x =element_text(size = 24, face = "bold"),
        axis.title.y =element_text(size = 24, angle = 90, face = "bold"),
        axis.text.x =element_text(size = 24, face = "bold"),
        axis.text.y =element_text(size = 24, face = "bold"),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12))

ggsave("sub_pro_16_wc_2026_pred/images/africa/tsi.png", width = 12, height = 12, dpi = 300)

# 6) FIFA Ranking vs Team Value

library(ggpmisc)

wc_2026_pred_clean |>
  filter(country %in% africa) |>
  ggplot(aes(fifa_ranking_points, team_value_euros)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point()

wc_2026_pred_clean |>
  filter(country %in% africa) |>
  ggplot(aes(elo_rating, team_value_euros, size = 5)) + 
  geom_jitter(size = 5) +
  geom_smooth(method=lm , color="black", se=FALSE, size = 1, linetype = "dashed") +
  scale_y_log10(labels  = label_number(scale = 1e-6, prefix = "€", suffix = "m", accuracy = 1, big.mark = ",")) +
  theme_light() +
  labs(x = "Elo Rating (June 2026)",
       y = "Team Market Value (Euros)",
       title = "",
       subtitle = "",
       caption = "") +
  geom_text_repel(aes(label = country), size = 8) +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        #plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        #panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = "plot",
        plot.subtitle.position = "plot",
        plot.caption.position = "plot",
        panel.grid= element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        #legend.background = element_rect("bisque1"),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ggsave("sub_pro_16_wc_2026_pred/images/africa/team_value_elo.png", width = 12, height = 12, dpi = 300)

# 7) Elo vs Team Value

wc_2026_pred_clean |>
  filter(country %in% africa) |>
  ggplot(aes(elo_rating, team_value_euros)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point()

wc_2026_pred_clean |>
  filter(country %in% africa) |>
  ggplot(aes(elo_rating, team_value_euros, size = 5)) + 
  geom_jitter(size = 5) +
  geom_smooth(method=lm , color="black", se=FALSE, size = 1, linetype = "dashed") +
  scale_y_log10(labels  = label_number(scale = 1e-6, prefix = "€", suffix = "m", accuracy = 1, big.mark = ",")) +
  theme_light() +
  labs(x = "FIFA Ranking Points (June 2026)",
       y = "Team Market Value (Euros)",
       title = "",
       subtitle = "",
       caption = "") +
  geom_text_repel(aes(label = country), size = 8) +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        #plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        #panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = "plot",
        plot.subtitle.position = "plot",
        plot.caption.position = "plot",
        panel.grid= element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        #legend.background = element_rect("bisque1"),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ggsave("sub_pro_16_wc_2026_pred/images/africa/team_value_fifa.png", width = 12, height = 12, dpi = 300)
