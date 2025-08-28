# KPL League Table Analysis (2017-2018)

# Load the required libraries
library(rvest) # webscrape
library(tidyverse)
library(ggtext)
library(readr)
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

# # Load the required data
# 
# page_17_18 <- read_html("https://www.espn.com/soccer/standings/_/league/ken.1/season/2017")
# tables_17_18 <- html_table(page_17_18)
# kpl_1_17_18 <- tables_17_18[[1]]
# kpl_2_17_18 <- tables_17_18[[2]]
# 
# kpl_1_17_18 <- kpl_1_17_18 |>
#   rename(Club = `Kenyan Premier League`)
# 
# kpl_1_17_18 <- kpl_1_17_18 |>
#   mutate(
#     matches = str_match(Club, "^([0-9]+)([A-Z]{3})(.*)$"),
#     code_number = matches[, 2],
#     code_letters = matches[, 3],
#     team_name = str_trim(matches[, 4])
#   ) |>
#   select(-matches)
# 
# kpl_merge_17_18 <- bind_cols(kpl_1_17_18, kpl_2_17_18) 
# 
# kpl_merge_17_18 <- kpl_merge_17_18 |>
#   select(-Club)
# 
# # Save data as csv in datasets
# write_csv(kpl_merge_17_18, here::here("sub_pro_1_kpl_analysis", 
#                                       "datasets", "kpl_merge_17_18.csv"))

# Read in data
kpl_merge_17_18 <- read_csv(here::here("sub_pro_1_kpl_analysis", 
                                       "datasets", "kpl_merge_17_18.csv"))

kpl_merge_17_18 <- kpl_merge_17_18 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(team_name = if_else(team_name == "SoNy Sugar",
                             "Sony Sugar", team_name))

# Display the structure of the data
str(kpl_merge_17_18)
head(kpl_merge_17_18)


# 1) Plot of Points and Goal Differences in a Bar Chart

kpl_merge_bar_17_18_pt_gd <- kpl_merge_17_18 |>
  select(team_name, GD, P)

team_order <- kpl_merge_bar_17_18_pt_gd %>%
  arrange(desc(P), desc(GD)) %>%
  pull(team_name)

kpl_merge_bar_17_18_pt_gd_long <- kpl_merge_bar_17_18_pt_gd |>
  pivot_longer(cols = c(P, GD), names_to = "metric", values_to = "value")

# Plot

kpl_merge_bar_17_18_pt_gd_long %>%
  mutate(team_name = factor(team_name, levels = rev(team_order))) %>%
  ggplot(aes(x = team_name, y = value, fill = metric)) +
  geom_col(position = position_dodge()) +
  geom_text(data = kpl_merge_bar_17_18_pt_gd,
            aes(x = team_name, y = P, label = team_name),
            hjust = -0.05, vjust = -0.25, size = 7, 
            inherit.aes = FALSE) +
  scale_fill_manual(values = c("P" = "purple3", "GD" = "salmon1")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)), 
                     breaks = seq(-40, 80, by = 10)) +
  labs(x = NULL, y = "Value", fill = "Metric") +
  coord_flip() + 
  labs(x = "", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 32, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_pt_gd.png", width = 12, height = 12, dpi = 300)

# 2) Plot of Wins and Losses in a Lollipop Chart

kpl_merge_lollipop_17_18_w_l <- kpl_merge_17_18 |>
  mutate(W_PCT = round(W/GP, 3),
         L_PCT = round(L/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, W_PCT, L_PCT, P) 

ggplot(kpl_merge_lollipop_17_18_w_l) +
  geom_segment(aes(x = L_PCT, xend = W_PCT, y = team_name, yend = team_name), 
               color = "darkolivegreen3", linewidth = 4) +
  geom_point(aes(x = W_PCT, y = team_name), color = "darkgreen", size = 8) +
  geom_point(aes(x = L_PCT, y = team_name), color = "goldenrod2", size = 8) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_w_l.png", width = 12, height = 12, dpi = 300)

# 3) Plot of Points per Goal

kpl_merge_bar_17_18_ppg <- kpl_merge_17_18 |>
  mutate(PPG = round(P/F, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, PPG) 

ggplot(kpl_merge_bar_17_18_ppg) +
  geom_segment(aes(x = 0, xend = PPG, y = team_name, yend = team_name), 
               color = "salmon", linewidth = 4) +
  geom_point(aes(x = PPG, y = team_name), color = "brown4", size = 8) +
  scale_x_continuous() +
  labs(x = "", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_ppg.png", width = 12, height = 12, dpi = 300)

# 4) Plot of Goals per Game

kpl_merge_bar_17_18_gpg <- kpl_merge_17_18 |>
  mutate(GPG = round(F/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GPG) 

ggplot(kpl_merge_bar_17_18_gpg) +
  geom_segment(aes(x = 0, xend = GPG, y = team_name, yend = team_name), 
               color = "yellow3", linewidth = 4) +
  geom_point(aes(x = GPG, y = team_name), color = "goldenrod3", size = 8) +
  scale_x_continuous() +
  labs(x = "", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_gpg.png", width = 12, height = 12, dpi = 300)


# 5) Plot of Goals Against per Game

kpl_merge_bar_17_18_gapg <- kpl_merge_17_18 |>
  mutate(GAPG = round(A/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GAPG) 

ggplot(kpl_merge_bar_17_18_gapg) +
  geom_segment(aes(x = 0, xend = GAPG, y = team_name, yend = team_name), 
               color = "grey", linewidth = 4) +
  geom_point(aes(x = GAPG, y = team_name), color = "black", size = 8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_ga_pg.png", width = 12, height = 12, dpi = 300)


# 6) Plot of Goal Difference per Game

kpl_merge_bar_17_18_gdpg <- kpl_merge_17_18 |>
  mutate(GDPG = round(GD/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GDPG) 

ggplot(kpl_merge_bar_17_18_gdpg) +
  geom_segment(aes(x = 0, xend = GDPG, y = team_name, yend = team_name), 
               color = "lightgreen", linewidth = 4) +
  geom_point(aes(x = GDPG, y = team_name), color = "green4", size = 8) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  labs(x = "", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_gd_pg.png", width = 12, height = 12, dpi = 300)

# 7) Scatterplot of GF vs GA

kpl_merge_bar_17_18_gf_ga <- kpl_merge_17_18 |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, F, A)

# Thresholds
against_thresh <- median(kpl_merge_bar_17_18_gf_ga$A)
for_thresh <- median(kpl_merge_bar_17_18_gf_ga$F)

ggplot(kpl_merge_bar_17_18_gf_ga, aes(x = A, y = F)) +
  geom_point(color = "orange", size = 6) +
  geom_text_repel(aes(label = team_name), vjust = -0.5, size = 8) +
  labs(x = "Goals Against", y = "Goals For", title = "") +
  annotate("rect", xmin = -Inf, xmax = against_thresh,
           ymin = for_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = for_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = against_thresh, linetype = "dashed", color = "gray") +
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
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_gf_ga.png", width = 12, height = 12, dpi = 300)


# 8) Scatterplot of PPG vs GD

kpl_merge_bar_17_18_ppg_gd <- kpl_merge_17_18 |>
  mutate(PPG = round(P/F, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, PPG, GD)

# Thresholds
goal_diff_thresh <- median(kpl_merge_bar_17_18_ppg_gd$GD)
ppg_thresh <- median(kpl_merge_bar_17_18_ppg_gd$PPG)

ggplot(kpl_merge_bar_17_18_ppg_gd, aes(x = GD, y = PPG)) +
  geom_point(color = "brown4", size = 6) +
  geom_text_repel(aes(label = team_name), vjust = -0.5, size = 8) +
  labs(x = "Goal Difference", y = "Points Per Game", title = "") +
  annotate("rect", xmin = goal_diff_thresh, xmax = Inf,
           ymin = ppg_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = ppg_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = goal_diff_thresh, linetype = "dashed", color = "gray") +
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
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_ppg_gd.png", width = 12, height = 12, dpi = 300)


# 9) Radar Chart - Sample teams to plot

# a) Non-Percentage metrics

kpl_merge_17_18_non_perc <- kpl_merge_17_18 |>
  mutate(
    PPG = P / GP,
    GPG = F / GP,
    GAPG = A / GP,
    GDPG = GD / GP,
    GF2GA = F / A,
    GD2P = GD / P
  )

# b) Percentage metrics

kpl_merge_17_18_perc <- kpl_merge_17_18 |>
  mutate(
    WinPerc = (W / GP) * 100,
    DrawPerc = (D / GP) * 100,
    LossPerc = (L / GP) * 100,
    PointsPerc = (P / (GP * 3)) * 100,
    GoalsSharePerc = (F / (F + A)) * 100,
    GoalsAgainstPerc = (A / (F + A)) * 100
  )

# c) Table with selected metrics

kpl_merge_17_18_non_perc_radar <- kpl_merge_17_18_non_perc |>
  select(team_name, PPG, GPG, GAPG, GF2GA, GDPG, GD2P) 

kpl_merge_17_18_perc_radar <- kpl_merge_17_18_perc |>
  select(team_name, WinPerc, DrawPerc, LossPerc, PointsPerc, GoalsSharePerc, GoalsAgainstPerc) 

# d) Radar Plots


# Top 3 Teams (Percent Metrics)

kpl_merge_17_18_perc_radar_top_3 <- kpl_merge_17_18_perc_radar |>
  filter(team_name == c("Gor Mahia", "Sofapaka", "Kariobangi Sharks"))

my_top_colors <- c("Gor Mahia" = "#BE8125", 
                   "Sofapaka" = "#2FBE25", 
                   "Kariobangi Sharks" = "#BE25AB")

perc_radar_top_3 <- ggradar(kpl_merge_17_18_perc_radar_top_3,
                            grid.min = 0,
                            grid.mid = 50,
                            grid.max = 100,
                            values.radar = c("", "", ""),
                            axis.labels = c("Wins (%)", "Draws (%)", "Losses (%)", 
                                            "Points Earned/Total\nPossible Points (%)", 
                                            "Goals For/\nTotal Goals (%)", 
                                            "Goals Against/\nTotal Goals (%)"),
                            axis.label.size = 8,
                            fill = TRUE, 
                            fill.alpha = 0.2,
                            group.line.width = 1.2,
                            group.point.size = 3,
                            group.colours = my_top_colors,
                            legend.position = "bottom",
                            legend.text.size = 24,
                            plot.title = "") +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    text = element_text(size = 32),
    panel.background = element_rect(fill = "azure2", color = "azure2"),
    plot.background  = element_rect(fill = "azure2",  color = "azure2"),
    legend.background = element_rect(
      fill = "azure2",       # or any fill color
      colour = "black",     # border color
      linewidth = 0.8,      # border thickness
      linetype = "solid"
    ),
    legend.box.background = element_rect(
      colour = "black",     # outer box (optional)
      linewidth = 1
    )
  )

perc_radar_top_3 <- perc_radar_top_3 +
  annotate("text", x = 0, y = 0, label = "0", size = 7.5) +
  annotate("text", x = 0, y = 50, label = "50", size = 7.5) +
  annotate("text", x = 0, y = 100, label = "100", size = 7.5)

perc_radar_top_3

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_perc_radar_top_3.png", height = 12, width = 12, dpi = 300)


# Bottom 3 Teams (Percent Metrics)

kpl_merge_17_18_perc_radar_bottom_3 <- kpl_merge_17_18_perc_radar |>
  filter(team_name == c("Thika United", "Western Stima", "Muhoroni Youth"))

my_bottom_colors <- c("Thika United" = "#BE8125", 
                      "Western Stima" = "#2FBE25", 
                      "Muhoroni Youth" = "#BE25AB")

perc_radar_bottom_3 <- ggradar(kpl_merge_17_18_perc_radar_bottom_3,
                               grid.min = 0,
                               grid.mid = 50,
                               grid.max = 100,
                               values.radar = c("", "", ""),
                               axis.labels = c("Wins (%)", "Draws (%)", "Losses (%)", 
                                               "Points Earned/Total\nPossible Points (%)", 
                                               "Goals For/\nTotal Goals (%)", 
                                               "Goals Against/\nTotal Goals (%)"),
                               axis.label.size = 8,
                               fill = TRUE, 
                               fill.alpha = 0.2,
                               group.line.width = 1.2,
                               group.point.size = 3,
                               group.colours = my_bottom_colors,
                               legend.position = "bottom",
                               legend.text.size = 24,
                               plot.title = "") +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    text = element_text(size = 32),
    panel.background = element_rect(fill = "azure2", color = "azure2"),
    plot.background  = element_rect(fill = "azure2",  color = "azure2"),
    legend.background = element_rect(
      fill = "azure2",       # or any fill color
      colour = "black",     # border color
      linewidth = 0.8,      # border thickness
      linetype = "solid"
    ),
    legend.box.background = element_rect(
      colour = "black",     # outer box (optional)
      linewidth = 1
    )
  )

perc_radar_bottom_3 <- perc_radar_bottom_3 +
  annotate("text", x = 0, y = 0, label = "0", size = 7.5) +
  annotate("text", x = 0, y = 50, label = "50", size = 7.5) +
  annotate("text", x = 0, y = 100, label = "100", size = 7.5)

perc_radar_bottom_3

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_perc_radar_bottom_3.png", height = 12, width = 12, dpi = 300)

# Top Bottom 2 Teams (Percent Metrics)

kpl_merge_17_18_perc_radar_top_bottom_2 <- kpl_merge_17_18_perc_radar |>
  filter(team_name %in% c("Gor Mahia", "Sofapaka", "Western Stima", "Muhoroni Youth"))

my_top_bottom_colors <- c("Gor Mahia" = "#000080", 
                          "Sofapaka" = "#2FBE25", 
                          "Western Stima" = "#BE8125", 
                          "Muhoroni Youth" = "#BE25AB")

perc_radar_top_bottom_2 <- ggradar(kpl_merge_17_18_perc_radar_top_bottom_2,
                                   grid.min = 0,
                                   grid.mid = 50,
                                   grid.max = 100,
                                   values.radar = c("", "", ""),
                                   axis.labels = c("Wins (%)", "Draws (%)", "Losses (%)", 
                                                   "Points Earned/Total\nPossible Points (%)", 
                                                   "Goals For/\nTotal Goals (%)", 
                                                   "Goals Against/\nTotal Goals (%)"),
                                   axis.label.size = 8,
                                   fill = TRUE, 
                                   fill.alpha = 0.2,
                                   group.line.width = 1.2,
                                   group.point.size = 3,
                                   group.colours = my_top_bottom_colors,
                                   legend.position = "bottom",
                                   legend.text.size = 24,
                                   plot.title = "") +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    text = element_text(size = 32),
    panel.background = element_rect(fill = "azure2", color = "azure2"),
    plot.background  = element_rect(fill = "azure2",  color = "azure2"),
    legend.background = element_rect(
      fill = "azure2",       # or any fill color
      colour = "black",     # border color
      linewidth = 0.8,      # border thickness
      linetype = "solid"
    ),
    legend.box.background = element_rect(
      colour = "black",     # outer box (optional)
      linewidth = 1
    )
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

perc_radar_top_bottom_2 <- perc_radar_top_bottom_2 +
  annotate("text", x = 0, y = 0, label = "0", size = 7.5) +
  annotate("text", x = 0, y = 50, label = "50", size = 7.5) +
  annotate("text", x = 0, y = 100, label = "100", size = 7.5)

perc_radar_top_bottom_2

ggsave("sub_pro_1_kpl_analysis/images/17_18/kpl_analysis_17_18_perc_radar_top_bottom_2.png", height = 12, width = 12, dpi = 300)
