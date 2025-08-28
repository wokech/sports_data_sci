# Ghana PL League Table Analysis (2024-2025)

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

# # Load the required data
# 
# page_24_25 <- read_html("https://www.espn.com/soccer/standings/_/league/GHA.1/season/2024")
# tables_24_25 <- html_table(page_24_25)
# ghana_pl_1_24_25 <- tables_24_25[[1]]
# ghana_pl_2_24_25 <- tables_24_25[[2]]
# 
# ghana_pl_1_24_25 <- ghana_pl_1_24_25 |>
#   rename(Club = `Ghanaian Premier League`)
# 
# ghana_pl_1_24_25 <- ghana_pl_1_24_25 |>
#   mutate(
#     matches = str_match(Club, "^([0-9]+)([A-Z]{3})(.*)$"),
#     code_number = matches[, 2],
#     code_letters = matches[, 3],
#     team_name = str_trim(matches[, 4])
#   ) |>
#   select(-matches)
# 
# ghana_pl_merge_24_25 <- bind_cols(ghana_pl_1_24_25, ghana_pl_2_24_25)
# 
# ghana_pl_merge_24_25 <- ghana_pl_merge_24_25 |>
#   select(-Club)
# 
# # Save data as csv in datasets
# write_csv(ghana_pl_merge_24_25, here::here("sub_pro_2_ghana_pl_analysis",
#                                      "datasets", "ghana_pl_merge_24_25.csv"))

# Read in data
ghana_pl_merge_24_25 <- read_csv(here::here("sub_pro_2_ghana_pl_analysis", 
                                       "datasets", "ghana_pl_merge_24_25.csv"))

# Display the structure of the data
str(ghana_pl_merge_24_25)
head(ghana_pl_merge_24_25)

# 1) Plot of Points and Goal Differences in a Bar Chart

ghana_pl_merge_bar_24_25_pt_gd <- ghana_pl_merge_24_25 |>
  select(team_name, GD, P)

team_order <- ghana_pl_merge_bar_24_25_pt_gd %>%
  arrange(desc(P), desc(GD)) %>%
  pull(team_name)

ghana_pl_merge_bar_24_25_pt_gd_long <- ghana_pl_merge_bar_24_25_pt_gd |>
  pivot_longer(cols = c(P, GD), names_to = "metric", values_to = "value")

# Plot

ghana_pl_merge_bar_24_25_pt_gd_long %>%
  mutate(team_name = factor(team_name, levels = rev(team_order))) %>%
  ggplot(aes(x = team_name, y = value, fill = metric)) +
  geom_col(position = position_dodge()) +
  geom_text(data = ghana_pl_merge_bar_24_25_pt_gd,
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_pt_gd.png", width = 12, height = 12, dpi = 300)


# 2) Plot of Wins and Losses in a Lollipop Chart

ghana_pl_merge_lollipop_24_25_w_l <- ghana_pl_merge_24_25 |>
  mutate(W_PCT = round(W/GP, 3),
         L_PCT = round(L/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, W_PCT, L_PCT, P) 

ggplot(ghana_pl_merge_lollipop_24_25_w_l) +
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_w_l.png", width = 12, height = 12, dpi = 300)

# 3) Plot of Points per Goal

ghana_pl_merge_bar_24_25_ppg <- ghana_pl_merge_24_25 |>
  mutate(PPG = round(P/F, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, PPG) 

ggplot(ghana_pl_merge_bar_24_25_ppg) +
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_ppg.png", width = 12, height = 12, dpi = 300)

# 4) Plot of Goals per Game

ghana_pl_merge_bar_24_25_gpg <- ghana_pl_merge_24_25 |>
  mutate(GPG = round(F/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GPG) 

ggplot(ghana_pl_merge_bar_24_25_gpg) +
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_gpg.png", width = 12, height = 12, dpi = 300)


# 5) Plot of Goals Against per Game

ghana_pl_merge_bar_24_25_gapg <- ghana_pl_merge_24_25 |>
  mutate(GAPG = round(A/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GAPG) 

ggplot(ghana_pl_merge_bar_24_25_gapg) +
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_ga_pg.png", width = 12, height = 12, dpi = 300)


# 6) Plot of Goal Difference per Game

ghana_pl_merge_bar_24_25_gdpg <- ghana_pl_merge_24_25 |>
  mutate(GDPG = round(GD/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GDPG) 

ggplot(ghana_pl_merge_bar_24_25_gdpg) +
  geom_segment(aes(x = 0, xend = GDPG, y = team_name, yend = team_name), 
               color = "lightgreen", linewidth = 4) +
  geom_point(aes(x = GDPG, y = team_name), color = "green4", size = 8) +
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_gd_pg.png", width = 12, height = 12, dpi = 300)

# 7) Scatterplot of GF vs GA

ghana_pl_merge_bar_24_25_gf_ga <- ghana_pl_merge_24_25 |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, F, A)

# Thresholds
against_thresh <- median(ghana_pl_merge_bar_24_25_gf_ga$A)
for_thresh <- median(ghana_pl_merge_bar_24_25_gf_ga$F)

ggplot(ghana_pl_merge_bar_24_25_gf_ga, aes(x = A, y = F)) +
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_gf_ga.png", width = 12, height = 12, dpi = 300)


# 8) Scatterplot of PPG vs GD

ghana_pl_merge_bar_24_25_ppg_gd <- ghana_pl_merge_24_25 |>
  mutate(PPG = round(P/F, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, PPG, GD)

# Thresholds
goal_diff_thresh <- median(ghana_pl_merge_bar_24_25_ppg_gd$GD)
ppg_thresh <- median(ghana_pl_merge_bar_24_25_ppg_gd$PPG)

ggplot(ghana_pl_merge_bar_24_25_ppg_gd, aes(x = GD, y = PPG)) +
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_ppg_gd.png", width = 12, height = 12, dpi = 300)


# 9) Radar Chart - Sample teams to plot

# a) Non-Percentage metrics

ghana_pl_merge_24_25_non_perc <- ghana_pl_merge_24_25 |>
  mutate(
    PPG = P / GP,
    GPG = F / GP,
    GAPG = A / GP,
    GDPG = GD / GP,
    GF2GA = F / A,
    GD2P = GD / P
  )

# b) Percentage metrics

ghana_pl_merge_24_25_perc <- ghana_pl_merge_24_25 |>
  mutate(
    WinPerc = (W / GP) * 100,
    DrawPerc = (D / GP) * 100,
    LossPerc = (L / GP) * 100,
    PointsPerc = (P / (GP * 3)) * 100,
    GoalsSharePerc = (F / (F + A)) * 100,
    GoalsAgainstPerc = (A / (F + A)) * 100
  )

# c) Table with selected metrics

ghana_pl_merge_24_25_non_perc_radar <- ghana_pl_merge_24_25_non_perc |>
  select(team_name, PPG, GPG, GAPG, GF2GA, GDPG, GD2P) 

ghana_pl_merge_24_25_perc_radar <- ghana_pl_merge_24_25_perc |>
  select(team_name, WinPerc, DrawPerc, LossPerc, PointsPerc, GoalsSharePerc, GoalsAgainstPerc) 

# d) Radar Plots


# Top 3 Teams (Percent Metrics)

ghana_pl_merge_24_25_perc_radar_top_3 <- ghana_pl_merge_24_25_perc_radar |>
  filter(team_name == c("Bibiani Gold Stars", "Heart of Lions", "Asante Kotoko"))

my_top_colors <- c("Bibiani Gold Stars" = "#BE8125", 
                   "Heart of Lions" = "#2FBE25", 
                   "Asante Kotoko" = "#BE25AB")

perc_radar_top_3 <- ggradar(ghana_pl_merge_24_25_perc_radar_top_3,
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_perc_radar_top_3.png", height = 12, width = 12, dpi = 300)


# Bottom 3 Teams (Percent Metrics)

ghana_pl_merge_24_25_perc_radar_bottom_3 <- ghana_pl_merge_24_25_perc_radar |>
  filter(team_name == c("Accra Lions", "Legon Cities", "Nsoatreman"))

my_bottom_colors <- c("Accra Lions" = "#BE8125", 
                      "Legon Cities" = "#2FBE25", 
                      "Nsoatreman" = "#BE25AB")

perc_radar_bottom_3 <- ggradar(ghana_pl_merge_24_25_perc_radar_bottom_3,
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_perc_radar_bottom_3.png", height = 12, width = 12, dpi = 300)


# Bottom 3 Teams (Percent Metrics)

ghana_pl_merge_24_25_perc_radar_top_bottom_2 <- ghana_pl_merge_24_25_perc_radar |>
  filter(team_name %in% c("Bibiani Gold Stars", "Heart of Lions", "Legon Cities", "Nsoatreman"))

my_top_bottom_colors <- c("Bibiani Gold Stars" = "#000080", 
                          "Heart of Lions" = "#2FBE25", 
                          "Legon Cities" = "#BE8125", 
                          "Nsoatreman" = "#BE25AB")

perc_radar_top_bottom_2 <- ggradar(ghana_pl_merge_24_25_perc_radar_top_bottom_2,
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

ggsave("sub_pro_2_ghana_pl_analysis/images/24_25/ghana_pl_analysis_24_25_perc_radar_top_bottom_2.png", height = 12, width = 12, dpi = 300)
