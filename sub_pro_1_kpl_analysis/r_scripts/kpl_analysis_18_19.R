# KPL League Table Analysis (2018-2019)

# Load the required libraries
library(rvest) # webscrape
library(tidyverse)
library(ggtext)
library(readr)

# # Load the required data
# 
# page_18_19 <- read_html("https://www.espn.com/soccer/standings/_/league/ken.1/season/2018")
# tables_18_19 <- html_table(page_18_19)
# kpl_1_18_19 <- tables_18_19[[1]]
# kpl_2_18_19 <- tables_18_19[[2]]
# 
# kpl_1_18_19 <- kpl_1_18_19 |>
#   rename(Club = `KPL (Transitional)`)
# 
# kpl_1_18_19 <- kpl_1_18_19 |>
#   mutate(
#     matches = str_match(Club, "^([0-9]+)([A-Z]{3})(.*)$"),
#     code_number = matches[, 2],
#     code_letters = matches[, 3],
#     team_name = str_trim(matches[, 4])
#   ) |>
#   select(-matches)
# 
# kpl_merge_18_19 <- bind_cols(kpl_1_18_19, kpl_2_18_19)
# 
# kpl_merge_18_19 <- kpl_merge_18_19 |>
#   select(-Club)
# 
# # Save data as csv in datasets
# write_csv(kpl_merge_18_19, here::here("sub_pro_1_kpl_analysis",
#                                       "datasets", "kpl_merge_18_19.csv"))

# Read in data
kpl_merge_18_19 <- read_csv(here::here("sub_pro_1_kpl_analysis", 
                                       "datasets", "kpl_merge_18_19.csv"))

# 1) Plot of Points and Goal Differences in a Bar Chart

kpl_merge_bar_18_19_pt_gd <- kpl_merge_18_19 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(team_name = if_else(team_name == "SoNy Sugar",
                             "Sony Sugar", team_name)) |>
  select(team_name, GD, P) 

kpl_merge_bar_18_19_pt_gd_long <- kpl_merge_bar_18_19_pt_gd %>%
  pivot_longer(cols = c(P, GD), names_to = "metric", values_to = "value") |>
  mutate(team_name = fct_reorder(team_name, kpl_merge_bar_18_19_pt_gd$P[match(team_name, kpl_merge_bar_18_19_pt_gd$team_name)]))

# Plot
ggplot(kpl_merge_bar_18_19_pt_gd_long, aes(x = team_name, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  geom_text(data = kpl_merge_bar_18_19_pt_gd,
            aes(x = team_name, y = P, label = team_name),
            hjust = -0.05, vjust = -0.25, size = 7, 
            inherit.aes = FALSE) +
  scale_fill_manual(values = c("P" = "purple3", "GD" = "salmon1")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)), 
                     breaks = seq(-50, 80, by = 10)) +
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

ggsave("sub_pro_1_kpl_analysis/images/18_19/kpl_analysis_18_19_pt_gd.png", width = 12, height = 12, dpi = 300)

# 2) Plot of Wins and Losses in a Lollipop Chart

kpl_merge_lollipop_18_19_w_l <- kpl_merge_18_19 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(team_name = if_else(team_name == "SoNy Sugar",
                             "Sony Sugar", team_name)) |>
  mutate(W_PCT = round(W/GP, 3),
         L_PCT = round(L/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, W_PCT, L_PCT, P) 

ggplot(kpl_merge_lollipop_18_19_w_l) +
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

ggsave("sub_pro_1_kpl_analysis/images/18_19/kpl_analysis_18_19_w_l.png", width = 12, height = 12, dpi = 300)

# 3) Plot of Points per Goal

kpl_merge_bar_18_19_ppg <- kpl_merge_18_19 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(team_name = if_else(team_name == "SoNy Sugar",
                             "Sony Sugar", team_name)) |>
  mutate(PPG = round(P/F, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, PPG) 

ggplot(kpl_merge_bar_18_19_ppg) +
  geom_segment(aes(x = 0, xend = PPG, y = team_name, yend = team_name), 
               color = "salmon", linewidth = 4) +
  geom_point(aes(x = PPG, y = team_name), color = "brown4", size = 8) +
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

ggsave("sub_pro_1_kpl_analysis/images/18_19/kpl_analysis_18_19_ppg.png", width = 12, height = 12, dpi = 300)

# 4) Plot of Goals per Game

kpl_merge_bar_18_19_gpg <- kpl_merge_18_19 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(team_name = if_else(team_name == "SoNy Sugar",
                             "Sony Sugar", team_name)) |>
  mutate(GPG = round(F/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GPG) 

ggplot(kpl_merge_bar_18_19_gpg) +
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

ggsave("sub_pro_1_kpl_analysis/images/18_19/kpl_analysis_18_19_gpg.png", width = 12, height = 12, dpi = 300)

# 5) Plot of Goals Against per Game

kpl_merge_bar_18_19_gapg <- kpl_merge_18_19 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(team_name = if_else(team_name == "SoNy Sugar",
                             "Sony Sugar", team_name)) |>
  mutate(GAPG = round(A/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GAPG) 

ggplot(kpl_merge_bar_18_19_gapg) +
  geom_segment(aes(x = 0, xend = GAPG, y = team_name, yend = team_name), 
               color = "grey", linewidth = 4) +
  geom_point(aes(x = GAPG, y = team_name), color = "black", size = 8) +
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

ggsave("sub_pro_1_kpl_analysis/images/18_19/kpl_analysis_18_19_ga_pg.png", width = 12, height = 12, dpi = 300)


# 6) Plot of Goal Difference per Game

kpl_merge_bar_18_19_gdpg <- kpl_merge_18_19 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(team_name = if_else(team_name == "SoNy Sugar",
                             "Sony Sugar", team_name)) |>
  mutate(GDPG = round(GD/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GDPG) 

ggplot(kpl_merge_bar_18_19_gdpg) +
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

ggsave("sub_pro_1_kpl_analysis/images/18_19/kpl_analysis_18_19_gd_pg.png", width = 12, height = 12, dpi = 300)

# 7) Scatterplot of GF vs GA

kpl_merge_bar_18_19_gf_ga <- kpl_merge_18_19 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(team_name = if_else(team_name == "SoNy Sugar",
                             "Sony Sugar", team_name)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, F, A)

# Thresholds
against_thresh <- median(kpl_merge_bar_18_19_gf_ga$A)
for_thresh <- median(kpl_merge_bar_18_19_gf_ga$F)

ggplot(kpl_merge_bar_18_19_gf_ga, aes(x = A, y = F)) +
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

ggsave("sub_pro_1_kpl_analysis/images/18_19/kpl_analysis_18_19_gf_ga.png", width = 12, height = 12, dpi = 300)


# 8) Scatterplot of PPG vs GD

kpl_merge_bar_18_19_ppg_gd <- kpl_merge_18_19 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(team_name = if_else(team_name == "SoNy Sugar",
                             "Sony Sugar", team_name)) |>
  mutate(PPG = round(P/F, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, PPG, GD)

# Thresholds
goal_diff_thresh <- median(kpl_merge_bar_18_19_ppg_gd$GD)
ppg_thresh <- median(kpl_merge_bar_18_19_ppg_gd$PPG)

ggplot(kpl_merge_bar_18_19_ppg_gd, aes(x = GD, y = PPG)) +
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

ggsave("sub_pro_1_kpl_analysis/images/18_19/kpl_analysis_18_19_ppg_gd.png", width = 12, height = 12, dpi = 300)

# 9) Radar Chart - Sample teams to plot

