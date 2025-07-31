# South Africa PL League Table Analysis (2024-2025)

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

# Load the required data

# page_24_25 <- read_html("https://www.espn.com/soccer/standings/_/league/rsa.1/season/2024")
# tables_24_25 <- html_table(page_24_25)
# sa_pl_1_24_25 <- tables_24_25[[1]]
# sa_pl_2_24_25 <- tables_24_25[[2]]
# 
# sa_pl_1_24_25 <- sa_pl_1_24_25 |>
#   rename(Club = `2024-25`)
# 
# sa_pl_1_24_25 <- sa_pl_1_24_25 |>
#   mutate(
#     matches = str_match(Club, "^([0-9]+)([A-Z]{3})(.*)$"),
#     code_number = matches[, 2],
#     code_letters = matches[, 3],
#     team_name = str_trim(matches[, 4])
#   ) |>
#   select(-matches)
# 
# sa_pl_merge_24_25 <- bind_cols(sa_pl_1_24_25, sa_pl_2_24_25)
# 
# sa_pl_merge_24_25 <- sa_pl_merge_24_25 |>
#   select(-Club)
# 
# # Save data as csv in datasets
# write_csv(sa_pl_merge_24_25, here::here("sub_pro_4_sa_pl_analysis",
#                                      "datasets", "sa_pl_merge_24_25.csv"))

# Read in data
sa_pl_merge_24_25 <- read_csv(here::here("sub_pro_4_sa_pl_analysis", 
                                             "datasets", "sa_pl_merge_24_25.csv"))

# Display the structure of the data
str(sa_pl_merge_24_25)
head(sa_pl_merge_24_25)

# 1) Plot of Points and Goal Differences in a Bar Chart

sa_pl_merge_bar_24_25_pt_gd <- sa_pl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "AKCCA",
                             "KCCA", team_name)) |>
  mutate(team_name = if_else(team_name == "FUPDF",
                             "UPDF", team_name)) |>
  mutate(team_name = if_else(team_name == "LMbale Heroes",
                             "Mbale Heroes", team_name)) |>
  select(team_name, GD, P) 

sa_pl_merge_bar_24_25_pt_gd_long <- sa_pl_merge_bar_24_25_pt_gd %>%
  pivot_longer(cols = c(P, GD), names_to = "metric", values_to = "value") |>
  mutate(team_name = fct_reorder(team_name, sa_pl_merge_bar_24_25_pt_gd$P[match(team_name, sa_pl_merge_bar_24_25_pt_gd$team_name)]))

# Plot
ggplot(sa_pl_merge_bar_24_25_pt_gd_long, aes(x = team_name, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  geom_text(data = sa_pl_merge_bar_24_25_pt_gd,
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

ggsave("sub_pro_4_sa_pl_analysis/images/24_25/sa_pl_analysis_24_25_pt_gd.png", width = 12, height = 12, dpi = 300)

# 2) Plot of Wins and Losses in a Lollipop Chart

sa_pl_merge_lollipop_24_25_w_l <- sa_pl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "AKCCA",
                             "KCCA", team_name)) |>
  mutate(team_name = if_else(team_name == "FUPDF",
                             "UPDF", team_name)) |>
  mutate(team_name = if_else(team_name == "LMbale Heroes",
                             "Mbale Heroes", team_name)) |>
  mutate(W_PCT = round(W/GP, 3),
         L_PCT = round(L/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, W_PCT, L_PCT, P) 

ggplot(sa_pl_merge_lollipop_24_25_w_l) +
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

ggsave("sub_pro_4_sa_pl_analysis/images/24_25/sa_pl_analysis_24_25_w_l.png", width = 12, height = 12, dpi = 300)

# 3) Plot of Points per Goal

sa_pl_merge_bar_24_25_ppg <- sa_pl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "AKCCA",
                             "KCCA", team_name)) |>
  mutate(team_name = if_else(team_name == "FUPDF",
                             "UPDF", team_name)) |>
  mutate(team_name = if_else(team_name == "LMbale Heroes",
                             "Mbale Heroes", team_name)) |>
  mutate(PPG = round(P/F, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, PPG) 

ggplot(sa_pl_merge_bar_24_25_ppg) +
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

ggsave("sub_pro_4_sa_pl_analysis/images/24_25/sa_pl_analysis_24_25_ppg.png", width = 12, height = 12, dpi = 300)

# 4) Plot of Goals per Game

sa_pl_merge_bar_24_25_gpg <- sa_pl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "AKCCA",
                             "KCCA", team_name)) |>
  mutate(team_name = if_else(team_name == "FUPDF",
                             "UPDF", team_name)) |>
  mutate(team_name = if_else(team_name == "LMbale Heroes",
                             "Mbale Heroes", team_name)) |>
  mutate(GPG = round(F/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GPG) 

ggplot(sa_pl_merge_bar_24_25_gpg) +
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

ggsave("sub_pro_4_sa_pl_analysis/images/24_25/sa_pl_analysis_24_25_gpg.png", width = 12, height = 12, dpi = 300)


# 5) Plot of Goals Against per Game

sa_pl_merge_bar_24_25_gapg <- sa_pl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "AKCCA",
                             "KCCA", team_name)) |>
  mutate(team_name = if_else(team_name == "FUPDF",
                             "UPDF", team_name)) |>
  mutate(team_name = if_else(team_name == "LMbale Heroes",
                             "Mbale Heroes", team_name)) |>
  mutate(GAPG = round(A/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GAPG) 

ggplot(sa_pl_merge_bar_24_25_gapg) +
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

ggsave("sub_pro_4_sa_pl_analysis/images/24_25/sa_pl_analysis_24_25_ga_pg.png", width = 12, height = 12, dpi = 300)


# 6) Plot of Goal Difference per Game

sa_pl_merge_bar_24_25_gdpg <- sa_pl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "AKCCA",
                             "KCCA", team_name)) |>
  mutate(team_name = if_else(team_name == "FUPDF",
                             "UPDF", team_name)) |>
  mutate(team_name = if_else(team_name == "LMbale Heroes",
                             "Mbale Heroes", team_name)) |>
  mutate(GDPG = round(GD/GP, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column
  select(team_name, GDPG) 

ggplot(sa_pl_merge_bar_24_25_gdpg) +
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

ggsave("sub_pro_4_sa_pl_analysis/images/24_25/sa_pl_analysis_24_25_gd_pg.png", width = 12, height = 12, dpi = 300)

# 7) Scatterplot of GF vs GA

sa_pl_merge_bar_24_25_gf_ga <- sa_pl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "AKCCA",
                             "KCCA", team_name)) |>
  mutate(team_name = if_else(team_name == "FUPDF",
                             "UPDF", team_name)) |>
  mutate(team_name = if_else(team_name == "LMbale Heroes",
                             "Mbale Heroes", team_name)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, F, A)

# Thresholds
against_thresh <- median(sa_pl_merge_bar_24_25_gf_ga$A)
for_thresh <- median(sa_pl_merge_bar_24_25_gf_ga$F)

ggplot(sa_pl_merge_bar_24_25_gf_ga, aes(x = A, y = F)) +
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

ggsave("sub_pro_4_sa_pl_analysis/images/24_25/sa_pl_analysis_24_25_gf_ga.png", width = 12, height = 12, dpi = 300)


# 8) Scatterplot of PPG vs GD

sa_pl_merge_bar_24_25_ppg_gd <- sa_pl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "AKCCA",
                             "KCCA", team_name)) |>
  mutate(team_name = if_else(team_name == "FUPDF",
                             "UPDF", team_name)) |>
  mutate(team_name = if_else(team_name == "LMbale Heroes",
                             "Mbale Heroes", team_name)) |>
  mutate(PPG = round(P/F, 3)) |>
  mutate(team_name = fct_reorder(team_name, GD)) |> # Order by tie-breaker
  mutate(team_name = fct_reorder(team_name, P)) |> # Order by main column       
  select(team_name, PPG, GD)

# Thresholds
goal_diff_thresh <- median(sa_pl_merge_bar_24_25_ppg_gd$GD)
ppg_thresh <- median(sa_pl_merge_bar_24_25_ppg_gd$PPG)

ggplot(sa_pl_merge_bar_24_25_ppg_gd, aes(x = GD, y = PPG)) +
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

ggsave("sub_pro_4_sa_pl_analysis/images/24_25/sa_pl_analysis_24_25_ppg_gd.png", width = 12, height = 12, dpi = 300)


# 9) Radar Chart - Sample teams to plot

# Create performance metrics for radar chart
# We'll normalize some metrics to make them comparable on the same scale
sa_pl_radar <- sa_pl_merge_24_25 %>%
  mutate(
    # Normalize wins, draws, losses to percentages
    Win_Pct = round((W / GP) * 100, 1),
    Draw_Pct = round((D / GP) * 100, 1),
    Loss_Pct = round((L / GP) * 100, 1),
    
    # Goals per game
    Goals_Per_Game = round(F / GP, 2),
    Conceded_Per_Game = round(A / GP, 2),
    
    # Defensive strength (lower goals conceded = higher defensive score)
    Defensive_Strength = round(100 - (A / max(A) * 100), 1),
    
    # Attacking strength
    Attacking_Strength = round((F / max(F)) * 100, 1),
    
    # Overall efficiency (points per game)
    Points_Per_Game = round(P / GP, 2) * 10  # Scale up for visibility
  )

# Function to create radar chart for selected teams
create_radar_chart <- function(team_indices, title = "sa_pl Teams Performance Radar") {
  
  # Select metrics for radar chart
  metrics <- c("Win_Pct", "Attacking_Strength", "Defensive_Strength", 
               "Points_Per_Game", "Goals_Per_Game")
  
  # Create data frame for radar chart
  radar_data <- sa_pl_radar[team_indices, metrics]
  
  # Add max and min rows (required by fmsb package)
  max_vals <- c(100, 100, 100, 30, 4)  # Maximum possible values
  min_vals <- c(0, 0, 0, 0, 0)         # Minimum values
  
  radar_data <- rbind(max_vals, min_vals, radar_data)
  colnames(radar_data) <- c("Win %", "Attack", "Defense", "Points/Game", "Goals/Game")
  
  # Set up colors
  colors <- brewer.pal(length(team_indices), "Set3")
  colors_fill <- paste0(colors, "40")  # Add transparency
  
  # Create the radar chart
  radarchart(radar_data, # team index and metrics
             axistype = 1, 
             pcol = colors, # line color
             pfcol = colors_fill, # fill color
             plwd = 2, # line width
             plty = 1, # line type
             cglcol = "grey", # color of the net
             cglty = 1, # net line type
             axislabcol = "grey", # color of axis labels
             caxislabels = seq(0, 100, 25), # vector of axis labels to display
             cglwd = 0.8, # net width
             vlcex = 0.8, # group labels size
             title = title)
  
  # Add legend
  legend(x = 0.8, y = 1.2, 
         legend = sa_pl_radar$team_name[team_indices], 
         bty = "n", pch = 20, col = colors, 
         text.col = "black", cex = 0.9, pt.cex = 2)
}

# Example 1: Compare top 4 teams
cat("=== TOP 4 TEAMS COMPARISON ===\n")
top_4 <- 1:4
print(sa_pl_radar[top_4, c("team_name", "P", "Win_Pct", "Attacking_Strength", "Defensive_Strength")])

par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
create_radar_chart(top_4, "sa_pl Top 4 Teams - Performance Radar")

# Example 2: Compare teams with different playing styles
cat("\n=== DIFFERENT PLAYING STYLES ===\n")
# Police (Champions), Gor Mahia (High scoring), Kakamega Homeboyz (Balanced), Sofapaka (Mid-table)
style_comparison <- c(1, 2, 3, 7)
print(sa_pl_radar[style_comparison, c("team_name", "F", "A", "GD", "Win_Pct")])

par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
create_radar_chart(style_comparison, "sa_pl Teams - Different Playing Styles")

# Example 3: Bottom vs Top teams comparison
cat("\n=== TOP vs BOTTOM TEAMS ===\n")
top_bottom <- c(1, 2, 17, 18)  # Police, Gor Mahia vs Talanta, Nairobi City Stars
print(sa_pl_radar[top_bottom, c("team_name", "P", "F", "A", "GD")])

par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
create_radar_chart(top_bottom, "sa_pl - Top Teams vs Bottom Teams")

# Display summary statistics
cat("\n=== LEAGUE SUMMARY STATISTICS ===\n")
cat("Champions:", sa_pl_merge_24_25$team_name[1], "with", sa_pl_merge_24_25$P[1], "points\n")
cat("Highest scoring team:", sa_pl_merge_24_25$team_name[which.max(sa_pl_merge_24_25$F)], "with", max(sa_pl_merge_24_25$F), "goals\n")
cat("Best defense:", sa_pl_merge_24_25$team_name[which.min(sa_pl_merge_24_25$A)], "with", min(sa_pl_merge_24_25$A), "goals conceded\n")
cat("Best goal difference:", sa_pl_merge_24_25$team_name[which.max(sa_pl_merge_24_25$GD)], "with", max(sa_pl_merge_24_25$GD), "GD\n")

# Create a comprehensive performance table
performance_summary <- sa_pl_radar %>%
  select(team_name, P, Win_Pct, Goals_Per_Game, Conceded_Per_Game, 
         Attacking_Strength, Defensive_Strength) %>%
  arrange(desc(P))

cat("\n=== PERFORMANCE METRICS TABLE ===\n")
print(performance_summary, row.names = FALSE)
