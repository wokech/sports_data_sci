# KPL League Table Analysis (2024-2025)

# Load the required libraries
library(rvest) # webscrape
library(tidyverse)
library(ggtext)

# Load the required data

page_24_25 <- read_html("https://www.espn.com/soccer/standings/_/league/ken.1/season/2024")
tables_24_25 <- html_table(page_24_25)
kpl_1_24_25 <- tables_24_25[[1]]
kpl_2_24_25 <- tables_24_25[[2]]

kpl_1_24_25 <- kpl_1_24_25 |>
  rename(Club = `2024-25 KPL`)

kpl_1_24_25 <- kpl_1_24_25 |>
  mutate(
    matches = str_match(Club, "^([0-9]+)([A-Z]{3})(.*)$"),
    code_number = matches[, 2],
    code_letters = matches[, 3],
    team_name = str_trim(matches[, 4])
  ) |>
  select(-matches)

kpl_merge_24_25 <- bind_cols(kpl_1_24_25, kpl_2_24_25) 

kpl_merge_24_25 <- kpl_merge_24_25 |>
  select(-Club)

# 1) Plot of Points and Goal Differences in a Bar Chart

kpl_merge_lollipop_24_25_pt_gd <- kpl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  select(team_name, GD, P) 

kpl_merge_lollipop_24_25_pt_gd_long <- kpl_merge_lollipop_24_25_pt_gd %>%
  pivot_longer(cols = c(P, GD), names_to = "metric", values_to = "value") |>
  mutate(team_name = fct_reorder(team_name, kpl_merge_lollipop_24_25_pt_gd$P[match(team_name, kpl_merge_lollipop_24_25_pt_gd$team_name)]))

# Plot
ggplot(kpl_merge_lollipop_24_25_pt_gd_long, aes(x = team_name, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  geom_text(data = kpl_merge_lollipop_24_25_pt_gd,
            aes(x = team_name, y = P, label = team_name),
            hjust = -0.05, vjust = -0.25, size = 7, 
            inherit.aes = FALSE) +
  scale_fill_manual(values = c("P" = "purple3", "GD" = "salmon1")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
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

ggsave("r_scripts/kpl_analysis/kpl_analysis_24_25_pt_gd.png", width = 12, height = 12, dpi = 300)

# 2) Plot of Wins and Losses in a Lollipop Chart

kpl_merge_lollipop_24_25_w_l <- kpl_merge_24_25 |>
  mutate(team_name = if_else(team_name == "CPosta Rangers",
                             "Posta Rangers", team_name)) |>
  mutate(team_name = if_else(team_name == "Bandari Mtwara",
                             "Bandari", team_name)) |>
  mutate(W_PCT = round(W/GP, 3),
         L_PCT = round(L/GP, 3),
         team_name = fct_reorder(team_name, P)) |>
  select(team_name, W_PCT, L_PCT, P) 

ggplot(kpl_merge_lollipop_24_25_w_l) +
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

ggsave("r_scripts/kpl_analysis/kpl_analysis_24_25_w_l.png", width = 12, height = 12, dpi = 300)
