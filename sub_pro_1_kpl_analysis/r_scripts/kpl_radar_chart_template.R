# KPL Analysis Radar Chart Template
# REVIEW AGAIN

# ChatGPT

# Step 1: Load Required Libraries

#install.packages("fmsb")  # for radar charts

library(fmsb)
library(tidyverse)
library(RColorBrewer)

# Step 2: Load and Inspect the Data

# Load your data
kpl_merge_24_25 <- read.csv(here::here("sub_pro_1_kpl_analysis",
                          "datasets", "kpl_merge_24_25.csv"))

# Display the structure of the data
str(kpl_merge_24_25)
head(kpl_merge_24_25)

# Create performance metrics for radar chart
# We'll normalize some metrics to make them comparable on the same scale
kpl_radar <- kpl_merge_24_25 %>%
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
create_radar_chart <- function(team_indices, title = "KPL Teams Performance Radar") {
  
  # Select metrics for radar chart
  metrics <- c("Win_Pct", "Attacking_Strength", "Defensive_Strength", 
               "Points_Per_Game", "Goals_Per_Game")
  
  # Create data frame for radar chart
  radar_data <- kpl_radar[team_indices, metrics]
  
  # Add max and min rows (required by fmsb package)
  max_vals <- c(100, 100, 100, 30, 4)  # Maximum possible values
  min_vals <- c(0, 0, 0, 0, 0)         # Minimum values
  
  radar_data <- rbind(max_vals, min_vals, radar_data)
  colnames(radar_data) <- c("Win %", "Attack", "Defense", "Points/Game", "Goals/Game")
  
  # Set up colors
  colors <- brewer.pal(length(team_indices), "Set3")
  colors_fill <- paste0(colors, "40")  # Add transparency
  
  # Create the radar chart
  radarchart(radar_data,
             axistype = 1,
             pcol = colors,
             pfcol = colors_fill,
             plwd = 2,
             plty = 1,
             cglcol = "grey",
             cglty = 1,
             axislabcol = "grey",
             caxislabels = seq(0, 100, 25),
             cglwd = 0.8,
             vlcex = 0.8,
             title = title)
  
  # Add legend
  legend(x = 0.8, y = 1.2, 
         legend = kpl_radar$team_name[team_indices], 
         bty = "n", pch = 20, col = colors, 
         text.col = "black", cex = 0.9, pt.cex = 2)
}

# Example 1: Compare top 4 teams
cat("=== TOP 4 TEAMS COMPARISON ===\n")
top_4 <- 1:4
print(kpl_radar[top_4, c("team_name", "P", "Win_Pct", "Attacking_Strength", "Defensive_Strength")])

par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
create_radar_chart(top_4, "KPL Top 4 Teams - Performance Radar")

# Example 2: Compare teams with different playing styles
cat("\n=== DIFFERENT PLAYING STYLES ===\n")
# Police (Champions), Gor Mahia (High scoring), Kakamega Homeboyz (Balanced), Sofapaka (Mid-table)
style_comparison <- c(1, 2, 3, 7)
print(kpl_radar[style_comparison, c("team_name", "F", "A", "GD", "Win_Pct")])

par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
create_radar_chart(style_comparison, "KPL Teams - Different Playing Styles")

# Example 3: Bottom vs Top teams comparison
cat("\n=== TOP vs BOTTOM TEAMS ===\n")
top_bottom <- c(1, 2, 17, 18)  # Police, Gor Mahia vs Talanta, Nairobi City Stars
print(kpl_radar[top_bottom, c("team_name", "P", "F", "A", "GD")])

par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
create_radar_chart(top_bottom, "KPL - Top Teams vs Bottom Teams")

# Display summary statistics
cat("\n=== LEAGUE SUMMARY STATISTICS ===\n")
cat("Champions:", kpl_merge_24_25$team_name[1], "with", kpl_merge_24_25$P[1], "points\n")
cat("Highest scoring team:", kpl_merge_24_25$team_name[which.max(kpl_merge_24_25$F)], "with", max(kpl_merge_24_25$F), "goals\n")
cat("Best defense:", kpl_merge_24_25$team_name[which.min(kpl_merge_24_25$A)], "with", min(kpl_merge_24_25$A), "goals conceded\n")
cat("Best goal difference:", kpl_merge_24_25$team_name[which.max(kpl_merge_24_25$GD)], "with", max(kpl_merge_24_25$GD), "GD\n")

# Create a comprehensive performance table
performance_summary <- kpl_radar %>%
  select(team_name, P, Win_Pct, Goals_Per_Game, Conceded_Per_Game, 
         Attacking_Strength, Defensive_Strength) %>%
  arrange(desc(P))

cat("\n=== PERFORMANCE METRICS TABLE ===\n")
print(performance_summary, row.names = FALSE)
