# soccermatics

# soccermatics provides tools to visualise spatial tracking and event data 
# from football (soccer) matches. There are currently functions to visualise 
# shot maps (with xG), average positions, heatmaps, and individual player 
# trajectories. There are also helper functions to smooth, interpolate, and 
# prepare x,y-coordinate tracking data for plotting and calculating further 
# metrics.

# Installation 

if (!require("devtools")) install.packages("devtools")
devtools::install_github("jogall/soccermatics")

library(soccermatics)
data(statsbomb)

# Shotmaps (showing xG)

statsbomb %>%
  filter(team.name == "France") %>%
  soccerShotmap(theme = "dark")

statsbomb %>%
  filter(team.name == "Argentina") %>%
  soccerShotmap(theme = "grass")

# Passing networks

statsbomb %>%
  filter(team.name == "Argentina") %>%
  soccerPassmap(fill = "lightblue", arrow = "r",
                title = "Argentina (vs France, 30th June 2018)")


statsbomb %>%
  filter(team.name == "France") %>%
  soccerPassmap(fill = "blue", minPass = 3,
                edge_max_width = 30, edge_col = "grey40", edge_alpha = 1,
                title = "France (vs Argentina, 30th June 2018)")

# Heatmaps

statsbomb %>%
  filter(type.name == "Pass" & team.name == "France") %>% 
  soccerHeatmap(x = "location.x", y = "location.y",
                title = "France (vs Argentina, 30th June 2016)", 
                subtitle = "Passing heatmap")

statsbomb %>%
  filter(type.name == "Pressure" & team.name == "France") %>% 
  soccerHeatmap(x = "location.x", y = "location.y", xBins = 21, yBins = 14,
                title = "France (vs Argentina, 30th June 2016)", 
                subtitle = "Defensive pressure heatmap")

# Average position

statsbomb %>% 
  filter(type.name == "Pass" & team.name == "France" & minute < 43) %>% 
  soccerPositionMap(id = "player.name", x = "location.x", y = "location.y", 
                    fill1 = "blue",
                    arrow = "r", 
                    title = "France (vs Argentina, 30th June 2016)", 
                    subtitle = "Average pass position (1' - 42')")


statsbomb %>% 
  filter(type.name == "Pass" & minute < 43) %>% 
  soccerPositionMap(id = "player.name", team = "team.name", x = "location.x", y = "location.y",
                    fill1 = "lightblue", fill2 = "blue", 
                    title = "France vs Argentina, 30th June 2018",
                    subtitle = "Average pass position (1' - 42')")


tromso_extra[1:11,] %>% 
  soccerPositionMap(grass = T, title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)", subtitle = "Average player position (1' - 16')")


# Custom plots

d2 <- statsbomb %>% 
  filter(type.name %in% c("Pressure", "Interception", "Block", "Dispossessed", "Ball Recovery") & team.name == "France")

soccerPitch(arrow = "r", 
            title = "France (vs Argentina, 30th June 2016)", 
            subtitle = "Defensive actions") +
  geom_point(data = d2, aes(x = location.x, y = location.y, col = type.name), size = 3, alpha = 0.5)


d3 <- statsbomb %>% 
  filter(type.name == "Pass" & team.name == "France") %>% 
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), 1, 0)))

soccerPitch(arrow = "r",
            title = "France (vs Argentina, 30th June 2016)", 
            subtitle = "Pass map") +
  geom_segment(data = d3, aes(x = location.x, xend = pass.end_location.x, y = location.y, yend = pass.end_location.y, col = pass.outcome), alpha = 0.75) +
  geom_point(data = d3, aes(x = location.x, y = location.y, col = pass.outcome), alpha = 0.5) +
  guides(colour = FALSE)

# Player paths

subset(tromso, id == 8)[1:1800,] %>%
  soccerPath(col = "red", grass = TRUE, arrow = "r",
             title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)",
             subtitle = "Player #8 path (1' - 3')")

tromso %>%
  dplyr::group_by(id) %>%
  dplyr::slice(1:1200) %>%
  soccerPath(id = "id", arrow = "r", 
             title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)", 
             subtitle = "Player paths (1')")
