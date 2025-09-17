# Sport, Data & R
# Alice Sweeting
# https://sportstatisticsrsweet.rbind.io/post/suncorp-super-netball-2020gf/
# https://sportstatisticsrsweet.github.io/SuperNetball2020_TeamAthleteStats.html

#####https://sportstatisticsrsweet.github.io/RLadiesMelbourneTalk/Slides#1#####

# Load the relevant libraries

# NetballRPackages <- c(
#   'tidyverse', 
#   'ggdark',
#   'devtools')
# # Install listed packages
# install.packages(NetballRPackages)

# devtools::install_github("stevelane/superNetballR")

# Analyze the Suncorp Super Netball (Team) Data

# Load required packages
library(superNetballR)
library(tidyverse)
library(ggdark)
# 2020 season ID
SeasonID = "11108" 
# Download the Firebirds versus Vixens Rd13 match
FirebirdsVixens_Rd13 <- downloadMatch(SeasonID, 13, 4)
# Tidy the match
FirebirdsVixens <- tidyMatch(FirebirdsVixens_Rd13)
# Inspect the first few rows
head(FirebirdsVixens, 12)

# Inspect the first 30 stats
head(unique(FirebirdsVixens$stat), 30)


# List of Super Netball colours that are CVD friendly
SquadName_Colours <- c("#FDE725FF", "#73D055FF", "#27AD81FF", 
                       "#7E4E90FF", "#CC6A70FF", "#2D708EFF", "#C0C0C0", "#F68F46FF")
names(SquadName_Colours) <- c("Sunshine Coast Lightning", "West Coast Fever", "Melbourne Vixens", 
                              "Queensland Firebirds", "Adelaide Thunderbirds", "NSW Swifts", "Collingwood Magpies", "GIANTS Netball")
# Plot - lets look at one stat to start with
FirebirdsVixens %>% 
  filter(stat=="generalPlayTurnovers") %>% 
  ggplot(aes(x = period, y = value, colour = squadName)) +
  geom_point()

# Make it even happier!
FirebirdsVixens %>% filter(stat=="generalPlayTurnovers") %>% 
  ggplot(aes(x = period, y = value, colour = squadName)) + geom_line(linetype = "dashed") + geom_point(size = 8) +
  geom_text(aes(label = value), size = 4, colour = "black", check_overlap = TRUE) +
  scale_colour_manual(values = SquadName_Colours) + scale_fill_manual(values = SquadName_Colours) +
  scale_x_continuous(limits = c(1,4), breaks = c(1:4), labels = function(x) paste0("Quarter ", x)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 10), breaks = seq(0,10, by = 2)) +
  labs(x = NULL, y = "Number of General Play Turnovers \n", title = "\n Rd13 - Firebirds v Vixens \n General Play Turnovers \n") +
  dark_theme_gray()  + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey10"), panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.background = element_blank(),
        axis.title.y = element_text(size = 12, face = "bold"), axis.ticks.x =  element_line(color = "grey30", size = 0.1),
        axis.line.x =  element_line(color = "grey30", size = 0.1), axis.text.x = element_text(size = 12, face = "bold"),
        axis.ticks.y =  element_line(color = "grey30", size = 0.1), axis.line.y =  element_line(color = "grey30", size = 0.1),
        axis.text.y = element_text(size = 10, face = "bold"), legend.title = element_blank(), legend.position = "bottom")


# Analysing Suncorp Super Netball (Athlete) Data

# Tidy individual player data
PlayerData <- tidyPlayers(FirebirdsVixens_Rd13)
# Inspect first 12 rows
head(PlayerData, 12)


# Call out the stats that are giving us issues (some players play multiple positions)
PlayerData %>% 
  filter((stat %in% c("startingPositionCode", "currentPositionCode"))) %>% 
  head(12)


# Analysing Suncorp Super Netball (League - Team) Data

# First, create an empty data.frame
SSN_Rd13 <- FirebirdsVixens[0,]
# Call out the round we are after, can change this to whatever round you are interested in!
getRound = 13
# Run a loop to grab data for Rd13
for (mm in 1:4) {
  # Download match
  matchData <- downloadMatch(SeasonID,getRound,mm)
  # Tidy data
  tidy_match <- tidyMatch(matchData)
  # Append
  SSN_Rd13 <- rbind(tidy_match,SSN_Rd13)
}

# Inspect
tail(SSN_Rd13, 12)


# Now plot GPT for each team
SSN_Rd13 %>% filter(stat=="generalPlayTurnovers") %>% group_by(squadName) %>% summarise(Total = sum(value)) %>% 
  arrange(desc(Total)) %>% ggplot(aes(x = reorder(squadName, -Total), y = Total, colour = squadName)) +
  geom_point(size = 10) + geom_segment(aes(x = squadName, y = 0, xend = squadName, yend = Total, colour = squadName), linetype = "dashed") +
  geom_text(aes(label = Total), size = 4, colour = "black", check_overlap = TRUE) +
  scale_colour_manual(values = SquadName_Colours) + scale_fill_manual(values = SquadName_Colours) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 40), breaks = seq(0,40, by = 10)) +
  labs(x = NULL, y = "Number of General Play Turnovers \n", title = "\n Suncorp Super Netball 2020 \n Rd13 - General Play Turnovers \n") +
  dark_theme_gray()  + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                             plot.background = element_rect(fill = "grey10"), panel.background = element_blank(),
                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.background = element_blank(),
                             axis.title.y = element_text(size = 12, face = "bold"), axis.ticks.x =  element_line(color = "grey30", size = 0.1),
                             axis.line.x =  element_line(color = "grey30", size = 0.1), 
                             axis.text.x = element_text(size = 12, face = "bold", angle = 45, vjust = 1, hjust = 1),
                             axis.ticks.y =  element_line(color = "grey30", size = 0.1), axis.line.y =  element_line(color = "grey30", size = 0.1),
                             axis.text.y = element_text(size = 10, face = "bold"),legend.title = element_blank(), legend.position = "none")


# First, create an empty data.frame
SSN_Rd13_Players <- PlayerData[0,]
# Call out the round we are after, can change this to whatever round you are interested in!
getRound = 13
# Run a loop to grab data for Rd13
for (mm in 1:4) {
  # Download match
  matchData <- downloadMatch(SeasonID,getRound,mm)
  # Tidy data
  tidy_player <- tidyPlayers(matchData)
  # Append
  SSN_Rd13_Players <- rbind(tidy_player,SSN_Rd13_Players)
}

# Inspect
tail(SSN_Rd13_Players, 12)

# Not squadName but shortDisplayName for Colors

#  Plot the top 10 athletes for stat = feedWithAttempt
SSN_Rd13_Players %>% filter(!(stat %in% c("startingPositionCode", "currentPositionCode"))) %>% mutate_at("value", as.numeric) %>% filter(stat=="feedWithAttempt") %>% group_by(shortDisplayName, playerId) %>%  summarise(Total = sum(value)) %>% 
  arrange(desc(Total)) %>% head(10) %>% ggplot(aes(x = reorder(shortDisplayName, -Total), y = Total, colour = shortDisplayName)) +
  geom_point(size = 10) + geom_segment(aes(x = shortDisplayName, y = 0, xend = shortDisplayName, yend = Total, colour = shortDisplayName), linetype = "dashed") +
  geom_text(aes(label = Total), size = 4, colour = "black", check_overlap = TRUE) +
  scale_colour_manual(values = SquadName_Colours) + scale_fill_manual(values = SquadName_Colours) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 40), breaks = seq(0,40, by = 10)) +
  labs(x = NULL, y = "Number of Feeds with Attempt \n", title = "\n Suncorp Super Netball 2020 \n Rd13 - Feeds with Attempt (Individual Players) \n") +
  dark_theme_gray()  + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                             plot.background = element_rect(fill = "grey10"), panel.background = element_blank(),
                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.background = element_blank(),
                             axis.title.y = element_text(size = 12, face = "bold"), axis.ticks.x =  element_line(color = "grey30", size = 0.1),
                             axis.line.x =  element_line(color = "grey30", size = 0.1), 
                             axis.text.x = element_text(size = 12, face = "bold", angle = 45, vjust = 1, hjust = 1),
                             axis.ticks.y =  element_line(color = "grey30", size = 0.1), axis.line.y =  element_line(color = "grey30", size = 0.1),
                             axis.text.y = element_text(size = 10, face = "bold"),legend.title = element_blank(), legend.position = "none")



