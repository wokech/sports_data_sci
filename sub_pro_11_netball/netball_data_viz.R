# Data Visualisation with Laine Heidenreich

# Load the required packages.

library(dplyr)
library(ggplot2)
library(tidyverse)

# Loading and cleaning the data

library(devtools)
devtools::install_github("stevelane/superNetballR")
library(superNetballR)
data("players_2017")
View(players_2017)
head(players_2017)

summary(players_2017)


player17stats <- players_2017 %>% spread(stat, value)
player17stats <- subset(player17stats, round <= 14)
head(player17stats)


glimpse(player17stats)


player17stats[,8:14] <- sapply(player17stats[,8:14], as.numeric)
player17stats[,16:37] <- sapply(player17stats[,16:37], as.numeric)
player17stats[,39:40] <- sapply(player17stats[,39:40], as.numeric)
player17stats$squadId <- as.character(player17stats$squadId)
player17stats$round <- as.character(player17stats$round)


total_player_stats_per_round <- player17stats %>% 
  group_by(shortDisplayName, playerId, squadId, round) %>% 
  summarise(badHands = sum(badHands), 
            badPasses = sum(badPasses), 
            blocked = sum(blocked), 
            blocks = sum(blocks), 
            breaks = sum(breaks), 
            centrePassReceives = sum(centrePassReceives), 
            contactPenalties = sum(contactPenalties), 
            defensiveRebounds = sum(defensiveRebounds), 
            deflections = sum(deflections), 
            disposals = sum(disposals), 
            feeds = sum(feeds), 
            gain = sum(gain), 
            goalAssists = sum(goalAssists), 
            goalAttempts = sum(goalAttempts), 
            goalMisses = sum(goalMisses), 
            goals = sum(goals), 
            intercepts = sum(intercepts), 
            minutesPlayed = sum(minutesPlayed), 
            missedGoalTurnover = sum(missedGoalTurnover), 
            obstructionPenalties = sum(obstructionPenalties), 
            offensiveRebounds = sum(offensiveRebounds), 
            offsides = sum(offsides), 
            passes = sum(passes), 
            penalties = sum(penalties), 
            pickups = sum(pickups), 
            possessions = sum(possessions), 
            quartersPlayed = sum(quartersPlayed), 
            rebounds = sum(rebounds), 
            tossUpWin = sum(tossUpWin), 
            turnovers = sum(turnovers))

head(total_player_stats_per_round)


# Using ggplot


ggplot(data = total_player_stats_per_round, aes(x = feeds, y = goalAssists)) + 
  geom_point()


ggplot(data = total_player_stats_per_round, aes(x = feeds, y = goalAssists, color = round)) + 
  geom_point()


ggplot(data = total_player_stats_per_round, aes(x = minutesPlayed, y = disposals, color = squadId)) + 
  geom_point()

# Size

squad801 <- total_player_stats_per_round %>% 
  filter(squadId == "801")

ggplot(squad801, aes(x = obstructionPenalties, y = contactPenalties, color = shortDisplayName, size = minutesPlayed)) + geom_point()

# Shape

squad801 <- squad801 %>% 
  filter(shortDisplayName == "Agbeze, A" | 
           shortDisplayName == "Bell, E" | 
           shortDisplayName == "Clarke, J" | 
           shortDisplayName == "Ingles, R" | 
           shortDisplayName == "Pitman, C")

ggplot(squad801, aes(x = obstructionPenalties, y = contactPenalties, color = shortDisplayName, size = minutesPlayed, shape = shortDisplayName)) + geom_point()

# Facets

ggplot(data = squad801, aes(x = deflections, y = gain, color = shortDisplayName, size = minutesPlayed)) + 
  geom_point() + 
  facet_wrap(~shortDisplayName, nrow = 2) + 
  theme_bw()

# Highlighting

ggplot(data = total_player_stats_per_round, aes(x = centrePassReceives, y = goalAssists)) +
  geom_point(alpha = 0.2, size = 0.1) +
  geom_point(data = subset(total_player_stats_per_round, shortDisplayName == "Wood, S"), aes(color = 'Steph Wood')) +
  geom_point(data = subset(total_player_stats_per_round, shortDisplayName == "Langman, L"), aes(color = 'Laura Langman')) +
  geom_point(data = subset(total_player_stats_per_round, shortDisplayName == "Pitman, C"), aes(color = 'Chelsea Pitman'))


ggplot(data = total_player_stats_per_round, aes(x = centrePassReceives, y = goalAssists, color = round)) +
  geom_point(alpha = 0.2, size = 0.1) +
  geom_point(data = subset(total_player_stats_per_round, shortDisplayName == "Wood, S"), aes(shape = 'Steph Wood')) +
  geom_point(data = subset(total_player_stats_per_round, shortDisplayName == "Langman, L"), aes(shape = 'Laura Langman')) +
  geom_point(data = subset(total_player_stats_per_round, shortDisplayName == "Pitman, C"), aes(shape = 'Chelsea Pitman'))

