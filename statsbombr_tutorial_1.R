# Statsbomb Tutorial
# https://github.com/statsbomb/StatsBombR

# A) Load the required packages

library(tidyverse)
library(ggplot2)
library(devtools)
library(remotes)
#devtools::install_github("statsbomb/SDMTools")
#devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)

# B) Pull in the StatsBomb data

# Show all the competition data
# FreeCompetitions()

# Store all the competition data in one variable
# Comp <- FreeCompetitions()

# Access all the available match data
#Matches <- FreeMatches(Comp)

# Pull all the event data for the matches that are chosen 
### IS TOO BIG!!
# StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)

# Clean the data 
# StatsBombData = allclean(StatsBombData)

# An example loading of the required dataset

# Load the required dataset - 2020/21 FA Womenʼs Super League season
Comp <- FreeCompetitions() %>%
  filter(competition_id==37 & season_name=="2020/2021")

# Pulls all the matches for the desired competition
Matches <- FreeMatches(Comp) 

# Create a dataframe
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)

# Clean all the data
StatsBombData = allclean(StatsBombData) 

# Know the data by reviewing the Github repo

# C) Data Use Cases

# There will be four use cases, increasing in complexity as they go:

# Use Case 1: Shots and Goals - A simple but important starting point. 
# Here we will extract shots and goals totals for each team, then 
# look at how to do the same but on a per game basis.

# Use Case 2: Graphing Shots On a Chart - After we have the shots and 
# goals data, how can we take that and create a starter chart from it?

# Use Case 3: Player Shots Per 90 - Getting shots for players is simple 
# enough after doing so for teams. But then how can we adjust those 
# figures on a per 90 basis?

# Use Case 4: Mapping Passes - Filtering our data down to just a subset 
# of passes and then using Rʼs ggplot2 to plot those passes on a pitch.

################################           #############################

# Data Use Case 1: Goals and Shots

shots_goals = StatsBombData %>%
  group_by(team.name) %>% #1
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)) #2

#1: This code groups the data by team, so that whatever operation we 
# perform on it will be done on a team by team basis. I.e, we will find 
# the shots and goals for every team one by one.

#2: Summarise takes whatever operation we give it and produces a 
# new, separate table out of it. The vast majority of summarise uses 
# come after group_by.

# On a per game basis
# Adding in the ʻn_distinct(match_id)ʼ means we are dividing the
# number of shots/goals by each distinct (or unique) instance of a
# match, for every team. i.e, we are dividing the numbers per game.

shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)/n_distinct(match_id),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)/n_distinct(match_id))

# Data Use Case 2: From Data to a Chart

library(ggplot2)
ggplot(data = shots_goals,
       aes(x = reorder(team.name, shots), y = shots)) + #1
  geom_bar(stat = "identity", width = 0.5) + #2
  labs(y="Shots") + #3
  theme(axis.title.y = element_blank()) + #4
  scale_y_continuous( expand = c(0,0)) + #5
  coord_flip() + #6
  theme_SB() #7

# Data Use Case 3: Player Shots Per 90

player_shots = StatsBombData %>%
  group_by(player.name, player.id) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)) #1
player_minutes = get.minutesplayed(StatsBombData) #2
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) #3
player_shots = left_join(player_shots, player_minutes) #4
player_shots = player_shots %>% mutate(nineties = minutes/90) #5
player_shots = player_shots %>% mutate(shots_per90 = shots/nineties) #6

# Data Use Case 4: Plotting Passes

# Finally, weʼre going to look at plotting a playerʼs passes on a pitch.

devtools::install_github("FCrSTATS/SBpitch")
library(SBpitch)

# Load the WSL data
## wsldata is StatsBombData, pull new data in a similar manner

# Pull out the Fran Kirby's data

passes = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name) &
           player.id==4641) %>% #1
  filter(pass.end_location.x>=102 & pass.end_location.y<=62 &
           pass.end_location.y>=18) #2

create_Pitch() +
  geom_segment(data = passes, aes(x = location.x, y = location.y,
                                  xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.5, colour = "#000000", arrow =
                 arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + #3
  labs(title = "Fran Kirby, Completed Box Passes", subtitle = "WSL,
2020-21") + #4
  scale_y_reverse() + #5
  coord_fixed(ratio = 105/100) #6

# Pull some of the FA WSL data of your choice and call it ʻwsldataʼ for 
# us to work with here. Then we can filter to Fran Kirbyʼs passes. 
# is.na(pass.outcome.name) filters to only completed passes.

###############      #################        ###################

# Useful StatsBombR Functions

# allclean() - this extrapolates lots of new, helpful columns 
# from the pre-existing columns.## Make sure to use. ##

# get.playerfootedness() - Gives you a playerʼs assumed preferred foot 
# using our pass footedness data.

# get.opposingteam() - Returns an opposing team column for each team 
# in each match.

# get.gamestate() - Returns information for how much time each team 
# spent in various game states (winning/drawing/losing) for each match.

# annotate_pitchSB() - Our own solution for plotting a pitch with ggplot.

# Other Useful Packages

# Ben Torvaney, ggsoccer - A package that contains an alternative for 
# plotting a pitch with SB Data.

# Joe Gallagher, soccermatics - Also offers an option for pitch plotting 
# along with other useful shortcuts for creating heatmaps and so on.

# ggrepel - Useful for when youʼre having issues with overlapping 
# labels on a chart.

# gganimate - If you ever feel like getting more elaborate with your 
# graphics, this gives you a simple way to create 
# animated ones within R and ggplot.

###############      #################        ###################

# More Data Use Cases

# Use Case 5: xG Assisted, Joining, and xG+xGA 

# Use Case 6: Graphing Shots On a Chart - Heatmaps are one of 
# the everpresents in football data. 

# Use Case 7: Shot Maps - Another of the quintessential football 
# visualisations, shot maps come in many shapes and sizes with an 
# inconsistent overlap in design language between them.

# Data Use Case 5: xG Assisted, Joining, and xG+xGA

# xG assisted does not exist in our data initially. However, given that xGA
# is the xG value of a shot that a key pass/assist created, and that xG
# values do exist in our data, we can create xGA quite easily via joining.

library(tidyverse)
library(StatsBombR)

xGA = StatsBombData %>%
  filter(type.name=="Shot") %>% #1
  select(shot.key_pass_id, xGA = shot.statsbomb_xg) #2

shot_assists = left_join(StatsBombData, xGA, by = c("id" = "shot.key_pass_id")) %>% #3
  select(team.name, player.name, player.id, type.name, pass.shot_assist,
         pass.goal_assist, xGA ) %>% #4
  filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE) %>% #5
  arrange(desc(xGA))

# Create a chart with the data

player_xGA = shot_assists %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xGA = sum(xGA, na.rm = TRUE)) #1

player_xG = StatsBombData %>%
  filter(type.name=="Shot") %>%
  filter(shot.type.name!="Penalty" | is.na(shot.type.name)) %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE)) %>%
  left_join(player_xGA) %>%
  mutate(xG_xGA = sum(xG+xGA, na.rm =TRUE)) #2

player_minutes = get.minutesplayed(StatsBombData)
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) #3

player_xG_xGA = left_join(player_xG, player_minutes) %>%
  mutate(nineties = minutes/90,
         xG_90 = round(xG/nineties, 2),
         xGA_90 = round(xGA/nineties,2),
         xG_xGA90 = round(xG_xGA/nineties,2)) #4

chart = player_xG_xGA %>%
  ungroup() %>%
  filter(minutes>=600) %>%
  top_n(n = 15, w = xG_xGA90) #5

chart<-chart %>%
  select(1, 9:10)%>%
  pivot_longer(-player.name, names_to = "variable", values_to = "value") %>%
  filter(variable=="xG_90" | variable=="xGA_90") #6

## Plot the data

ggplot(chart, aes(x =reorder(player.name, value), y = value, fill=fct_rev(variable))) + #1
  geom_bar(stat="identity", colour="white")+
  labs(title = "Expected Goal Contribution", subtitle = "FA Women's Super League, 2020-21",
       x="", y="Per 90", caption ="Minimum 600 minutes\nNPxG = Value of shots taken (no penalties)\nxG assisted = Value of shots assisted")+
  theme(axis.text.y = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.title = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.text.x = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(size=24, color="#333333", family="Source Sans Pro" , face="bold"),
        plot.subtitle=element_text(size=18, color="#333333", family="Source Sans Pro", face="bold"),
        plot.caption=element_text(color="#333333", family="Source Sans Pro", size =10),
        text=element_text(family="Source Sans Pro"),
        legend.title=element_blank(),
        legend.text = element_text(size=14, color="#333333", family="Source Sans Pro"),
        legend.position = "bottom") + #2
  scale_fill_manual(values=c("#3371AC", "#DC2228"), labels = c( "xG Assisted","NPxG")) + #3
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(chart$value) + 0.3)) + #4
  coord_flip()+ #5
  guides(fill = guide_legend(reverse = TRUE)) #6



## Data Use Case 6: Heatmaps


library(tidyverse)
heatmap = StatsBombData %>%mutate(location.x = ifelse(location.x>120, 120, location.x),
                           location.y = ifelse(location.y>80, 80, location.y),
                           location.x = ifelse(location.x<0, 0, location.x),
                           location.y = ifelse(location.y<0, 0, location.y)) #1
# Get the coordinates all inside 0-80 by 0-120
heatmap$xbin <- cut(heatmap$location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE )
heatmap$ybin <- cut(heatmap$location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE) #2
# Create heatmaps with  specific cuts

# Draw the heatmaps

heatmap = heatmap%>%
  filter(type.name=="Pressure" | duel.type.name=="Tackle" |
           type.name=="Foul Committed" | type.name=="Interception" |
           type.name=="Block" ) %>%
  group_by(team.name) %>%
  mutate(total_DA = n()) %>%
  group_by(team.name, xbin, ybin) %>%
  summarise(total_DA = max(total_DA),
            bin_DA = n(),
            bin_pct = bin_DA/total_DA,
            location.x = median(location.x),
            location.y = median(location.y)) %>%
  group_by(xbin, ybin) %>%
  mutate(league_ave = mean(bin_pct)) %>%
  group_by(team.name, xbin, ybin) %>%
  mutate(diff_vs_ave = bin_pct - league_ave) #3


library(grid)
defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51",
                             "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
                             "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                             "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd",
                             "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", "#798590",
                             "#697785", "#526173", "#435367", "#3a4b60", "#2e4257", "#1d3048",
                             "#11263e", "#11273e", "#0d233a", "#020c16") #1

#1: These are the colours we'll be using for our heatmap later on.

# Plot the data

ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group =diff_vs_ave)) +
  geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) + #2
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) +
  annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
  annotate("path", colour = "white", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") + #3
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=22,family="Source Sans Pro"),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5,
                                  family="Source Sans Pro", colour = "black", hjust = 0.5),
        legend.direction = "vertical",
        axis.ticks=element_blank(),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) + #4
  scale_y_reverse() + #5
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels =
                         scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) + #6
  labs(title = "Where Do Teams Defend vs League Average?", subtitle = "FA Women's Super
League, 2020/21") + #7
  coord_fixed(ratio = 95/100) + #8
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                 length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                    xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9
  facet_wrap(~team.name)+ #10
  guides(fill = guide_legend(reverse = TRUE)) #11


# Data Use Case 7: Shot Maps


shots = StatsBombData %>%
  filter(type.name=="Shot" & (shot.type.name!="Penalty" | is.na(shot.type.name)) & player.name=="Samantha May Kerr") #1
shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", "#FCDC5F",
                     "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000") #2
#1: Simple filtering, leaving out penalties. Choose any player you like of course.
#2: Much like the defensive activity colours earlier, these will set the colours for our xG values.

# Plot

ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(data = shots, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, shape = shot.body_part.name),
             size = 6, alpha = 0.8) + #3
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_text(size=22,family="Source Sans Pro"),
        legend.text=element_text(size=20,family="Source Sans Pro"),
        legend.margin = margin(c(20, 10, -85, 50)),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, family="Source Sans
Pro", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) +
  labs(title = "Sam Kerr, Shot Map", subtitle = "FA Women's Super League, 2020/21") + #4
  scale_fill_gradientn(colours = shotmapxgcolors, limit = c(0,0.8), oob=scales::squish, name = "Expected Goals
Value") + #5
  scale_shape_manual(values = c("Head" = 21, "Right Foot" = 23, "Left Foot" = 24), name ="") + #6
  guides(fill = guide_colourbar(title.position = "top"),
         shape = guide_legend(override.aes = list(size = 7, fill = "black"))) + #7
  coord_flip(xlim = c(85, 125)) #8
