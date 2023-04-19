# Getting started in R with StatsBomb Data (Part 1)

# Install packages
install.packages("devtools")
install.packages("tidyverse")

# Load libraries
library(devtools)
library(tidyverse)

# Next steps are to load in the StatsBomb data and 
# FCrSTATS pitch

devtools::install_github("statsbomb/StatsBombR")
devtools::install_github("FCrSTATS/SBpitch")

# If you check the packages, 
# there should now be 'SBpitch' and 'StatsBombR'.

library(tidyverse)
library(StatsBombR)
library(SBpitch)

# Load the available data
Comp<-FreeCompetitions()
View(Comp)

# Filter the competition data specifying the 'competition_id' 
# and 'season_name'...

Comp<-FreeCompetitions()%>%
  filter(competition_id==37, season_name=="2019/2020")
View(Comp)

# Load the FA WSL data

Matches<-FreeMatches(Comp)

# Load the free event data associated with the Matches

StatsBombData<-StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)

View(StatsBombData)

StatsBombData = allclean(StatsBombData)

View(StatsBombData)

# Filter StatsBombData for 1 match

# get(<-) StatsBombData and filter (%>%filter) the 
# match (match_id == 2275096) for passes (type.name == "Pass") 
# associated with Arsenal (team.name == "Arsenal WFC") and assign 
# to d1'

d1<-StatsBombData%>%
  filter(match_id == 2275096, 
         type.name == "Pass", team.name == "Arsenal WFC")

View(d1)

create_Pitch() + # this is a base layer for a plot

# Great - now to add the passes. The best way to think of this is 
# simply as an elaborate scatter plot where you add layers. We have 
# the base layer with the pitch, now to add where passes occurred:

# create a point (geom_point) using the filtered match data (data=d1) 
# and plot the x and y coordinate (aes(x = location.x, y = location.y))

geom_point(data = d1, aes(x = location.x, y = location.y), alpha=0.5, colour = "red") +

geom_segment(data = d1, aes(x = location.x, y = location.y, 
                            xend = pass.end_location.x, yend = pass.end_location.y), 
             alpha = 0.5, colour = "red", arrow = arrow(length = unit(0.08,"inches"))) + 

# (geom_segment) to be drawn using the match data (d1) 
  # from the x/y start point to the x/y end point

# The y axis is incorrect on the create_pitch function...therefore 
# if you plot the passes of a left back it will show up on the right. 

scale_y_reverse() + 
  
  labs(title = "Arsenal WFC",
       subtitle = "vs West Ham United LFC")

# One can also filter the passes by player

d11<-StatsBombData%>%
  filter(match_id == 2275096, type.name == "Pass", 
         team.name == "Arsenal WFC", player.name == "Leah Williamson")

create_Pitch() + 
    geom_point(data = d11, aes(x = location.x, y = location.y), 
             alpha=0.5, colour = "red") +
    geom_segment(data = d11, aes(x = location.x, 
                               y = location.y, xend = pass.end_location.x, 
                              yend = pass.end_location.y), 
               alpha = 0.5, colour = "red", 
               arrow = arrow(length = unit(0.08,"inches"))) + 
      scale_y_reverse() + 
    labs(title = "Arsenal WFC",
       subtitle = "vs West Ham United LFC - Leah Williamson")

# Getting started in R with StatsBomb Data (Part 2)

# Edit the previous code for d1

# Create a new column called pass.outcome where 
# pass.outcome.name = NA is complete and 
d1<-StatsBombData%>%
  filter(match_id == 2275096, type.name == "Pass" & is.na(pass.type.name),
         team.name == "Arsenal WFC") %>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), 
                                          "Complete", "Incomplete")))

View(d1)
 
# Keep colour within the aes...

create_Pitch() +
geom_point(data=d1, 
           aes(x=location.x, y=location.y, 
               colour = pass.outcome), alpha = 0.5) +
geom_segment(data = d1, aes(x = location.x, y = location.y, 
                            xend = pass.end_location.x, yend = pass.end_location.y, 
                            colour = pass.outcome), 
             alpha = 0.5, arrow = arrow(length = unit(0.08,"inches"))) +
scale_y_reverse() + labs(title = "Arsenal FC", subtitle = "vs West Ham United LFC") + 

# Switch the colours (blue = complete / red = incomplete)
  # https://www.color-hex.com/
scale_colour_manual(values = c("#89b043", "#b00000"), name = "Outcome") + 
theme(legend.position = "bottom")

# Our aim....to find the individual number of successful and unsuccessful 
# passes before finding the total number of passes - then add to the pass map!

# The code to find successful and unsuccessful passes:
  
  passes<-d1%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally() #sum all the similar items
  
# Sum the passes
  passes_1<-sum(passes$n)
  passes_1
# Finally, we will specify only complete passes 
# by using:
  passes_2<-passes%>%
    distinct(passes$n[1])
  passes_2


create_Pitch() +
geom_point(data=d1, 
              aes(x=location.x, y=location.y, 
                  colour = pass.outcome), alpha = 0.5) +
geom_segment(data = d1, aes(x = location.x, y = location.y, 
                              xend = pass.end_location.x, yend = pass.end_location.y, 
                              colour = pass.outcome), 
              alpha = 0.5, arrow = arrow(length = unit(0.08,"inches"))) +
scale_y_reverse() + labs(title = "Arsenal FC", subtitle = "vs West Ham United LFC") + 
scale_colour_manual(values = c("#89b043", "#b00000"), name = "Outcome") + 
theme(legend.position = "bottom") + 
geom_text(aes(x = 5, y=90, label = paste0("Passes: ", passes_1))) +
geom_text(aes(x = 5, y=100, label = paste0("Passes: ", passes_2)))

# Review the StatsBomb pressure events

d1<-StatsBombData%>%
  filter(match_id == 2275096, type.name == "Pass" & is.na(pass.type.name), 
         team.name == "Arsenal WFC")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), 
                                          "Complete", "Incomplete")))

# The code to find successful and unsuccessful passes:

passes<-d1%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally() #sum all the similar items

# Sum the passes
passes_1<-sum(passes$n)
passes_1
# Finally, we will specify only complete passes 
# by using:
passes_2<-passes%>%
  distinct(passes$n[1])
passes_2

create_Pitch() +
  geom_point(data=d1, 
             aes(x=location.x, y=location.y, 
                 colour = pass.outcome), alpha = 0.5) +
  geom_segment(data = d1, aes(x = location.x, y = location.y, 
                              xend = pass.end_location.x, yend = pass.end_location.y, 
                              colour = pass.outcome), 
               alpha = 0.5, arrow = arrow(length = unit(0.08,"inches"))) +
  scale_y_reverse() + labs(title = "Arsenal FC", subtitle = "vs West Ham United LFC") + 
  scale_colour_manual(values = c("#89b043", "#b00000"), name = "Outcome") + 
  theme(legend.position = "bottom") + 
  geom_text(aes(x = 5, y=90, label = paste0("Passes: ", passes_1))) +
  geom_text(aes(x = 5, y=100, label = paste0("Passes: ", passes_2)))


# Review the StatsBomb pressure events and us the West Ham data

d1<-StatsBombData%>%
  filter(match_id == 2275096, type.name == "Pass" & is.na(pass.type.name), 
         team.name == "West Ham United LFC")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), 
                                          "Complete", "Incomplete")))

# The code to find successful and unsuccessful passes:

passes<-d1%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally() #sum all the similar items

# Sum the passes
passes_1<-sum(passes$n)
passes_1
# Finally, we will specify only complete passes 
# by using:
passes_2<-passes%>%
  distinct(passes$n[1])
passes_2

create_Pitch() +
  geom_point(data=d1, 
             aes(x=location.x, y=location.y, 
                 colour = pass.outcome), alpha = 0.5) +
  geom_segment(data = d1, aes(x = location.x, y = location.y, 
                              xend = pass.end_location.x, yend = pass.end_location.y, 
                              colour = pass.outcome), 
               alpha = 0.5, arrow = arrow(length = unit(0.08,"inches"))) +
  scale_y_reverse() + labs(title = "West Ham United LFC", subtitle = "vs Arsenal FC") + 
  scale_colour_manual(values = c("#89b043", "#b00000"), name = "Outcome") + 
  theme(legend.position = "bottom") + 
  geom_text(aes(x = 5, y=90, label = paste0("Passes: ", passes_1))) +
  geom_text(aes(x = 5, y=100, label = paste0("Passes: ", passes_2)))