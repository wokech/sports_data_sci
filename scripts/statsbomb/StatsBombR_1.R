StatsBombR

# A) Introduction

# 1) Installation

install.packages("devtools")
devtools::install_github("statsbomb/SDMTools")
devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)

# 2) Read in the Free Data (1)

## To read in all free events available:
StatsBombData <- free_allevents()

## To read in all of the free competitions we offer simply run:
FreeCompetitions()

## Store it as a data frame object
Comp <- FreeCompetitions()

## To read in the free matches available:
Matches <- FreeMatches(Comp)

## To read in free events for a certain game:
get.matchFree(Matches[1,])

View(Comp)
View(Matches)

# 3) Read in the Free Data (2)

library(tidyverse) 
library(StatsBombR) #1 

#1:  tidyverse loads many different packages. Most important for this 
#    task are dplyr and magrittr. StatsBombR loads StatsBombR.

Comp <- FreeCompetitions() %>% 
  filter(competition_id==37 & season_name=="2020/2021") #2 

#2: This grabs the competitions that are available to the user and filters it
#   down, using dplyrʼs ʻfilterʼ function, to just the 2020/21 FA Womenʼs Super
#   League season in this example.

Matches <- FreeMatches(Comp) #3

#3: This pulls all the matches for the desired competition.

StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T) #4 

#4: Now we have created a ʻdataframeʼ (essentially a table) called
# ʻStatsBombDataʼ (or whatever you choose to call it) of the free event data
# for the FAWSL season in 2020/2021.

StatsBombData = allclean(StatsBombData) #5

#5: Extracts lots of relevant information such as x/y coordinates. More
# information can be found in the package info. Be sure to familiarise
# yourself with the columns it creates using names(nameofyourdfhere).

# 4) Working with the Data - Use Cases

# A) Use Case 1: Shots and Goals - A simple but important starting point. 
# Here we will extract shots and goals totals for each team, then look 
# at how to do the same but on a per game basis.

shots_goals = StatsBombData %>%
  group_by(team.name) %>% #1
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)) #2

#1: This code groups the data by team, so that whatever operation we 
# perform on it will be done on a team by team basis. I.e, we will 
# find the shots and goals for every team one by one.

#2: Summarise takes whatever operation we give it and produces a new, 
# separate table out of it. The vast majority of summarise uses 
# come after group_by.

# shots = sum(type.name=="Shot", na.rm = TRUE) is telling it to create 
# a new column called ʻshotsʼ that sums up all the rows under the 
# ʻtype.nameʼ column that contain the word “Shot”. na.rm = TRUE tells 
# it to ignore any NAs within that column.

# shot.outcome.name=="Goal", na.rm = TRUE) does the same but for goals.

# On a per-game basis

shots_goals_1 = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)/n_distinct(match_id),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)/n_distinct(match_id))

# Adding in the ʻn_distinct(match_id)ʼ means we are dividing the
# number of shots/goals by each distinct (or unique) instance of a
# match, for every team. I.e, we are dividing the numbers per game.

# B) Use Case 2: Graphing Shots On a Chart - After we have the shots and 
# goals data, how can we take that and create a starter chart from it?

library(ggplot2)
ggplot(data = shots_goals,
       aes(x = reorder(team.name, shots), y = shots)) + #1
  geom_bar(stat = "identity", width = 0.5, color = "blue", fill = "yellow") + #2
  labs(y="Shots",
       title = "Shots per Game",
       subtitle = "Women's Super League, 2020-21") + #3
  theme(axis.title.y = element_blank()) + #4
  scale_y_continuous( expand = c(0,0)) + #5 ###*****important!
  coord_flip() + #6
  theme_SB() #7

#1: Here we are telling ggplot what data we are using and what we
# want to plot on the x/y axes of our chart. ʻReorderʼ quite literally
# reorders the team names according to the number of shots they have.

#2: Now we are telling ggplot to format it as a bar chart.

#3: This relabels the shots axis.

#4: This removes the title for the axis.

#5: Here we cut down on the space between the bars and the edge
# of the plot.

#6: This flips the entire plot, with the bars now going horizontally
# instead.

#7: theme_SB() is our own internal visual aesthetic for ggplot charts
# that we have packaged with StatsBombR. Optional of course.

# C) Use Case 3: Player Shots Per 90 - Getting shots for players is simple 
# enough after doing so for teams. But then how can we adjust those figures 
# on a per 90 basis?

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


#1: Much the same as the team calculation. We are including
# ʻplayer.idʼ here as it will be important later.

#2: This function gives us the minutes played in each match by
# ever player in the dataset.

#3: Now we group that by player and sum it altogether to get
# their total minutes played.

#4: left_join allows us to combine our shots table and our
# minutes table, with the the player.id acting as a reference point.

#5: mutate is a dplyr function that creates a new column. In this
# instance we are creating a column that divides the minutes
# totals by 90, giving us each playerЀs number of 90s played for
# the season.

#6: Finally we divide our shots totals by our number of 90s to get
# our shots per 90s column.

# D) Use Case 4: Mapping Passes - Filtering our data down to just a subset 
# of passes and then using Rʼs ggplot2 to plot those passes on a pitch.

# Finally, weʼre going to look at plotting a playerʼs passes on a pitch. 
# For this we of course need some sort of pitch visualisation.

# The one weʼll be using here comes courtesy of FC rStats. A twitter user 
# who has put together various helpful, public R packages for parsing 
# football data. The package is called ʻSBPitchʼ and it does exactly what 
# it says on the tin.

devtools::install_github("FCrSTATS/SBpitch")

## wsldata is StatsBombData, pull new data in a similar manner

library(SBpitch)

passes = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name) &
           player.id==4641) %>% #1
  filter(pass.end_location.x>=102 & pass.end_location.y<=62 &
           pass.end_location.y>=18) #2

create_Pitch() +
  geom_segment(data = passes, aes(x = location.x, y = location.y,
                                  xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.5, colour = "purple", arrow =
                 arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + #3
  labs(title = "Fran Kirby, Completed Box Passes", subtitle = "WSL,
  2020-21") + #4
  scale_y_reverse() + #5
  coord_fixed(ratio = 105/100) #6

#1: Pull some of the FA WSL data of your choice and call it
# ʻwsldataʼ for us to work with here. Then we can filter to Fran
# Kirbyʼs passes. is.na(pass.outcome.name) filters to only
# completed passes.

#2: Filtering to passes within the box. The coordinates for pitch
# markings in SBD can be found in our event spec.

#3: This creates an arrow from one point (location.x/y, the start
# part of the pass) to an end point (pass.end_location.x/y, the end
# of the pass). Lineend, size and length are are all customisation
# options for the arrow.

#4: Creates a title and a subtitle for the plot. You can also add
# captions using caption =, along with other options.

#5: Reverses the y axis. Otherwise the data would be plotted on
# the wrong side of the pitch.

#6: Fixes the plot to a certain aspect ratio of your choice, so it
# doesnʼt look stretched.

# E) Useful StatsBombR Functions

# allclean() - Mentioned previously but to elucidate: this extrapolates 
# lots of new, helpful columns from the pre-existing columns. For example, 
# it takes the location column and splits it up into separate x/y columns. 
# It also extracts freeze frame data and goalkeeper information. 
# Make sure to use.

# get.playerfootedness() - Gives you a playerʼs assumed preferred foot using 
# our pass footedness data.

# get.opposingteam() - Returns an opposing team column for each team in each 
# match.

# get.gamestate() - Returns information for how much time each team spent 
# in various game states (winning/drawing/losing) for each match.

# annotate_pitchSB() - Our own solution for plotting a pitch with ggplot.

# B) Doing More With StatsBomb Data In R

# More Data Use Cases

# Use Case 5: xG Assisted, Joining, and xG+xGA - 

# An example of how to create and then plot custom metrics with the data, 
# creating xG Assisted in a dataframe using ʻjoiningʼ and then creating an 
# xG + xG Assisted plot.

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
  filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE) #5

#1 Filtering the data to just shots, as they are the only events with xG
# values.

#2 Select() allows you to choose which columns you want to, well,
# select, from your daata, as not all are always necessary - especially with
# big datasets. First we are selecting the shot.key_pass_id column, which
# is a variable attached to shots that is just the ID of the pass that created
# the shot. You can also rename columns within select() which is what we
# are doing with xGA = shot.statsbomb_xg. This is so that, when we join it
# with the passes, it already has the correct name.

#3 left_join() lets you combine the columns from two different DFs by
# using two columns within either side of the join as reference keys. So in
# this example we are taking our initial DF (ʻeventsʼ) and joining it with the
# one we just made (ʻxGAʼ). The key is the by = c("id" = "shot.key_pass_id")
# part, this is saying ʻjoin these two DFs on instances where the id column
# in events matches the ʻshot.key_pass_idʼ column in xGAʼ. So now the
# passes have the xG of the shots they created attached to them under the
# new column ʻxGAʼ.

#4 Again selecting just the relevant columns.

#5 Filtering our data down to just key passes/assists.

# Make a chart

# events = StatsBombData

player_xGA = shot_assists %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xGA = sum(xGA, na.rm = TRUE)) #1

player_xG = StatsBombData %>%
  filter(type.name=="Shot") %>%
  filter(shot.type.name!="Penalty" | is.na(shot.type.name)) %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE)) %>%
  left_join(player_xGA) %>%
  mutate(xG_xGA = sum(xG+xGA, na.rm =TRUE) ) #2

player_minutes = get.minutesplayed(StatsBombData)
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) #3

player_xG_xGA = left_join(player_xG, player_minutes) %>%
  mutate(nineties = minutes/90,
         xG_90 = round(xG/nineties, 2),
         xGA_90 = round(xGA/nineties,2),
         xG_xGA90 = round(xG_xGA/nineties,2) ) #4

chart <- player_xG_xGA %>%
  ungroup() %>%
  filter(minutes>=600) %>%
  top_n(n = 15, w = xG_xGA90) #5

chart <-chart %>%
  select(1, 9:10)%>%
  pivot_longer(-player.name, names_to = "variable", values_to = "value") %>%
  filter(variable=="xG_90" | variable=="xGA_90") #6

#1 Grouping by player and summing their total xGA for the season.

#2 Filtering out penalties and summing each player's xG, then joining with
#  the xGA and adding the two together to get a third combined column.

#3 Getting minutes played for each player. If you went through the earlier
#  data use cases in this guide you will have done this already.

#4 Joining the xG/xGA to the minutes, creating the 90s and dividing each stat
#  by the 90s to get xG per 90 etc.

#5 Here we ungroup as we need the data in ungrouped form for what we're
#  about to do. First we filter to players with a minimum of 600 minutes, 
#  just to get rid of notably small samples. Then we use top_n(). This 
#  filters your DF to the top *insert number of your choice here* based 
#  on a column you specify.So here we're filtering to the top 15 players
#  in terms of xG90+xGA90.

#6 The pivot_longer() function flattens out the data. It's easier to 
#  explain what that means if you see it first:
  
# Itʼs used the player.name as a reference point and creates separate
# rows for every variable that's left over. We then filter down to just the
# xG90 and xGA90 variables so now each player has a separate variable
# and value row for those two metrics.

# Now plot the data

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

#1: Two things are going on here that are different from your average
# bar chart. First is reorder(), which allows you reorder a variable along
# either axis based on a second variable. In this instance we are putting
# the player names on the x axis and reordering them by value - i.e the xG
# and xGA combined - meaning they are now in descending order from
# most to least combined xG+xGA. Second is that we've put the 'variable'
# on the bar fill. This allows us to put two separate metrics onto one bar
# chart and have them stack, as you will see below, by having them be
# separate fill colours.

#2: Everything within labs() and theme() is fairly self explanatory and is
# just what we have used internally. You can get rid of all this if you like
# and change it to suit your own design tastes.

#3: Here we are providing specific colour hex codes to the values 
# (so xG = red and xGA = blue) and then labelling them so they are named
# correctly on the chart's legend.

#4: Expand() allows you to expand the boundaries of the x or y axis, but
# if you set the values to (0,0) it also removes all space between the axis
# and the inner chart itself (if you're having a hard time envisioning that,
# try removing expand() and see what it looks like). Then we are setting
# the limits of the y axis so the longest bar on the chart isn't too close to
# the edge of the chart. 'max(chart$value) + 0.3' is saying 'take the max
# value and add 0.3 to make that the upper limit of the y axis'.

#5: Flipping the x axis and y axis so we have a nice horizontal bar chart
# rather than a vertical one.

#6: Reversing the legend so that the order of it matches up with the
# order of xG and xGA on the chart itself.

# Use Case 6: Graphing Shots On a Chart - 

# Heatmaps are one of the everpresents in football data. They are
# fairly easy to make in R once you get your head round how to do so, but can 
# be unintuitive without having it explained first.

# For this example we're going to do a defensive heatmap, looking at how often 
# teams make a % of their overall defensive actions in certain zones, then 
# comparing that % vs league average:

library(tidyverse)
heatmap = StatsBombData %>%mutate(location.x = ifelse(location.x>120, 120, location.x),
location.y = ifelse(location.y>80, 80, location.y),
location.x = ifelse(location.x<0, 0, location.x),
location.y = ifelse(location.y<0, 0, location.y)) #1
heatmap$xbin <- cut(heatmap$location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE )
heatmap$ybin <- cut(heatmap$location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE) #2

#1 Some of the coordinates in our data sit outside the bounds of the 
# pitch (you can see the layout of our pitch coordinates in our event spec,
# but it's 0-120 along the x axis and 0-80 along the y axis). This will cause 
# issue with a heatmap and give you dodgy looking zones outside the
# pitch. So what we're doing here is using ifelse() to say 'if a 
# location.x/y coordinate is outside the bounds that we want, then replace 
# it with one that's within the boundaries. If it is not outside the 
# bounds just leave it as is'.

#2 cut() literally cuts up the data how you ask it to. Here, we're 
# cutting along the x axis (from 0-120, again the length of our pitch 
# according to our coordinates in the spec) and the y axis (0-80), and 
# we're cutting them 'by' the value we feed it, in this case 20. So we're 
# splitting it up into buckets of 20. This creates 6 buckets/zones along 
# the x axis (120/20 = 6) and 4 along the y axis (80/20 = 4). This creates 
# the buckets we need to plot our zones.

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

#3: This is using those buckets to create the zones. Let's
# break it down bit-by-bit:
# - Filtering to only defensive events
# - Grouping by team and getting how many defensive events
# they made in total ( n() just counts every row that you ask it
# to, so here we're counting every row for every team - i.e
# counting every defensive event for each team)
# - Then we group again by team and the xbin/ybin to count
#   how many defensive events a team has in a given bin/zone -
#   that's what 'bin_DA = n()' is doing. 'total_DA = max(total_DA),'
#   is just grabbing the team totals we made earlier. 
#   'bin_pct = bin_DA/total_DA,' is dividing the two to see what percentage
# of a team's overall defensive events were made in a given
# zone. The 'location.x = median(location.x/y)' is doing what it
# says on the tin and getting the median coordinate for each
# zone. This is used later in the plotting.
# - Then we ungroup and mutate to find the league average for
# each bin, followed by grouping by team/bin again
# subtracting the league average in each bin from each team's
# % in those bins to get the difference.

# Now onto the plotting. For this please install the package 'grid' 
# if you do not have it, and load it in. You could use a package like 
# 'ggsoccer' or 'SBPitch' for drawing the pitch, but for these
# purposes it's helpful to try and show you how to create your own 
# pitch, should you want to:

library(grid)
defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51",
                             "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
                             "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                             "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd",
                             "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", "#798590",
                             "#697785", "#526173", "#435367", "#3a4b60", "#2e4257", "#1d3048",
                             "#11263e", "#11273e", "#0d233a", "#020c16") #1

#1: These are the colours we'll be using for our heatmap later on.

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

# Bear in mind this next section of code--on
# this slide and the next--should be pasted into
# the console in one block. Be careful to do this
# when entering the code.
  
#2: 'geom_bin2d' is what will create the
# heatmap itself. We've set the binwidths to 20
# as that's what we cut the pitch up into earlier
# along the x and y axis. Feeding 'div_vs_ave' to
# 'fill' and 'group' in the ggplot() will allow us to
# colour the heatmaps by that variable.

#3: Everything up to here is what is drawing
# the pitch. There's a lot going on here and,
# rather than have it explained to you, just
# delete a line from it and see what disappears
# from the plot. Then you'll see which line is
# drawing the six-yard-box, which is drawing
# the goal etc.

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

# 4: Again more theming. You can change this to be
# whatever you like to fit your aesthetic preferences.

# 5: Reversing the y axis so the pitch is the correct way
# round along that axis (0 is left in SBD coordinates, but
# starts out as right in ggplot).

# 6: Here we're setting the parameters for the fill
# colouring of heatmaps. First we're feeding the
# 'defensiveactivitycolors' we set earlier into the 'colours'
# parameter, 'trans = "reverse"' is there to reverse the
# output so red = high. 'labels = scales::percent_format(accuracy = 1)' 
# formats the text on the legend as a percentage rather than a raw number
# and 'limits = c(0.03, -0.03)' sets the limits of the chart to
# 3%/-3% (reversed because of the previous trans = reverse).

#7: Setting the title and subtitle of the chart.

# 8: 'coord_fixed()' allows us to set the aspect ratio of the
# chart to our liking. Means the chart doesn't come out
# looking all stretched along one of the axes.

annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                               length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                  xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9
  facet_wrap(~team.name)+ #10
  guides(fill = guide_legend(reverse = TRUE)) #11

#9: This is what the grid package is used for. It's
# drawing the arrow across the pitches to indicate
# direction of play. There's multiple ways you could
# accomplish though, up to you how you do it.

#10: 'facet_wrap()' creates separate 'facets' for
# your chart according to the variable you give it.
# Without it, we'd just be plotting every team's
# numbers all at once on chart. With it, we get
# every team on their own individual pitch.

#11: Our previous trans = reverse also reverses
# the legend, so to get it back with the positive
# numbers pointing upwards we can re-reverse it.
  
# Use Case 7: Shot Maps - 

# Another of the quintessential football visualisations, shot maps come in many
# shapes and sizes with an inconsistent overlap in design language between them. 
# This version will attempt to give you the basics.
  
# Another of the quintessential football visualisations, shot maps come in many 
# shapes and sizes with an inconsistent overlap in design language between them.
# This version will attempt to give you the basics, let you get to grip with how
# to put one of these together so that if you want to elaborate or make any of 
# your own changes you can explore outwards from it. Be forewarned though -
# the options for what makes a good, readable shot map are surprisingly small 
# when you get into visualising it!

shots = StatsBombData %>%
  filter(type.name=="Shot" & (shot.type.name!="Penalty" | is.na(shot.type.name)) & player.name=="Samantha May Kerr") #1
shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", "#FCDC5F",
                     "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000") #2

#1: Simple filtering, leaving out penalties. Choose any player you like of course.

#2: Much like the defensive activity colours earlier, these will set the colours 
# for our xG values.

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
  
# Again bear in mind that this next set of ggplot code (on this slide
# and the next two) should be pasted in one block.

# #3: Here's where the actual plotting of shots comes in, via
# geom_point. We're using the the xG values as the fill and the body
# part for the shape of the points. This could reasonably be
# anything though. You could even add in colour parameters which
# would change the colour of the outline of the shape.
  
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
  
# #4: Again titling. This can be done dynamically so that it
# changes according to the player/season etc but we will
# leave that for now. Feel free to explore for youself though.

# #5: Same as last time but worth pointing out that
# 'name' allows you to change the title of a legend from
# within the gradient setting.

scale_shape_manual(values = c("Head" = 21, "Right Foot" = 23, "Left Foot" = 24), name ="") + #6

guides(fill = guide_colourbar(title.position = "top"),
         shape = guide_legend(override.aes = list(size = 7, fill = "black"))) + #7
coord_flip(xlim = c(85, 125)) #8

# #6: Setting the shapes for each body part name. The shape numbers correspond to ggplot's
# pre-set shapes, which you can find here. The shapes numbered 21 and up are the ones
# which have inner colouring (controlled by fill) and outline colouring (controlled by colour) 
# so that's why those have been chosen here. oob=scales::squish takes any values that are
# outside the bounds of our limits and squishes them within them.
# 
# #7: guides() allows you to alter the legends for shape, fill and so on. Here we are changing
# the the title position for the fill so that it is positioned above the legend, as well as 
# changing the size and colour of the shape symbols on that legend.
# 
# #8: coord_flip() does what it says on the tin - switches the x and y axes. xlim allows us 
# to set boundaries for the x axis so that we can show only a certain part of the pitch, 
# giving us:



