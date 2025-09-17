# Getting Started with superNetballR
# https://stevelane.github.io/superNetballR/articles/getting-started.html

# Install the relevant libraries

# devtools::install_github("stevelane/superNetballR")
library(tidyverse)
library(superNetballR)

# To download statistics from a single match, you use 
# the downloadMatch function. As an example, the following 
# code will download the match from round 5, game 3:

round5_game3 <- downloadMatch("10083", 5, 3)

class(round5_game3)

names(round5_game3)

# Tidy the match data

tidied_match <- tidyMatch(round5_game3)
tidied_match

# Tidying player statistics using the tidyPlayers function:

tidied_players <- tidyPlayers(round5_game3)
tidied_players

# Season Data and Ladders

data(season_2017)
season_2017

# Ladders

ladder <- ladders(season_2017, round_num = 14)
ladder


