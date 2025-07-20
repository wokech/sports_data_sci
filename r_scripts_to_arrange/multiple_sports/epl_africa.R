#  African players in Premier League (July 2023)

#  To date, there are 56 players registered under an African nation on the 
# Premier League’s official site. Only two of the 20 clubs in the English 
# top flight don’t have a player from the continent.


# https://acefootball.com/football-news/african-players-in-premier-league/
# Add FIFA ranking and market value for the players

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)
        
# (B) Get the data from Ace Football

link_epl <- "https://acefootball.com/football-news/african-players-in-premier-league/"
af_soccer_epl <- link_epl %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(fill = TRUE)

algeria <- af_soccer_epl[[1]]
algeria <- algeria %>%
  mutate(Country = "Algeria")

b_faso <- af_soccer_epl[[2]]
b_faso <- b_faso %>%
  mutate(Country = "Burkina Faso")

cameroon <- af_soccer_epl[[3]]
cameroon <- cameroon %>%
  mutate(Country = "Cameroon")

dr_congo <- af_soccer_epl[[4]]
dr_congo <- dr_congo %>%
  mutate(Country = "Democratic Republic of the Congo")

egypt <- af_soccer_epl[[5]]
egypt <- egypt %>%
  mutate(Country = "Egypt")

gabon <- af_soccer_epl[[6]]
gabon <- gabon %>%
  mutate(Country = "Gabon")

ghana <- af_soccer_epl[[7]]
ghana <- ghana %>%
  mutate(Country = "Ghana")

ivory_coast <- af_soccer_epl[[8]]
ivory_coast <- ivory_coast %>%
  mutate(Country = "Ivory Coast")

mali <- af_soccer_epl[[9]]
mali <- mali %>%
  mutate(Country = "Mali")

morocco <- af_soccer_epl[[10]]
morocco <- morocco %>%
  mutate(Country = "Morocco")

nigeria <- af_soccer_epl[[11]]
nigeria <- nigeria %>%
  mutate(Country = "Nigeria")

senegal <- af_soccer_epl[[12]]
senegal <- senegal %>%
  mutate(Country = "Senegal")

s_leone <- af_soccer_epl[[13]]
s_leone <- s_leone %>%
  mutate(Country = "Sierra Leone")

s_africa <- af_soccer_epl[[14]]
s_africa <- s_africa %>%
  mutate(Country = "South Africa")

tunisia <- af_soccer_epl[[15]]
tunisia <- tunisia %>%
  mutate(Country = "Tunisia")

zimbabwe <- af_soccer_epl[[16]]
zimbabwe <- zimbabwe %>%
  mutate(Country = "Zimbabwe")

# Join all the country tables together by row

epl_africa_table_1 <- bind_rows(algeria, b_faso, cameroon, dr_congo, egypt, gabon,
                                gabon, ivory_coast, mali, morocco, nigeria,
                                senegal, s_leone, s_africa, tunisia, zimbabwe)

epl_africa_table_2 <- af_soccer_epl[[17]]

