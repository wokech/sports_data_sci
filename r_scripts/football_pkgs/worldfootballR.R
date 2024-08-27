# worldfootballR
# Jason Zivkovic
# https://jaseziv.github.io/worldfootballR/

## Installation

install.packages("worldfootballR")

library(worldfootballR)

###########################################################################

## Extracting data from Transfermarkt

############################################################################

### 1) Join FBref and Transfermarkt data

mapped_players <- player_dictionary_mapping()
dplyr::glimpse(mapped_players)

### Transfermarkt Helper Functions

### 2) Team URLs

team_urls <- tm_league_team_urls(country_name = "England", start_year = 2020)
# if it's not a league in the stored leagues data in worldfootballR_data repo:
league_one_teams <- tm_league_team_urls(start_year = 2020, league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")

### 3) Player URLs

tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")

### 4) Staff URLs

# get a list of team URLs for the EPL 2021/22 season
epl_teams <- tm_league_team_urls(country_name = "England", start_year = 2021)
# get all EPL managers for the 2021/22 season
epl_managers <- tm_team_staff_urls(team_urls = epl_teams, staff_role = "Manager")

# get all EPL goal keeping coaches for the 2021/22 season
epl_gk_coaches <- tm_team_staff_urls(team_urls = epl_teams, staff_role = "Goalkeeping Coach")

## League Season-Level Data

### 1) League Table by Matchdays

#----- to get the EPL table after matchday 1 of the 20/21 season: -----#
epl_matchday_1_table <- tm_matchday_table(country_name="England", start_year="2020", matchday=1)
dplyr::glimpse(epl_matchday_1_table)


# #----- to get the EPL table after each matchdays from matchday 1 to matchday 35 of the 20/21 season: -----#
epl_matchday_1to35_table <- tm_matchday_table(country_name="England", start_year="2020", matchday=c(1:35))

#----- to get the League One table after each matchdays from matchday 1 to matchday 5 of the 20/21 season: -----#
league_one_matchday_1_table <- tm_matchday_table(start_year="2020", matchday=1:5,
                                                 league_url="https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")
dplyr::glimpse(league_one_matchday_1_table)

### 2) League Debutants

# Laliga players making their LaLiga debut in 2021/2022
laliga_debutants <- tm_league_debutants(country_name = "Spain", debut_type = "league", debut_start_year = 2021, debut_end_year = 2021)
dplyr::glimpse(laliga_debutants)

# English League One players making their PRO debuts in 2021/2022
league_one_PRO_debutants <- tm_league_debutants(country_name = "", league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3", debut_type = "pro", debut_start_year = 2021, debut_end_year = 2021)
dplyr::glimpse(league_one_PRO_debutants)

### 3) Expiring Contracts

#----- LaLiga players with expiring contracts in 2022: -----#
laliga_expiring <- tm_expiring_contracts(country_name = "Spain", contract_end_year = 2023)
dplyr::glimpse(laliga_expiring)

### 4) League Injuries

# to get all current injuries for LaLiga
laliga_injuries <- tm_league_injuries(country_name = "Spain")
dplyr::glimpse(laliga_injuries)

#----- Can even do it for non-standard leagues - get all current injuries for League One in England
league_one_injuries <- tm_league_injuries(country_name = "",
                                                league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")

## Team Data
 
### 1) Transfer activity by team
 
#----- for one team: -----#
bayern <- tm_team_transfers(team_url = "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020", transfer_window = "all")
dplyr::glimpse(bayern)
 
#----- or for multiple teams: -----#
team_urls <- tm_league_team_urls(country_name = "England", start_year = 2020)
epl_xfers_2020 <- tm_team_transfers(team_url = team_urls, transfer_window = "all")

### 2) Squad Player Stats

#----- for one team: -----#
bayern <- tm_squad_stats(team_url = "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020")
dplyr::glimpse(bayern)

#----- or for multiple teams: -----#
team_urls <- tm_league_team_urls(country_name = "England", start_year = 2020)
epl_team_players_2020 <- tm_squad_stats(team_url = team_urls)

### 3) Player Valuations

#----- Can do it for a single league: -----#
a_league_valuations <- get_player_market_values(country_name = "Australia",
                                                start_year = 2021)
dplyr::glimpse(a_league_valuations)

#----- Can also do it for multiple leagues: -----#
big_5_valuations <- get_player_market_values(country_name = c("England", "Spain", "France", "Italy", "Germany"),
                                       start_year = 2021)

#----- Can also do it for non standard leagues: -----#
league_one_valuations <- get_player_market_values(country_name = "",
                                      start_year = 2021,
                                      league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")


## Player Data

### 1) Player Bios

#----- for a single player: -----#
hazard_bio <- tm_player_bio(player_url = "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202")
dplyr::glimpse(hazard_bio)

#----- for multiple players: -----#
# # can make use of a tm helper function:
burnley_player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020")
# # then pass all those URLs to the tm_player_bio
burnley_bios <- tm_player_bio(player_urls = burnley_player_urls)

### 2) Player Injury History

#----- for a single player: -----#
hazard_injuries <- tm_player_injury_history(player_urls = "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202")
dplyr::glimpse(hazard_injuries)

#----- for multiple players: -----#
# # can make use of a tm helper function:
burnley_player_urls <- tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2021")
# # then pass all those URLs to the tm_player_bio
burnley_player_injuries <- tm_player_injury_history(player_urls = burnley_player_urls)

## Club Staff Data

### 1) Club Staff History

# get a list of team URLs for the EPL 2021/22 season
epl_teams <- tm_league_team_urls(country_name = "England", start_year = 2021)
#----- then use the URLs to pass to the function, and select the role you wish to see results for: -----#
club_manager_history <- tm_team_staff_history(team_urls = epl_teams, staff_role = "Manager")
dplyr::glimpse(club_manager_history)

### 2) Staff Member’s History

# get a list of team URLs for the EPL 2021/22 season
epl_teams <- tm_league_team_urls(country_name = "England", start_year = 2021)

# get all EPL goal keeping coaches for the 2021/22 season
epl_gk_coaches <- tm_team_staff_urls(team_urls = epl_teams[1:3], staff_role = "Goalkeeping Coach")

# then you can pass these URLs to the function and get job histories for the selected staff members
epl_gk_coach_job_histories <- tm_staff_job_history(staff_urls = epl_gk_coaches)
dplyr::glimpse(epl_gk_coach_job_histories)

###########################################################################

## Extracting data from FBref

###########################################################################

### 1) Join FBref and Transfermarkt data

mapped_players <- player_dictionary_mapping()
dplyr::glimpse(mapped_players)

### FBref Helper Functions

### 1) League URLs

fb_league_urls(country = "ENG", gender = "M", season_end_year = 2021, tier = '2nd')

### 2) Team URLs

fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")

### 3) Player URLs

fb_player_urls("https://fbref.com/en/squads/fd962109/Fulham-Stats")

### 4) Get match urls

epl_2021_urls <- get_match_urls(country = "ENG", gender = "M", season_end_year = 2021, tier="1st")

## League Season-Level Data

### 1) Get Season Team Stats

#----- function to extract season teams stats -----#
prem_2020_shooting <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2020", tier = "1st", stat_type = "shooting")
dplyr::glimpse(prem_2020_shooting)

#----- to get shooting stats for the English Championship: -----#
championship_2020_shooting <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2020", tier = "2nd", stat_type = "shooting")

#----- Can also run this for multiple leagues at a time: -----#
multiple_2020_shooting <- get_season_team_stats(country = c("USA", "NED"),
                                                gender = "M", season_end_year = 2020, 
                                                tier = "1st", stat_type = "shooting")


### 2) The Big 5 Euro Leagues

#----- Get data for big five leagues for TEAMS -----#
big5_team_shooting <- fb_big5_advanced_season_stats(season_end_year= c(2019:2021), stat_type= "shooting", team_or_player= "team")
dplyr::glimpse(big5_team_shooting)

#----- Get data for big five leagues for PLAYERS -----#
big5_player_shooting <- fb_big5_advanced_season_stats(season_end_year= c(2019:2021), stat_type= "shooting", team_or_player= "player")
dplyr::glimpse(big5_player_shooting)


## Match-Level Data

### 1) Get match results

# function to extract Serie A match results data
serieA_2020 <- get_match_results(country = "ITA", gender = "M", season_end_year = 2020, tier = "1st")
dplyr::glimpse(serieA_2020)

# for international friendlies:
get_match_results(country = "", gender = "M", season_end_year = 2018, tier = "", non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")

### 2) More than one league season

big_5_2020_results <- get_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"),
                                        gender = "M", season_end_year = 2020, tier = "1st")

### 3) Get match report

# function to extract match report data
liv_mci_2020 <- get_match_report(match_url = "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League")
dplyr::glimpse(liv_mci_2020)

### 4) Get match summaries

# function to extract match summary data
liv_mci_2020_summary <- get_match_summary(match_url = "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League")
dplyr::glimpse(liv_mci_2020_summary)

### 5) Get match lineups

# function to extract match lineups
liv_mci_2020_lineups <- get_match_lineups(match_url = "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League")
dplyr::glimpse(liv_mci_2020_lineups)

### 6) Get shooting and shot creation events

#----- Get shots data for a single match played: -----#
shot_one_match <- get_match_shooting(match_url = "https://fbref.com/en/matches/a3eb7a37/Sheffield-United-Wolverhampton-Wanderers-September-14-2020-Premier-League")

#----- Can also extract for multiple matches at a time: -----#
test_urls_multiple <- c("https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                       "https://fbref.com/en/matches/9cbccb37/Dijon-Angers-August-22-2020-Ligue-1",
                       "https://fbref.com/en/matches/f96cd5a0/Lorient-Strasbourg-August-23-2020-Ligue-1")
shot_multiple_matches <- get_match_shooting(test_urls_multiple)


### 7) Get advanced match statistics

test_urls_multiple <- c("https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                        "https://fbref.com/en/matches/9cbccb37/Dijon-Angers-August-22-2020-Ligue-1")

advanced_match_stats <- get_advanced_match_stats(match_url = test_urls_multiple, stat_type = "possession", team_or_player = "player")
dplyr::glimpse(advanced_match_stats)


test_urls_multiple <- c("https://fbref.com/en/matches/c0996cac/Bordeaux-Nantes-August-21-2020-Ligue-1",
                        "https://fbref.com/en/matches/9cbccb37/Dijon-Angers-August-22-2020-Ligue-1")

advanced_match_stats_team <- get_advanced_match_stats(match_url = test_urls_multiple, stat_type = "passing_types", team_or_player = "team")
dplyr::glimpse(advanced_match_stats_team)


## Team-Level Data

### 1) Match Results by team

#----- for single teams: -----#
man_city_2021_url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
man_city_2021_results <- get_team_match_results(man_city_2021_url)
dplyr::glimpse(man_city_2021_results)

#----- get all team URLs for a league: -----#
epl_2021_team_urls <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
epl_2021_team_results <- get_team_match_results(team_url = team_urls)


### 2) Stat Logs for Season

# can do it for one team:
man_city_url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
man_city_logs <- fb_team_match_log_stats(team_urls = man_city_url, stat_type = "passing")

dplyr::glimpse(man_city_logs)

# or multiple teams:
urls <- c("https://fbref.com/en/squads/822bd0ba/Liverpool-Stats",
          "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats")

shooting_logs <- fb_team_match_log_stats(team_urls = urls, stat_type = "shooting")


## Player-Level Data


### 1) Get Player Scouting Report

# TO GET THE LAST 365 DAYS REPORT:
scout <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi",
                                   pos_versus = "primary") %>% 
dplyr::filter(scouting_period == "Last 365 Days")

#----- Get scouting report for the players primary position (first position listed in fbref): -----#
messi_primary <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi", pos_versus = "primary")
dplyr::glimpse(messi_primary)

#----- Get scouting report for the players secondary position (second position listed in fbref): -----#
messi_secondary <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi", pos_versus = "secondary")
dplyr::glimpse(messi_secondary)


### 2) Get Player Season Stats

#----- can use for a single player: -----#
mo_shooting <- fb_player_season_stats("https://fbref.com/en/players/e342ad68/Mohamed-Salah", stat_type = 'shooting')
dplyr::glimpse(mo_shooting)

#----- Or for multiple players at a time: -----#
multiple_playing_time <- fb_player_season_stats(player_url = c("https://fbref.com/en/players/d70ce98e/Lionel-Messi",
                                               "https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo"),
                                stat_type = "playing_time")


### 3) The Big 5 Euro League Players

big5_player_possession <- fb_big5_advanced_season_stats(season_end_year= 2021, stat_type= "possession", team_or_player= "player")
dplyr::glimpse(big5_player_possession)

### 4) Player Season Statistics for a teams season

#----- to get stats for just a single team: -----#
fleetwood_standard_stats <- fb_team_player_stats(team_urls= "https://fbref.com/en/squads/d6a369a2/Fleetwood-Town-Stats", stat_type= 'standard')
dplyr::glimpse(fleetwood_standard_stats)

#----- Can even get stats for a series of teams: -----#
#league_url <- fb_league_urls(country = "ENG", gender = "M",
#teams <- fb_teams_urls(league_url)

#multiple_playing_time <- fb_team_player_stats(team_urls= teams, stat_type= "playing_time")


### 5) Player Match Logs

# ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/3bb7b8b4/Ederson", season_end_year = 2021, stat_type = 'summary')
# dplyr::glimpse(ederson_summary)


###########################################################################

## Extracting data from Understat

############################################################################

### Understat Helper Functions

## 1) Team URLs

team_urls <- understat_team_meta(team_name = c("Liverpool", "Manchester City"))

## League Season-Level Data

### 1) Match Results

# to get the EPL results:
epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2020)
dplyr::glimpse(epl_results)

### 2) Season Shooting locations

ligue1_shot_location <- understat_league_season_shots(league = "Ligue 1", season_start_year = 2020)

## Match-Level Data

### 1) Match Shooting Locations

wba_liv_shots <- understat_match_shots(match_url = "https://understat.com/match/14789")
dplyr::glimpse(wba_liv_shots)

## Team Data

### 1) Team Shooting Locations

# for one team:
man_city_shots <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_City/2020")
dplyr::glimpse(man_city_shots)

### 2) Team Stat Breakdowns

#----- Can get data for single teams at a time: -----#
team_breakdown <- understat_team_stats_breakdown(team_urls = "https://understat.com/team/Liverpool/2020")
dplyr::glimpse(team_breakdown)


#----- Or for multiple teams: -----#
team_urls <- c("https://understat.com/team/Liverpool/2020",
              "https://understat.com/team/Manchester_City/2020")
team_breakdown <- understat_team_stats_breakdown(team_urls = team_urls)

## Player Data

### 1) Player Shooting Locations

raheem_sterling_shots <- understat_player_shots(player_url = "https://understat.com/player/618")
dplyr::glimpse(raheem_sterling_shots)

### 2) Team Player Season Stats

team_players <- understat_team_players_stats(team_url = c("https://understat.com/team/Liverpool/2020", "https://understat.com/team/Manchester_City/2020"))
dplyr::glimpse(team_players)



###########################################################################

library(worldfootballR)
library(dplyr)
library(tidyr)
## Extracting data from fotmob

############################################################################


### fotmob Helper Functions

## League Season-Level Data

### 1) Team Stats

epl_team_xg_2021 <- fotmob_get_season_stats(
  country = "ENG",
  league_name = "Premier League",
  season_name = "2020/2021",
  stat_name = "Expected goals",
  team_or_player = "team"
)

epl_team_xg_2021 %>%
  dplyr::select(
    league_id,
    league_name,
    season_id,
    season_name,
    team_id,
    team_name = participant_name,
    matches_played,
    xg = stat_value,
    g = sub_stat_value
  ) %>%
  dplyr::glimpse()

fotmob_get_season_stats(
  league_id = 47,
  season_name = "2020/2021",
  stat_name = "Expected goals",
  team_or_player = "team"
)

team_xgs_2021 <- fotmob_get_season_stats(
  country =        c("ITA",     "ESP"),
  league_name =    c("Serie A", "LaLiga"),
  season_name =    c("2020/2021", "2021/2022"),
  stat_name =      c("Expected goals", "xG conceded"),
  team_or_player = "team"
)

team_xgs_2021 %>% nrow()


m <- lubridate::month(Sys.Date())
if(m >= 1 && m <= 5) {
  fotmob_get_season_stats(
    league_id = 42,
    season_name = "2020/2021",
    stat_name = "Expected goals",
    team_or_player = "team"
  )
}


### 2) Player Stats

epl_player_xg_2021 <- fotmob_get_season_stats(
  country = "ENG",
  league_name = "Premier League",
  season = "2020/2021",
  stat_name = "Expected goals (xG)",
  team_or_player = "player"
)

epl_player_xg_2021 %>%
  dplyr::select(
    league_id,
    league_name,
    season_id,
    season_name,
    team_id,
    ## NOTE: particiant_id is a typo on behalf of fotmob! We leave it as is.
    player_id = particiant_id,
    player_name = participant_name,
    minutes_played,
    matches_played,
    xg = stat_value,
    g = sub_stat_value
  ) %>%
  dplyr::glimpse()


### 3) Match Results


league_matches <- fotmob_get_league_matches(
  country =     c("ENG",            "ESP"   ),
  league_name = c("Premier League", "LaLiga")
)

league_matches_unnested <- league_matches %>%
  dplyr::select(match_id = id, home, away) %>%
  tidyr::unnest_wider(c(home, away), names_sep = "_")
dplyr::glimpse(league_matches_unnested)


results <- fotmob_get_matches_by_date(date = c("20210925", "20210926"))
dplyr::glimpse(results)


results <- fotmob_get_matches_by_date("20220412")
results %>%
  dplyr::filter(name == "Champions League Final Stage", ccode == "INT")


### 4) Standings

league_tables <- fotmob_get_league_tables(
  country =     c("ENG",            "ESP"   ),
  league_name = c("Premier League", "LaLiga")
)
# or
# league_tables <- fotmob_get_league_tables(league_id = c(47, 87))

away_league_tables <- league_tables %>%
  dplyr::filter(table_type == "away")
dplyr::glimpse(away_league_tables)


m <- lubridate::month(Sys.Date())
if(m >= 1 && m <= 5) {
  cl_table <- fotmob_get_league_tables(league_id = 42)
  
  cl_table %>%
    dplyr::filter(table_type == "all") %>% 
    dplyr::glimpse()
}


## Match-Level Data


### 1) Match Shooting Locations


fotmob_matches <- c(3609994, 3610132)
match_details <- fotmob_get_match_details(fotmob_matches)
dplyr::glimpse(match_details)


### 2) Players


players <- fotmob_get_match_players(fotmob_matches)
dplyr::glimpse(players)


####################################################################################

# Extracting data from FBref for International Matches

####################################################################################

### Helper Functions

### 1) Get match urls

wc_2018_urls <- get_match_urls(country = "", gender = "M", season_end_year = 2018, tier = "", non_dom_league_url = "https://fbref.com/en/comps/1/history/World-Cup-Seasons")

friendly_int_2021_urls <- get_match_urls(country = "", gender = "M", season_end_year = 2021, tier = "", non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")

euro_2021_urls <- get_match_urls(country = "", gender = "M", season_end_year = 2021, tier = "", non_dom_league_url = "https://fbref.com/en/comps/676/history/European-Championship-Seasons")

copa_2019_urls <- get_match_urls(country = "", gender = "M", season_end_year = 2019, tier = "", non_dom_league_url = "https://fbref.com/en/comps/685/history/Copa-America-Seasons")


## Match-Level Data

### 1) Get match results

# euro 2016 results
euro_2016_results <- get_match_results(country = "", gender = "M", season_end_year = 2016, tier = "", non_dom_league_url = "https://fbref.com/en/comps/676/history/European-Championship-Seasons")

# 2019 Copa America results:
copa_2019_results <- get_match_results(country = "", gender = "M", season_end_year = 2019, non_dom_league_url = "https://fbref.com/en/comps/685/history/Copa-America-Seasons")

# for international friendlies:
international_results <- get_match_results(country = "", gender = "M", season_end_year = 2021, tier = "", non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")


### 2) Get match report

# function to extract match report data for 2018 world cup
wc_2018_report <- get_match_report(match_url = wc_2018_urls)
# function to extract match report data for 2021 international friendlies
friendlies_report <- get_match_report(match_url = friendly_int_2021_urls)

### 3) Get match summaries

# first get the URLs for the 2016 Euros
euro_2016_match_urls <- get_match_urls(country = "", gender = "M", season_end_year = 2016, tier = "", non_dom_league_url = "https://fbref.com/en/comps/676/history/European-Championship-Seasons")

# then pass these to the function to get match summaries:
euro_2016_events <- get_match_summary(euro_2016_match_urls)

### 4) Get match lineups

# function to extract match lineups
copa_2019_lineups <- get_match_lineups(match_url = copa_2019_urls)

### 5) Get shooting and shot creation events

shots_wc_2018 <- get_match_shooting(wc_2018_urls)

### 6) Get advanced match statistics

advanced_match_stats_player <- get_advanced_match_stats(match_url = wc_2018_urls, stat_type = "possession", team_or_player = "player")

advanced_match_stats_team <- get_advanced_match_stats(match_url = wc_2018_urls, stat_type = "passing_types", team_or_player = "team")



###########################################################################################

# Load Scraped Data Functions

###########################################################################################

library(worldfootballR)
library(dplyr)


## Load FBref

### 1) Load FBref match results

eng_match_results <- load_match_results(country = "ENG", gender = c("M", "F"), season_end_year = c(2020:2022), tier = "1st")
dplyr::glimpse(eng_match_results)

### 2) Load FBref match results for Cups and International Comps

load_match_comp_results()

cups <- c("FIFA Women's World Cup","FIFA World Cup")
world_cups <- load_match_comp_results(comp_name = cups)
dplyr::glimpse(world_cups)

# 3) Load FBref big 5 league advanced season stats

all_season_player <- load_fb_big5_advanced_season_stats(stat_type = "defense", team_or_player = "player")
current_season_player <- load_fb_big5_advanced_season_stats(season_end_year = 2022, stat_type = "defense", team_or_player = "player")

all_season_team <- load_fb_big5_advanced_season_stats(stat_type = "defense", team_or_player = "team")
current_season_team <- load_fb_big5_advanced_season_stats(season_end_year = 2022, stat_type = "defense", team_or_player = "team")

## Load Understat

### 1) Load League Shots

serie_a_shot_locations <- load_understat_league_shots(league = "Serie A")
dplyr::glimpse(serie_a_shot_locations)


## Load fotmob

### 1) Load fotmob Big 5 Match Shots

epl_match_details <- load_fotmob_match_details(
  country = "ENG",
  league_name = "Premier League"
)
## or
load_fotmob_match_details(league_id = 47)
dplyr::glimpse(epl_match_details)

## multiple leagues at once
epl_ll_match_details <- load_fotmob_match_details(league_id = c(47, 87))


epl_matches <- load_fotmob_matches_by_date(
  country = "ENG",
  league_name = "Premier League"
)
dplyr::glimpse(epl_matches)


## multiple leagues at once
epl_ll_matches <- load_fotmob_matches_by_date(league_id = c(47, 87))

