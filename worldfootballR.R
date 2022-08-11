# worldfootballR

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
league_url <- fb_league_urls(country = "ENG", gender = "M",
teams <- fb_teams_urls(league_url)

multiple_playing_time <- fb_team_player_stats(team_urls= teams, stat_type= "playing_time")


### 5) Player Match Logs

ederson_summary <- fb_player_match_logs("https://fbref.com/en/players/3bb7b8b4/Ederson", season_end_year = 2021, stat_type = 'summary')
dplyr::glimpse(ederson_summary)