library(tidyverse)
library(sp)
library(readxl)

# load pbp detail
fullpbp <- read.csv("23_PBP_WHKYHAC_SPORTLOGIQ.csv")

fullpbp <- fullpbp %>%
  # Create Game ID to distinguish between same opponents on different days
  mutate(gameid = paste0(gsub("-","",date),gsub("-","",game))) %>%
  # Remove year redundancy Game ID
  mutate(gameid = substring(gameid,3))

# Return where a possessions goal = 1
goalposlist <- (fullpbp %>%
  group_by(gameid,currentpossession) %>%
  summarise_at(vars(goal), list(scoringpos = sum)) %>%
  filter(scoringpos > 0))

# Link noted goal possessions to entire possession in fullpbp
join_scoringpos <- fullpbp %>%
  left_join(goalposlist, by = c('gameid'='gameid','currentpossession'='currentpossession')) %>%
  mutate(scoringpos = coalesce(scoringpos, 0))

# Return desired repeated values where Goal = 1, grouped by GameID and CurrentPossession
# For ease of use in Tableau
add_goalinfo <- fullpbp %>%
  filter(goal == 1) %>%
  group_by(gameid,currentpossession) %>%
  mutate(goalscorer = player) %>%
  mutate(goalscorer_team = teamname) %>%
  mutate(goalagainst_team = opposingteamname) %>%
  mutate(homeaway = ishomegame) %>%
  mutate(strength = strengthstate) %>%
  summarise(gameid, currentpossession, homeaway, goalscorer, goalscorer_team, goalagainst_team, strength)

# Join noted goal play info to entire possession in fullpbp/join_scoringpos
join_goalinfo <- join_scoringpos %>%
  left_join(add_goalinfo, by = c('gameid'='gameid','currentpossession'='currentpossession'))

# Generate per-possession sequential numbers for ordering pbp events
seq_num <- join_goalinfo %>%
  group_by(gameid,currentpossession) %>%
  mutate(sequence = row_number())

# Generate running total of goals by GOALSCORER
add_player_goal_totals <- seq_num %>%
  filter(scoringpos == 1) %>%
  group_by(goalscorer) %>%
  mutate(player_goal_num=cumsum(!duplicated(cbind(gameid, currentpossession))))

# Generate running total of goals by TEAM
add_team_goal_totals <- add_player_goal_totals %>%
  filter(scoringpos == 1) %>%
  group_by(goalscorer_team) %>%
  mutate(team_goal_num=cumsum(!duplicated(cbind(gameid, currentpossession))))

# Normalize coordinates relative to home/away status
fix_coords <- add_team_goal_totals %>%
  mutate(xadjcoord = ifelse(homeaway != ishomegame,(xadjcoord*-1),xadjcoord)) %>%
  mutate(yadjcoord = ifelse(homeaway != ishomegame,(yadjcoord*-1),yadjcoord))

# Generate danger polygons
# Coordinates based on An Nguyen shot plotter @ https://shot-plotter.netlify.app/
scoring_plays <- fix_coords %>%
  # high danger = between faceoff dots to top of trapezoid 
  mutate(event_highdanger = point.in.polygon(xadjcoord,
                                             yadjcoord,
                                             c(70,70,90,90),
                                             c(20,-20,-11,11),
                                             mode.checked = FALSE)) %>%
  # medium danger = any other area in offensive zone
  mutate(event_meddanger = point.in.polygon(xadjcoord,
                                            yadjcoord,
                                            c(25,25,100,100),
                                            c(42,-42,-42,42),
                                            mode.checked = FALSE)) %>%
  # low danger = outside of offensive zone
  mutate(event_lowdanger = point.in.polygon(xadjcoord,
                                           yadjcoord,
                                           c(-100,-100,100,100),
                                           c(42,-42,-42,42),
                                           mode.checked = FALSE)) %>%
  mutate(danger_level = ifelse(event_highdanger > 0,"high",
                               ifelse(event_meddanger > 0,"medium",
                                      ifelse(event_lowdanger > 0,"low","error"))))

# Hard code a single player name to remedy Tableau's stubbornness
scoring_plays <- scoring_plays %>%
  mutate(goalscorer = ifelse(goalscorer == 'Kristin ONeill',"Kristin ONeill",goalscorer))

# Create .csv file for Tableau import
write.csv(scoring_plays,"scoringplays.csv")

##### Player Stats #####

# Count of each eventname by player for total stats
player_stats <- scoring_plays %>%
  group_by(player,eventname) %>% 
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame()

# Hard code a single player name to remedy Tableau's stubbornness
player_stats <- player_stats %>%
  mutate(player = ifelse(player == 'Kristin ONeill',"Kristin ONeill",player))

# Create .csv file for Tableau import
write.csv(player_stats,"playerstats.csv")

##### Player Bio/Details #####

summary_data <- read_xlsx("23_SUMMARY_WHKYHAC_SPORTLOGIQ.xlsx")

# Player details for viz header
player_details <- summary_data %>%
  distinct(Player,Position,jerseyNum) 

# Emily Curlett and Kristen Richards appear as both D and F
find_duplicates <- player_details %>%
  group_by(Player) %>%
  tally()

# Apply position exception to Curlett and Richards
player_details <- player_details %>%
  left_join(find_duplicates, by = c('Player'='Player')) %>%
  mutate(Position = ifelse(n > 1,"D/F",Position)) %>%
  distinct(Player,Position,jerseyNum)

# Hard code a single player name to remedy Tableau's stubbornness
player_details <- player_details %>%
  mutate(Player = ifelse(Player == "Kristin O’Neill","Kristin ONeill",Player))

# Create .csv files for Tableau import
write.csv(player_details,"playerdetails.csv")