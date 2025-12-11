library(tidyverse)
library(readxl)

pbp_data = read.csv("play_by_play_2024.csv")
team_stats_2023 = read_excel("team_stats_2023.xlsx")

fourth_down_plays = pbp_data %>% 
  filter(play_type %in% c("pass", "run"), !is.na(epa)) %>% 
  filter(down == "4")

# Create a table with ONLY the offensive stats.
offensive_stats = team_stats_2023 %>%
  select(team_abbr, Offensive_DVOA, PBWR, RBWR) %>%
  mutate(season = 2024)

# Create a table with ONLY the defensive stats.
defensive_stats = team_stats_2023 %>%
  select(team_abbr, Defensive_DVOA, PRWR, RSWR) %>%
  mutate(season = 2024)


#Merge the datasets

pbp_merged = fourth_down_plays %>%
  # Merge #1: Join the offensive stats based on the 'posteam'
  left_join(offensive_stats, by = c("posteam" = "team_abbr", "season")) %>%
  
  # Merge #2: Join the defensive stats based on the 'defteam'
  left_join(defensive_stats, by = c("defteam" = "team_abbr", "season"))

pbp_filter_vars = pbp_merged %>% 
  select(
    #Identifiers
    week, home_team, away_team, season_type, posteam, defteam,
    
    #Outcome variable
    fourth_down_converted,
    
    #Situational predictors
    yardline_100, quarter_seconds_remaining, 
    half_seconds_remaining, game_seconds_remaining, game_half, drive, qtr,
    goal_to_go, ydstogo, posteam_timeouts_remaining, defteam_timeouts_remaining,
    score_differential, div_game, temp, wind,
    
    #Strategic predictors
    play_type, shotgun, no_huddle, qb_dropback, qb_scramble, pass_length, 
    pass_location, run_location, run_gap, ep, wp,
    
    #Team Quality predictors (Offense)
    Offensive_DVOA, PBWR, RBWR,
    
    #Team Quality predictors (Defense)
    Defensive_DVOA, PRWR, RSWR
  )

pbp_edit_vars = pbp_filter_vars %>%
  
  #Make binary variable based on home/away team
  mutate(
    is_home_team = ifelse(posteam == home_team, 1, 0)
  ) %>% 
  
  #Make binary variable based on season type
  mutate(
    is_postseason = ifelse(season_type == "POST", 1, 0)
  ) %>% 
  
  #Make binary variable based on half of game, "Half2"/"Overtime" is reference category
  mutate(
    is_first_half = ifelse(game_half == "Half1", 1, 0)
  ) %>% 
  
  #Make binary variable based on play type
  mutate(
    is_pass = ifelse(play_type == "pass", 1, 0)
  ) %>% 
  
  #Make binary variable based on pass length
  mutate(
    is_deep_pass = ifelse(pass_length == "deep", 1, 0)
  ) %>% 
  
  #Make binary variable for ANY outside play (pass OR run), "middle" is reference
  mutate(
    is_outside = ifelse(
      (!is.na(pass_location) & pass_location %in% c("left", "right")) | 
        (!is.na(run_location) & run_location %in% c("left", "right")), 
      1, 0
    )
  ) %>%
  
  #Make dummy variables based on run gap, "end" is reference category
  mutate(
    is_inside_rungap = ifelse(run_gap %in% c("guard", "tackle"), 1, 0)
  ) %>% 
  
  #Make binary variable based on indoor/outdoor stadium
  mutate(is_dome_game = ifelse(is.na(temp), 1, 0)) %>% 
  
  #Make "net" varaibles for matchups
  mutate(
    Net_DVOA = Offensive_DVOA - Defensive_DVOA,
    Net_Pass_Block = PBWR - PRWR,
    Net_Run_Block = RBWR - RSWR
  ) 

pbp_final_analysis = pbp_edit_vars %>% 
  select(
    
    #Outcome variable
    fourth_down_converted,
    
    #Identifiers
    week, home_team, away_team, season_type, posteam, defteam,
    
    #Situational predictors
    is_home_team, is_postseason, is_first_half, yardline_100, 
    quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
    drive, qtr, goal_to_go, ydstogo, posteam_timeouts_remaining, 
    defteam_timeouts_remaining, score_differential, div_game, is_dome_game,
    
    #Strategic predictors
    is_pass, shotgun, no_huddle, qb_dropback, qb_scramble, is_deep_pass, 
    is_outside, is_inside_rungap, ep, wp,
    
    #Team Quality predictors (Offense)
    Offensive_DVOA, PBWR, RBWR,
    
    #Team Quality predictors (Defense)
    Defensive_DVOA, PRWR, RSWR,
    
    #Net Team Quality predictors
    Net_DVOA, Net_Pass_Block, Net_Run_Block
  )

#install.packages("writexl")
#library(writexl)
write_xlsx(pbp_final_analysis, "fourth_down_analysis.xlsx")
