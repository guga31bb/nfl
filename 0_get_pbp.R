library(tidyverse)
library(dplyr)
library(na.tools)

#most recent season of nflscrapR data here
last = 2019


#grab all the data from nick's github
datalist = list()
for (yr in 1999:2008) {
  pbp <- read_csv(url(paste0("https://github.com/CroppedClamp/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv"))) %>%
    select(play_id, game_id, home_team, away_team, posteam, defteam, yardline_100, half_seconds_remaining, game_date, drive,
           qtr, down, goal_to_go, ydstogo, desc, play_type, yards_gained, pass_location, air_yards, score_differential,
           ep, epa, wp, wpa, first_down_rush, first_down_pass, first_down_penalty, complete_pass,
           incomplete_pass, interception, fumble_lost, passer_player_name, rusher_player_name, receiver_player_name,
           passer_player_id, rusher_player_id, receiver_player_id,
           pass_location) %>%
    mutate(year = yr)
  datalist[[yr]] <- pbp # add it to your list
}

pbp_pre <- dplyr::bind_rows(datalist)


#grab all the data from ron's github
datalist = list()
for (yr in 2009:last) {
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv"))) %>%
    select(play_id, game_id, home_team, away_team, posteam, defteam, yardline_100, half_seconds_remaining, game_date, drive,
           qtr, down, goal_to_go, ydstogo, desc, play_type, yards_gained, pass_location, air_yards, score_differential,
           ep, epa, wp, wpa, first_down_rush, first_down_pass, first_down_penalty, complete_pass,
           incomplete_pass, interception, fumble_lost, passer_player_name, rusher_player_name, receiver_player_name,
           passer_player_id, rusher_player_id, receiver_player_id,
           pass_location) %>%
    mutate(year = yr)
  datalist[[yr]] <- pbp # add it to your list
}

pbp_post <- dplyr::bind_rows(datalist)

pbp_all <- bind_rows(pbp_pre, pbp_post)

pbp_all <- pbp_all %>% 
  mutate_at(vars(home_team, away_team, posteam, defteam), funs(case_when(
    . %in% "JAX" ~ "JAC",
    . %in% "STL" ~ "LA",
    . %in% "SL" ~ "LA",
    . %in% "ARZ" ~ "ARI",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  ))) 


#save raw dataset
saveRDS(pbp_all, file="data/pbp_all.rds")
pbp_all <- readRDS("data/pbp_all.rds")



