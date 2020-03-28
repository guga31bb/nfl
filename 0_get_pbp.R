library(tidyverse)
library(dplyr)
library(na.tools)

#most recent season of nflscrapR data here
last = 2019

#grab all the data from nick's (pre-2009) and ron's (2009 and later) githubs
datalist = list()
for (yr in 1999:last) {
  un <- ifelse(yr <= 2008,"CroppedClamp","ryurko")
  pbp <- read_csv(paste0("https://raw.githubusercontent.com/",un,"/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_",yr,".csv")) %>%
      select(play_id, game_id, home_team, away_team, posteam, defteam, yardline_100, half_seconds_remaining, game_date, drive,
           qtr, down, goal_to_go, ydstogo, desc, play_type, yards_gained, pass_location, air_yards, score_differential,
           ep, epa, wp, wpa, first_down_rush, first_down_pass, first_down_penalty, complete_pass,
           incomplete_pass, interception, fumble_lost, passer_player_name, rusher_player_name, receiver_player_name,
           passer_player_id, rusher_player_id, receiver_player_id,
           pass_location) %>%
    mutate(year = yr)
  datalist[[yr]] <- pbp # add it to your list
}

pbp_all <- dplyr::bind_rows(datalist)


pbp_all <- pbp_all %>% 
  mutate_at(vars(home_team, away_team, posteam, defteam), funs(case_when(
    . %in% "JAC" ~ "JAX",
    . %in% "STL" ~ "LA",
    . %in% "SL" ~ "LA",
    . %in% "ARZ" ~ "ARI",
    . %in% "BLT" ~ "BAL",
    . %in% "CLV" ~ "CLE",
    . %in% "HST" ~ "HOU",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  ))) 


#save raw dataset
saveRDS(pbp_all, file="data/pbp_all.rds")
pbp_all <- readRDS("data/pbp_all.rds")



#w/l results
results <- read_csv("http://www.habitatring.com/games.csv") %>%
  filter(season >= 1999 & !is.na(result) & week <= 17) %>%
  gather(away_team, home_team, key = "type", value = "team") %>%
  arrange(game_id) %>% select(game_id, season, result, type, team, home_score, away_score, week) %>%
  mutate(game_id = as.numeric(game_id), 
         result = ifelse(type == "home_team", result, -result),
         points_scored = ifelse(type == "home_team", home_score, away_score),
         points_allowed = ifelse(type == "home_team", away_score, home_score),
         diff = result,
         win = case_when(result > 0 ~ 1, result < 0 ~ 0, result == 0 ~ 0.5)) %>%
  select(-type, -result, -season, -home_score, -away_score) %>%
  mutate_at(vars(team), funs(case_when(
    . %in% "JAC" ~ "JAX",
    . %in% "STL" ~ "LA",
    . %in% "SL" ~ "LA",
    . %in% "ARZ" ~ "ARI",
    . %in% "BLT" ~ "BAL",
    . %in% "CLV" ~ "CLE",
    . %in% "HST" ~ "HOU",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  )))

saveRDS(results, file="data/game_results.rds")

