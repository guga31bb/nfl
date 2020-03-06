source("helpers.R")

pbp_all <- readRDS("data/pbp_all.rds") %>%
  fix_pbp() %>%
  filter(season >= 2006) %>%
  filter(complete_pass == 1 | incomplete_pass == 1) %>%
  filter(!is.na(air_yards) & air_yards >= -10 & !is.na(receiver_player_name) & !is.na(pass_location)) %>%
  mutate(air_is_zero=ifelse(air_yards == 0,1,0))
  
# add cp
cp_model <- gam(complete_pass ~ s(air_yards) + s(yardline_100) +log(ydstogo) + air_is_zero + 
                  factor(down) + factor(pass_location) + factor(season),
                data=pbp_all, method="REML", family = "binomial")
pbp_all$cp <- predict.gam(cp_model, pbp_all, type = "response")

#get QB stats
qbs <- pbp_all %>%
  group_by(name, season) %>%
  summarize (
    n_plays = sum(play),
    cpoe1 = mean(100*(complete_pass - cp1), na.rm = TRUE)
  ) %>%
  filter(n_plays >= 320) %>% ungroup() %>%
  group_by(name) %>%
  mutate(
    lcpoe1 = lag(cpoe1, n = 1, order_by = season)
  )

#y/y correlation
cor(qbs$cpoe1, qbs$lcpoe1, use = "complete.obs")




