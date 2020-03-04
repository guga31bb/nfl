
source("helpers.R")

pbp_all <- readRDS("data/pbp_all.rds") %>%
  fix_pbp() %>%
  filter(season >= 2006) %>%
  filter(complete_pass == 1 | incomplete_pass == 1) %>%
  filter(!is.na(air_yards) & air_yards >= -10 & !is.na(receiver_player_name) & !is.na(pass_location)) %>%
  mutate(air_is_zero=ifelse(air_yards == 0,1,0))
  

#for looking at stuff
pbp_all %>% select(desc, receiver_player_name, complete_pass, incomplete_pass)


# add cp
cp_model1 <- gam(complete_pass ~ s(air_yards) + s(yardline_100) +log(ydstogo) + air_is_zero + 
                  factor(down) + factor(pass_location) + factor(season),
                data=pbp_all,method="REML")
pbp_all$cp1 <- predict.gam(cp_model1, pbp_all, type = "response")

pbp_all %>% select(desc, air_yards, cp1)


qbs <- pbp_all %>%
  group_by(name, posteam, season) %>%
  summarize (
    n_plays = sum(play),
    cpoe1 = mean(100*(complete_pass - cp1), na.rm = TRUE)
  ) %>%
  filter(n_plays >= 320) %>% ungroup() %>%
  group_by(name) %>%
  mutate(
    lcpoe1 = lag(cpoe1, n = 1, order_by = season)
  )


qbs %>% filter(name=="R.Wilson")


cor(qbs$cpoe1, qbs$lcpoe1, use = "complete.obs")





#all
pbp_all %>% group_by(air_yards) %>%
  summarize(c = mean(complete_pass)) %>%
  filter(air_yards <= 50) %>%
  ggplot(aes(air_yards, c)) +
  geom_point()

#inside 10
pbp_all %>% 
  mutate(goal = ifelse(yardline_100 <= 10, 1, 0)) %>%
  group_by(air_yards, goal) %>%
  summarize(c = mean(complete_pass)) %>%
  filter(air_yards <= 20) %>%
  ggplot(aes(air_yards, c)) +
  geom_point(aes(colour = factor(goal)), size = 4) +
  theme_bw()


#down
pbp_all %>% 
  mutate(down = ifelse(down <= 2, 1, 0)) %>%
  group_by(air_yards, down) %>%
  summarize(c = mean(complete_pass)) %>%
  filter(air_yards <= 40) %>%
  ggplot(aes(air_yards, c)) +
  geom_point(aes(colour = factor(down)), size = 4) +
  theme_bw()


#middle
pbp_all %>% 
  mutate(loc = ifelse(pass_location == "middle", 1, 0)) %>%
  group_by(air_yards, loc) %>%
  summarize(c = mean(complete_pass)) %>%
  filter(air_yards <= 40) %>%
  ggplot(aes(air_yards, c)) +
  geom_point(aes(colour = factor(loc)), size = 4) +
  theme_bw()



