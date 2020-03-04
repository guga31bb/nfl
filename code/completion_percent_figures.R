source("helpers.R")

pbp_all <- readRDS("data/pbp_all.rds") %>%
  fix_pbp() %>%
  filter(season >= 2006) %>%
  filter(complete_pass == 1 | incomplete_pass == 1) %>%
  filter(!is.na(air_yards) & air_yards >= -10 & !is.na(receiver_player_name) & !is.na(pass_location)) %>%
  mutate(air_is_zero=ifelse(air_yards == 0,1,0))



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


