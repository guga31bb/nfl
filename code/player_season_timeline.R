
source("helpers.R")

library(scales)

res <- 800 #size of exported plots


# *********************************************************************************
# variables to set
# *********************************************************************************

source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")
source("C:/Users/bback/Dropbox/nfl/r/helpers.R")

nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
nfl_logos_df$url[31] <-  "http://habitatring.com/titans.png" #fix for the Titans


nfl_colors <- get_colors()

big_data <- readRDS("data/pbp_all.rds") %>%
  fix_pbp()

games <- readRDS("data/game_results.rds")
  


# *********************************************************************************
# scrape and load data
# *********************************************************************************

qbs <- big_data %>% filter(complete_pass == 1 | incomplete_pass == 1) %>%
  group_by(name) %>% summarize(atts = n()) %>%
  filter(atts >= 30) %>% mutate(qb = 1)

rb_data <- big_data %>% left_join(qbs,by=c("name")) %>%
  filter(is.na(qb)) %>% filter(pass == 0) 

big_data <- big_data %>% left_join(qbs,by=c("name")) %>%
  filter(qb == 1) %>%
  fix_fumbles()


# *********************************************************************************
# do stuff: QBs
# *********************************************************************************


g <- big_data %>%
  group_by(name, posteam, game_date, defteam, game_id, season) %>%
#  filter(qtr == 1 | qtr == 2 | qtr == 3) %>%
  summarize(
    epa = mean(epa),
    n=n()
    ) %>% ungroup() %>%
  left_join(nfl_colors,by="posteam") %>%
  left_join(nfl_logos_df, by = c("defteam" = "team_code")) %>%
#  left_join(heads,by="name") %>%
  left_join(games, by = c("posteam" = "team", "game_id")) %>%
  select(-division, -team) %>%
  filter(n >= 10) %>%
  ungroup() %>%
  group_by(season) %>%
  mutate(
    p10 = quantile(epa, probs=0.10, na.rm=TRUE),
  p25 = quantile(epa, probs=0.25, na.rm=TRUE),
  p30 = quantile(epa, probs=0.30, na.rm=TRUE),
  p50 = quantile(epa, probs=0.5, na.rm=TRUE),
  p75 = quantile(epa, probs=0.75, na.rm=TRUE),
  p90 = quantile(epa, probs=0.90, na.rm=TRUE),
  name = ifelse(name == "Jos.Allen", "J.Allen", name)
  ) %>% ungroup()
  

g %>% filter(season == 2019) %>% arrange(-epa)


player <- "P.Mahomes"
year <- 2019

p <- g %>% filter(name == player) %>%
  filter(season >= year) %>%
  filter(n > 10)

sum <- g %>% group_by(season) %>% summarize(
  p25 = quantile(epa, probs=0.25, na.rm=TRUE),
  p50 = quantile(epa, probs=0.5, na.rm=TRUE),
  p75 = quantile(epa, probs=0.75, na.rm=TRUE),
  p90 = quantile(epa, probs=0.90, na.rm=TRUE),
  p10 = quantile(epa, probs=0.10, na.rm=TRUE)
) %>% filter(season >= year)

ggplot(data=p, aes(x=game_date,y=epa)) +
  geom_line(color = p$primary, size = 3, alpha = .2) +
  geom_point(color = p$secondary, cex=p$n / 5) +
  theme_bw() +
  geom_hline(data=sum, aes(yintercept=p10),linetype="dashed",color="black") +
  geom_hline(data=sum, aes(yintercept=p25),linetype="dashed",color="black") +
  geom_hline(data=sum, aes(yintercept=p50), size = 1) +
  geom_hline(data=sum, aes(yintercept=p75),linetype="dashed",color="black") +
  geom_hline(data=sum, aes(yintercept=p90),linetype="dashed",color="black") +
  geom_image(aes(image = url), size = 0.06, asp = 16/9, nudge_y = .07) +
  ylab("EPA Per Play") + xlab("Game Date") +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(size=20,face = 2,hjust=.5),
        plot.subtitle = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black")) +
  labs(title=paste(player, "EPA/Play Timeline"),
       caption = "Figure: @benbbaldwin | Data: @nflscrapR | Offense not penalized for fumbles on completed passes") +
  scale_y_continuous(limits = c(min(p$epa) - .1, max(p$epa) + .1), oob = rescale_none, breaks = scales::pretty_breaks(n = 10)) +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-10"), y= mean(p$p90) + .01, fontface=2), label = "90th pctile\ngame", color="red") +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-03"), y= mean(p$p75) + .03, fontface=2), label = "75th", color="red") +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-03"), y= mean(p$p50) + .03, fontface=2), label = "50th", color="red") +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-03"), y= mean(p$p10) + .03, fontface=2), label = "10th", color="red") +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-03"), y= mean(p$p25) + .03, fontface=2), label = "25th", color="red") +
  facet_wrap( ~ season, scales = "free_x", nrow=ifelse(min(p$season) >= 2018, 1, 2)) 
  


ggsave(paste0('output/timelines/qb_',year,'_',player,'.png'), dpi=res, height=9*.8, width=16*.8)


# *********************************************************************************
# do stuff: RBs
# *********************************************************************************


g <- rb_data %>%
  group_by(posteam, game_date, defteam, game_id, season) %>%
  summarize(
    epa = mean(epa),
    n=n()
  ) %>% ungroup() %>%
  left_join(nfl_colors,by="posteam") %>%
  left_join(nfl_logos_df, by = c("defteam" = "team_code")) %>%
  left_join(games, by = c("posteam" = "team", "season", "game_id")) %>%
  select(-division, -team) %>%
  ungroup() %>%
  group_by(season) %>%
  mutate(
    p10 = quantile(epa, probs=0.10, na.rm=TRUE),
    p25 = quantile(epa, probs=0.25, na.rm=TRUE),
    p30 = quantile(epa, probs=0.30, na.rm=TRUE),
    p50 = quantile(epa, probs=0.5, na.rm=TRUE),
    p75 = quantile(epa, probs=0.75, na.rm=TRUE),
    p90 = quantile(epa, probs=0.90, na.rm=TRUE)
    ) %>% ungroup()

g %>% filter(season == 2019 & posteam== "GB")


player <- "BAL"
year <- 2019

p <- g %>% filter(posteam == player) %>%
  filter(season >= year) 

sum <- g %>% group_by(season) %>% summarize(
  p25 = quantile(epa, probs=0.25, na.rm=TRUE),
  p50 = quantile(epa, probs=0.5, na.rm=TRUE),
  p75 = quantile(epa, probs=0.75, na.rm=TRUE),
  p90 = quantile(epa, probs=0.90, na.rm=TRUE),
  p10 = quantile(epa, probs=0.10, na.rm=TRUE)
) %>% filter(season >= year)

ggplot(data=p, aes(x=game_date,y=epa)) +
  geom_line(color = p$primary, size = 3, alpha = .2) +
  geom_point(color = p$secondary, cex=p$n / 3) +
  theme_bw() +
  geom_hline(data=sum, aes(yintercept=p10),linetype="dashed",color="black") +
  geom_hline(data=sum, aes(yintercept=p25),linetype="dashed",color="black") +
  geom_hline(data=sum, aes(yintercept=p50), size = 1) +
  geom_hline(data=sum, aes(yintercept=p75),linetype="dashed",color="black") +
  geom_hline(data=sum, aes(yintercept=p90),linetype="dashed",color="black") +
  geom_image(aes(image = url), size = 0.06, asp = 16/9, nudge_y = .07) +
  ylab("EPA Per Play") + xlab("Game Date") +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(size=20,face = 2,hjust=.5),
        plot.subtitle = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black")) +
  labs(title=paste(player, "Non-QB Rushing EPA/Play Timeline"),
       caption = "Figure: @benbbaldwin | Data: @nflscrapR | Dot size = # plays") +
  scale_y_continuous(limits = c(min(p$epa) - .1, max(p$epa) + .1), oob = rescale_none, breaks = scales::pretty_breaks(n = 10)) +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-05"), y= mean(p$p90) + .01, fontface=2), label = "90th pctile\ngame", color="red") +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-03"), y= mean(p$p75) + .03, fontface=2), label = "75th", color="red") +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-03"), y= mean(p$p50) + .03, fontface=2), label = "50th", color="red") +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-03"), y= mean(p$p10) + .03, fontface=2), label = "10th", color="red") +
  geom_text(data = data.frame(season=2019), mapping = aes(x=as.Date("2019-09-03"), y= mean(p$p25) + .03, fontface=2), label = "25th", color="red") +
  facet_wrap( ~ season, scales = "free_x", nrow=ifelse(min(p$season) >= 2018, 1, 2)) 



ggsave(paste0('C:/Users/bback/Dropbox/nfl/current_season/results/misc/timeline_rb_',player,year,'.png'), dpi=res, height=9*.8, width=16*.8)

 

# *********************************************************************************
# do stuff: defenses
# *********************************************************************************


g <- big_data %>% filter(season==2019) %>%
  group_by(posteam, game_date, defteam) %>%
  summarize(
    epa = mean(epa),
    n=n()
  ) %>% ungroup() %>%
  left_join(nfl_colors,by=c("defteam" = "posteam")) %>%
  left_join(nfl_logos_df, by = c("posteam" = "team_code")) %>%
  select(-division, -team) %>%
  ungroup() %>%
  mutate(
    p10 = quantile(epa, probs=0.10, na.rm=TRUE),
    p25 = quantile(epa, probs=0.25, na.rm=TRUE),
    p50 = quantile(epa, probs=0.5, na.rm=TRUE),
    p75 = quantile(epa, probs=0.75, na.rm=TRUE),
    p90 = quantile(epa, probs=0.90, na.rm=TRUE)
  )

g


player <- "SF"

p <- g %>% filter(defteam == player)

ggplot(data=p, aes(x=game_date,y=epa)) +
  geom_line(color = p$primary, size = 3, alpha = .3) +
  geom_point(color = p$secondary, size = 5) +
  theme_bw() +
  geom_hline(yintercept= max(g$p10),linetype="dashed",color="black") +
  geom_hline(yintercept= max(g$p25),linetype="dashed",color="black") +
  geom_hline(yintercept= mean(g$p50),color="black", size = 1) +
  geom_hline(yintercept= mean(g$p75),linetype="dashed",color="black") +
  geom_hline(yintercept= mean(g$p90),linetype="dashed",color="black") +
  geom_image(aes(image = url), size = 0.06, asp = 16/9, nudge_y = .08) +
  ylab("QB EPA Per Play") + xlab("Game Date") +
  theme(panel.grid.major.x = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(),
        plot.title = element_text(size=20,face = 2,hjust=.5),
        plot.subtitle = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  labs(title=paste(player, "2019 EPA/Play Allowed to Quarterbacks"),
       caption = "Figure: @benbbaldwin | Data: @nflscrapR | Offense not penalized for fumbles on completed passes") +
  scale_y_continuous(limits = c(min(p$epa) - .1, max(p$epa) + .1), oob = rescale_none, breaks = scales::pretty_breaks(n = 10)) +
  annotate("text",x=as.Date("2019-09-09"), y= mean(p$p90) - .03, label = "90th pctile game", color="red") +
  annotate("text",x=as.Date("2019-09-03"), y= mean(p$p75) + .03, label = "75th", color="red") +
  annotate("text",x=as.Date("2019-09-03"), y= mean(p$p50) + .03, label = "50th", color="red") +
  annotate("text",x=as.Date("2019-09-03"), y= mean(p$p10) + .03, label = "10th", color="red") +
  annotate("text",x=as.Date("2019-09-03"), y= mean(p$p25) + .03, label = "25th", color="red") 


ggsave(paste0('output/timelines/def_2019_',player,'.png'), dpi=res)



# *********************************************************************************
# do stuff: RB defenses
# *********************************************************************************


g <- rb_data %>% filter(season==2019) %>%
  group_by(posteam, game_date, defteam) %>%
  summarize(
    epa = mean(epa),
    n=n()
  ) %>% ungroup() %>%
  left_join(nfl_colors,by=c("defteam" = "posteam")) %>%
  left_join(nfl_logos_df, by = c("posteam" = "team_code")) %>%
  select(-division, -team) %>%
  ungroup() %>%
  mutate(
    p10 = quantile(epa, probs=0.10, na.rm=TRUE),
    p25 = quantile(epa, probs=0.25, na.rm=TRUE),
    p50 = quantile(epa, probs=0.5, na.rm=TRUE),
    p75 = quantile(epa, probs=0.75, na.rm=TRUE),
    p90 = quantile(epa, probs=0.90, na.rm=TRUE)
  )

g

#see which are high and low var
g%>%group_by(defteam) %>% summarize(var = var(epa))%>%arrange(-var)


player <- "SF"

p <- g %>% filter(defteam == player)

ggplot(data=p, aes(x=game_date,y=epa)) +
  geom_line(color = p$primary, size = 3, alpha = .3) +
  geom_point(color = p$secondary, size = 5) +
  theme_bw() +
  geom_hline(yintercept= max(g$p10),linetype="dashed",color="black") +
  geom_hline(yintercept= max(g$p25),linetype="dashed",color="black") +
  geom_hline(yintercept= mean(g$p50),color="black", size = 1) +
  geom_hline(yintercept= mean(g$p75),linetype="dashed",color="black") +
  geom_hline(yintercept= mean(g$p90),linetype="dashed",color="black") +
  geom_image(aes(image = url), size = 0.06, asp = 16/9, nudge_y = .08) +
  ylab("EPA Per Play") + xlab("Game Date") +
  theme(panel.grid.major.x = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(),
        plot.title = element_text(size=20,face = 2,hjust=.5),
        plot.subtitle = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  labs(title=paste(player, "2019 EPA/Play Allowed to Non-Quarterbacks"),
       caption = "Figure: @benbbaldwin | Data: @nflscrapR") +
  scale_y_continuous(limits = c(min(p$epa) - .1, max(p$epa) + .1), oob = rescale_none, breaks = scales::pretty_breaks(n = 10)) +
  annotate("text",x=as.Date("2019-09-09"), y= mean(p$p90) - .03, label = "90th pctile game", color="red") +
  annotate("text",x=as.Date("2019-09-03"), y= mean(p$p75) + .03, label = "75th", color="red") +
  annotate("text",x=as.Date("2019-09-03"), y= mean(p$p50) + .03, label = "50th", color="red") +
  annotate("text",x=as.Date("2019-09-03"), y= mean(p$p10) + .03, label = "10th", color="red") +
  annotate("text",x=as.Date("2019-09-03"), y= mean(p$p25) + .03, label = "25th", color="red") 


ggsave(paste0('C:/Users/bback/Dropbox/nfl/current_season/results/misc/def_rb_timeline_',player,'.png'), dpi=res)


