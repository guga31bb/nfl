source("helpers.R")
library(tidylog)

library(ggforce)
library(concaveman)


pbp_all <- readRDS("data/pbp_all.rds") %>%
  filter(down == 4) %>%
  filter(!is.na(posteam)) %>%
  filter(half_seconds_remaining > 60) %>%
  filter(play_type %in% c("field_goal", "pass", "punt", "run", "no_play")) %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    go = if_else(pass == 1 | rush == 1, 1, 0),
    fd = ifelse(first_down_pass == 1 | first_down_rush == 1 | first_down_penalty == 1, 1, 0),
    fd = ifelse(go == 1, fd, NA)
  ) 

pbp_all %>% filter(ydstogo == 1 & go == 1) %>%
  select(desc, fd)

chart <- pbp_all %>%
  filter(ydstogo == 1 | ydstogo == 2 | ydstogo == 3 | ydstogo == 4) %>%
  group_by(year, ydstogo) %>% 
  summarize(go = mean(go), fd = mean(fd, na.rm=TRUE)) %>%
  rename(season = year) %>%
  filter(ydstogo <= 2)



### 4th downs over time
g <- ggplot(data=chart, aes(x=season,y=go)) +
  geom_line(data=chart,
            aes(x=season,y=go),size=1) +
  geom_point(data=chart,
             aes(x=season,y=go),size=4) +
  facet_wrap(~ydstogo) +
  theme_bw() +
  ylab("Go Rate") + xlab("Season") + 
  theme(plot.title = element_text(size=16,face = 2,hjust=.5),
        plot.subtitle = element_text(size=16, hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  labs(title=paste("4th down go-for-it rate by yards to go"))
  

### 4th downs over time
f <- ggplot(data=chart, aes(x=season,y=fd)) +
  geom_line(data=chart,
            aes(x=season,y=fd),size=1) +
  geom_point(data=chart,
             aes(x=season,y=fd),size=4) +
  facet_wrap(~ydstogo) +
  theme_bw() +
  ylab("First Down Rate") + xlab("Season") + 
  theme(plot.title = element_text(size=16,face = 2,hjust=.5),
        plot.subtitle = element_text(size=16, hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  labs(title=paste("1st down rate on 4th down go attempts by yards to go"))

library(patchwork)

g / f +
  labs(caption = "Figure: @benbbaldwin | Data: @nflscrapR")

ggsave("output/4thdowns.png", dpi=900)


g+
  labs(caption = "Figure: @benbbaldwin | Data: @nflscrapR | excludes final minute of each half") +
  geom_mark_hull(
    aes(
      filter = season > 2017,
      description = "Post- Philly Special"
    ),
    color = "red", label.fontface = "bold", label.colour = "red", con.colour = "red",
    label.buffer = unit(15, 'mm')
  ) +
  geom_mark_hull(
    aes(
      filter = season > 2010 & season < 2016,
      description = "Dark ages"
      ),
    color = "red", label.fontface = "bold", label.colour = "red", con.colour = "red",
    label.buffer = unit(30, 'mm')
  ) +
  geom_text(data = data.frame(ydstogo=1),label = "4th and 1\ngo rate", mapping = aes(x =2010 , y = .25), color="red", size = 15) +
  geom_text(data = data.frame(ydstogo=2),label = "4th and 2\ngo rate", mapping = aes(x =2010 , y = .5), color="red", size = 15) 


ggsave("output/4thdowns_rate.png", dpi=900)


