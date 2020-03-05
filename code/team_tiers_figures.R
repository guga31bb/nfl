source("helpers.R")

#################################################################
# Initial setup
#################################################################
res <- 800 #size of exported plots
slope = -1.5 #for the tiers stuff
qb_min <- 320 #min # of qb plays
year <- 2019


#################################################################
# Read data
#################################################################
# can't use fix_pbp() here bc it crashes rstudio.cloud
pbp_all <- readRDS("data/pbp_all.rds") %>%
  fix_pbp()

nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
nfl_logos_df$url[31] <-  "http://habitatring.com/titans.png" #fix for the Titans

nfl_colors <- get_colors()


#################################################################
# Get at season-team level
#################################################################
offense <- pbp_all %>%
  group_by(posteam, season)%>%
  summarize(
    n_pass=sum(pass),
    n_rush=sum(rush),
    epa_per_pass=sum(epa*pass)/n_pass,
    epa_per_rush=sum(epa*rush)/n_rush,
    success_per_pass=sum(pass*epa>0)/n_pass,
    success_per_rush=sum(rush*epa>0)/n_rush,
    off_epa=mean(epa),
    off_success=mean(success)
  )

defense <- pbp_all %>%
  group_by(defteam, season)%>%
  summarize(
    def_n_pass=sum(pass),
    def_n_rush=sum(rush),
    def_epa_per_pass=sum(epa*pass)/def_n_pass,
    def_epa_per_rush=sum(epa*rush)/def_n_rush,
    def_success_per_pass=sum(pass*epa>0)/def_n_pass,
    def_success_per_rush=sum(rush*epa>0)/def_n_rush,
    def_epa=mean(epa),
    def_success=mean(success)
  )

chart_all <- offense %>% 
  inner_join(defense, by=c("season", "posteam" = "defteam")) %>%
  left_join(nfl_logos_df, by = c("posteam" = "team_code")) %>%
  left_join(nfl_colors,by="posteam")

#################################################################
# Make figures
#################################################################

year = 2019
chart <- chart_all %>% filter(season == year)

chart %>%
  ggplot(aes(x = off_epa, y = def_epa)) +
  geom_image(aes(image = url), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart$off_epa), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart$def_epa), color = "red", linetype = "dashed") +
  labs(x = "Offense EPA/play",
       y = "Defense EPA/play",
       caption = "Figure: @benbbaldwin | Data: @nflscrapR",
       title = paste(year, "NFL team tiers")) +
  geom_abline(slope=slope, intercept=.4, alpha=.2) +
  geom_abline(slope=slope, intercept=.3, alpha=.2) +
  geom_abline(slope=slope, intercept=0, alpha=.2) +
  geom_abline(slope=slope, intercept=.1, alpha=.2) +
  geom_abline(slope=slope, intercept=.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.1, alpha=.2) +
  geom_abline(slope=slope, intercept=-.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.3, alpha=.2) +
  theme_bw()+
  scale_y_reverse() +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    #panel.grid.minor = element_blank()
  )

ggsave(paste0('output/team_tiers/team_tiers_',year,'.png'), dpi=res, height=9*.8, width=16*.8)



