library(gt)
source("helpers.R")

# *********************************************************************************
# variables to set
# *********************************************************************************


qb_min <- 150 #min # of action plays to be included in tables


# *********************************************************************************
# data reading
# *********************************************************************************

pbp_all <- readRDS("data/pbp_all.rds") %>%
  filter(season >= 2006) %>%
  fix_pbp() %>%
  apply_completion_probability() %>%
  fix_fumbles()


# *********************************************************************************
# data reading
# *********************************************************************************


#get QB stats
qbs <- pbp_all %>%
  group_by(name, season, posteam) %>%
  mutate(
    unadjusted_epa = epa,
    epa = ifelse(epa < -4.5, -4.5, epa)) %>%
  summarize (
    n_plays = sum(play),
    n_db = sum(complete_pass) + sum(incomplete_pass),
    cpoe = mean(100*(complete_pass - cp), na.rm = TRUE),
    epa = mean(epa),
    unadjusted_epa = mean(unadjusted_epa),
    success = mean(success),
    index = 0.248070 * epa + 0.006686 * cpoe + 0.080851
  ) %>%
  filter(n_db > 50 & n_plays >= qb_min) %>% ungroup() %>%
  group_by(name) %>%
  mutate(
    lcpoe = lag(cpoe, n = 1, order_by = season),
    lepa = lag(epa, n = 1, order_by = season),
    lindex = lag(index, n = 1, order_by = season)
  )


#testing y/y stability of things
cor(qbs$index, qbs$lindex, use = "complete.obs")
cor(qbs$epa, qbs$lepa, use = "complete.obs")
cor(qbs$epa, qbs$lindex, use = "complete.obs")
cor(qbs$cpoe, qbs$lcpoe, use = "complete.obs")



for (year in 2006:2019) {
  #qbs
  q<- qbs %>% ungroup() %>% 
    filter(season == year) %>%
    select(name,posteam, n_plays,index, epa,unadjusted_epa, success,cpoe) %>%
    mutate(
      ranke = rank(-epa),
      ranki = rank(-index),
      rankeu = rank(-unadjusted_epa),
      ranks = rank(-success),
      rankc = rank(-cpoe),    
      i = paste0(round(index, 2)," (",ranki,")"),
      e = paste0(round(epa, 2)," (",ranke,")"),
      eu = paste0(round(unadjusted_epa, 2)," (",rankeu,")"),
      s = paste0(round(success, 2)," (",ranks,")"),
      c = paste0(round(cpoe, 1)," (",rankc,")")
    ) 
  
  
  table <- q %>% arrange(desc(index)) %>%  select(name,posteam, n_plays,i,e, eu, s, c) %>%
    gt() %>%
    cols_align(align = "center") %>%
    tab_header(title = paste(year, "Quarterback Efficiency")) %>%
    cols_label(
      e = md("**EPA/<br>play**"), 
      i = md("**EPA+CPOE<br>composite**"), 
      eu = md("**Unadjusted<br>EPA/play**"), 
      s=md("**Success Rate**"), 
      c = md("**CPOE**"), 
      n_plays = md("**Plays**"), 
      posteam = md("**Team**"),
      name = md("**Player**")
    ) %>% 
    tab_source_note(
      source_note = paste("Table: @benbbaldwin | Data: @nflscrapR | 
      Adjusted EPA per play caps negative EPA at -4.5 | QBs not penalized for fumbles after catches | 
      CPOE = Completion Percentage Over Expectation based on target depth. | Min", qb_min, "plays |
      Composite formula: 0.25*EPA + 0.007*CPOE + 0.08 based on coefficients which best predict EPA/play in following year")) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_data(
        columns = vars(name))
    ) 
  
  
  table
  
  
  table %>% gtsave(paste0("output/qb_stats/qb_stats_",year,".png"))

}

