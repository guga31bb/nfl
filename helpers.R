#libraries

library(nflscrapR)
library(tidyverse)
library(na.tools)
library(mgcv)
library(teamcolors)
library(rvest)
library(stringr)
library(jsonlite)
library(glue)
library(janitor)
library(tidyverse)

#functions

#scrape
#note plays_filename defined outside this function
scrape <- function(y) {
  
  report("Loading game data")
  games <- read_csv("http://www.habitatring.com/games.csv")
  games <- games %>%
    filter(season == y & !is.na(result)) %>% 
    mutate(game_id=as.character(game_id))
  
  # load previous data
  report("Loading existing plays data")
  old_warning_level <- getOption("warn")
  options(warn=-1)
  tryCatch(plays <- readRDS(plays_filename) %>% fix_inconsistent_data_types(),error=report)
  options(warn=old_warning_level)
  
  # update plays
  if (exists("plays")) {
    # we have data! identify any missing games
    pulled_games <- plays %>% pull(game_id) %>% unique()
    missing <- games %>% filter(!(game_id %in% pulled_games)) %>% pull(game_id)
    
    # handle missing games
    if (length(missing) > 0)
    {
      # get new plays
      new_plays <- NULL
      for (g in missing)
      {
        tryCatch(plays <- readRDS(plays_filename) %>% fix_inconsistent_data_types(),error=report)
        report(paste0("Scraping plays from game: ",g))
        game_plays <- scrape_json_play_by_play(g)
        game_plays <- game_plays %>%
          fix_inconsistent_data_types()  
        report("Merging existing plays and new plays")
        new_plays <- bind_rows(plays,game_plays) %>% arrange(game_id,play_id)
        saveRDS(new_plays,plays_filename)
        
      }
      
      # finally merge things together
      report("Done hopefully")
      
      rm(new_plays)  # no need for this to take up memory anymore
    }
  }
  
}


#fix
fix_pbp <- function(pbp) {
  data <- pbp %>%
    filter(!is_na(epa), !is_na(posteam), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
    mutate(
      pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
      rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
      success = ifelse(epa>0, 1 , 0),
      passer_player_name = 
        str_replace_all(
          str_extract(desc, "(?<=\\s)[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
          " ", ""),
      receiver_player_name = 
        str_replace_all(
          str_extract(desc, "(?<=(to|for)\\s)[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
          " ", ""),
      rusher_player_name = 
        str_replace_all(
          str_extract(desc, "(?<=\\s)[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
          " ", ""),
      name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
      name = ifelse(name=="G.MinshewII", "G.Minshew", name),
      name = ifelse(name=="R.GriffinIII", "R.Griffin", name),
      name = ifelse(name=="Jos.Allen", "J.Allen", name),
      name = ifelse(name=="Jo.Freeman", "J.Freeman", name),
      name = ifelse(name=="Da.Brown", "D.Brown", name),
      name = ifelse(name=="JJohnson", "J.Johnson", name),
      name = ifelse(name=="Sh.Hill", "S.Hill", name),
      name = ifelse(name=="Matt.Moore", "M.Moore", name),
      name = ifelse(name=="Tr.Brown", "T.Brown", name),
      yards_gained=ifelse(play_type=="no_play",NA,yards_gained),
      play=1,season=year, incomplete_pass=if_else(interception==1, 1, incomplete_pass)
    ) %>%
    filter(pass==1 | rush==1)
  return(data) 
}



#colors
get_colors <- function() {
  colors <- teamcolors %>%
    filter(league == "nfl") %>%
    mutate(
      team_abb = case_when(
        name == "Arizona Cardinals" ~ "ARI",
        name == "Atlanta Falcons" ~ "ATL",
        name == "Baltimore Ravens" ~ "BAL",
        name == "Buffalo Bills" ~ "BUF",
        name == "Carolina Panthers" ~ "CAR",
        name == "Chicago Bears" ~ "CHI",
        name == "Cincinnati Bengals" ~ "CIN",
        name == "Cleveland Browns" ~ "CLE",
        name == "Dallas Cowboys" ~ "DAL",
        name == "Denver Broncos" ~ "DEN",
        name == "Detroit Lions" ~ "DET",
        name == "Green Bay Packers" ~ "GB",
        name == "Houston Texans" ~ "HOU",
        name == "Indianapolis Colts" ~ "IND",
        name == "Jacksonville Jaguars" ~ "JAX",
        name == "Kansas City Chiefs" ~ "KC",
        name == "Los Angeles Rams" ~ "LA",
        name == "Los Angeles Chargers" ~ "LAC",
        name == "Miami Dolphins" ~ "MIA",
        name == "Minnesota Vikings" ~ "MIN",
        name == "New England Patriots" ~ "NE",
        name == "New Orleans Saints" ~ "NO",
        name == "New York Giants" ~ "NYG",
        name == "New York Jets" ~ "NYJ",
        name == "Oakland Raiders" ~ "OAK",
        name == "Philadelphia Eagles" ~ "PHI",
        name == "Pittsburgh Steelers" ~ "PIT",
        name == "Seattle Seahawks" ~ "SEA",
        name == "San Francisco 49ers" ~ "SF",
        name == "Tampa Bay Buccaneers" ~ "TB",
        name == "Tennessee Titans" ~ "TEN",
        name == "Washington Redskins" ~ "WAS",
        TRUE ~ NA_character_
      ),
      posteam = team_abb,
      team_name = name
    ) %>% select(posteam,primary,secondary, team_name, division)
  
  return(colors)
}




#cpoe in each season
#trains on seasons at least as recent as y
#estimates cpoe on new data pbp using older data old_pbp
get_cpoe <- function(pbp, old_pbp, y) {
  old_data <- readRDS(old_pbp) %>%
    mutate(incomplete_pass=ifelse(interception==1, 1, incomplete_pass)) %>%
    filter((complete_pass==1 | incomplete_pass==1) & !is.na(air_yards) & air_yards >= -10 & season >= y & !is.na(receiver_player_id) & !is.na(pass_location)) %>%
    select(complete_pass,desc,air_yards,pass_location,name) %>%
    mutate(air_is_zero=ifelse(air_yards==0,1,0))
  gam_y <- gam(complete_pass ~ s(air_yards) + air_is_zero + factor(pass_location), data=old_data, method = "REML")
  
  passes <- pbp%>%filter((complete_pass==1 | incomplete_pass==1) & air_yards >= -10 & !is.na(receiver_player_id) & !is.na(pass_location)) %>%
    select(complete_pass,desc,air_yards,pass_location,name,season) %>%
    mutate(air_is_zero=ifelse(air_yards==0,1,0))
  
  passes$hat <- predict.gam(gam_y,passes)
  passes$r <- passes$complete_pass - passes$hat
  
  cp<-passes %>%group_by(name,season)%>%
    summarize(cpoe=100*mean(r))
  
  return(cp)
  
}


# apply completion probability
apply_completion_probability <- function(p) 
{
  
  # sort p and create cp column
  p$cp <- NA
  
  # season loop
  ## since our data only goes back to 2009, no CP for 2009
  
  # get data from previous three seasons
  passes <- p %>%
    filter(season >= 2006) %>%
    filter(complete_pass == 1 | incomplete_pass == 1) %>%
    filter(!is.na(air_yards) & air_yards >= -10 & !is.na(receiver_player_name) & !is.na(pass_location)) %>%
    mutate(air_is_zero=ifelse(air_yards == 0,1,0))
  
  
  # determine CPOE formula
  cp_model <- gam(complete_pass ~ s(air_yards) + s(yardline_100) +log(ydstogo) + air_is_zero + 
                    factor(down) + factor(pass_location) + factor(season),
                  data=passes, method="REML", family = "binomial")
  
  # apply CPOE
  passes$cp <- predict.gam(cp_model, passes, type = "response")
  passes <- passes %>% 
    select(game_id,play_id,cp)
  
  # merge into p
  p <- p %>%
    left_join(passes,by=c("game_id","play_id")) %>% 
    mutate(cp=ifelse(!is.na(cp.y),cp.y,cp.x)) %>% 
    select(-cp.x,-cp.y)
  
  return(p)
}




#give credit for where fumble happened in EPA like how yards gained works
fix_fumbles <- function(d) {
  
  n <- d %>% filter(complete_pass == 1 & fumble_lost == 1 & !is.na(epa)) %>%
    select(desc, game_id, play_id, epa, posteam, half_seconds_remaining, yardline_100, down, ydstogo, yards_gained, goal_to_go, ep) %>%
    mutate(
      #save old stuff for testing/checking
      down_old = down, ydstogo_old = ydstogo, epa_old = epa,
      #update yard line, down, yards to go from play result
      yardline_100 = yardline_100 - yards_gained, down = ifelse(yards_gained >= ydstogo, 1, down + 1),
      #if the fumble spot would have resulted in turnover on downs, need to give other team the ball and fix
      change = ifelse(down == 5, 1, 0), down = ifelse(down == 5 , 1, down),
      #yards to go is 10 if its a first down, update otherwise
      ydstogo = ifelse(down == 1, 10, ydstogo - yards_gained), 
      #fix yards to go for goal line (eg can't have 1st & 10 inside opponent 10 yard line)
      ydstogo = ifelse(yardline_100 < ydstogo, yardline_100, ydstogo), 
      #10 yards to go if possession change
      ydstogo = ifelse(change == 1 , 10, ydstogo),
      #flip field for possession change
      yardline_100 = ifelse(change == 1, 100 - yardline_100, yardline_100),
      goal_to_go = ifelse(yardline_100 == ydstogo, 1, 0),
      ep_old = ep) %>% 
    select(-ep, -epa)
  
  t2 <- calculate_expected_points(n, "half_seconds_remaining", "yardline_100", 
                                  "down", "ydstogo", "goal_to_go") %>%
    mutate(ep = ifelse(change == 1, -ep, ep), fixed_epa = ep - ep_old) %>%
    select(game_id, play_id, fixed_epa)
  
  d <- d %>%
    left_join(t2, by=c("game_id", "play_id")) %>%
    mutate(epa = ifelse(!is.na(fixed_epa), fixed_epa, epa)) %>%
    select(-fixed_epa)
  
  return(d)
  
}


get_qbr <- function(y) {
  
  # Thanks to Thomas Mock for the code
  # Be respectful - always pause between scraping sessions
  
  url <- paste0('https://www.espn.com/nfl/qbr/_/season/',y)
  
  # Build up URL
  raw_table <- url %>% 
    read_html() %>% 
    html_table() 
  
  comb_df <- cbind(raw_table[[1]], raw_table[[2]])
  
  r <- comb_df %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate(season = y) %>% 
    mutate(team = str_sub(name, -3)) %>% 
    mutate(team = str_remove(team, "[:lower:]+"),
           name = str_remove(name, team)) %>%
    separate(name, c("f","l"), sep= " ") %>%
    mutate(f=substr(f, 1, 1),
           name = paste0(f,".",l)) %>%
    select(name, season, qbr)
  
  return(r)
  
}




########## pbwr
get_pbwr <- function() {
  
  url <- paste0('https://www.espn.com/nfl/story/_/id/27584726/nfl-pass-blocking-pass-rushing-rankings-2019-pbwr-prwr-leaderboard')
  data<-html_nodes(read_html(url),'p') %>%  html_text()
  
  tmp <- strsplit(data[14], "\n")[[1]] %>% as_tibble()
  colnames(tmp) <- "data"
  
  wr <- tmp %>%
    separate(data, c("f","l"), sep= "[.]") %>%
    separate(l, c("team", "p"), sep= "[,]") %>%
    mutate(pbwr = substring(p, 2,3),
           pbwr = strtoi(pbwr),
           name = substring(team,2),
           posteam = case_when(
             name == "Arizona Cardinals" ~ "ARI",
             name == "Atlanta Falcons" ~ "ATL",
             name == "Baltimore Ravens" ~ "BAL",
             name == "Buffalo Bills" ~ "BUF",
             name == "Carolina Panthers" ~ "CAR",
             name == "Chicago Bears" ~ "CHI",
             name == "Cincinnati Bengals" ~ "CIN",
             name == "Cleveland Browns" ~ "CLE",
             name == "Dallas Cowboys" ~ "DAL",
             name == "Denver Broncos" ~ "DEN",
             name == "Detroit Lions" ~ "DET",
             name == "Green Bay Packers" ~ "GB",
             name == "Houston Texans" ~ "HOU",
             name == "Indianapolis Colts" ~ "IND",
             name == "Jacksonville Jaguars" ~ "JAX",
             name == "Kansas City Chiefs" ~ "KC",
             name == "Los Angeles Rams" ~ "LA",
             name == "Los Angeles Chargers" ~ "LAC",
             name == "Miami Dolphins" ~ "MIA",
             name == "Minnesota Vikings" ~ "MIN",
             name == "New England Patriots" ~ "NE",
             name == "New Orleans Saints" ~ "NO",
             name == "New York Giants" ~ "NYG",
             name == "New York Jets" ~ "NYJ",
             name == "Oakland Raiders" ~ "OAK",
             name == "Philadelphia Eagles" ~ "PHI",
             name == "Pittsburgh Steelers" ~ "PIT",
             name == "Seattle Seahawks" ~ "SEA",
             name == "San Francisco 49ers" ~ "SF",
             name == "Tampa Bay Buccaneers" ~ "TB",
             name == "Tennessee Titans" ~ "TEN",
             name == "Washington Redskins" ~ "WAS",
             TRUE ~ NA_character_
           ),) %>%
    select(posteam,pbwr)
  
  
  return(wr)
  
}





get_dvoa <- function(year, week) {
  
  #note: week 10 for 2015 is broken
  
  #for testing
  #year <- 2016
  #week <- 10
  
  url <- paste0('https://www.footballoutsiders.com/dvoa-ratings/',year,'/week-',week,'-dvoa-ratings')
  raw <-html_nodes(read_html(url),'table') %>%  html_text() %>% as_tibble()
  
  table <- raw %>% 
    mutate(lastwk = if_else(str_detect(value, "(LASTWEEK)|(LAST\n\t\t\tWEEK)"), 1, 0)) %>%
    filter(lastwk == 1) %>% select(-lastwk)
  
  data <- unlist(strsplit(table$value, split="\n")) %>% as_tibble() %>%
    mutate(one = ifelse(value=="1", 1, 0), tot=cumsum(one),
           strs = if_else(str_detect(value, "(TEAM)|(DVOA)|(WEEK)|(RANK)|(W-L)"), 1, 0),
           miss = ifelse(value=="", 1, 0)) %>%
    filter(tot>0 & strs == 0 & miss == 0) %>% select(-tot, -one, -strs, -miss)
  
  #scrape and clean
  bigdat <- NULL
  for (row in 1:(nrow(data)/13)) {
    #row obs per team
    #2 = team, 3 = dvoa, 7 = record, 8 = offense, 10 = defense
    
    if (row > 16 & week < 8 & year < 2018) {
      i <- 13 * (row - 1) +1
    } else if (week == 10 & year == 2015 & row > 16) {
      i <- 13 * (row - 1) + 4
    }
    else {
      i <- 13 * (row - 1)
    }
    
    toadd <- c(data%>%slice(i+2), data%>%slice(i+3), data%>%slice(i+7), data%>%slice(i+8), data%>%slice(i+10))
    bigdat <- rbind(bigdat,toadd)
  }
  bigdat <- bigdat %>% as_tibble()
  
  colnames(bigdat) <- c("posteam", "dvoa", "record", "dvoa_o", "dvoa_d")
  bigdat <- bigdat %>% filter(posteam!=" TEAM ") %>%
    mutate(
      season=year,  week = week,
      dvoa = str_remove(dvoa, "[%]"),
      dvoa_o = str_remove(dvoa_o, "[%]"),
      dvoa_d = str_remove(dvoa_d, "[%]"),
      dvoa = as.numeric(dvoa),
      dvoa_o = as.numeric(dvoa_o),
      dvoa_d = as.numeric(dvoa_d),
      posteam = as.character(posteam)
    )
  
  #to handle ties
  if (max(nchar(bigdat$record)) == 5) {
    
    r <- bigdat %>% separate(record, c("W", "L", "T"), sep = '-', fill = 'right') %>%
      mutate(T=ifelse(is.na(T), 0, T),
             W=as.numeric(W),
             L=as.numeric(L),
             T=as.numeric(T),
             games = W+L+T) %>% select(posteam, dvoa, dvoa_o, dvoa_d, season, week, games)
  }
  
  else {
    
    r <- bigdat %>% separate(record, c("W", "L"), sep = '-') %>%
      mutate(
        W=as.numeric(W),
        L=as.numeric(L),
        games = W+L) %>% select(posteam, dvoa, dvoa_o, dvoa_d, season, week, games)
    
  }
  
  
  return(r)
  
}




get_qb_dvoa <- function(year) {
  
  #note: week 10 for 2015 is broken
  
  #for testing
  #year <- 2016
  
  #needed bc they added ALEX column
  if (year < 2016) {
    l = 20
  } else {
    l = 21
  }
  
  url <- paste0('https://www.footballoutsiders.com/stats/qb/',year)
  
  raw <-html_nodes(read_html(url),'table') %>%  html_text() %>% as_tibble() %>% slice(1)
  
  data <- unlist(strsplit(raw$value, split="\n")) %>% as_tibble() %>%
    mutate(strs = if_else(str_detect(value, "(Player)|(Team)|(YAR)|(Rk)|(VOA)|(QBR)|(Passes)|(Yards)|(EYds)|(TD)|(FK)|(FL)|(INT)|(C%)|(DPI)|(Rank)|(Pass)|(ALEX)"), 1, 0),
           miss = ifelse(value=="", 1, 0)) %>%
    filter(strs == 0 & miss == 0) %>% select(-strs, -miss)
  
  #scrape and clean
  bigdat <- NULL
  for (row in 1:(nrow(data)/l)) {
    #row obs per team
    #1 = player, 7 = dvoa, 9 = voa
    
    i <- l * (row - 1)
    
    toadd <- c(data%>%slice(i+1), data%>%slice(i+7), data%>%slice(i+9))
    bigdat <- rbind(bigdat,toadd)
  }
  bigdat <- bigdat %>% as_tibble()
  
  colnames(bigdat) <- c("name", "dvoa", "voa")
  r <- bigdat %>% 
    mutate(
      season=year,
      dvoa = str_remove(dvoa, "[%]"),
      voa = str_remove(voa, "[%]"),
      dvoa = as.numeric(dvoa),
      voa = as.numeric(voa),
      name = as.character(name)
    )
  
  return(r)
  
}

