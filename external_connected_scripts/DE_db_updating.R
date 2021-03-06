DE_db_updating <- function(n_teams){
    
  setwd("D:/Het Project/Voetbal predictions/Bundesliga")
  library(rvest)
  library(XML)
  library(RCurl)
  library("RSQLite")
  library(DBI)
  
  # for in case we have to change the team names to make them in line with db names
  url.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","AFC Bournemouth","Bradford City","Brighton and Hove Albion","Burnley","Cardiff","Charlton","Chelsea","Coventry City","Crystal Palace","Derby","Everton","Fulham","Huddersfield Town","Hull City","Ipswich","Leeds","Leicester City","Liverpool","Manchester City","Manchester United","Middlesbrough","Newcastle United","Norwich","Portsmouth FC","QPR","Reading","Sheffield Utd.","Southampton","Stoke City","Sunderland","Swansea City","Tottenham Hotspur","Watford","West Bromwich Albion","West Ham United","Wigan","Wolves","Wimbledon")
  team.names=c("FCKoln","Kaiserslautern","Nurnberg","Mainz","Aachen","Bielefeld","Leverkusen","BayernMunich","Dortmund","M'gladbach","Braunschweig","EinFrankfurt","Augsburg","Cottbus","HansaRostock","Ingolstadt","Schalke04","StPauli","FortunaDusseldorf","Hamburg","Hannover","Hertha","Karlsruhe","Duisburg","RBLeipzig","Freiburg","Paderborn","GreutherFurth","Darmstadt","WerderBremen","Hoffenheim","Stuttgart","Bochum","Wolfsburg")
  
  # connect to the sqlite db
  con = dbConnect(RSQLite::SQLite(), dbname="historic_data/DE_football.db")
  
  # Check how many games of season 2017-2018 are already in the db (table 2017)
  last_game_in_db <- as.numeric(dbGetQuery(con,'SELECT 
                            COUNT(*)
                            FROM 
                              "DE_2018"'))
  
  count_matches_hist <- as.numeric(dbGetQuery(con,'SELECT 
                                                COUNT(*)
                                                FROM 
                                                DE_matches_hist'))
  
  count_lineup_hist <- as.numeric(dbGetQuery(con,'SELECT 
                                                COUNT(*)
                                                FROM 
                                                DE_match_lineup'))
  
  # Import match detail data from internet (updated csv file)
  raw.data.18 = read.csv("http://www.football-data.co.uk/mmz4281/1718/D1.csv")
  raw.data.18$Date = as.Date(raw.data.18$Date,"%d/%m/%y")
  raw.data.18 = raw.data.18[order(raw.data.18$Date,raw.data.18$HomeTeam),]
  
  #to make it import as text instead of changing it to a number into sqlite
  raw.data.18$Date <- as.character(raw.data.18$Date)
  
  #check what the last match was of which the data is available
  last_game_available <- nrow(raw.data.18)
  
  # Run as long as row count of database table is less than row count of available data
  if (last_game_in_db < last_game_available){
    
    # Select the matches which are not in the database (based on the row count of the database table)
    new_games_for_db <- cbind(((last_game_in_db+1):last_game_available),raw.data.18[((last_game_in_db+1):last_game_available),])
    
    #Insert the new rows into the db 69 columns of which the first one is just the row number
    add_matches=dbSendQuery(con, 'INSERT INTO "DE_2018"  
                                  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? )')
    for (i in 1:nrow(new_games_for_db)){
      dbBind(add_matches, unname(new_games_for_db[i,]))
    }
    
    # Clear result of the query
    dbClearResult(add_matches)
  }
  
  # close connection 
  dbDisconnect(con)
  
  # Triggers in the sqlite database update the DE_matches_hist and DE_match_odds table whenever we insert something in the 2017 table
  
  ## Now for updating the lineup tables, through the DE_R_lineup_input table
  # this check is based on the table which is updated above. Below we actually update another table, need to make a check if that table is updated
  if (count_lineup_hist < count_matches_hist){
      
    # Create dataframe with one row and the correct colnames
    match_lineups=data.frame(matrix(rep(0,26),nrow=1))
    colnames(match_lineups)=c("match_date","hometeam","awayteam","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","season")
    
    # This doesn't need to be a loop as we only do it for current season, will need some fix to make it more adaptable for next season
    for (start_year in 2018){
      
      # Url of all the season games , need to find site for German football
      weblink <- paste("https://www.11v11.com/competitions/bundesliga/",start_year,"/matches/",sep = "")
      
      # read in the html structure
      raw_page <- read_html(weblink)
      
      # Getting the link to all the match details for every match, has a lot of duplicates
      all_matches_link=raw_page %>% html_nodes(xpath='//div[@class="knockout-stage-list"]//a') %>% html_attr('href')
      
      # selecting the unique match links, so every match has one linke
      unique_matches_link=unique(all_matches_link)
      
      # matches which are not yet tracked (should maybe take extra matches in past and do a delete duplicates join to avoid different updates of the two source sites)
      non_captured_matches <- unique_matches_link[((count_lineup_hist %% (n_teams*(n_teams-1)))+1):(last_game_available %% (n_teams*(n_teams-1)))]
      
      # For every link of the matches not yet in the db scrape the lineups
      for (match_link in non_captured_matches){
        url <- paste("https://www.11v11.com",match_link,sep = "")
        
        page  <- read_html(url)
        
        #Team name and active players
        home_team <- page %>% html_nodes(xpath='//div[@class="home"]//a') %>% html_text()
        away_team <- page %>% html_nodes(xpath='//div[@class="away"]//a') %>% html_text()
        
        # extracting club name and starting player names from above vector
        home_name <- home_team[1]
        away_name <- away_team[1]
        
        home_name <- team.names[which(home_name==url.names)]
        away_name <- team.names[which(away_name==url.names)]
        
        home_starting11 <- home_team[2:12]
        away_starting11 <- away_team[2:12]
        
        # Getting the date of the match
        raw_match_details <- page %>% html_nodes(xpath='//div[@class="match-report"]//h1') %>% html_text()
        raw_match_date <- sub("^.*\\,", "", raw_match_details)
        
        months_all=c("January","February","March","April","May","June","July","August","September","October","November","December")
        month=as.numeric(which(sapply(months_all, function(x) grepl(x, raw_match_date))))
        
        day_year_time=gsub('\\D','',raw_match_date)
        day=substr(day_year_time,1,2)
        year=substr(day_year_time,3,6)
        
        # putting day month and year together in date format equal to the one in the db
        match_date=format(as.Date(paste(year,month,day, sep = "-"),"%Y-%m-%d"))
        season = ifelse(strftime(match_date,format = "%V") < 26, paste0(as.numeric(format(Sys.Date(),"%Y"))-1,as.numeric(format(Sys.Date(),"%Y"))),paste0(as.numeric(format(Sys.Date(),"%Y")),as.numeric(format(Sys.Date(),"%Y"))+1))
        
        lineup = c(match_date,home_name,away_name,home_starting11,away_starting11,season)
        match_lineups = rbind(match_lineups,lineup)
      }
    }
    
    #delete the first row with only zero's caused by the creation of the vector in the first place
    match_lineups <- match_lineups[match_lineups$match_date!=0,]
    colnames(match_lineups) <- NULL
    rownames(match_lineups) <- 1:nrow(match_lineups)
    
    # Open connectino to the db
    con = dbConnect(RSQLite::SQLite(), dbname="historic_data/DE_football.db")
    
    # Delete content of input table (because we just use this table for input, a trigger adds it to the main table)
    dbSendQuery(con, 'DELETE FROM DE_R_lineup_input')
    add_lineups =dbSendQuery(con, 'INSERT INTO DE_R_lineup_input  
                                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)')
    
    for (i in 1:nrow(match_lineups)){
      dbBind(add_lineups, unlist(match_lineups[i,]))
    }
    
    dbClearResult(add_lineups)
    
    dbDisconnect(con)
  }
  
  # Updating the next expected match lineups
  # First getting the expected line ups for the coming 10 games (need to make back up when not available or something else wrong)
  exp_lineups <- DE_exp_lineups_V2()
  exp_lineups$season <- ifelse(strftime(exp_lineups$match_date, format = "%V")>26,paste0(as.numeric(strftime(exp_lineups$match_date, format = "%Y")),as.numeric(strftime(exp_lineups$match_date, format = "%Y"))+1),paste0(as.numeric(strftime(exp_lineups$match_date, format = "%Y"))-1,as.numeric(strftime(exp_lineups$match_date, format = "%Y"))))
  names(exp_lineups) <- NULL
  
  # Open connectino to the db
  con = dbConnect(RSQLite::SQLite(), dbname="historic_data/DE_football.db")
  
  # Delete content of input table (because we just use this table for input, a trigger adds it to the main table)
  dbSendQuery(con, 'DELETE FROM DE_exp_lineups')
  add_lineups =dbSendQuery(con, 'INSERT INTO DE_exp_lineups
                                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)')
  
  for (i in 1:nrow(exp_lineups)){
    dbBind(add_lineups, exp_lineups[i,])
  }
  
  dbClearResult(add_lineups)
  
  dbDisconnect(con)
}