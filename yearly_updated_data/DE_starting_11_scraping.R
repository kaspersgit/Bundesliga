setwd("D:/Het Project/Voetbal predictions/Bundesliga")
library(rvest)
library(XML)
library(RCurl)

match_lineups=matrix(rep(0,25),nrow=1)
colnames(match_lineups)=c("match_date","hometeam","awayteam","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11")

# for in case we have to change the team names to make them in line with db names
url.names=c("1. FC Köln","1. FC Kaiserslautern","1. FC Nürnberg","1. FSV Mainz 05","Alemannia Aachen","Arminia Bielefeld","Bayer Leverkusen","Bayern München","Borussia Dortmund","Bor. Mönchengladbach","Eintracht Braunschweig","Eintracht Frankfurt","FC Augsburg","Energie Cottbus","Hansa Rostock","FC Ingolstadt 04","FC Schalke 04","FC St. Pauli","Fortuna Düsseldorf","Hamburger SV","Hannover 96","Hertha BSC","Karlsruher SC","MSV Duisburg","RB Leipzig","SC Freiburg","SC Paderborn 07","SpVgg Greuther Fürth","SV Darmstadt 98","SpVgg Unterhaching","TSV 1860 München","Werder Bremen","1899 Hoffenheim","VfB Stuttgart","VfL Bochum","VfL Wolfsburg")
team.names=c("FCKoln","Kaiserslautern","Nurnberg","Mainz","Aachen","Bielefeld","Leverkusen","BayernMunich","Dortmund","M'gladbach","Braunschweig","EinFrankfurt","Augsburg","Cottbus","HansaRostock","Ingolstadt","Schalke04","StPauli","FortunaDusseldorf","Hamburg","Hannover","Hertha","Karlsruhe","Duisburg","RBLeipzig","Freiburg","Paderborn","GreutherFurth","Darmstadt","Unterhaching","Munich 1860","WerderBremen","Hoffenheim","Stuttgart","Bochum","Wolfsburg")

# to scrape all previous seasons, only make sense to run once. Updating it on a weekly basis should at max only the last season be used
for (start_year in 2001:2018){
  for (round in 1:34){

      # Url of all the season games
    weblink <- paste("http://www.worldfootball.net/schedule/bundesliga-",start_year-1,"-",start_year,"-spieltag/",round,"/",sep = "")
    
    # read in the html structure
    raw_page <- read_html(weblink)
    
    # Getting the link to all the match details for every match, has a lot of duplicates
    all_matches_link_1=raw_page %>% html_nodes(xpath='//td[@class="hell"]//a') %>% html_attr('href')
    all_matches_link_2=raw_page %>% html_nodes(xpath='//td[@class="dunkel"]//a') %>% html_attr('href')
    
    all_matches_link=c(all_matches_link_1[grepl("report",all_matches_link_1)]
                       ,all_matches_link_2[grepl("report",all_matches_link_2)])
  
    # selecting the unique match links, so every match has one linke
    unique_matches_link=unique(all_matches_link)
    
    for (match_link in unique_matches_link){
      url <- paste("http://www.worldfootball.net",match_link,sep = "")
    
      page  <- read_html(url)
      
      #Team name and active players
      team_and_coach <- page %>% html_nodes(xpath='//th//a') %>% html_text()
      
      # extracting club name and starting player names from above vector (have to get the teams the same names as in the db)
      home_name <- team_and_coach[1]
      away_name <- team_and_coach[2]
      
      # starting players 
      raw_home_players <- page %>% html_nodes('td:nth-child(1) td:nth-child(2)') %>% html_text()
      home_players <- trimws(gsub("['0-9]","",gsub("\\n","",gsub("\\t", "", raw_home_players))))
      home_starting11 <- home_players[1:11]
      
      raw_away_players <- page %>% html_nodes('.box_zelle td:nth-child(2)') %>% html_text()
      away_players <- trimws(gsub("['0-9]","",gsub("\\n","",gsub("\\t", "", raw_away_players))))
      away_starting11 <- away_players[1:11]
      
      # Getting the date of the match , work needs to be continued from raw_match_details
      raw_match_details <- page %>% html_nodes('.standard_tabelle:nth-child(1) th:nth-child(2)') %>% html_text()
      match_details <- gsub("\\r","",gsub("\\n","",gsub("\\t", "", raw_match_details)))
      day <- sub("[^0-9]","",sub("*\\s.*","",sub('.*,\\s*','', match_details)))
      
      month_text <- gsub("[^a-zA-Z]","", match_details)
      months_all=c("January","February","March","April","May","June","July","August","September","October","November","December")
      month=as.numeric(which(sapply(months_all, function(x) grepl(x, month_text))))
      
      year <- substring(gsub("[^0-9]","",gsub(".*\\.","",match_details)),1,4)

      # putting day month and year together in date format equal to the one in the db
      match_date=format(as.Date(paste(year,month,day, sep = "-"),"%Y-%m-%d"))
      
      
      lineup = c(match_date,home_name,away_name,home_starting11,away_starting11)
      match_lineups = rbind(match_lineups,lineup)
    }
  }
}

for (i in 1:nrow(match_lineups)){
  match_lineups[i,2] <- team.names[which(match_lineups[i,2]==url.names)]
  match_lineups[i,3] <- team.names[which(match_lineups[i,3]==url.names)]
}

write.csv(match_lineups,"DE_match_lineups.csv",row.names = FALSE)

