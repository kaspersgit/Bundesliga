setwd("D:/Het Project/Voetbal predictions/Premier-League")
library(rvest)
library(XML)
library(RCurl)

match_lineups=matrix(rep(0,25),nrow=1)
colnames(match_lineups)=c("match_date","hometeam","awayteam","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11")

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
      
      # extracting club name and starting player names from above vector
      home_name <- team_and_coach[1]
      away_name <- team_and_coach[2]
      
      home_starting11 <- home_team[2:12]
      away_starting11 <- away_team[2:12]
      
      # Getting the date of the match , work needs to be continued from raw_match_details
      raw_match_details <- page %>% html_nodes('td td:nth-child(2)') %>% html_text()
      raw_match_date <- gsub("t", "", raw_match_details)
      
      months_all=c("January","February","March","April","May","June","July","August","September","October","November","December")
      month=as.numeric(which(sapply(months_all, function(x) grepl(x, raw_match_date))))
      
      day_year_time=gsub('\\D','',raw_match_date)
      day=substr(day_year_time,1,2)
      year=substr(day_year_time,3,6)
      
      # putting day month and year together in date format equal to the one in the db
      match_date=format(as.Date(paste(year,month,day, sep = "-"),"%Y-%m-%d"))
      
      
      lineup = c(match_date,home_name,away_name,home_starting11,away_starting11)
      match_lineups = rbind(match_lineups,lineup)
    }
  }
}



