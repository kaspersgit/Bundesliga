library(XML)
library(rvest)

# let it run for every season 
for (s in 1:17){
  season_start <- 2000+s
  # Overview of the teams in te league in certain year, used for the links to team pages
  league_url <- paste0("https://www.transfermarkt.com/1-bundesliga/startseite/wettbewerb/L1/plus/?saison_id=",season_start)
  
  # read html file of page
  league_html <- read_html(league_url)
  
  #extract the links to the team pages
  teams_urls <- league_html %>% html_nodes(".hide-for-pad .vereinprofil_tooltip") %>% html_attr("href") %>% as.character()
  
  assign(paste0("players_value_",season_start),setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("name", "MV", "season")))
  
  # Loop over the different team pages
  for (team_url in teams_urls){
    # paste the team links behind the main site adress 
    url <- paste0("https://www.transfermarkt.co.uk",team_url)
    
    # read html file of page
    html_url <- read_html(url)
    
    # extract club name
    team_name <- html_url %>% html_nodes(".dataName b") %>% html_text() %>% as.character()
    
    # extract the player names and current market value
    players_values <- html_url %>% html_nodes("#yw1 .hauptlink") %>% html_text() %>% as.character()
  
  
    index <- seq(from = 1, to = length(players_values), by = 2)
    names <- players_values[index]
    values <- players_values[index+1]
    team <- rep(team_name,length(index))
    
    players_values_team <- cbind(paste0(season_start,season_start+1),team,names,values)
    colnames(players_values_team) <- c("season","team","name","MV")
    assign(paste0("players_value_",season_start),rbind(get(paste0("players_value_",season_start)),players_values_team))
  }
}

# get all seasons in one table
player_values_raw = rbind(players_value_2006,
                     players_value_2007,
                     players_value_2008,
                     players_value_2009,
                     players_value_2010,
                     players_value_2011,
                     players_value_2012,
                     players_value_2013,
                     players_value_2014,
                     players_value_2015,
                     players_value_2016,
                     players_value_2017)


# editing the names and values to be usefull
# to extract the name correctly, used later on 
get_name <- function(long_name){
  long_name <- as.vector(long_name)
  pn_split <- strsplit(long_name," ")
  pn <- vector(mode = "character", length = length(long_name))
  for (n in 1:length(long_name)){
    if (length(pn_split[[n]])==1){
      pn[n] <- substr(pn_split[[n]],1,nchar(pn_split[[n]])/2)}
    else if (length(pn_split[[n]]) > 1){
      pn[n] <- paste(pn_split[[n]][c(1:floor(length(pn_split[[n]])/2),length(pn_split[[n]]))],collapse = " ")
    }
  }
  return(pn)
}

# set mv 0 when not available and deals with k and m(illion)
get_mv <- function(long_value){
  long_value <- as.vector(long_value)
  mv <- vector(mode = "numeric", length = length(long_value))
  for (n in 1:length(long_value)){
    if (nchar(long_value[n])==1){
      mv[n] <- 0
    }else if (gsub("[^a-z]","",long_value[n]) == "k"){
      mv[n] <- as.numeric(gsub("[^0-9.]","",long_value[n])) *1000 
    }else if (gsub("[^a-z]","",long_value[n]) == "m"){
      mv[n] <- as.numeric(gsub("[^0-9.]","",long_value[n])) *1000000 
    }
  }
  return(mv)
}

player_values_raw[,2]=as.character(player_values_raw[,2])
player_values_raw[,3]=get_name(player_values_raw[,3])
player_values_raw[,4]=get_mv(player_values_raw[,4])

urlteams=c("1. FC K�ln","1.FC Kaiserslautern","1.FC Nuremberg","1.FSV Mainz 05","Alemannia Aachen","Arminia Bielefeld","Bayer 04 Leverkusen","Bayern Munich","Borussia Dortmund","Borussia M�nchengladbach","Eintracht Braunschweig","Eintracht Frankfurt","FC Augsburg","FC Energie Cottbus","FC Hansa Rostock","FC Ingolstadt 04","FC Schalke 04","FC St. Pauli","Fortuna D�sseldorf","Hamburger SV","Hannover 96","Hertha BSC","Karlsruher SC","MSV Duisburg","RB Leipzig","SC Freiburg","SC Paderborn 07","SpVgg Greuther F�rth","SV Darmstadt 98","SV Werder Bremen","TSG 1899 Hoffenheim","VfB Stuttgart","VfL Bochum","VfL Wolfsburg")
team.names=c("FCKoln","Kaiserslautern","Nurnberg","Mainz","Aachen","Bielefeld","Leverkusen","BayernMunich","Dortmund","M'gladbach","Braunschweig","EinFrankfurt","Augsburg","Cottbus","HansaRostock","Ingolstadt","Schalke04","StPauli","FortunaDusseldorf","Hamburg","Hannover","Hertha","Karlsruhe","Duisburg","RBLeipzig","Freiburg","Paderborn","GreutherFurth","Darmstadt","WerderBremen","Hoffenheim","Stuttgart","Bochum","Wolfsburg")

for (n in 1:nrow(player_values_raw)){
  player_values_raw$team[n]=team.names[which(player_values_raw$team[n]==urlteams)]
}

for (n in 1:nrow(player_values_raw)){
  player_values_raw$name[n]=gsub("[*.]","",player_values_raw$name[n])
}

write.csv(player_values_raw,"DE_player_values_clean.csv")


