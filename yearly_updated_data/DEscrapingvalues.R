# scraping premier league market values from transfermarkt.co.uk
library(XML)
library(RCurl)
library(httr)
library(rvest)

years=as.character(2000+c(0:17))
url.names=c("Bor. Dortmund","Bayern Munich","SC Freiburg","Hamburger SV","1.FC K'lautern","Bay. Leverkusen","Werder Bremen","E. Frankfurt","FC Schalke 04","Energie Cottbus","1860 Munich","VfL Bochum","Hertha BSC","VfB Stuttgart","Unterhaching","VfL Wolfsburg","1. FC Köln","Hansa Rostock","Bor. M'gladbach","FC St. Pauli","1.FC Nuremberg","Arm. Bielefeld","Hannover 96","1.FSV Mainz 05","MSV Duisburg","Alem. Aachen","Karlsruher SC","TSG Hoffenheim","FC Augsburg","Greuther Fürth","F. Düsseldorf","E. Braunschweig","SC Paderborn","SV Darmstadt 98","FC Ingolstadt","RB Leipzig")
team.names=c("Dortmund","BayernMunich","Freiburg","Hamburg","Kaiserslautern","Leverkusen","WerderBremen","EinFrankfurt","Schalke04","Cottbus","Munich1860","Bochum","Hertha","Stuttgart","Unterhaching","Wolfsburg","FCKoln","HansaRostock","M'gladbach","StPauli","Nurnberg","Bielefeld","Hannover","Mainz","Duisburg","Aachen","Karlsruhe","Hoffenheim","Augsburg","GreutherFurth","FortunaDusseldorf","Braunschweig","Paderborn","Darmstadt","Ingolstadt","RBLeipzig")

AvgAge.matrix=matrix(rep(0,length(url.names)*length(years)),ncol = length(years))
AvgMV.matrix=matrix(rep(0,length(url.names)*length(years)),ncol = length(years))
TotMV.matrix=matrix(rep(0,length(url.names)*length(years)),ncol = length(years))

colnames(AvgAge.matrix)=years
rownames(AvgAge.matrix)=url.names

colnames(AvgMV.matrix)=years
rownames(AvgMV.matrix)=url.names

colnames(TotMV.matrix)=years
rownames(TotMV.matrix)=url.names

# filling the matrix with distances between te teams, skipping distance between a team and itself
for (year in years){
  # url has a simple structure with the teamnames
  weblink <- paste("https://www.transfermarkt.co.uk/1-bundesliga/startseite/wettbewerb/L1/plus/?saison_id=",year,sep = "" )
  # get the html text behind it
  page = GET(weblink)
  # Use rvest to extract all the tables
  tables <- rvest::html_table(content(page),fill=TRUE)
  # select table of interest 
  market.values=tables[4][[1]]
  # clean table
  market.values=market.values[-1,-1]
  colum.names=names(market.values)
  colum.names=colum.names[-3]
  market.values=market.values[,-ncol(market.values)]
  names(market.values)=colum.names
  market.values$Age=as.numeric(sub(",",".",market.values$Age))
  market.values$TMV=regmatches(market.values$`Total market value`, gregexpr("[[:digit:]]+[[:punct:]][[:digit:]]+", market.values$`Total market value`))
  market.values$TMV=as.numeric(market.values$TMV)
  market.values$AMV=regmatches(market.values$`ø Market value`, gregexpr("[[:digit:]]+[[:punct:]][[:digit:]]+", market.values$`ø-MV`))
  market.values$AMV=as.numeric(market.values$AMV)
    
  for (team in market.values$name){
    AvgAge.matrix[team,year]=market.values[market.values$name==team,"Age"]
    TotMV.matrix[team,year]=market.values[market.values$name==team,"TMV"]
    AvgMV.matrix[team,year]=market.values[market.values$name==team,"AMV"]

  }
}

# matching the team names as they are in other csv files 
rownames(AvgAge.matrix)=team.names
rownames(AvgMV.matrix)=team.names
rownames(TotMV.matrix)=team.names

write.csv(AvgAge.matrix,"DE_AvgAge.csv")
write.csv(AvgMV.matrix,"DE_AvgMV.csv")
write.csv(TotMV.matrix,"DE_TotMV.csv")
