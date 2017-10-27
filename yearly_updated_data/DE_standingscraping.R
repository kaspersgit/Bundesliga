# scraping premier league market values from transfermarkt.co.uk
library(XML)
library(RCurl)
library(httr)
library(rvest)

years=as.character(2000+c(0:17))
url.names=c("Dortmund","Bayern München","SC Freiburg","Hamburger SV","K'lautern","Bayer Leverkusen","Werder Bremen","Eintracht","FC Schalke 04","Energie Cottbus","1860 München","VFL Bochum","Hertha BSC","VFB Stuttgart","Unterhaching","VFL Wolfsburg","1.FC Köln","Hansa Rostock","M'gladbach","Sankt Pauli","1.FC Nürnberg","Bielefeld","Hannover 96","FSV Mainz 05","MSV Duisburg","Alemannia Aachen","Karlsruher SC","1899 Hoffenheim","FC Augsburg","SpVgg Greuther Fürth","Düsseldorf","Eintracht Braunschwe","SC Paderborn 07","SV Darmstadt 98","FC Ingolstadt 04","RB Leipzig")
team.names=c("Dortmund","BayernMunich","Freiburg","Hamburg","Kaiserslautern","Leverkusen","WerderBremen","EinFrankfurt","Schalke04","Cottbus","Munich1860","Bochum","Hertha","Stuttgart","Unterhaching","Wolfsburg","FCKoln","HansaRostock","M'gladbach","StPauli","Nurnberg","Bielefeld","Hannover","Mainz","Duisburg","Aachen","Karlsruhe","Hoffenheim","Augsburg","GreutherFurth","FortunaDusseldorf","Braunschweig","Paderborn","Darmstadt","Ingolstadt","RBLeipzig")

SBL.matrix=matrix(rep(0,length(url.names)*length(years)),ncol = length(years))

colnames(SBL.matrix)=years
rownames(SBL.matrix)=url.names

# filling the matrix with distances between te teams, skipping distance between a team and itself
for (year in years){
  # url has a simple structure with the teamnames
  if (which(year==years)==1){weblink <- "https://www.fcupdate.nl/stand/s111/duitsland-bundesliga-2000-2001/"}
  if (which(year==years)==2){weblink <- "https://www.fcupdate.nl/stand/s110/duitsland-bundesliga-2001-2002/"}
  if (which(year==years)==3){weblink <- "https://www.fcupdate.nl/stand/s109/duitsland-bundesliga-2002-2003/"}
  if (which(year==years)==4){weblink <- "https://www.fcupdate.nl/stand/s108/duitsland-bundesliga-2003-2004/"}
  if (which(year==years)==5){weblink <- "https://www.fcupdate.nl/stand/s3/duitsland-bundesliga-2004-2005/"}
  if (which(year==years)==6){weblink <- "https://www.fcupdate.nl/stand/s26/duitsland-bundesliga-2005-2006/"}
  if (which(year==years)==7){weblink <- "https://www.fcupdate.nl/stand/s54/duitsland-bundesliga-2006-2007/"}
  if (which(year==years)==8){weblink <- "https://www.fcupdate.nl/stand/s200/duitsland-bundesliga-2007-2008/"}
  if (which(year==years)==9){weblink <- "https://www.fcupdate.nl/stand/s272/duitsland-bundesliga-2008-2009/"}
  if (which(year==years)==10){weblink <- "https://www.fcupdate.nl/stand/s551/duitsland-bundesliga-2009-2010/"}
  if (which(year==years)==11){weblink <- "https://www.fcupdate.nl/stand/s649/duitsland-bundesliga-2010-2011/"}
  if (which(year==years)==12){weblink <- "https://www.fcupdate.nl/stand/s720/duitsland-bundesliga-2011-2012/"}
  if (which(year==years)==13){weblink <- "https://www.fcupdate.nl/stand/s836/duitsland-bundesliga-2012-2013/"}
  if (which(year==years)==14){weblink <- "https://www.fcupdate.nl/stand/s961/duitsland-bundesliga-2013-2014/"}
  if (which(year==years)==15){weblink <- "https://www.fcupdate.nl/stand/s1097/duitsland-bundesliga-2014-2015/"}
  if (which(year==years)==16){weblink <- "https://www.fcupdate.nl/stand/s1164/duitsland-bundesliga-2015-2016/"}
  if (which(year==years)==17){weblink <- "https://www.fcupdate.nl/stand/s1229/duitsland-bundesliga-2016-2017/"}
  # get the html text behind it
  page = GET(weblink)
  # Use rvest to extract all the tables
  tables <- rvest::html_table(content(page),fill=TRUE)
  # select table of interest 
  if (year==2007){
    standings=tables[[4]]
  }else{
      standings=tables[[3]]
  }
  # clean table
  standings=standings[c(seq(1,27,3)),]
  
  for (team in url.names){
    if (team %in% standings$X3){
      SBL.matrix[team,year]=as.numeric(substring(standings[standings$X3==team,"X2"],1,1))
    }else if(team %in% standings$X8){
      SBL.matrix[team,year]=as.numeric(standings[standings$X8==team,"X7"])
    }else{
      SBL.matrix[team,year]=16
    }
  }
}

# matching the team names as they are in other csv files 
rownames(SBL.matrix)=team.names

write.csv(SBL.matrix,"DE_standings.csv")

