# scraping premier league distances from http://www.sportmapworld.com/distance
library(XML)
library(Rcurl)

urlteams=c("borussia-dortmund","bayern-munchen","sc-freiburg","hamburger-sv","kaiserslautern","bayer-leverkusen","werder-bremen","eintracht-frankfurt","schalke-04","energie-cottbus","tsv-1860-munchen","vfl-bochum","hertha-bsc","vfb-stuttgart","bayern-munchen","vfl-wolfsburg","fc-koln","hansa-rostock","monchengladbach","fc-st-pauli","fc-nurnberg","arminia-bielefeld","hannover-96","fsv-mainz-05","msv-duisburg","alemannia-aachen","karlsruher-sc","tsg-1899-hoffenheim","augsburg","spvgg-greuther-furth","fortuna-dusseldorf","eintracht-braunschweig","paderborn","sv-darmstadt-98","fc-ingolstadt-04","rb-leipzig")
team.names=c("Dortmund","BayernMunich","Freiburg","Hamburg","Kaiserslautern","Leverkusen","WerderBremen","EinFrankfurt","Schalke04","Cottbus","Munich1860","Bochum","Hertha","Stuttgart","Unterhaching","Wolfsburg","FCKoln","HansaRostock","M'gladbach","StPauli","Nurnberg","Bielefeld","Hannover","Mainz","Duisburg","Aachen","Karlsruhe","Hoffenheim","Augsburg","GreutherFurth","FortunaDusseldorf","Braunschweig","Paderborn","Darmstadt","Ingolstadt","RBLeipzig")

distance.matrix=matrix(rep(0,length(urlteams)^2),ncol = length(urlteams))

colnames(distance.matrix)=urlteams
rownames(distance.matrix)=urlteams

# filling the matrix with distances between te teams, skipping distance between a team and itself
for (home in urlteams){
  for (away in urlteams){
    if(home!=away){
      # url has a simple structure with the teamnames
      weblink <- paste("http://www.sportmapworld.com/distance/",home,"/",away,"/",sep = "" )
      # get the html text behind it
      rawpage_distance<- htmlTreeParse(weblink, useInternalNodes = TRUE)
      # look for the sentence where the distance is included
      sentence<-xpathSApply(rawpage_distance, "//div[@class='12u 12u(mobile)']//section[@class='boxwhite']//p",xmlValue)
      # make sure you take the last piece where only km number is included
      short.sentence=substr(sentence,nchar(sentence)-20,nchar(sentence))
      # extract only numeric values from that piece of sentence
      matches <- regmatches(short.sentence, gregexpr("[[:digit:]]+", short.sentence))
      distance=as.numeric(unlist(matches))
      # save distance on the right place in matrix
      distance.matrix[home,away]=distance
    }
  }
}

# matching the team names as they are in other csv files 
colnames(distance.matrix)=team.names
rownames(distance.matrix)=team.names

write.csv(distance.matrix,"DE_distances.csv")
