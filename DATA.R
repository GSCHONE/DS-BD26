library(RSQLite)
library(tidyverse)
library(lubridate)
###https://www.kaggle.com/datasets/hugomathien/soccer

conn <- dbConnect(RSQLite::SQLite(), "C:/Users/cepe-s3-01/Documents/DS/database.sqlite")


dbListTables(conn)


Country<-dbGetQuery(conn, "SELECT * FROM Country")
Match<-dbGetQuery(conn, "SELECT * FROM Match")

Match %>% mutate(date=as.Date(substr(date,1,10),"%Y-%m-%d")) -> Match
Player<-dbGetQuery(conn, "SELECT * FROM Player")
Player_Attributes<-dbGetQuery(conn, "SELECT * FROM Player_Attributes")
Player_Attributes %>% mutate(date=as.Date(substr(date,1,10),"%Y-%m-%d")) -> Player_Attributes
Player_Attributes %>% mutate(Saison=ifelse(month(date)<9,
  paste0(year(date),"/",year(date)+1),
  paste0(year(date)+1,"/",year(date)+2))) %>%  
  group_by(player_api_id,Saison) %>% 
  arrange(desc(date)) %>% 
  slice(1)->test

min(Match$date)
max(Player_Attributes$date)
Team<-dbGetQuery(conn, "SELECT * FROM Team")
Team_Attributes<-dbGetQuery(conn,"SELECT * FROM Team_Attributes")
sqlite_sequence<-dbGetQuery(conn,"SELECT * FROM sqlite_sequence")




Player_Attributes
Player_Attributes %>% mutate(date_max_2008 = max(date) )

Player_Attributes %>% group_by(year(date),months(date)) %>% summarise(n=n())-> test

annee=c(2007,2008)
date_max=vector()
for (i in annee){
  date_max[i]=as.Date(paste0("30-09-",i),"%Y-%m-%d")
}



  
  
  
Player_Attributes %>% mutate(date2018=max(date))

date_saison=function(id, date_perso,saison){
  
  
  
}