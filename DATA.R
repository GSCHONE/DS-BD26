library(RSQLite)
library(tidyverse)
library(lubridate)
###https://www.kaggle.com/datasets/hugomathien/soccer


#NB ATTENTION impossible à charger sur le GIT trop volumineux
#repertoirePerso pour les données volumineuses REP
rep= "U:/DATASCIENCE/DS-BD26/"
file="database.sqlite"

conn <- dbConnect(RSQLite::SQLite(), paste0(rep,file))

###RECUPERATION EN LOCAL DES TABLES
for (table in dbListTables(conn)){
tmp = dbGetQuery(conn, paste0("SELECT * FROM ",table))
write_rds(tmp,paste0(rep,table))
rm(tmp)
}



##GENERE EN LOCAL LES FICHIERS RDS POUR CHAQUE CHAMPIONNAT / PAS NECESSAIRE SI ON TRAVAILLE QUE SUR PREMIERE LEAGUE
# unique(Match$country_id)
# for (id_ in unique(Match$country_id)) {
#   Match %>% filter(country_id == id_) -> tmp
#   write_rds(tmp, paste0(rep, "Match", id_))
#   rm(tmp)
# }

#VERSION AVEC JUSTE PL 
Match=read_rds(paste0(rep,"Match"))
Match %>% filter(country_id==1729)->Match_PL
rm(Match)

Player=read_rds(paste0(rep,"Player"))
Player_Attributes=read_rds(paste0(rep,"Player_Attributes"))
Team_Attributes=read_rds(paste0(rep,"Team_Attributes"))
Team=read_rds(paste0(rep,"Team"))



###SUPPRESSION DES VARIABLES Player_X_1 etc et Player_Y_1 etc (je ne sais pas à quoi ca correspond ? )
Match_PL %>% 
  select(-matches("_player_X"),-matches("_Player_Y")) %>% 
  mutate(date=as.Date(substr(date,1,10),"%Y-%m-%d")) -> Match_PL


Team_Attributes %>% mutate(date=as.Date(substr(date,1,10),"%Y-%m-%d")) -> Team_Attributes
Team_Attributes %>% mutate(Saison=ifelse(month(date)<9,
  paste0(year(date),"/",year(date)+1),
  paste0(year(date)+1,"/",year(date)+2))) %>%  
  group_by(team_api_id,Saison) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  select(-id,-team_fifa_api_id,-date)->Team_Attributes_L #L Pour light

Player_Attributes %>% mutate(date=as.Date(substr(date,1,10),"%Y-%m-%d")) -> Player_Attributes
Player_Attributes %>% mutate(Saison=ifelse(month(date)<9,
  paste0(year(date),"/",year(date)+1),
  paste0(year(date)+1,"/",year(date)+2))) %>%  
  group_by(player_api_id,Saison) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  select(-id,-player_fifa_api_id,-date)->Player_Attributes_L #L Pour light

Player %>%  mutate(birthday=as.Date(substr(birthday,1,10),"%Y-%m-%d")) %>% select(-id,-player_fifa_api_id) -> Player_L


unique(Match_PL2$season) 
unique(Team_Attributes_L$Saison)
unique(Player_Attributes_L$Saison)
## ON POURRAIT LIMITER AUX SAISONS DISPONIBLES DANS TEAM ATTRIBUTES

# Explore=Match_PL2[1,]
# 
# Explore %>% left_join(Team_Attributes_L %>% rename_with(~paste0("Home_",.)),by=c("home_team_api_id"="Home_team_api_id","season"="Home_Saison")) %>% 
#           left_join(Team_Attributes_L %>% rename_with(~paste0("Away_",.)),by=c("away_team_api_id"="Away_team_api_id","season"="Away_Saison")) ->Explore 
 
Match_PL %>% left_join(Team_Attributes_L %>% rename_with(~paste0("Home_",.)),by=c("home_team_api_id"="Home_team_api_id","season"="Home_Saison")) %>% 
          left_join(Team_Attributes_L %>% rename_with(~paste0("Away_",.)),by=c("away_team_api_id"="Away_team_api_id","season"="Away_Saison")) ->DON

      
for (k in 1:11){
  
  #home player
  namesHome=c(paste0("home_player_",k),"season")
  valuesHome=c(paste0("H",k,"_player_api_id"),paste0("H",k,"_Saison"))
  vecHome=setNames(valuesHome,namesHome)
  vecHomep=setNames(paste0("H",k,"_player_api_id"),paste0("home_player_",k))
  
  #away player
  namesAway=c(paste0("away_player_",k),"season")
  valuesAway=c(paste0("A",k,"_player_api_id"),paste0("A",k,"_Saison"))
  vecAway=setNames(valuesAway,namesAway)
  vecAwayp=setNames(paste0("A",k,"_player_api_id"),paste0("away_player_",k))

  DON %>% left_join(Player_Attributes_L %>% rename_with(~paste0("H",k,"_",.)),
                       by=vecHome) %>% 
    left_join(Player_Attributes_L %>% rename_with(~paste0("A",k,"_",.)),
                       by=vecAway) %>% 
    left_join(Player_L %>% rename_with(~paste0("H",k,"_",.)),by=vecHomep) %>% 
    left_join(Player_L %>% rename_with(~paste0("A",k,"_",.)),by=vecAwayp) ->DON
}

# unique(Team_Attributes_L$Saison)
# unique(DON_BK$season)
# unique(Player_Attributes_L$Saison)
Match_PL %>% select(id,date,home_team_api_id,away_team_api_id) %>% 
  left_join(Team %>% select(-id,-team_fifa_api_id,-team_short_name) %>% rename_with(~paste0("home_",.))) %>% 
  left_join(Team %>% select(-id,-team_fifa_api_id,-team_short_name) %>% rename_with(~paste0("away_",.))) %>% 
  select(-home_team_api_id,-away_team_api_id)->ListMatch

ListMatch %>% head(10)


DON_BK=DON
#DON=DON_BK
col=comes(DON)
# write.csv(col,"listCol.csv")

DON %>% mutate(Y=case_when(
  home_team_goal>away_team_goal~"H",
  home_team_goal<away_team_goal~"A",
  TRUE~"D")) -> DON


DON %>% 
  select(-ends_with("id")) %>% 
  select(-starts_with("Home_Player")) %>% 
  select(-starts_with("Away_Player")) -> DON


DON %>% filter(! season %in% c("2009/2010","2008/2009" ))-> DON
DON %>% select(-c(season,date,stage,goal,shoton,shotoff,foulcommit,card,cross,corner,possession,home_team_goal,away_team_goal))->DON
DON %>% colnames()
DON %>% select(-(1:30))-> DON
