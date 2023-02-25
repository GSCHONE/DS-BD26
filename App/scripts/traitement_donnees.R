stat_desc_player %>% arrange(.$home_team_api_id) %>% filter(home_team_api_id==1601)


Match_PL=Match_PL %>% filter(season=="2015/2016") 

table_1=Match_PL %>% select(starts_with("home_")) %>% select(-ends_with("goal"))
table_2=Match_PL %>% select(starts_with("away_")) %>% select(-ends_with("goal"))
colname_player_team=c("Team","Player_1","Player_2","Player_3","Player_4","Player_5","Player_6"
                      ,"Player_7","Player_8","Player_9","Player_10","Player_11")
colnames(table_1)=colname_player_team
colnames(table_2)=colname_player_team
table_equipe_player=rbind(table_1,table_2)

#### ici on va chercher à faire le 11 de départ 

table_11_player=c(unique(table_equipe_player$Team))

for (j in (2:length(names(table_equipe_player)))){

  colonne=c()
  
  for (k in unique(table_equipe_player$Team)){

    temp_ligne=as.integer(names(sort(table(table_equipe_player[j][which(table_equipe_player$Team==k),]),decreasing = TRUE))[1])
    colonne=append(colonne,temp_ligne)
    
  }
  table_11_player=cbind(table_11_player,colonne)
}

colnames(table_11_player)=colname_player_team

#### ajout des noms équipe
temp_team=Team %>% select(c(4,2))
temp_player=Player %>% select(c(2,3))
temp_name_equipe=as_tibble(table_11_player) %>% left_join(temp_team,by=c("Team"="team_api_id"))


save_name=colnames(temp_name_equipe)
for (i in 1:11){
  temp_colname=c(names(temp_name_equipe)[1:i],"player_api_id",names(temp_name_equipe)[i+2:length(names(temp_name_equipe))])
  colnames(temp_name_equipe)=temp_colname
  temp_name_equipe=temp_name_equipe %>% left_join(temp_player,by="player_api_id")
  save_name=append(save_name,paste0('name_player_',i))
  colnames(temp_name_equipe)=save_name
}
