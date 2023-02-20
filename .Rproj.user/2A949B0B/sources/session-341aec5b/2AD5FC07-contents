#Analyse des variables pour sélectionner les plus pertinentes 
#Après jointures, la base contient 1037 variables

#Suppression des variables :
#  Nom des joueurs, date anniversaire (-> Age ?) 
#  variables "class" des équipes
#  variables bookmaker --> moyenne 


#Analyse corrélation attributs des joueurs

library(corrplot)

colnames(Player_Attributes_L)

corr_player <- Player_Attributes_L

#Suppression des variables caractères
corr_player[ , c('player_api_id',
                 'preferred_foot',
                 'attacking_work_rate',
                 'defensive_work_rate',
                 'Saison')] <- list(NULL)

#Matrice de correlation (par défaut, méthod = pearson)
mcor <- cor(corr_player, use = "complete.obs")   #use = complete.obs pour les NA

corrplot(mcor, type="upper",tl.col="black", tl.srt=45)

mcor_abs <- abs(mcor)
colMeans(mcor_abs)


#Analyse corrélation attributs des équipes
colnames(Team_Attributes_L)

Team_Attributes_L %>% 
  select(-matches("class")) -> corr_team

corr_team[,c("Saison", "team_api_id")] <- list(NULL)

mcor <- cor(corr_team, use = "complete.obs")   #use = complete.obs pour les NA
corrplot(mcor, type="upper",tl.col="black", tl.srt=45)

mcor_abs <- abs(mcor)
colMeans(mcor_abs)

#Analyse corrélation bookmakers

colnames(Match_PL)
corr_book <- Match_PL[, 42:71]
mcor <- cor(corr_book, use = "complete.obs")   #use = complete.obs pour les NA

corrplot(mcor, type="upper",tl.col="black", tl.srt=45)

mcor_abs <- abs(mcor)
colMeans(mcor_abs)

