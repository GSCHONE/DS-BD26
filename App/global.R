#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("shinythemes")
#install.packages("rintrojs")

library(shiny)
library(ggplot2)
library(shinythemes)
library(rintrojs)
library(tidyverse)

source("./scripts/chargement_donnees_team.R")
source("./scripts/chargement_donnees_player.R")
source("./scripts/chargement_Player.R")
source("./scripts/chargement_Team.R")
source("./scripts/chargement_ListMatch.R")
source("./scripts/chargement_Match_PL.R")
source("./scripts/traitement_donnees.R")

choiceschampionnat<-c("championnat 1",
                      "championnat 2",
                      "championnat 3")

choicesteam <- names(table(unique(ListMatch$home_team_long_name)))

### fabrication de la table qui permettra de mettre les attributs des équipes sur la dernière saison
# table_transi=
#   Team %>% filter(team_long_name %in% names(table(unique(ListMatch$home_team_long_name)))) %>% 
#   select(c(4,2)) %>% 
#   arrange(.$team_api_id)
# 
# 
# table_attribut_team=
#   Team_Attributes_L %>% filter(team_api_id %in% table_transi$team_api_id) %>% 
#   filter(Saison =="2016/2017") %>% 
#   arrange(.$team_api_id) %>% 
#   right_join(table_transi,.)



#### pas vu comment faire le lien entre teams et équipe
#### probablement un passage à faire pour mettre dans l'ui pour en faire des choix interactifs. 
#### ajout d'un potentiel choix "autre" après ça qui arrive sur un recherche en fonction de ce que tape l'utilisateur
choicesjoueurdom <- names(table(unique(Player$player_name)))

choicesjoueurext <- names(table(unique(Player$player_name)))

