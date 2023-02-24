#Stat desc par équipe

library(tidyverse)

#Résultat par équipe et par saison : part de victoire/nul/défaite total/domicile/extérieur
#Domicile
result_dom <- Match %>% 
  mutate(result_dom=case_when(
    home_team_goal>away_team_goal~"victoire_dom",
    home_team_goal<away_team_goal~"defaite_dom",
    TRUE~"nul_dom")) %>% 
  group_by(country_id, season, home_team_api_id, result_dom) %>% 
  summarise(nb=n())%>% 
  pivot_wider(names_from = result_dom, values_from = nb) %>% 
  mutate(victoire_dom = replace_na(victoire_dom, 0),
         nul_dom = replace_na(nul_dom, 0),
         defaite_dom = replace_na(defaite_dom, 0)) %>% 
  rename( id_equipe=home_team_api_id)

#Exétieur
result_ext <- Match %>% 
  mutate(result_ext=case_when(
    home_team_goal>away_team_goal~"defaite_ext",
    home_team_goal<away_team_goal~"victoire_ext",
    TRUE~"nul_ext")) %>% 
  group_by(country_id, season, away_team_api_id, result_ext) %>% 
  summarise(nb=n())%>% 
  pivot_wider(names_from = result_ext, values_from = nb) %>% 
  mutate(defaite_ext = replace_na(defaite_ext, 0),
         nul_ext = replace_na(nul_ext, 0),
         victoire_ext = replace_na(victoire_ext, 0))%>% 
  rename( id_equipe=away_team_api_id)

#Total
result_tot <- left_join(result_dom, result_ext, by = c("country_id", "season", "id_equipe"))%>%
  rowwise() %>%                               #Pour faire le calcul sur chaque ligne
  mutate(nb_match = sum(victoire_dom, nul_dom, defaite_dom, victoire_ext, nul_ext, defaite_ext),
         part_victoire_dom = victoire_dom/sum(victoire_dom, nul_dom, defaite_dom),
         part_nul_dom = nul_dom/sum(victoire_dom, nul_dom, defaite_dom),
         part_defaite_dom = defaite_dom/sum(victoire_dom, nul_dom, defaite_dom),
         part_victoire_ext = victoire_ext/sum(victoire_ext, nul_ext, defaite_ext),
         part_nul_ext = nul_ext/sum(victoire_ext, nul_ext, defaite_ext),
         part_defaite_ext = defaite_ext/sum(victoire_ext, nul_ext, defaite_ext),
         part_victoire_tot = sum(victoire_dom,victoire_ext)/nb_match,
         part_nul_tot = sum(nul_dom,nul_ext)/nb_match,
         part_defaite_tot = sum(defaite_dom,defaite_ext)/nb_match
         )

  
#Nombre de buts par équipe
#Domicile
but_dom <- Match %>% 
  group_by(country_id, season, home_team_api_id) %>% 
  summarise(but_marq_dom=sum(home_team_goal), but_enc_dom=sum(away_team_goal)) %>% 
  rename( id_equipe=home_team_api_id)

#Extérieur
but_ext <- Match %>% 
  group_by(country_id, season, away_team_api_id) %>% 
  summarise(but_marq_ext=sum(away_team_goal), but_enc_ext=sum(home_team_goal)) %>% 
  rename( id_equipe=away_team_api_id)

#Total
but_tot <- left_join(but_dom, but_ext, by = c("country_id", "season", "id_equipe"))%>%
  rowwise() %>%    
  mutate(but_marq_tot = sum(but_marq_dom, but_marq_ext),
         but_enc_tot =  sum(but_enc_dom, but_enc_ext))

#Jointure avec table résultats pour calculer le nombre de but moyen par match
result_but_tot <- left_join(result_tot, but_tot, by = c("country_id", "season", "id_equipe")) %>% 
  mutate(moy_but_marq_dom = but_marq_dom/(nb_match/2),
         moy_but_marq_ext = but_marq_ext/(nb_match/2),
         moy_but_marq_tot = but_marq_tot/nb_match,
         moy_but_enc_dom = but_enc_dom/(nb_match/2),
         moy_but_enc_ext = but_enc_ext/(nb_match/2),
         moy_but_enc_tot = but_enc_tot/nb_match)


#Attributs des équipes (dernière MAJ)
stat_Team_Attributes <- Team_Attributes %>% 
  arrange(team_api_id, date) %>% 
  group_by(team_api_id) %>% 
  slice_max(date)%>% 
  select(-ends_with("Class"), -buildUpPlayDribbling, -id, -team_fifa_api_id, -date) %>% 
  rename( id_equipe=team_api_id)



#Compilation des données
stat_desc_equipe <- left_join(result_but_tot, stat_Team_Attributes, by = "id_equipe")


#export excel pour test
library(xlsx)
write.xlsx(stat_desc_equipe, "stat_desc_equipe.xlsx")




