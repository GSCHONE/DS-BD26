# DS-BD26
PROJET R
Problématique : 

Prévisions des scores des matchs de foot en fonction de plusieurs variables :
- notes des joueurs issus du jeux vidéo FIFA
- affluence dans les stades
- séries de matchs en cours
- ...

Données sources : 
- webscrapping sur l'affluence des stades (1)
- https://www.kaggle.com/datasets/hugomathien/soccer (2)
- https://www.kaggle.com/datasets/technika148/football-database - a voir
- ...


TODO : 

Pour la source (2) travailler les champs stats qui ne fonctionnent pas 

pour le scrapping généraliser et faire un lien entre le match_id_api et le scrap.J'ai créé une liste des matchs de première ligue dans le dossier DATA (à partir du script DATA.R) format RDS ou csv.

Choisir la modélisation : 
 - modéliser comme les paris sportifs : victoire domicile, nul, victoire exterieure
 - modéliser le score des deux equipes en fonctions de toutes les autres variables pour en déduire l'issue du match 
 - autre ? 


A partir de la base SQL LITE (2)
Trop volumineux pour intégrer à GIT donc on essaye de ventiler par numéro de championnat. 

country_id `n()`    id name                    
        <int> <int> <int> <chr>                   
 1          1  1728     1 Belgium Jupiler League  
 2       1729  3040  1729 England Premier League  
 3       4769  3040  4769 France Ligue 1          
 4       7809  2448  7809 Germany 1. Bundesliga   
 5      10257  3017 10257 Italy Serie A           
 6      13274  2448 13274 Netherlands Eredivisie  
 7      15722  1920 15722 Poland Ekstraklasa      
 8      17642  2052 17642 Portugal Liga ZON Sagres
 9      19694  1824 19694 Scotland Premier League 
10      21518  3040 21518 Spain LIGA BBVA         
11      24558  1422 24558 Switzerland Super League

Cela reste également trop volumineux pour un hébergement direct sur GIT ... A VOIR en attendant local et premier test avec uniquement la Première League (Match_PL) 

WARNING LES CHAMPS STATS DU MATCH SONT A RETRAVAILLER goal shoton shotoff foulcommit card cross corner possession on se retrouve avec un sting assez volumineux ressemblant a du XML. Soit on vire tout et on rescrap pour obtenir ces infos soit faut modifier la chaine.

TRAITEMENT :

- SUPPRESSION DES VARIABLES Player_X_1 etc et Player_Y_1 etc (je ne sais pas à quoi ca correspond ? )
Pour info on dispose aussi des côtes de plusieurs bookmaker dans la table Match.

- recodage des champs date et création d'un champ Saison pour les tables Team_attributes et Player_attributes. 
     exemple 01/08/2010 -> Saison 2010-2011 / 01/09/2010-> saison 2011-2012. 
     Si on a plusieurs notes pour une même saison on ne conserve que la plus récente de la saison correspondante.

- Jointure avec la team attributes pour la team qui recoit et la team qui se déplace.
Puis jointure avec les 11 joueurs domicile et les 11 joueurs exterieurs avec les table Player (pour l'age, la taille, le poids) 
et player_attributes pour toutes les références techniques.

     