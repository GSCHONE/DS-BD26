#install.packages("rvest")
#install.packages("tidyverse")

#library(rvest)
#library(tidyverse)


saisons=c("2008-2009","2009-2010","2010-2011",
          "2011-2012","2012-2013","2013-2014","2014-2015",
          "2015-2016","2016-2017","2017-2018","2018-2019",
          "2019-2020","2020-2021","2021-2022")
saison_init="2007-2008"
  
  
html_part1="https://fbref.com/en/comps/9/"
html_part2="/schedule/"
html_part3="-Premier-League-Scores-and-Fixtures"
#html_voulu <- read_html("https://fbref.com/en/comps/9/2021-2022/schedule/2021-2022-Premier-League-Scores-and-Fixtures") 


## initialisation des tables match
html_table_match=read_html(paste0(html_part1,saison_init,html_part2,saison_init,html_part3))
#recuperation de la table
table=html_table_match %>% html_nodes("tbody") %>% html_table() %>% data_frame(.[[1]])

#preparation donnee
table=table[,-c(1,13)]
table_inter=table[-(which(rowSums(is.na(table))>3)),]
vec_saison=rep(saison_init,dim(table_inter)[1])
table_inter=cbind(table_inter,vec_saison)
table_finale=table_inter

vecteur_nom_colonne=colnames(table_finale)

## boucle a partir de la seconde rotation
for (i in saisons){
  html_table_match=read_html(paste0(html_part1,i,html_part2,i,html_part3))
  
  table=html_table_match %>% html_nodes("tbody") %>% html_table() %>% data_frame(.[[1]])
  if (dim(table)[2]==15){
    table=table[,-c(1,7,9,15)]
    print(table)
    table_inter=table[-(which(rowSums(is.na(table))>2)),]
  }
  else{
    table=table[,-c(1,13)]
    table_inter=table[-(which(rowSums(is.na(table))>3)),]
  }
  
  vec_saison=rep(i,dim(table_inter)[1])
  table_inter=cbind(table_inter,vec_saison)
  
  colnames(table_inter)=vecteur_nom_colonne
  
  table_finale=rbind(table_finale,table_inter)
  
}




# table=html_voulu %>% html_nodes("tbody") %>% html_table() %>% data_frame(.[[1]])
# 
# 
# ## traitement table
# table=table[,-c(1,15)]
# table_finale=table[-(which(rowSums(is.na(table))>2)),]
# saison="2021-2022"
# vec_saison=rep(saison,dim(table_finale)[1])
# table_finale=cbind(table_finale,vec_saison)
