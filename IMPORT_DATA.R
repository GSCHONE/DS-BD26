

  +  +#PROJECT SOURCES
library(readxl)
library(tidyverse)

#1 DONNEES PUBLIQUES, MIS 12/06/2022 VARIABLE Y : https://www.data.gouv.fr/fr/datasets/films-ayant-realise-plus-dun-million-dentrees/
colName=c("titre",
      "nationalité",
      "sortie",
      "entrées (millions)",
      "rang")
colType=c("numeric","text","text","date","numeric")

data<-data.frame(matrix(nrow=0,ncol=length(colName)))
colnames(data)<-colName


for (k in 2003:2021){
  ENTREES<-read_excel("D:/DATA_SCIENCE/PROJET/DATA/1_ENTREES.xlsx",
                      skip=6,
                      sheet=as.character(k),
                      col_types=colType)
  names(ENTREES)[1]="rang"
 
 data=rbind(data,ENTREES)
 k=k+1
}



data %>%
  select(-rang) %>% 
  rename(Y=`entrées (millions)`) %>% 
  arrange(desc(Y))->data



###ATTENTION ECARTS AVEC LES VRAIS DATA --> ON VA ALLER REQUETER LES NOMS DES FILMS LISTES 


BASE_NOM <- unique(data$titre)
data %>% group_by(titre) %>% summarise(totY=sum(Y)) %>% arrange(desc(totY)) %>% 
  rename(TITRE=titre)->TOT_TITRE
data %>% select(titre,nationalité,sortie)-> REF_TITRE
REF_TITRE %>% rename(NAT=nationalité,TITRE=titre,SORTIE=sortie)->REF_TITRE


REF_TITRE %>% left_join(TOT_TITRE)-> DATA_OK
unique(DATA_OK$NAT) #852 noms différents




