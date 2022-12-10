

  +  +#PROJECT SOURCES
library(readxl)
library(tidyverse)
library(lubridate)
  library(json)
  library(httr)
library(rjson)
  #1 DONNEES PUBLIQUES, MIS 12/06/2022 VARIABLE Y : https://www.data.gouv.fr/fr/datasets/films-ayant-realise-plus-dun-million-dentrees/
COL_NAME=c("titre",
      "nationalit?",
      "sortie",
      "entr?es (millions)",
      "rang")
COL_TYPE=c("numeric","text","text","date","numeric")

DATA<-data.frame(matrix(nrow=0,ncol=length(COL_NAME)))
colnames(DATA)<-COL_NAME


for (k in 2003:2021){
  ENTREES<-read_excel("./DATA/1_ENTREES.xlsx",
                      skip=6,
                      sheet=as.character(k), #k définit l'onlet du fichier EXCEL
                      col_types=COL_TYPE)
  names(ENTREES)[1]="rang" ###DEFAU DE NOMMAGE DES PREMIERES COLONNES. RENOMMER SYSTEMATIQUEMENT PERMET D'EVITER LES ERREURS
 
  DATA=rbind(DATA,ENTREES) ### ON AJOUTE ONGLET PAR ONGLET LES DONNEES
 k=k+1

}



DATA %>%
  select(-rang) %>% 
  rename(Y=`entrées (millions)`) %>%
  rename(NAT=nationalité) %>% 
  arrange(desc(Y))->DATA

as.Date(as.POSIXct("2013-01-01 07:00", 'GMT'))

###ATTENTION ECARTS AVEC LES VRAIS DATA --> ON VA ALLER REQUETER LES NOMS DES FILMS LISTES 


BASE_NOM <- unique(data$titre)
DATA %>% filter(!is.na(titre))->DATA

DATA %>% 
     group_by(titre,sortie) %>%
     summarise(Y=sum(Y),.groups = 'drop') %>%
     rename(TITRE=titre)->TOT_TITRE

#WARNING PROBLEME DE DOUBLON DANS LES TITRES 


TOT_TITRE %>% group_by(TITRE) %>% summarise(n=n()) %>% filter(n>1) %>% arrange(desc(n))->DOUBLON
TOT_TITRE %>% filter(TITRE %in% DOUBLON$TITRE)
data %>% filter(titre %in% DOUBLON$TITRE)

### 2 FILMS EN DOUBLONS, COCO DESSIN ANIMEE ET CELUI AVEC GAD ELMALEH NON ?  // 4 FANTASTIQUES IDEM , DEUX FILMS 2015 ET 2005

write.csv(TOT_TITRE,"./DATA_CLEAN/FILMS_1MILLIONS.csv")

TOT_TITRE %>% 
     mutate(ifelse(TI))

for (k in 1:nrow(TOT_TITRE)){
     nom=paste(TOT_TITRE[k,"TITRE"],year(as.Date(TOT_TITRE[k,"sortie"]$sortie)))
     print(nom)
     URL=paste0("https://imdb-api.com/en/API/SearchMovie/k_04ykrtva/",nom)
     result=GET(URL)
     result_jon=fromJSON(rawToChar(result$content))
     if(is.null(dim(result_jon$results))){
          TOT_TITRE$id="INCONNU"
     }else{
          TOT_TITRE$id=result_jon$results[[1]]$id
     }
}
