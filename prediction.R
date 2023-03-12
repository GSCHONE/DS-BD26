library(tidymodels)
library(tidyverse)

#on charge le dernier workflow qui contient le modele finalisÃ© 
# et la recette de preproc
final_workflow=readRDS("final_workflow.rds")


#df_shiny recuperation du data frame Ã  partir du shiny 
#(stats equipes + stats individuelles)
# a voir pour le nom
df_shiny=data_classif[1,]

pred=predict(final_workflow,df_shiny,type="prob")
pred
### ON obtient les 3 Probas 
# avec 0 pour victoire domicile, 1 match nul, 2 victoire exterieure
