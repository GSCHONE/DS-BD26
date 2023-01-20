#Test RF mode bourrin --> toute la base
library(rpart)
library(randomForest)

tuneRF

#1er RF en modélisant H, D ou A (victoire Home, match nul, victoire Away)
set.seed(1234)

# DON_TEST=DON
# 
# for (i in 1:ncol(DON)) {
#           if (is.character(DON[,i])){
#                DON_TEST[,i]=as.factor(DON[,i])
#      }
# }

rf_bourin <- randomForest(Y~., data=DON, ntree=200, na.action=na.omit)
rf_bourin

DON %>% group_by(Y) %>% summarise(n())

DON[which(rowSums(is.na(DON))==0),] %>% group_by(Y) %>% summarise(n())

varImpPlot(rf_bourin)
x=varImpPlot(rf_bourin)


#2eme RF en modélisant le nombre de but par équipe
rf_bourin <- randomForest(Y~., data=DON, ntree=200, na.action=na.omit)
rf_bourin

DON %>% group_by(Y) %>% summarise(n())

DON[which(rowSums(is.na(DON))==0),] %>% group_by(Y) %>% summarise(n())

varImpPlot(rf_bourin)
x=varImpPlot(rf_bourin)



