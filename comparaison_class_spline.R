library(tidyverse)
library(rpart)
library(ranger)
library(glmnet)
library(xgboost)
library(tidymodels)
library(pROC)
# setwd(dir="U:/GitHub/DS-BD26/")
# data=readRDS("data_classif.RDS")
# data %>% 
#      mutate(Y=case_when(
#           Y=="H"~0,
#           Y=="D"~1,
#           Y=="A"~2))->data
# data$Y=as.factor(data$Y)
# summary(data$Y)


##### RECEIPE ####
rec_basic <- 
     recipe(Y~., data = data_classif) %>% 
     
     step_normalize(all_numeric_predictors()) %>% 
     
     step_impute_mean(all_numeric_predictors()) %>%
     step_impute_mode(all_nominal_predictors()) %>%
     step_zv(all_nominal_predictors()) %>% 
     step_dummy(all_nominal_predictors())


#### SPLINE ####
rec_spline <- 
     rec_basic %>% 
     step_ns(ends_with('overall_rating'), deg_free=3)
prep(rec_spline)
juice(prep(rec_spline))->data


data_split <- initial_split(data, prop = 0.8, strata = Y) # par defaut prop = 0.75
data_train <- training(data_split)
data_test <- testing(data_split)

don=data_train

####" CREATION BLOCS #####
XX <- model.matrix(Y~.,data=don)
YY <- don$Y

nb <- 5
set.seed(1234)
blocs <- sample(rep(1:nb,length=nrow(don)))
RES <- data.frame(Y=don$Y)


##XGBOOST PARAM ####
param <- list("objective" = "multi:softprob",    
              "num_class" = 3,    
              "eval_metric" = "auc",    
              "nthread" = 8,   
              "max_depth" = 16,   
              "eta" = 0.1,    
              "gamma" = 0,    
              "subsample" = 1,   
              "colsample_bytree" = 1,  
              "min_child_weight" = 12)

for(ii in 1:nb){
print(ii)
     ### DECOUPAGE ####
     donA <- don[blocs!=ii,]
     donT <- don[blocs==ii,]
     XXA <- XX[blocs!=ii,]
     XXT <- XX[blocs==ii,]
     YYA <- YY[blocs!=ii]
     
     
     
     #############GLM ###########
     print(paste0(ii," bloc -GLMNET"))
     tmp <- cv.glmnet(XXA,YYA,alpha=0.5,family="multinomial")
     tmppred<-predict(tmp,XXT,s="lambda.min",type="response")
     RES[blocs==ii,"elasmin"] <- colnames(tmppred)[apply(tmppred, 1, which.max)]
     tmppred<-predict(tmp,XXT,s="lambda.1se",type="response")
     RES[blocs==ii,"elas1se"] <- colnames(tmppred)[apply(tmppred, 1, which.max)]
     
     ##Algo XGBOOST ####
     print(paste0(ii," bloc -XGBOOT"))
     tmp <- xgboost(param=param, data=XXA,label=as.numeric(YYA)-1, nrounds=50 ,verbose = 0)
     tmppred<-predict(tmp,XXT,type="prob")
     pred <- matrix(tmppred, ncol=3, byrow=TRUE)
     colnames(pred)=c('0','1','2')
     RES[blocs==ii,"xgboost"] <- colnames(tmppred)[apply(tmppred, 1, which.max)]
     
     ###Algo KNN ####
     print(paste0(ii," bloc -KNN"))
     tmp <- nearest_neighbor(neighbors = 15, weight_func = 'rectangular') %>% 
          set_engine('kknn') %>% 
          set_mode('classification') %>%
          fit(Y~., data = donA)
     tmppred <- predict(tmp,donT,type="prob")
     colnames(tmppred)<-c('0','1','2')
     RES[blocs==ii,"kknn"] <- colnames(tmppred)[apply(tmppred, 1, which.max)]
     
     ###ALGO NNM ####
     print(paste0(ii," bloc -NNM"))
     tmp <- mlp() %>% 
          set_engine('nnet',MaxNWts =6000) %>% 
          set_mode('classification') %>% 
          fit(Y~.,data=donA)     
     tmppred <- predict(tmp,donT,type="prob")
     colnames(tmppred)<-c('0','1','2')
     RES[blocs==ii,"mlp"] <- colnames(tmppred)[apply(tmppred, 1, which.max)]
     
     
     ##### FORET SANS PRECISION MTRY=15 ####
     print(paste0(ii," bloc -FORET15"))
     tmp <- ranger(Y~.,donA,mtry = 15)
     tmppred <- predict(tmp,donT,type="prob")
     colnames(tmppred) <-c('0','1','2')
     RES[blocs==ii,"foretmtry15"]<-colnames(tmppred)[apply(tmppred, 1, which.max)]
     
     
     ##### FORET SANS PRECISION MTRY ####
     print(paste0(ii," bloc -FORET"))
     tmp <- ranger(Y~.,donA)
     tmppred <- predict(tmp,donT,type="prob")
     colnames(tmmpred) <-c('0','1','2')
     RES[blocs==ii,"foret"]<-colnames(tmppred)[apply(tmppred, 1, which.max)]
     
}

saveRDS(RES,"resCLASUP.RDS")
RES2=readRDS("resCLASUP.RDS")
RES2 %>% mutate_if(is.character,as.factor)->RES2
RES2 %>% select(-elas1se) -> RES2
resultatsGlobaux =data.frame(colnames(RES2)[2:ncol(RES2)])
for (k in 2:ncol(RES2)){
     
     cm<-caret::confusionMatrix(reference=RES2$Y,data=RES2[,k])
     resultatsGlobaux[k-1,"Accuracy"]=cm$overall[1]
     
}

