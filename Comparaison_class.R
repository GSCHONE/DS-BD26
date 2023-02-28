# CHARGEMENT DATA ####

library(aws.s3)
df <- 
     aws.s3::s3read_using(
          FUN = readRDS,
          # Mettre les options de FUN ici
          object = "don_class.rds",
          bucket = "gschone",
          opts = list("region" = "")
     )
library(tidyverse)
library(rpart)
library(ranger)
library(glmnet)
library(xgboost)
library(tidymodels)
library(pROC)
library(randomForest)


#### preparation données ####
rec_basic <- 
     recipe(Y~., data = df) %>% 
     
     step_normalize(all_numeric_predictors()) %>% 
     
     step_impute_mean(all_numeric_predictors()) %>%
     step_impute_mode(all_nominal_predictors()) %>%
     step_zv(all_nominal_predictors()) %>% 
     step_dummy(all_nominal_predictors())


prep(rec_basic)
juice(prep(rec_basic))->data_receipe

don=data_receipe

#### preparation données ####
XX <- model.matrix(Y~.,data=don)
YY <- don$Y

nb <- 5
set.seed(1234)
blocs <- sample(rep(1:nb,length=nrow(don)))
RES <- data.frame(Y=don$Y)


##XGBOOST PARAM  ####
param <- list("objective" = "multi:softmax",    
              "num_class" = 3,    
              "eval_metric" = "mlogloss",    
              "max_depth" = 16,   
              "eta" = 0.3,    
              "gamma" = 0,    
              "subsample" = 1,   
              "colsample_bytree" = 1,  
              "min_child_weight" = 12)

#### ALGO ####

for(ii in 1:nb){
     print(ii)
     ### DECOUPAGE
     donA <- don[blocs!=ii,]
     donT <- don[blocs==ii,]
     XXA <- XX[blocs!=ii,]
     XXT <- XX[blocs==ii,]
     YYA <- YY[blocs!=ii]
     
     
     
     ######################## 
     print(paste0(ii," GLMNET"))
     tmp <- cv.glmnet(XXA,YYA,alpha=0.5,family="multinomial")
     tmppred<-predict(tmp,XXT,s="lambda.min",type="response")
     RES[blocs==ii,"elasmin"] <- colnames(tmppred)[apply(tmppred, 1, which.max)]
     tmppred<-predict(tmp,XXT,s="lambda.1se",type="response")
     RES[blocs==ii,"elas1se"] <- colnames(tmppred)[apply(tmppred, 1, which.max)]
     
     ##Algo XGBOOST
     
     print(paste0(ii," XGBOOST"))
     tmp <- xgboost(param=param, data=XXA,label=as.numeric(YYA)-1, nrounds=100)
     tmppred<-predict(tmp,XXT)
     tmppred<-case_when(tmppred==0~"A",tmppred==1~"D",TRUE~"H")
     RES[blocs==ii,"xgboost"] <- tmppred
     
     ###Algo KNN 
     print(paste0(ii," KNN"))
     tmp <- nearest_neighbor(neighbors = 15, weight_func = 'rectangular') %>% 
          set_engine('kknn') %>% 
          set_mode('classification') %>%
          fit(Y~., data = donA)
     tmppred <- predict(tmp,donT,type="prob")
     colnames(tmppred)<-c("A","D","H")
     RES[blocs==ii,"kknn"] <- colnames(tmppred)[apply(tmppred, 1, which.max)]
     
     ###ALGO NNM
     print(paste0(ii," NNM"))
     tmp <- mlp() %>% 
          set_engine('nnet',MaxNWts =15000) %>% 
          set_mode('classification') %>% 
          fit(Y~.,data=donA)     
     tmppred <- predict(tmp,donT,type="prob")
     colnames(tmppred)<-c("A","D","H")
     RES[blocs==ii,"mlp"] <- colnames(tmppred)[apply(tmppred, 1, which.max)]
     
     
     ##ALGO FORET
     print(paste0(ii," FORET"))
     tmp <- ranger(Y~.,donA)
     tmppred <- predict(tmp,donT) #,type="prob")
     
     RES[blocs==ii,"foret"]<-tmppred$predictions
}

aws.s3::s3write_using(
     RES,
     FUN = write_rds,
     object = "resCLASUP.RDS",
     bucket = "gschone",
     opts = list("region" = "")
)





