data=readRDS("data_norm.RDS")
library(randomForest)
library(ranger)
library(tidymodels)
library(tidyverse)
# recipe is nothing than a formula
rec_basic <- 
     recipe(Y~., data = data) %>% 

     step_normalize(all_numeric_predictors()) %>% 

     step_impute_mean(all_numeric_predictors()) %>%
     step_impute_mode(all_nominal_predictors()) %>%
     step_zv(all_nominal_predictors()) %>% 
     step_dummy(all_nominal_predictors())
prep(rec_basic)
juice(prep(rec_basic))->data_receipe

modRF=ranger(Y~.,data_receipe)
summary(modRF)
Yhat=predict(modRF,data_receipe)
result=data.frame(Y=data_receipe$Y,Yhat=Yhat$predictions )
result %>% 
     mutate(class=case_when(
         Y>0~"H",
          Y==0~"D",
          Y<0~"A",
         TRUE~"W"))->result
result$class=as.factor(result$class)
ggplot(data=result)+
     aes(x=class,y=Yhat)+
     geom_boxplot() -> ggbox

plotly::ggplotly(ggbox)

result %>% filter(class=="D")->resultD
sd(resultD$Yhat)
mean(resultD$Yhat)
conf
conf = 1.96*sd(resultD$Yhat)#/sqrt(nrow(resultD))
conf
confbas= mean(resultD$Yhat)-conf
confhaut=mean(resultD$Yhat)+conf
confhaut=quantile(resultD$Yhat,0.95)
confbas=quantile(resultD$Yhat,0.05)
result %>% 
     mutate(class2=as.factor(case_when(
          Yhat>confhaut~"H",
          Yhat<confbas~"A",
          TRUE~"D")))-> result2
glimpse(result2)
yardstick::conf_mat(result2,class,class2)
