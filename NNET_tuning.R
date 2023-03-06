

library(tidyverse)
library(xgboost)
library(tidymodels)
library(ranger)
library(keras)
setwd(dir="U:/GitHub/DS-BD26/")

data_classif=readRDS("data_classif.RDS")

unique(data_classif$Y)
data_classif %>% 
     mutate(Y=case_when(
          Y=="H"~0,
          Y=="D"~1,
          Y=="A"~2))->data_classif
summary(data_classif$Y)
class(data_classif$Y)
data_classif$Y=as.factor(data_classif$Y)
set.seed(1234)
data_split <- initial_split(data_classif, prop = 0.8, strata = Y) # par defaut prop = 0.75
data_train <- training(data_split)
data_test <- testing(data_split)


rec_basic <- 
     recipe(Y~., data = data_train) %>% 
     
     step_normalize(all_numeric_predictors()) %>% 
     
     step_impute_mean(all_numeric_predictors()) %>%
     step_impute_mode(all_nominal_predictors()) %>%
     step_zv(all_nominal_predictors()) %>% 
     step_dummy(all_nominal_predictors())
prep(rec_basic)
juice(prep(rec_basic))->data_train_t

rec_basic_t <- 
     recipe(Y~., data = data_test) %>% 
     
     step_normalize(all_numeric_predictors()) %>% 
     
     step_impute_mean(all_numeric_predictors()) %>%
     step_impute_mode(all_nominal_predictors()) %>%
     step_zv(all_nominal_predictors()) %>% 
     step_dummy(all_nominal_predictors())
prep(rec_basic_t)
juice(prep(rec_basic_t))->data_test_t





##NNET PARAM 

data_resample <- vfold_cv(data_train, v = 5)

mnet_tune <- 
     parsnip::mlp(
          mode = "classification",
          epochs=tune(),
          hidden_units =  tune(),
          penalty=tune()) %>%
     set_engine("keras")

nnet_params <- 
     dials::parameters(
          hidden_units(),
          penalty(),
          epochs()
     ) 


nnet_grid <- 
     dials::grid_max_entropy(
          nnet_params, 
          size = 20
     )


nnet_wf <- 
     workflows::workflow() %>%
     add_model(mnet_tune) %>% 
     add_recipe(rec_basic)


nnet_tuned <- tune::tune_grid(
     object = nnet_wf,
     resamples = data_resample,
     grid = nnet_grid,
     metrics = yardstick::metric_set(accuracy,kap),
     control = tune::control_grid(verbose = TRUE)
)



mod=fit(mnet_tune,Y~.,data=data_receipe)
summary(mod)
pred=predict(mod,new_data=data_receip_t)
summary(pred)
RES=data.frame(Y=data_receip_t$Y)


c                                                                           
