

library(tidyverse)
library(xgboost)
library(tidymodels)
library(ranger)
library(keras)
library(tensorflow)
library(reticulate)
repl_python()
use_condaenv("r-tensorflow")
#setwd(dir="U:/GitHub/DS-BD26/")

#data_classif=readRDS("data_classif.RDS")

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
     set_engine("nnet",MaxNWts =60000)

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
autoplot(nnet_tuned,metric="kap")
summary(nnet_tuned)
nnet_best_params <- nnet_tuned %>%
     tune::select_best("accuracy")

nnet_model <- mnet_tune %>% 
     finalize_model(parameters = nnet_best_params)
mod=fit(nnet_model,Y~.,data=data_train_t)
summary(mod)
pred=predict(mod,new_data=data_test_t)
summary(pred)
RES=data.frame(Y=data_test_t$Y,est=pred)


                                                                        
