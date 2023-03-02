library(tidyverse)
library(xgboost)
library(tidymodels)


data=readRDS("don_class.RDS")

rec_basic <- 
     recipe(Y~., data = data_reg_train) %>% 
     
     step_normalize(all_numeric_predictors()) %>% 
     
     step_impute_mean(all_numeric_predictors()) %>%
     step_impute_mode(all_nominal_predictors()) %>%
     step_zv(all_nominal_predictors()) %>% 
     step_dummy(all_nominal_predictors())
prep(rec_basic)
juice(prep(rec_basic))->data_receipe

don=data_receipe
set.seed(1234)
data_reg_split <- initial_split(data_classif, prop = 0.8, strata = Y) # par defaut prop = 0.75
data_reg_train <- training(data_reg_split)
data_reg_test <- testing(data_reg_split)
#################################
XX <- model.matrix(Y~.,data=data_reg_train)
YY <- data_reg_train$Y
#######################################
##création de blocs


##XGBOOST PARAM 

data_reg_resample <- vfold_cv(data_reg_train, v = 5)
xgboost_tune <- 
     parsnip::boost_tree(
          mode = "classification",
          trees = 1000,
          min_n = tune(),
          tree_depth = tune(),
          learn_rate = tune(),
          loss_reduction = tune()
     ) %>%
     set_engine("xgboost", objective = "multi:softmax")
# grid specification
xgboost_params <- 
     dials::parameters(
          min_n(),
          tree_depth(),
          learn_rate(),
          loss_reduction()
     )


xgboost_grid <- 
     dials::grid_max_entropy(
          xgboost_params, 
          size = 50
     )

xgboost_wf <- 
     workflows::workflow() %>%
     add_model(xgboost_tune) %>% 
     add_recipe(rec_basic)

xgboost_tuned <- tune::tune_grid(
     object = xgboost_wf,
     resamples = data_reg_resample,
     grid = xgboost_grid,
     metrics = yardstick::metric_set(accuracy,roc_auc),
     control = tune::control_grid(verbose = TRUE)
)
