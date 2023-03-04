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

rec_test <- 
     recipe(Y~., data = data_reg_test) %>% 
     
     step_normalize(all_numeric_predictors()) %>% 
     
     step_impute_mean(all_numeric_predictors()) %>%
     step_impute_mode(all_nominal_predictors()) %>%
     step_zv(all_nominal_predictors()) %>% 
     step_dummy(all_nominal_predictors())
prep(rec_test)
juice(prep(rec_test))->data_test
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

data_reg_resample <- vfold_cv(data_reg_train, v = 3)
xgboost_tune <- 
     parsnip::boost_tree(
          mode = "classification",
          trees = 15,
          min_n = tune(),
          tree_depth = tune(),
          learn_rate = tune(),
          loss_reduction = tune(),
          num_class = 3) %>%
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

xgboost_tuned2 <- tune::tune_grid(
     object = xgboost_wf,
     resamples = data_reg_resample,
     grid = xgboost_grid,
     metrics = yardstick::metric_set(accuracy,kap),
     control = tune::control_grid(verbose = TRUE)
)
xg_best_params <- xgboost_tuned %>%
     tune::select_best("accuracy")

summary(xgboost_tuned)
xgboost_tuned
show_best(xgboost_tuned)
autoplot(xgboost_tuned,metric="accuracy")
xg_stage_2_model <- xgboost_tune %>% 
     finalize_model(parameters = xg_best_params) %>% 
     fit(Y~.,juice(prep(rec_basic)))
data_train=prep(rec_basic)
Yhat=predict(xg_stage_2_model,data_test)
summary(Yhat)
RES=data.frame(Y=data_reg_test$Y,Yhat=predict(xg_stage_2_model,data_test))
summary(xg_stage_2_model)
write_rds(xgboost_tuned,"xgParam_1000T_50grid.rds")
#15h44