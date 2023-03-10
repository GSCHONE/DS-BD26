

library(tidyverse)

library(tidymodels)
library(ranger)

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

#### SPLINE ####
rec_spline <- 
     rec_basic %>% 
     step_interact(~starts_with('Home_chance'):starts_with('Away_defence')) %>% 
     step_interact(~starts_with('Home_defence'):starts_with('Away_chance')) %>% 
     step_ns(ends_with('overall_rating'), deg_free=3)
     
prep(rec_spline)
juice(prep(rec_spline))->data_train_st

rec_spline_t <- 
     rec_basic_t %>% 
     step_interact(~starts_with('Home_chance'):starts_with('Away_defence')) %>% 
     step_interact(~starts_with('Home_defence'):starts_with('Away_chance')) %>% 
     step_ns(ends_with('overall_rating'), deg_free=3)

prep(rec_spline_t)
juice(prep(rec_spline_t))->data_test_st



##NNET PARAM 

data_resample <- vfold_cv(data_train, v = 5)

rf_model_tune <- 
     
     rand_forest(trees = tune(),mtry = tune(),min_n=tune()) %>% 
     set_engine("ranger") %>% 
     set_mode("classification")


rf_params <- 
     dials::parameters(
          trees(),
          finalize(mtry(),select(data_train_t,-Y)),
          min_n()
     )
rf_grid <- 
     dials::grid_max_entropy(
          rf_params, 
          size = 45
     )
rf_wf <- 
     workflows::workflow() %>%
     add_model(rf_model_tune) %>% 
     add_recipe(rec_spline)



rf_tuned <- tune::tune_grid(
     object = rf_wf,
     resamples = data_resample,
     grid = rf_grid,
     metrics = yardstick::metric_set(accuracy,roc_auc,kap),
     control = tune::control_grid(verbose = TRUE)
)

rf_tuned %>%
     tune::show_best(metric = "roc_auc") %>%
     knitr::kable()
autoplot(rf_tuned,metric="accuracy")

rf_best_params <- rf_tuned %>%
     tune::select_best("accuracy")

rf_stage_2_model <- rf_model_tune %>% 
     finalize_model(parameters = rf_best_params)

mod=fit(rf_stage_2_model,Y~.,data=data_train_st)
summary(mod)
pred=predict(mod,new_data=data_test_st)
summary(pred)
RES=data.frame(Y=data_test_t$Y,est=pred)
cm =yardstick::conf_mat(data=RES,truth=Y, estimate=.pred_class)
cm
ggplot2::autoplot(cm, type = "heatmap")
table(data_test_t$Y)
saveRDS(mod,"modRFfinSPINT.rds")
summary(cm)
