
library(tidyverse)
library(tidymodels)
library(purrr)


data_reg=readRDS("data_norm.RDS")



### Definition of training set, validation set and test set
set.seed(1234)
data_reg_split <- initial_split(data_reg, prop = 0.8, strata = Y) # par defaut prop = 0.75
data_reg_train <- training(data_reg_split)
data_reg_test <- testing(data_reg_split)
data_reg_val_split <- initial_split(data_reg_train)
data_reg_train_set <- training(data_reg_val_split)
data_reg_val_set <- testing(data_reg_val_split)


## Creation of a collection of recipes

### Basic recipe and visualisation of designe matrice

# recipe is nothing than a formula
rec_basic <- 
     recipe(Y~., data = data_reg_train_set) %>% 
     step_normalize(all_numeric_predictors()) %>% 
     step_impute_mean(all_numeric_predictors()) %>%
     step_impute_mode(all_nominal_predictors()) %>%
     step_zv(all_nominal_predictors()) %>% 
     step_dummy(all_nominal_predictors())

prep(rec_basic)
juice(prep(rec_basic))

formula(prep(rec_basic))

### Interaction recipe and visualisation of designe matrice


# #rec_interaction <- 
#  
#   rec_basic %>% 
#   ###MAJ NECESSAIRE !!!!
#   step_interact(~starts_with('lum'):starts_with('traj'))
#   step_interact(~defense_class:attack class)
# 
# prep(rec_interaction)
# juice(prep(rec_interaction))


### Spine recipe and visualisation of designe matrice

#rec_spline <- 
# rec_interaction %>% 
#step_ns(age, deg_free = 3)
# step_ns(age, deg_free = tune())

#prep(rec_spline)
#juice(prep(rec_spline))



## Model : https://www.tidymodels.org/find/parsnip/ (mentioned in the diapo)

data_reg_resample <- vfold_cv(data_reg_train_set, v = 5)
#UTILISER POUR LES HYPERPARAMETRES

# 
# lin_model <- 	linear_reg() %>% 
#      set_engine("glm") %>% 
#      set_mode("regression")

rf_model_tune <- 
     rand_forest(trees = tune()) %>% 
     set_engine("ranger") %>% 
     set_mode("regression")

rf_params <- 
     dials::parameters(
          trees()
     )
rf_grid <- 
     dials::grid_max_entropy(
          rf_params, 
          size = 10
     )
rf_wf <- 
     workflows::workflow() %>%
     add_model(rf_model_tune) %>% 
     add_recipe(rec_basic)



rf_tuned <- tune::tune_grid(
     object = rf_wf,
     resamples = data_reg_resample,
     grid = rf_grid,
     metrics = yardstick::metric_set(rmse, rsq, mae),
     control = tune::control_grid(verbose = TRUE)
)
rf_best_params <- rf_tuned %>%
     tune::select_best("rmse")

rf_stage_2_model <- rf_model_tune %>% 
     finalize_model(parameters = rf_best_params)

xgboost_tune <- 
     parsnip::boost_tree(
          mode = "regression",
          trees = 1000,
          min_n = tune(),
          tree_depth = tune(),
          learn_rate = tune(),
          loss_reduction = tune()
     ) %>%
     set_engine("xgboost", objective = "reg:squarederror")
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
          size = 20
     )



xgboost_wf <- 
     workflows::workflow() %>%
     add_model(xgboost_tune) %>% 
     add_recipe(rec_basic)
   
     
     
xgboost_tuned <- tune::tune_grid(
          object = xgboost_wf,
          resamples = data_reg_resample,
          grid = xgboost_grid,
          metrics = yardstick::metric_set(rmse, rsq, mae),
          control = tune::control_grid(verbose = TRUE)
     )
xg_best_params <- xgboost_tuned %>%
     tune::select_best("rmse")

xg_stage_2_model <- xgboost_tune %>% 
     finalize_model(parameters = xg_best_params)



knn_tune <- 
     nearest_neighbor(neighbors = tune()) %>%  # dist_power = tune(), weight_func = tune()
     set_engine('kknn') %>% 
     set_mode('regression')

knn_params <- 
     dials::parameters(
          neighbors()
     )


knn_grid <- 
     dials::grid_max_entropy(
          knn_params, 
          size = 20
     )



knn_wf <- 
     workflows::workflow() %>%
     add_model(knn_tune) %>% 
     add_recipe(rec_basic)



knn_tuned <- tune::tune_grid(
     object = knn_wf,
     resamples = data_reg_resample,
     grid = knn_grid,
     metrics = yardstick::metric_set(rmse, rsq, mae),
     control = tune::control_grid(verbose = TRUE)
)
knn_best_params <- knn_tuned %>%
     tune::select_best("rmse")

knn_stage_2_model <- knn_tune %>% 
     finalize_model(parameters = knn_best_params)


nnet_tune <- 
     mlp(penalty = tune(), epochs = tune()) %>% # hidden_units = tune() The number of hidden neurons
     set_engine('nnet') %>% 
     set_mode('regression')

nnet_params <- 
     dials::parameters(
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
     add_model(nnet_tune) %>% 
     add_recipe(rec_basic)



nnet_tuned <- tune::tune_grid(
     object = nnet_wf,
     resamples = data_reg_resample,
     grid = nnet_grid,
     metrics = yardstick::metric_set(rmse, rsq, mae),
     control = tune::control_grid(verbose = TRUE)
)
nnet_best_params <- nnet_tuned %>%
     tune::select_best("rmse")

nnet_stage_2_model <- nnet_tune %>% 
     finalize_model(parameters = nnet_best_params)
# svm_p_tune <- 
#      svm_poly(degree = tune()) %>% # cost = tune()
#      set_engine('kernlab') %>% 
#      set_mode('regression')
# xgboost_params <- 
#      dials::parameters(
#           min_n(),
#           tree_depth(),
#           learn_rate(),
#           loss_reduction()
#      )
# 
# 
# xgboost_grid <- 
#      dials::grid_max_entropy(
#           xgboost_params, 
#           size = 30
#      )
# 
# 
# 
# xgboost_wf <- 
#      workflows::workflow() %>%
#      add_model(xgboost_tune) %>% 
#      add_recipe(rec_basic)
# 
# 
# 
# xgboost_tuned <- tune::tune_grid(
#      object = xgboost_wf,
#      resamples = data_reg_resample,
#      grid = xgboost_grid,
#      metrics = yardstick::metric_set(rmse, rsq, mae),
#      control = tune::control_grid(verbose = TRUE)
# )










workflow_set(
     preproc = list(
          rec_basic,
          # rec_interaction,
          # rec_spline
          
     ), # a list of recipes
     
     models= list(
          rf_stage_2_model,
          xg_stage_2_model,
          knn_stage_2_model,
          nnet_stage_2_model
     ) # a list of models
     
)


chi_models

# **Grid search**
# tune_grid() is the primary function for conducting grid search. It resembles fit_resamples() from prior chapters, but adds
# 
# grid: An integer or data frame. When an integer is used, the function creates a space-filling design. If specific parameter combinations exist, the grid parameter is used to pass them to the function.
# 
# param_info: An optional argument for defining the parameter ranges, when grid is an integer.
# 

# **tunebayes()**
# The tune_bayes() function sets up Bayesian optimization iterative search. It’s similar to tune_grid() but with additional arguments. You can specify the maximum number of search iterations, the acquisition function to be used, and whether to stop the search if no improvement is detected. See Max and Julia for details and additional arguments.
# 
# test for tune_bayes
# ```{r}
data_reg_resample <- vfold_cv(data_reg_train_set, v = 3)



ctrl_bayes = control_bayes(save_pred = TRUE,
                           parallel_over = "everything",
                           save_workflow = TRUE)

bayes_results <-
     chi_models %>%
     workflow_map(
          seed = 1,
          fn = "tune_bayes",
          resamples = data_reg_resample,
          initial = 10, # le pb est probablement ici : 1 hyperparametre a tuner, une valeur initial, 
          control = ctrl_bayes
     )

bayes_results


#https://finetune.tidymodels.org/
# **The tune_sim_anneal() function**
# ```{r}

ctrl_sa <- control_sim_anneal(verbose = TRUE, no_improve = 10L)

set.seed(1234)
# 
# svm_sa <-
#   svm_wflow %>%
#   tune_sim_anneal(
#     resamples = penguins_folds,
#     metrics = roc_res,
#     initial = svm_initial,
#     param_info = svm_param,
#     iter = 50,
#     control = ctrl_sa
#   )
# 
# svm_sa <-
#   chi_models %>%
#   workflow_map(
#     seed = 1,
#     fn = finetune::tune_sim_anneal,
#     metrics = roc_res,
#     resamples = data_reg_resample,
#     initial = 10, # le pb est probablement ici : 1 hyperparametre a tuner, une valeur initial, 
#     iter = 5,
#     control = ctrl_bayes
#   )
# 
# ```
# 
# Warning: All models failed. See the `.notes` column.
# Run `rlang::last_error()`
# 
# 

## fitting
# ```{r}
data_reg_resample <- vfold_cv(data_reg_train_set, v = 5)

# result view gestion
keep_pred <- control_resamples(save_pred = T, save_workflow = T)

chi_models <- chi_models %>% 
     workflow_map(
          resamples = data_reg_resample,
          grid = 20,
          metrics = metric_set(accuracy, roc_auc, sensitivity, specificity, f_meas),
          control=keep_pred,
          verbose = T
     )


## Evaluation des models
# 
# bayes_results
# rank_results(chi_models, rank_metric = 'f_meas')
# 
# rank_results(bayes_results, rank_metric = 'roc_auc')
# 
# autoplot(chi_models, metric = "roc_auc", , std_errs = qnorm(0.95))
# 
# autoplot(chi_models, metric = "roc_auc", , std_errs = qnorm(0.9))
# 
# autoplot(chi_models, metric = "accuracy", , std_errs = qnorm(0.9))
# 
# 
# rank_results(chi_models, rank_metric = 'roc_auc') %>% 
#   filter(wflow_id == 'spline_xgboost') %>% 
#   group_by(.config) %>% 
#   summarise(n=n())

# 0.6558810 ROC


### select the best workflow and see the best hyperparameter tuning results


best_result <- 
     chi_models %>% 
     extract_workflow_set_result('basic_xgboost') %>% 
     select_best(metric = 'specificity')

best_result


## last fitting

best_result_fit <- 
     chi_models %>% 
     extract_workflow('basic_xgboost') %>% # in my opinion here we should improuve the coding manner
     finalize_workflow(best_result) %>% 
     last_fit(split = data_reg_split) # notice that here we learn from all dataset


best_result_fit %>% collect_predictions() 

best_result_fit %>% collect_metrics()


best_result_fit %>% collect_predictions() %>% group_by(grav_or_not, .pred_class) %>% summarise(n = n()) 



