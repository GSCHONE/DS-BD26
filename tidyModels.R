
library(tidyverse)
library(tidymodels)
library(purrr)


data_reg=load_rds("data_reg.rds")



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
     step_normalize(all_numeric_predictors) %>% 
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



log_model <- logistic_reg() %>% 
     set_engine("glm") %>% 
     set_mode("regression")

rf_model <- 
     rand_forest(trees = tune()) %>% 
     set_engine("ranger") %>% 
     set_mode("regression")


xgboost_model <- 
     boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), loss_reduction = tune()) %>% 
     set_engine('xgboost') %>% 
     set_mode('regression')

knn_tune <- 
     nearest_neighbor(neighbors = tune()) %>%  # dist_power = tune(), weight_func = tune()
     set_engine('kknn') %>% 
     set_mode('regression')

nnet_tune <- 
     mlp(penalty = tune(), epochs = tune()) %>% # hidden_units = tune() The number of hidden neurons
     set_engine('nnet') %>% 
     set_mode('regression')

svm_p_tune <- 
     svm_poly(degree = tune()) %>% # cost = tune()
     set_engine('kernlab') %>% 
     set_mode('regression')




workflow_set(
     preproc = list(
          rec_basic,
          # rec_interaction,
          # rec_spline
          
     ), # a list of recipes
     
     models= list(
          log_model,
          rf_model,
          xgboost_model,
          knn_tune,
          nnet_tune,
          svm_p_tune
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

# **tune_bayes()**
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
# data_reg_resample <- vfold_cv(data_reg_train_set, v = 3)

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



