# load libraries
library(glmnetUtils)
library(tidyverse)
source("code/functions/get_misclass_errors.R")

# load data
census_train = read_csv("data/clean/census_train.csv")
census_test = read_csv("data/clean/census_test.csv")

# Read all model metrics tables
glm_age_t <- read_csv("results/glm-age-metrics-table.csv")
glm_t <- read_csv("results/glm-metrics-table.csv")
ridge_t <- read_csv("results/ridge-metrics-table.csv")
lasso_t <- read_csv("results/lasso-metrics-table.csv")
tree_t <- read_csv("results/opt-tree-metrics.csv")
rf_t <- read_csv("results/rf-tuned-metrics.csv")
boost_t <- read_csv("results/gbm-tuned-metrics.csv")

comparison <- tibble("Model" = "Uni-Log (Age)", 
                     "Misclass_Err" = glm_age_t[1,2]$classifier_performance,
                     "FPR" = glm_age_t[2,2]$classifier_performance,
                     "FNR" = glm_age_t[3,2]$classifier_performance)

final_comparison <- comparison %>%
  add_row(Model = "Multi-Log", 
          Misclass_Err = glm_t[1, 2]$classifier_performance, 
          FPR = glm_t[2,2]$classifier_performance, 
          FNR = glm_t[3, 2]$classifier_performance) %>%
  add_row(Model = "Ridge", 
          Misclass_Err = ridge_t[1, 2]$classifier_performance, 
          FPR = ridge_t[2,2]$classifier_performance, 
          FNR = ridge_t[3, 2]$classifier_performance) %>%
  add_row(Model = "Lasso", 
          Misclass_Err = lasso_t[1, 2]$classifier_performance, 
          FPR = lasso_t[2,2]$classifier_performance, 
          FNR = lasso_t[3, 2]$classifier_performance) %>%
  add_row(Model = "Ordinary Tree", 
          Misclass_Err = tree_t[1, 2]$classifier_performance, 
          FPR = tree_t[2,2]$classifier_performance, 
          FNR = tree_t[3, 2]$classifier_performance) %>%
  add_row(Model = "Random Forest", 
          Misclass_Err = rf_t[1, 2]$classifier_performance, 
          FPR = rf_t[2,2]$classifier_performance, 
          FNR = rf_t[3, 2]$classifier_performance) %>%
  add_row(Model = "Boosting", 
          Misclass_Err = boost_t[1, 2]$classifier_performance, 
          FPR = boost_t[2,2]$classifier_performance, 
          FNR = boost_t[3, 2]$classifier_performance)

write_csv(x = final_comparison, file = "results/final-model-comparison.csv")

# Lazily check effect of changing threshold on model performance

load("results/gbm-fit-opt.Rda")

gbm_probabilities = predict(gbm_fit_opt,
                            n.trees = opt_num_trees,
                            type = "response",
                            newdata = census_test)

get_misclass_errors(gbm_probabilities, 
                    census_test)$classifier_performance[1]

get_misclass_errors(gbm_probabilities, 
                    census_test,
                    threshold = 0.4)$classifier_performance[1]

get_misclass_errors(gbm_probabilities, 
                    census_test,
                    threshold = 0.6)$classifier_performance[1]
