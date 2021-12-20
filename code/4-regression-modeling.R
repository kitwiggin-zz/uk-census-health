# load libraries
library(glmnetUtils)  # to run ridge and lasso
library(tidyverse)
source("code/functions/plot_glmnet.R") # for lasso/ridge trace plots
source("code/functions/get_misclass_errors.R")

# read in the data
census_train = read_csv("data/clean/census_train.csv")
census_test = read_csv("data/clean/census_test.csv")

# Run Univariate logistic regression using Age
glm_fit_age <- glm(Health ~ Age, family = 'binomial', data = census_train)

save(glm_fit_age, file = "results/glm_fit_age.Rda")

age_probs <- predict(glm_fit_age, newdata = census_test, type = "response")

# Use the misclass function to calculate a table of misclassification error
# as well as false positive and false negative rate
glm_age_model_metrics <- get_misclass_errors(age_probs, census_test)

# Lazily check if increasing or decreasing the threshold slightly improves it
high_thresh <- get_misclass_errors(age_probs, 
                                   census_test, 
                                   threshold = 0.6)$classifier_performance[1]
high_thresh
low_thresh <- get_misclass_errors(age_probs, 
                                   census_test, 
                                   threshold = 0.4)$classifier_performance[1]
low_thresh

# Save these errors (using the 0.5 threshold)
write_csv(x = glm_age_model_metrics, 
          file = "results/glm-age-metrics-table.csv")

# Run multivariate logistic regression
glm_fit_full <- glm(`Health` ~., family = 'binomial', data = census_train)

save(glm_fit_full, file = "results/glm_fit_full.Rda")

summary(glm_fit_full)

glm_full_probs <- predict(glm_fit_full_loaded, 
                                newdata = census_test,
                                type = "response")

glm_model_metrics <- get_misclass_errors(fitted_probabilities, census_test)

write_csv(x = glm_model_metrics, file = "results/glm-metrics-table.csv")

# Ridge penalised logistic regression
set.seed(5)
ridge_fit <- cv.glmnet(`Health` ~ .,   
                      alpha = 0, 
                      nfolds = 10, 
                      family = 'binomial',
                      type.measure = 'class',
                      data = census_train)

# save the ridge fit object
save(ridge_fit, file = "results/ridge_fit.Rda")
load("results/ridge_fit.Rda")
ridge_fit$lambda.1se

ridge_probabilities <- predict(ridge_fit,
                        newdata = census_test,
                        s = "lambda.1se",
                        type = "response") %>%
  as.numeric()

ridge_model_metrics <- get_misclass_errors(ridge_probabilities, census_test)

write_csv(x = ridge_model_metrics, file = "results/ridge-metrics-table.csv")

# Create Ridge CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# Create ridge trace plot
ggsave(filename = "results/ridge-trace-plot.png", 
       plot = plot_glmnet(ridge_fit, 
                          census_train, 
                          features_to_plot = 6), 
       device = "png", 
       width = 6, 
       height = 4)

# run lasso regression
set.seed(5)
lasso_fit <- cv.glmnet(`Health` ~ .,   
                      alpha = 1, 
                      nfolds = 10, 
                      family = 'binomial',
                      type.measure = 'class',
                      data = census_train)

# save the lasso fit object
save(lasso_fit, file = "results/lasso_fit.Rda")

lasso_fit$lambda.1se

lasso_probabilities = predict(lasso_fit,
                        newdata = census_test,
                        s = "lambda.1se",
                        type = "response") %>%
  as.numeric()

lasso_model_metrics <- get_misclass_errors(lasso_probabilities, census_test)

write_csv(x = lasso_model_metrics, file = "results/lasso-metrics-table.csv")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
ggsave(filename = "results/lasso-trace-plot.png", 
       plot = plot_glmnet(lasso_fit, 
                          census_train, 
                          features_to_plot = 6), 
       device = "png", 
       width = 6, 
       height = 4)
