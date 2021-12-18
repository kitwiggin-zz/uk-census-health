# load libraries
library(kableExtra)
library(glmnetUtils)  # to run ridge and lasso
library(tidyverse)
source("code/functions/plot_glmnet.R") # for lasso/ridge trace plots
source("code/functions/get_misclass_errors.R")


# read in the training data
census_train = read_csv("data/clean/census_train.csv")
census_test = read_csv("data/clean/census_test.csv")

# Run Univariate logistic regression using Age
glm_fit_age <- glm(Health ~ Age, family = 'binomial', data = census_train)

save(glm_fit_age, file = "results/glm_fit_age.Rda")

# Run multivariate logistic regression

glm_fit_full <- glm(`Health` ~., family = 'binomial', data = census_train)

save(glm_fit_full, file = "results/glm_fit_full.Rda")

fitted_probabilities <- predict(glm_fit_full, 
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

ridge_probabilities <- predict(ridge_fit,
                        newdata = census_test,
                        s = "lambda.1se",
                        type = "response") %>%
  as.numeric()

ridge_model_metrics <- get_misclass_errors(ridge_probabilities, census_test)

write_csv(x = ridge_model_metrics, file = "results/ridge-metrics-table.csv")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# create ridge trace plot
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

# extract features selected by lasso and their coefficients
beta_hat_std <- extract_std_coefs(lasso_fit, census_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_csv("results/lasso-features-table.csv")
