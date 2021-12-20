library(rpart) # to train decision trees 
library(rpart.plot) # to plot decision trees 
library(randomForest) # random forests 
library(gbm) # boosting 
library(tidyverse)
library(cowplot)   
source("code/functions/get_misclass_errors.R")

# read in the training data
census_train = read_csv("data/clean/census_train.csv")
census_test = read_csv("data/clean/census_test.csv")

# Fit a pruned, cross-validated, default tree
tree_fit = rpart(`Health` ~ .,
                 method = "class", # classification
                 parms = list(split = "gini"), # Gini index for splitting
                 data = census_train)

# Find optimum tree fit by starting with the deepest fit
set.seed(5)
deepest_tree_fit <- rpart(`Health` ~ .,
                         method = "class",
                         parms = list(split = "gini"),
                         control = rpart.control(minsplit = 1, 
                                                 minbucket = 1, 
                                                 cp = 0),
                         data = census_train)

save(deepest_tree_fit, file = "results/deep_tree_fit.Rda")

cp_table <- deepest_tree_fit$cptable %>% as_tibble()

# Fit cv plot to deepest tree
tree_fit_cv_plot <- cp_table %>%
  filter(nsplit >= 2) %>%
  ggplot(aes(x = nsplit+1, 
             y = xerror, 
             ymin = xerror - xstd, 
             ymax = xerror + xstd)) + 
  scale_x_log10() +
  geom_point() + 
  geom_line() +
  geom_errorbar(width = 0.25) +
  xlab("Number of terminal nodes on log scale") + 
  ylab("CV error") + 
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  theme_bw()

ggsave(filename = "results/deep-tree-cv-plot.png", 
       plot = tree_fit_cv_plot, 
       device = "png", 
       width = 6, 
       height = 4)

optimal_tree_info <- cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>% 
  head(1)

optimal_tree_info$nsplit

optimal_tree <- prune(tree = tree_fit, cp = optimal_tree_info$CP)

save(optimal_tree, file = "results/optimal_tree_fit.Rda")

png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/optimal-tree-plot.png")
rpart.plot(optimal_tree)
dev.off()

opt_tree_predictions <- predict(optimal_tree, 
                               newdata = census_test, 
                               type = "class")

opt_tree_metrics <- get_misclass_errors(opt_tree_predictions, 
                                        census_test, 
                                        probabilities = FALSE)

write_csv(x = opt_tree_metrics, file = "results/opt-tree-metrics.csv")

# Random Forest

### A bit of data fixing so that the random forest package works as it should
census_train <- census_train %>%
  mutate_all(as.factor)

census_test <- census_test %>%
  mutate_all(as.factor)

# Get rid of spaces in column names
new_names = rep(NA, ncol(census_train))
for (i in 1:ncol(census_train)) {
  new_names[i] = str_replace_all(colnames(census_train)[i], " ", "_")
}
colnames(census_train) <- new_names
colnames(census_test) <- new_names
####

# Unfortunately, too much memory is required to run an Rf on all the training 
# data - a sub-sample must be taken
set.seed(5)
train_sub_sample = sample(1:nrow(census_train), round(0.2*nrow(census_train)))
census_train_ss <- census_train[train_sub_sample,]

# Fit a Random Forest to the data
rf_fit = randomForest(factor(`Health`) ~., data = census_train_ss)

# Plot OOB error over number of trees in forest
rf_oob_err <- tibble(oob_error = rf_fit$err.rate[,"OOB"], trees = 1:500) %>% 
  ggplot(aes(x = trees, y = oob_error)) + 
  geom_line() +
  labs(x = "Number of trees", y = "OOB error") + 
  theme_bw()

ggsave(filename = "results/rf-oob-err.png", 
       plot = rf_oob_err, 
       device = "png", 
       width = 6, 
       height = 4)

set.seed(5)
mvalues = seq.int(1, 15) # num columns without Health
oob_errors = numeric(length(mvalues))
ntree = 100 # Smallest number with OOB error comfortably on the plateau

# Iterate through all possible values of m to find the one with least OOB error
for(i in 1:length(mvalues) ) {
  m = mvalues[i]
  set.seed(5)
  rf_fit <- randomForest(Health ~ ., 
                        ntree = ntree, 
                        mtry = m, 
                        data = census_train_ss)
  oob_errors[i] <- rf_fit$err.rate[ntree]
}

m_and_oob_errors = tibble(m = mvalues, oob_err = oob_errors)
rf_tune_mtry <- m_and_oob_errors %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = mvalues) + 
  labs(x = "Value for m", y = "OOB error") + 
  theme_bw()

ggsave(filename = "results/rf-tune-mtry.png", 
       plot = rf_tune_mtry, 
       device = "png", 
       width = 6, 
       height = 4)

rf_fit_tuned <- randomForest(Health ~ ., 
                             ntree = 100, 
                             mtry = 2,
                             importance = TRUE,
                             data = census_train_ss)

save(rf_fit_tuned, file = "results/rf-fit-tuned.Rda")

png(width = 9, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/rf-tuned-var-imp.png")
varImpPlot(rf_fit_tuned, n.var = 8)
dev.off()

rf_tuned_predictions <- predict(rf_fit_tuned, 
                                newdata = census_test, 
                                type = "class")

rf_tuned_metrics <- get_misclass_errors(rf_tuned_predictions, 
                                        census_test, 
                                        probabilities = FALSE)

write_csv(x = rf_tuned_metrics, file = "results/rf-tuned-metrics.csv")

# Boosting

## Boosting data cleaning
census_train$Health <- as.numeric(as.character(census_train$Health))
census_test$Health <- as.numeric(as.character(census_test$Health))

train_half_sample = sample(1:nrow(census_train), round(0.5*nrow(census_train)))
##

set.seed(1) # for reproducibility
# fit random forest with interaction depth 1 
gbm_fit_1 = gbm(Health ~ ., 
                distribution = "bernoulli", 
                n.trees = 500,
                interaction.depth = 1, 
                shrinkage = 0.1, 
                cv.folds = 5,
                data = census_train)

set.seed(1) # for reproducibility 
# fit random forest with interaction depth 2 
gbm_fit_2 = gbm(Health ~ ., 
                distribution = "bernoulli", 
                n.trees = 500,
                interaction.depth = 2, 
                shrinkage = 0.1, 
                cv.folds = 5,
                data = census_train)

set.seed(1) # for reproducibility 
# fit random forest with interaction depth 3 
gbm_fit_3 = gbm(Health ~ ., 
                distribution = "bernoulli", 
                n.trees = 500,
                interaction.depth = 3, 
                shrinkage = 0.1, 
                cv.folds = 5,
                data = census_train)

ntrees = 500 
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1), 
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2), 
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3)
)

# plot CV errors
boost_cv_err <- cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) +
  # add horizontal dashed lines at the minima of the three curves 
  geom_hline(yintercept = min(gbm_fit_1$cv.error),
             linetype = "dashed", color = "red") + 
  geom_hline(yintercept = min(gbm_fit_2$cv.error),
             linetype = "dashed", color = "green") + 
  geom_hline(yintercept = min(gbm_fit_3$cv.error),
             linetype = "dashed", color = "blue") + 
  geom_line() + # set colors to match horizontal line minima
  scale_color_manual(labels = c("1", "2", "3"),
                     values = c("red", "green", "blue")) +
  labs(x = "Number of trees", y = "CV error", colour = "Interaction depth") + 
  theme_bw()

ggsave(filename = "results/boost-cv-err.png", 
       plot = boost_cv_err, 
       device = "png", 
       width = 6, 
       height = 4)

# Would like to test shrinkage parameter

gbm_fit_opt <- gbm_fit_3
opt_num_trees = gbm.perf(gbm_fit_opt, plot.it = FALSE)
gbm_rel_inf_table <- summary(gbm_fit_opt, 
                             n.trees = opt_num_trees, 
                             plotit = FALSE)

save(gbm_fit_opt, file = "results/gbm-fit-opt.Rda")

write_csv(x = gbm_rel_inf_table, file = "results/gbm-influence-tbl.csv")

# Partial dependency plots for the three most influential features
p1 = plot(gbm_fit_opt,
          i.var = "Economic_Activity", 
          n.trees = opt_num_trees,
          type = "response")

p2 = plot(gbm_fit_opt,
          i.var = "Age", 
          n.trees = opt_num_trees, 
          type = "response")

p3 = plot(gbm_fit_opt,
          i.var = "Approximated_Social_Grade", 
          n.trees = opt_num_trees, 
          type = "response")

ggsave(filename = "results/gbm-top-part-depend.png", 
       plot = plot_grid(p1, p2, p3, nrow = 1), 
       device = "png", 
       width = 9, 
       height = 4)

gbm_probabilities = predict(gbm_fit_opt,
                            n.trees = opt_num_trees,
                            type = "response",
                            newdata = census_test)

gbm_tuned_metrics <- get_misclass_errors(gbm_probabilities, census_test)

write_csv(x = gbm_tuned_metrics, file = "results/gbm-tuned-metrics.csv")
