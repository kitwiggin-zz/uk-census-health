get_misclass_errors <- function(fitted_probabilities, 
                                census_test, 
                                probabilities = TRUE) {
  
  if (probabilities) {
    predictions <- as.numeric(fitted_probabilities > 0.5)
  } else {
    predictions <- fitted_probabilities
  }
  
  census_test <- census_test %>% 
    mutate(predicted_glm = predictions)
  
  # calculate misclassification rate
  misclassification_rate <- census_test %>% 
    summarise(misclassification_rate = mean(`Health` != predicted_glm)) %>% 
    pull(misclassification_rate)
  
  
  # false positive rate = number of false positives / total actual negatives
  false_positives <- census_test %>% 
    select(`Health`, predicted_glm) %>% 
    filter(`Health` == 0 & predicted_glm == 1) %>% 
    nrow()
  
  actual_negatives <- census_test %>%
    select(`Health`, predicted_glm) %>%
    filter(`Health` == 0) %>%
    nrow()
  
  fp_rate <- false_positives / actual_negatives
  
  # false negative rate = number of false negatives / total actual positives
  false_negatives = census_test %>%
    select(`Health`, predicted_glm) %>%
    filter(`Health` == 1 & predicted_glm == 0) %>%
    nrow()
  
  actual_positives = census_test %>%
    select(`Health`, predicted_glm) %>% 
    filter(`Health` == 1) %>%
    nrow()
  
  fn_rate = false_negatives / actual_positives
  
  # create table of these three metrics
  model_metrics <- tribble(
    ~metric, ~classifier_performance, 
    #------/-----------------------/ 
    "Misclassification rate", misclassification_rate, 
    "False positive rate", fp_rate,
    "False negative rate", fn_rate
  )
  return (model_metrics)
}
