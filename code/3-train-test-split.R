# read in the cleaned data
census_data = read_csv("data/clean/census_data.csv")

set.seed(5) # seed set for reproducibility
n = nrow(census_data)
train_samples = sample(1:n, round(0.8*n))
census_train <- census_data[train_samples,]
census_test <- census_data[-train_samples,]

# save the train and test data
write_csv(x = census_train, file = "data/clean/census_train.csv")
write_csv(x = census_test, file = "data/clean/census_test.csv")
