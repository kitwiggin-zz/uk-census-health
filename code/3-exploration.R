# load libraries
library(cowplot)  # for side by side plot
library(tidyverse)
library(scales)     # For formatting axis labels

# read in the cleaned data
census_data = read_csv("data/clean/census_data.csv")
census_train = read_csv("data/clean/census_train.csv")

# create histogram of healthy vs unhealthy 
health_histo <- census_data %>%
  ggplot(aes(x = `Health`)) + 
  geom_histogram(stat="count") +
  xlab("Unhealthy/Healthy (0/1)") +
  scale_y_continuous(name="Number of survey respondants", 
                     labels = comma) +
  theme_bw()

# Save the histogram
ggsave(filename = "results/health-histogram.png", 
       plot = health_histo, 
       device = "png", 
       width = 5, 
       height = 3)

# Number of healthy observations
nrow(census_data %>% filter(`Health` == 1))
# Number of unhealthy observations
nrow(census_data %>% filter(`Health` == 0))

# Examine histograms of health category faceted by age
health_by_age_histo <- census_train %>%
  ggplot(aes(x = `Health`)) + 
  geom_histogram(stat="count") + 
  scale_y_continuous(name="Number of survey respondants", labels = comma) +
  facet_wrap(~ `Age`, nrow = 4) + 
  theme_bw()

ggsave(filename = "results/new-health-by-age-histos.png", 
       plot = health_by_age_histo, 
       device = "png", 
       width = 4, 
       height = 8)