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

#####
h1 <- census_data %>%
  ggplot(aes(x = `Region`)) + 
  geom_histogram(stat="count") +
  labs(x = "Family Composition Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 7, element_text(angle = 90))
h1
h2 <- census_data %>%
  ggplot(aes(x = `Family Composition`)) + 
  geom_histogram() +
  labs(x = "Family Composition Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 7)

marital_status_histo <- census_data %>%
  ggplot(aes(x = `Marital Status`)) + 
  geom_histogram(stat="count") +
  labs(x = "Marital Status Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

age_histo <- census_data %>%
  ggplot(aes(x = `Age`)) + 
  geom_histogram(stat="count") +
  labs(x = "Age Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

ethnicity_histo <- census_data %>%
  ggplot(aes(x = `Ethnic Group`)) + 
  geom_histogram(stat="count") +
  labs(x = "Ethnic Group Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

relig_histo <- census_data %>%
  ggplot(aes(x = `Religion`)) + 
  geom_histogram(stat="count") +
  labs(x = "Religion Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

econ_activity_histo <- census_data %>%
  ggplot(aes(x = `Economic Activity`)) + 
  geom_histogram(stat="count") +
  labs(x = "Economic Activity Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

attribute_histograms <- plot_grid(h1,
                                  marital_status_histo,
                                  age_histo,
                                  ethnicity_histo,
                                  relig_histo,
                                  econ_activity_histo,
                                  nrow = 2)

attribute_histograms

ggsave(filename = "results/attribute-histos.png", 
       plot = attribute_histograms, 
       device = "png", 
       width = 7, 
       height = 4)