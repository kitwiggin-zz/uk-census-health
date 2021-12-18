# load libraries
# library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
# library(lubridate)                      # for dealing with dates
# library(maps)                           # for creating maps
library(tidyverse)
library(scales)     # For formatting axis labels

# Given this is random sample from census, histo for most variables to check
# for potential significant bias

# read in the cleaned data
census_data = read_csv("data/clean/census_data.csv")
census_train = read_csv("data/clean/census_train.csv")

# create histogram of case fatality rate
health_histo <- census_data %>%
  ggplot(aes(x = `Health`)) + 
  geom_histogram() +
  labs(x = "Health score (1-5)") +
  scale_y_continuous(name="Number of survey respondants", 
                     labels = comma) +
  theme_bw()
# Save the histogram
ggsave(filename = "results/health-histogram.png", 
       plot = health_histo, 
       device = "png", 
       width = 5, 
       height = 3)

# create histograms for other attributes
fam_comp_histo <- census_data %>%
  ggplot(aes(x = `Family Composition`)) + 
  geom_histogram() +
  labs(x = "Family Composition Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

marital_status_histo <- census_data %>%
  ggplot(aes(x = `Marital Status`)) + 
  geom_histogram() +
  labs(x = "Marital Status Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

age_histo <- census_data %>%
  ggplot(aes(x = `Age`)) + 
  geom_histogram() +
  labs(x = "Age Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

ethnicity_histo <- census_data %>%
  ggplot(aes(x = `Ethnic Group`)) + 
  geom_histogram() +
  labs(x = "Ethnic Group Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

relig_histo <- census_data %>%
  ggplot(aes(x = `Religion`)) + 
  geom_histogram() +
  labs(x = "Religion Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

econ_activity_histo <- census_data %>%
  ggplot(aes(x = `Economic Activity`)) + 
  geom_histogram() +
  labs(x = "Economic Activity Category") +
  scale_y_continuous(name="Num survey respondants", 
                     labels = comma) +
  theme_bw(base_size = 8)

attribute_histograms <- plot_grid(fam_comp_histo,
                                  marital_status_histo,
                                  age_histo,
                                  ethnicity_histo,
                                  relig_histo,
                                  econ_activity_histo,
                                  nrow = 2)

ggsave(filename = "results/attribute-histos.png", 
       plot = attribute_histograms, 
       device = "png", 
       width = 7, 
       height = 4)


# Examine histograms of health category faceted by age
mean_health_by_age <- census_train %>% 
  group_by(`Age`) %>% 
  summarise(mean_health_cat = mean(`Health`))

mean_health_by_age

health_by_age_histo <- census_train %>%
  ggplot(aes(x = `Health`)) + 
  geom_histogram() + 
  scale_y_continuous(name="Number of survey respondants", labels = comma) +
  facet_wrap(~ `Age`, nrow = 4) + 
  geom_vline(data = mean_health_by_age, 
             aes(xintercept = mean_health_cat), 
             color = "red",
             linetype = "dashed") + 
  theme_bw()

ggsave(filename = "results/health-by-age-histos.png", 
       plot = health_by_age_histo, 
       device = "png", 
       width = 4, 
       height = 8)

