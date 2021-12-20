# load libraries
# library(lubridate)
library(tidyverse)

# load raw case data
census_data_raw = read_csv(file = "data/raw/census_data_raw.csv")

## Clean Census data

# Turn first row into column headers
census_data_tib <- as_tibble(census_data_raw)
colnames(census_data_tib) = census_data_tib[1,]
census_data_tib <- census_data_tib[-1,-1]

# Count the number of -9 entries for each feature
count_nines_raw <- census_data_tib %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(value = if_else(value == '-9',"-9","Informative")) %>% 
  count(name,value) %>% 
  pivot_wider(names_from = value,values_from = n)
count_nines_raw
write_csv(x = count_nines_raw, file = "results/count-nines-raw.csv")

# It appears as though there are 6804 entries that consistently have -9 entries
# These likely represent people not present for the survey - almost entirely
# schoolchildren living away during term-time. So we remove these.
# Also, all the work-related attributes are set to -9 if person is <16. This
# is wasted info - create new factor level in work questions if person is under
# 16...
# Remove Occupation due to likely lack of independence
# Turn health into binary (1,2 = healthy, 3,4,5 = not)

census_data_tib <- census_data_tib %>%
  select(- `Occupation`) %>%
  filter(`Country of Birth` != '-9') %>%
  filter(`Age` != '1')  %>%
  mutate(`Health` = if_else(`Health` == '1' | `Health` == '2', '1', '0')) %>%
  mutate(`Approximated Social Grade` = if_else(
    `Approximated Social Grade` == '-9', '0', `Approximated Social Grade`)) %>%
  mutate(`Economic Activity` = if_else(
    `Economic Activity` == '-9', '0', `Economic Activity`)) %>%
  mutate(`Hours worked per week` = if_else(
    `Hours worked per week` == '-9', '0', `Hours worked per week`)) %>%
  mutate(`Industry` = if_else(
    `Industry` == '-9', '0', `Industry`))

# Convert column types to factors and factor types to be more descriptive
# wherever it's easy to try and minimize the amount of cross-referencing 
# required
census_data <- census_data_tib %>%
  mutate(Region = fct_recode(Region, "North East" = "E12000001", 
             "North West" = "E12000002",
             "Yorkshire and the Humber" = "E12000003",
             "East Midlands" = "E12000004",
             "West Midlands" = "E12000005",
             "East of England" = "E12000006",
             "London" = "E12000007",
             "South East" = "E12000008",
             "South West" = "E12000009",
             "Wales" = "W92000004")) %>%
  mutate(`Residence Type` = fct_recode(`Residence Type`, "Communal" = "C", 
                                       "Non-communal" = "H")) %>%
  mutate(`Population Base` = fct_recode(`Population Base`, 
                                        "Usual Resident" = "1", 
                                        "Short-term" = "3")) %>%
  mutate(`Sex` = fct_recode(`Sex`, "Male" = "1", "Female" = "2")) %>%
  mutate(`Student` = fct_recode(`Student`, "Student" = "1", 
                                "Not Student" = "2")) %>%
  mutate(`Country of Birth` = fct_recode(`Country of Birth`, "UK" = "1", 
                                "Non UK" = "2"))

# write cleaned data to file
write_csv(census_data, file = "data/clean/census_data.csv")
