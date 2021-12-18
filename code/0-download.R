# load libraries
library(tidyverse)

# download case data from NY Times website
url = "http://www.ons.gov.uk/ons/rel/census/2011-census/2011-census-teaching-file/rft-teaching-file.zip"
url2 = "https://webarchive.nationalarchives.gov.uk/ukgwa/20160105160709/http://www.ons.gov.uk/ons/rel/census/2011-census/2011-census-teaching-file/rft-teaching-file.zip"

fn = "2011 Census Microdata Teaching File.csv"

census_data_raw <- read_csv("/Users/kitwiggin/Downloads/rft-teaching-file/2011 Census Microdata Teaching File.csv")

# write raw data to file
write_csv(x = census_data_raw, file = "data/raw/census_data_raw.csv")

# download health rankings data from the web 
# (omitted from this template)
