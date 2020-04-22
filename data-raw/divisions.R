## code to prepare `divisions` dataset goes here
library(dplyr)
data1 <- read.csv("data-raw/division_classifications.csv")
divisions <- read.csv("data-raw/country_and_area_classification.csv")
divisions <- divisions %>% rename(name_country = 'Country or area', name_sub_region = 'Region', name_region = "Major area") %>% select(division_numeric_code, name_country, name_region, name_sub_region)
divisions <- left_join(divisions, data1)

#2020 EDIT
divisions <- fpemdata::divisions
divisions$is_unmarried_sexual_activity[which(divisions$name_country == "South Sudan")] <- "N"
divisions <- divisions %>% dplyr::filter(division_numeric_code != 732) # [1] "Western Sahara"
usethis::use_data(divisions, overwrite = TRUE)

