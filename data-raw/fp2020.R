library(dplyr)
temp <- read.csv("data-raw/legacy.csv")
fp2020 <- temp %>% filter(FP2020.country == "Yes") %>% select(division_numeric_code)
usethis::use_data(fp2020, overwrite = TRUE)
