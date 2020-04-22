contraceptive_use <- readr::read_csv("data-raw/contraceptive_use.csv") #use of readdelimn here is to format as tibble
# Name changes for 2019
# contraceptive_use <- contraceptive_use %>% dplyr::rename(contraceptive_use_any = contraceptive_use_all) %>% 
#   dplyr::rename(unmet_need_modern = unmet_need_for_modern_methods) %>% 
#   dplyr::rename(unmet_need_any = unmet_need_for_any_method)
usethis::use_data(contraceptive_use, overwrite = TRUE)

