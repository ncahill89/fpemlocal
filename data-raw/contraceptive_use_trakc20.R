contraceptive_use_track20 <- read.csv("data-raw/Track20Database121819_Fixed.csv")
# Name changes for 2019
# contraceptive_use_track20 <- contraceptive_use_track20 %>% dplyr::rename(contraceptive_use_any = contraceptive_use_all) %>%
#   dplyr::rename(unmet_need_modern = unmet_need_for_modern_methods) %>%
#   dplyr::rename(unmet_need_any = unmet_need_for_any_method)
usethis::use_data(contraceptive_use_track20, overwrite = TRUE)

