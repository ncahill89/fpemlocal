load("data-raw/globalrun_data/global_estimates_married.rda")
load("data-raw/globalrun_data/global_estimates_unmarried.rda")
global_estimates_married <- mw_perc %>%
  dplyr::mutate(is_in_union = "Y")
global_estimates_unmarried <- uw_perc %>%
  dplyr::mutate(is_in_union = "N")
global_estimates <- dplyr::bind_rows(global_estimates_married,
                 global_estimates_unmarried) %>%
  tidyr::gather(year, value, "1970.5":"2030.5") %>% 
  dplyr::mutate(year = as.numeric(year)) %>%
  tidyr::spread(Percentile,  value) %>%
  dplyr::rename(division_numeric_code = Iso,
                indicator = par) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::select(-name) %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          unmet = "unmet_need_any",
                                          modern = "contraceptive_use_modern",
                                          traditional = "contraceptive_use_traditional")) %>%
  dplyr::rename("2.5%" = "0.025",
                "10%" = "0.1",
                "50%" = "0.5",
                "90%" = "0.9",
                "97.5%" = "0.975") %>%
  dplyr::mutate(model = "global")


usethis::use_data(global_estimates, overwrite = TRUE)
