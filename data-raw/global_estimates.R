load("data-raw/globalrun_data/global_estimates_married.rda")
global_estimates_married <- mw_perc
usethis::use_data(global_estimates_married, overwrite = TRUE)
load("data-raw/globalrun_data/global_estimates_unmarried.rda")
global_estimates_unmarried <- uw_perc
usethis::use_data(global_estimates_unmarried, overwrite = TRUE)
