# Script is run annually 
# make sure to update fpemdata with the latest global run data before running this script
# WARNING: NEW CONTRACEPTIVE_USE DATA NEEDS TO BE IN THE PACKAGE AND R NEEDS TO BE RESTARTED FIRST
library(dplyr)

imputed_data_y <- fpemdata:::impute_package_data("Y")
imputed_data_n <- fpemdata:::impute_package_data("N")
imputed_data <- list(
  contraceptive_use_imputed = rbind(imputed_data_y$contraceptive_use_imputed, imputed_data_n$contraceptive_use_imputed),
  medians = rbind(data.frame(imputed_data_y$medians, is_in_union = "Y"), data.frame(imputed_data_n$medians, is_in_union = "N")),
  imputed_max_se = rbind(data.frame(imputed_data_y$imputed_max_se, is_in_union = "Y"), data.frame(imputed_data_y$imputed_max_se, is_in_union = "N"))
)
usethis::use_data(imputed_data, overwrite = TRUE)
