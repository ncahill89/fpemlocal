
# globalrun_input_m <- fpemdata::read_rda(path = "data-raw/globalrun_data/globalrun_input_m.rda")
# globalrun_input_u <- fpemdata::read_rda(path = "data-raw/globalrun_data/globalrun_input_u.rda")

globalrun_input_m <- readr::read_csv("data-raw/globalrun_data/data_input_married_exported_from_mcmc_meta.csv")
globalrun_input_u <- readr::read_csv("data-raw/globalrun_data/data_input_unmarried_exported_from_mcmc_meta.csv")
globalrun_output_m <- FPEMcountry::read_rda(path = "data-raw/globalrun_data/globalrun_output_m.rda")
globalrun_output_u <- FPEMcountry::read_rda(path = "data-raw/globalrun_data/globalrun_output_u.rda")

index_area <- FPEMcountry::index_area(globalrun_input_m, run_type = "mwra")
index_datatype <- FPEMcountry::index_datatype(globalrun_input_m)
index_m <- list(index_area_df = index_area,
                index_datatype = index_datatype)

index_area <- FPEMcountry::index_area(globalrun_input_u, run_type = "uwra")
index_datatype <- FPEMcountry::index_datatype(globalrun_input_u)
index_u <- list(index_area_df = index_area,
                index_datatype = index_datatype)

usethis::use_data(index_m, index_u,
                  globalrun_input_m, 
                  globalrun_input_u,
                  globalrun_output_m,
                  globalrun_output_u, 
                  internal = TRUE,
                  overwrite= TRUE)

