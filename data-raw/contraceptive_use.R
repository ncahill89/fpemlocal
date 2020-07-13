devtools::load_all()
contraceptive_use <- readr::read_csv("data-raw/contraceptive_use.csv") #use of readdelimn here is to format as tibble

contraceptive_use <- impute_packagedata_se(contraceptive_use) %>% 
  purrr::pluck("contraceptive_use_imputed") 

usethis::use_data(contraceptive_use, overwrite = TRUE)

