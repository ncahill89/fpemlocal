# This entire script must run annually when new UNPD data is provided.
devtools::load_all()
contraceptive_use <- readr::read_csv("data-raw/contraceptive_use.csv") #use of readdelimn here is just to format as tibble

# We now add imputed version of se columns to contracetpive use data. Meta data has been updated accordingly.
contraceptive_use <- impute_packagedata_se(contraceptive_use) %>% 
  purrr::pluck("contraceptive_use_imputed") 

usethis::use_data(contraceptive_use, overwrite = TRUE)


# This format list uses the data above. The format list is used to check user survey data.
library(dplyr)
data <- contraceptive_use
#missing = FLASE if missing not allowed
contraceptive_use_format <- list(
  "division_numeric_code" = list(
    "type" = "value",
    "valid" = fpemdata::divisions$division_numeric_code %>% unique(),
    "missing" = FALSE,
    "required" = TRUE
  ),
  "start_date" = list(
    "basic" = TRUE,
    "required" = TRUE
  ),
  "end_date" = list(
    "basic" = TRUE,
    "required" = TRUE
  ),
  "is_in_union" = list(
    "type" = "value",
    "valid" = data$is_in_union %>% unique(),
    "missing" = data$is_in_union %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "age_range" = list(
    "type" = "value",
    "valid" = data$age_range %>% unique(),
    "missing" = data$age_range %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "data_series_type" = list(
    "type" = "value",
    "valid" = data$data_series_type %>% unique(),
    "missing" = data$data_series_type %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "group_type_relative_to_baseline" = list(
    "type" = "value",
    "valid" = data$group_type_relative_to_baseline %>% unique(),
    "missing" = data$group_type_relative_to_baseline %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "contraceptive_use_modern" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$contraceptive_use_modern %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "contraceptive_use_traditional" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$contraceptive_use_traditional %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "contraceptive_use_any" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$contraceptive_use_any %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "unmet_need_modern" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$unmet_need_modern %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "unmet_need_any" = list(
    "type" = "range",
    "valid" = c(0, 1),
    "missing" = data$unmet_need_any %>% is.na() %>% any(),
    "required" = TRUE
  ),
  "age_group_bias" = list(
    "type" = "value",
    "valid" = data$age_group_bias %>% unique(),
    "missing" = data$age_group_bias %>% is.na() %>% any(),
    "required" = FALSE
  ),
  "modern_method_bias" = list(
    "type" = "value",
    "valid" = data$modern_method_bias %>% unique(),
    "missing" = data$modern_method_bias %>% is.na() %>% any(),
    "required" = FALSE
  ),
  "has_traditional_method_bias" = list(
    "type" = "value",
    "valid" = data$has_traditional_method_bias %>% unique(),
    "missing" = data$has_traditional_method_bias %>% is.na() %>% any(),
    "required" = FALSE
  )
)

usethis::use_data(contraceptive_use_format, overwrite = TRUE)