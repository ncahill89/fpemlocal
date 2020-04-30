
unit_data <- function(division_numeric_code, is_in_union) {
  divisions %>%
    dplyr::filter(!is.na(sub_region_numeric_code)) %>% # why do we need to filter this here? when is it ever NA?
    dplyr::mutate(is_developed_region = factor(is_developed_region, levels = c("Y", "N"))) %>% # why does this need to be a factor? can we change it in the data instead
    dplyr::filter(division_numeric_code == !!division_numeric_code) 
}
