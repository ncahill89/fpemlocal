
unit_data <- function(division_numeric_code, is_in_union) {
  divisions %>%
    dplyr::filter(!is.na(sub_region_numeric_code)) %>% 
    dplyr::mutate(is_developed_region = factor(is_developed_region, levels = c("Y", "N"))) %>%
    dplyr::filter(division_numeric_code == !!division_numeric_code) 
}
