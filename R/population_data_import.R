population_data_import <- function(
  population_data = NULL,
  fit
) 
{
  is_in_union <- fit %>% purrr::chuck("core_data", "is_in_union")
  division_numeric_code <- fit %>% purrr::chuck("core_data", "units", "division_numeric_code")
  first_year <- fit %>% 
    purrr::chuck("core_data","year_sequence_list", "result_seq_years") %>% 
    min
  last_year <- fit %>% 
    purrr::chuck("core_data","year_sequence_list", "result_seq_years") %>% 
    max
  if(!is.null(population_data)) {
    population_data <- population_data %>%
      dplyr::filter(is_in_union == {{is_in_union}}) %>%
      dplyr::filter(division_numeric_code == {{division_numeric_code}}) %>%
      dplyr::filter(mid_year >= first_year) %>%
      dplyr::filter(mid_year <= last_year)
  } else {
    population_data <- population_counts %>%
      dplyr::filter(is_in_union == {{is_in_union}}) %>%
      dplyr::filter(division_numeric_code == {{division_numeric_code}}) %>%
      dplyr::filter(mid_year >= first_year) %>%
      dplyr::filter(mid_year <= last_year)
  }
  return(population_data)
}

