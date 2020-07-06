population_data_filter <- function(
  population_data,
  is_in_union,  
  division_numeric_code,
  first_year,
  last_year
) 
{
    population_data <- population_data %>%
      dplyr::filter(is_in_union == {{is_in_union}}) %>%
      dplyr::filter(division_numeric_code == {{division_numeric_code}}) %>%
      dplyr::filter(mid_year >= first_year) %>%
      dplyr::filter(mid_year <= last_year)
  return(population_data)
}