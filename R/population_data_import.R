population_data_import <- function(
  population_data = NULL,
  is_in_union,  
  division_numeric_code,
  first_year,
  last_year
) 
{
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