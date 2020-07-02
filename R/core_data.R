
#' core_data
#'
#' Creates core_data that consists of manipulated observations and run settings for \code{\link{do_1country_run}}
#'
#' @inheritParams do_1country_run
#'
core_data <- function(is_in_union,
                      surveydata_filepath,
                      division_numeric_code,
                      first_year,
                      last_year,
                      subnational) {
  # a single row of data which describes the country
  unit_data <- unit_data(division_numeric_code = division_numeric_code)
  # import user data and impute or import package data which is pre imputed
  contraceptive_use <- contraceptive_use_import(
    is_in_union = is_in_union,
    surveydata_filepath = surveydata_filepath,
    division_numeric_code = division_numeric_code,
    subnational = subnational
  )
  # get sequence of years for model and for filtering of results
  year_sequence_list <- year_sequence_maker(first_year_max = 1989,
                                            first_year = first_year,
                                            last_year = last_year,
                                            contraceptive_use = contraceptive_use
  )
                                            # first_year_obs = min(contraceptive_use$ref_date),
                                            # last_year_obs = max(contraceptive_use$ref_date))

  # additional processing required to align with the global run (to be minimized next round)
  if (nrow(contraceptive_use) > 0) {
    
    
    
    
    contraceptive_use <- contraceptive_use %>%
      ad_hoc_calculate_cp_trad()
    contraceptive_use <- contraceptive_use %>%
      data_series_type_relabel %>%
      dplyr::mutate(indicate_rounding_trad = indicate_rounding(contraceptive_use_traditional)) %>%
      dplyr::mutate(indicate_rounding_mod = indicate_rounding(contraceptive_use_modern)) %>%
      dplyr::mutate(contraceptive_use_traditional = round_from_zero(contraceptive_use_traditional)) %>%
      dplyr::mutate(contraceptive_use_modern = round_from_zero(contraceptive_use_modern)) %>%
      dplyr::mutate(contraceptive_use_any = round_from_zero(contraceptive_use_any)) %>%
      dplyr::mutate(unmet_need_any = round_from_zero(unmet_need_any))
    contraceptive_use <- contraceptive_use %>%
      ad_hoc_recalculate_cp_any() %>%
      ad_hoc_blankmodern_ifequals()
  }

  # the core_data list
  core_data = list(
    is_in_union = is_in_union,
    units = unit_data,
    start_year = 1990, # reference year for rate model (1990 in current version),
    observations = contraceptive_use,
    year_sequence_list = year_sequence_list,
    subnational = subnational
  )
  return(core_data)
}
