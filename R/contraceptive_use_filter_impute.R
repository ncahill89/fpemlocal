#' contraceptive_use_filter
#'
#' @inheritParams fit_fp_c
contraceptive_use_filter_impute <-
  function(survey_dataset,
           custom_data_indicator,
           is_in_union,
           division_numeric_code,
           subnational) {
    if (custom_data_indicator) {
      format_check(contraceptive_use_format, survey_dataset)
    }
    contraceptive_use <- survey_dataset %>%
      dplyr::filter(is_in_union == !!is_in_union) %>%
      dplyr::filter(age_range == "15-49")  %>%
      dplyr::filter(division_numeric_code == !!division_numeric_code) %>%
      dplyr::mutate(ref_date = floor((start_date + end_date) / 2))
    if (custom_data_indicator) {
      contraceptive_use <- contraceptive_use %>%
        impute_user_se(subnational = subnational, is_in_union = is_in_union) 
    }
    return(contraceptive_use)
  }
