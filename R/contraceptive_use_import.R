#' contraceptive_use_import
#'
#' @inheritParams fit_fp_c
contraceptive_use_import <- function(is_in_union, surveydata_filepath, division_numeric_code, subnational) {
  if (!is.null(surveydata_filepath)) {
    contraceptive_use <- readr::read_csv(surveydata_filepath)
    format_check(contraceptive_use_format, contraceptive_use)
    contraceptive_use <- contraceptive_use %>%
      dplyr::filter(is_in_union == !!is_in_union) %>%
      dplyr::filter(age_range == "15-49")  %>%
      dplyr::filter(division_numeric_code == !!division_numeric_code) %>%
      impute_user_se(subnational = subnational, is_in_union = is_in_union) %>%
      dplyr::mutate(ref_date = floor((start_date + end_date) / 2))

  } else {
    contraceptive_use <- imputed_data$contraceptive_use_imputed  %>%
      dplyr::filter(is_in_union == !!is_in_union) %>%
      dplyr::filter(age_range == "15-49")  %>%
      dplyr::filter(division_numeric_code == !!division_numeric_code) %>%
      dplyr::mutate(ref_date = floor((start_date + end_date) / 2))
  }
  return(contraceptive_use)
}
