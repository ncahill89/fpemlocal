#' check_inputs
#'
#' @inheritParams fit_fp_c
#'
check_inputs <- function(custom_data_indicator, subnational, division_numeric_code) {
  # determine if user supplied custom data, if they did not and if they selected subnation stop the run
  if (!custom_data_indicator & subnational) {
    stop("FUNCTION INPUT ERROR: Default data does not exist for sub-national runs, please supply user data. ")
  }
  if(!division_numeric_code %in% divisions$division_numeric_code) {
    stop("FUNCTION INPUT ERROR: division_numeric_code does not match any on file. ")
  }
  # if(!is.null(first_year)) {
  #   if (first_year > first_year_max) {
  #     stop("FUNCTION INPUT ERROR: bad dates provided")
  #   }
  # }
}
