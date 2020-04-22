#' check_inputs
#'
#' @inheritParams do_1country_run
#'
check_inputs <- function(surveydata_filepath, subnational, division_numeric_code) {
  if (is.null(surveydata_filepath) & subnational) {
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
