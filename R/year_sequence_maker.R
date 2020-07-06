#' year_sequence_maker
#'
#' creates a list of years and respective indices based on user specified years to estimate and observed years
#'
#' @inheritParams fit_fp_c
#' @param first_year_max model estimation cannot start any later than this year
#' @param contraceptive_use
#'
#' @return \emph{\sQuote{List}}
#' \enumerate{
#'   \item \strong{model_seq}  \emph{\sQuote{Numeric vector}} Year index for model estimation
#'   \item \strong{model_seq_years}  \emph{\sQuote{Numeric vector}} Years of model estimation
#'   \item \strong{result_seq }  \emph{\sQuote{Numeric vector}} Year index for results based on user request
#'   \item \strong{result_seq_years}  \emph{\sQuote{Numeric vector}} Years of results based on user request
#' }
#'
year_sequence_maker <- function(first_year_max,
                          first_year,
                          last_year,
                          contraceptive_use
) {
  if (nrow(contraceptive_use) > 0) {
    first_year_obs = min(contraceptive_use$ref_date)
    last_year_obs = max(contraceptive_use$ref_date)
  } else {
    first_year_obs = NA
    last_year_obs = NA
  }
  min_year <- min(first_year, first_year_obs, first_year_max, na.rm = TRUE)
  max_year <- max(last_year, last_year_obs, na.rm = TRUE)
  model_seq <- seq(min_year, max_year) - min_year + 1
  model_seq_years <- seq(min_year, max_year)
  first_index <- which(first_year == model_seq_years)
  last_index <- which(last_year == model_seq_years)
  result_seq <- seq(first_index, last_index)
  result_seq_years <- seq(model_seq_years[first_index], model_seq_years[last_index])
  return(list(
    model_seq = model_seq,
    model_seq_years = model_seq_years,
    result_seq = result_seq,
    result_seq_years = result_seq_years,
    first_year_observed = first_year_obs
  ))
}
