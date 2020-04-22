#' year_sequence_maker
#' 
#' creates a list of years and respective indices based on user specified years to estimate and observed years 
#'
#' @inheritParams do_1country_run
#' @param first_year_max model estimation cannot start any later than this year
#' @param first_year_obs first year observed in data
#' @param last_year_obs last year observed in data
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
                          first_year_obs,
                          last_year_obs
) {
  min_year <- min(first_year, first_year_obs, first_year_max)
  max_year <- max(last_year, last_year_obs)
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
    result_seq_years = result_seq_years
  ))
}

