#' Calculate fp indicators from samples, autosave
#'
#' A wrapper for \code{\link{calc_fp_c}} which reads in the model fit from the respective `runname`. The output is saved in the results directory with the same `runname`. The output can be automatically read in by proceeding wrapper functions with the `runname` argument specified.
#'
#' @param runname
#' @param country_population_counts
#'
#' @return
#' @export
#'
#' @examples
calc_fp_c_autosave <-
  function(runname = NULL,
           population_data = NULL) {

    fit <- readRDS(file.path("output/runs", paste0(runname, ".rds")))
    results <- calc_fp_c(fit = fit, population_data = population_data)
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/results")) dir.create("output/results")
    pathout <- file.path("output/results", paste0(runname, ".rds"))
    saveRDS(results, pathout)
    print(paste0("Your file was saved to ", pathout))
  }



#' calculate fp estimates for one country
#'
#' @param fit \emph{\sQuote{List}} The fit object from \code{\link{fit_fp_c}}
#' @param population_data \emph{\sQuote{Data frame}} Population count data in the format of \code{\link{population_counts}}
#'
#' @return
#' @export
#'
#' @examples
calc_fp_c <-
  function(fit,
           population_data = population_counts) {
    purrr::pmap(list(fit, list(population_data)), calc_fp_csub)
  }


calc_fp_csub <- function(fit,
                         population_data) {
  posterior_samples <- fit %>% purrr::chuck("posterior_samples")
  first_year <- fit %>%
    purrr::chuck("core_data", "year_sequence_list", "result_seq_years") %>%
    min
  last_year <- fit %>%
    purrr::chuck("core_data", "year_sequence_list", "result_seq_years") %>%
    max
  is_in_union <- fit %>% purrr::chuck("core_data", "is_in_union")
  division_numeric_code <-
    fit %>% purrr::chuck("core_data", "units", "division_numeric_code")
  population_data <- population_data_filter(
    custom_data_indicator,
    population_data = population_data,
    is_in_union = is_in_union,
    division_numeric_code = division_numeric_code,
    first_year = first_year,
    last_year = last_year
  )
  results <- calc_fp(posterior_samples = posterior_samples,
                     population_data = population_data,
                     first_year = first_year
                         )
  return(results)
}
