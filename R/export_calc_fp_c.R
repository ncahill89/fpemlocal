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
           country_population_counts = NULL) {

    runlist <- readRDS(file.path("output/runs", paste0(runname, ".rds")))
    results <- fpet_calculate_indicators(runlist = runlist,
                     country_population_counts = country_population_counts)
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/results")) dir.create("output/results")
    pathout <- file.path("output/results", paste0(runname, ".rds"))
    saveRDS(results, pathout)
    print(paste0("Your file was saved to ", pathout))
  }


#' Calculate fp indicators from samples
#'
#' Maps multiple sets of runs to \code{\link{calc_fp}}
#'
#' @inherit calc_fp
#'
#' @export
calc_fp_c <-
  function(fit,
           country_population_counts = NULL) {
    purrr::pmap(list(fit, list(population_data)), calc_fp_csub)
  }


calc_fp_csub <- function(fit,
                         population_data = NULL) {
  if(is.null(country_population_counts)) {
    population_data <- population_counts %>%
      dplyr::filter(division_numeric_code == fit$core_data$units$division_numeric_code) %>%
      dplyr::filter(is_in_union == fit$core_data$is_in_union)
  }
  posterior_samples <- fit$posterior_samples
  first_year <- fit$core_data$year_sequence_list$result_seq_years %>% min()
  results <- calc_fp(posterior_samples,
                     population_data,
                     first_year
                         )
  return(results)
}
