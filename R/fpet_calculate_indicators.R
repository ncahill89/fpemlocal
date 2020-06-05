#' Get results from samples
#'
#' A wrapper for \code{\link{fpem_results}} which reads in the model fit from the respective `runname`. The output is saved in the results directory with the same `runname`. The output can be automatically read in by proceeding fpem functions with the `runname` argument specified.
#'
#' @param runname 
#' @param country_population_counts 
#'
#' @return
#' @export
#'
#' @examples
fpet_calculate_indicators_autosave <-
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


#' Get results from samples
#'
#' Maps multiple sets of runs to \code{\link{fpem_calculate_results}}
#'
#' @inherit fpem_calculate_results
#'
#' @export
fpet_calculate_indicators <-
  function(fitlist,
           country_population_counts = NULL) {
    purrr::pmap(list(fitlist, list(country_population_counts)), fpet_calculate_easy_wrapper)
  }


fpet_calculate_easy_wrapper <- function(fit,
                                        country_population_counts = NULL) {
  if(is.null(country_population_counts)) {
    country_population_counts <- population_counts %>%
      dplyr::filter(division_numeric_code == fit$core_data$units$division_numeric_code) %>%
      dplyr::filter(is_in_union == fit$core_data$is_in_union)
  }
  posterior_samples <- fit$posterior_samples
  first_year <- fit$core_data$year_sequence_list$result_seq_years %>% min()
  results <- fpem_calculate_results(posterior_samples,
                                    country_population_counts,
                                    first_year
                         )
  return(results)
}


