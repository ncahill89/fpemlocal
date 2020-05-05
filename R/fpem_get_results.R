#' Get results from samples
#' 
#' @inherit fpem_calculate_results
#'
#' @export
fpem_get_results_autosave <-
  function(runname = NULL,
           country_population_counts = NULL) {
    
    runlist <- readRDS(file.path("runs", paste0(runname, ".rds")))
    results <- fpem_get_results(runlist = runlist,
                     country_population_counts = country_population_counts)
    if (!dir.exists("results"))
      dir.create("results")
    saveRDS(results, file.path("results", paste0(runname, ".rds")))
  }


#' Get results from samples
#'
#' @inherit fpem_calculate_results
#'
#' @export
fpem_get_results <-
  function(...) {
    UseMethod("fpem_get_results")
  }


fpem_get_results.sinlge_union <- function(runlist,
                                          country_population_counts = NULL) {
  results <- fpem_calculate_results(runlist = runlist,
                                    country_population_counts = country_population_counts)
  return(results)
}


fpem_get_results.all_union <- function(runlist,
                                       country_population_counts = NULL) {
  resultsy <- fpem_calculate_results(runlist = runlist$runy,
                                     country_population_counts = country_population_counts)
  resultsn <- fpem_calculate_results(runlist = runlist$runn,
                                     country_population_counts = country_population_counts)
  resultsall <- fpem_calculate_results(runlist = runlist$runall,
                                     country_population_counts = country_population_counts)
  return(list(
    resultsn = resultsn,
    resultsy = resultsy,
    resultsall = resultsall
  ))
}


