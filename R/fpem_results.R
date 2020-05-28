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
fpem_results_autosave <-
  function(runname = NULL,
           country_population_counts = NULL) {
    
    runlist <- readRDS(file.path("output/runs", paste0(runname, ".rds")))
    results <- fpem_results(runlist = runlist,
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
fpem_results <-
  function(runlist,
           country_population_counts = NULL) {
    purrr::pmap(list(runlist, list(country_population_counts)), fpem_calculate_results)
  }






#' Calculate results from samples
#'
#' Returns point estimates from posterior samples in long format.
#'
#' @param runlist \emph{'list'} a list with core_data and posterior samples
#' @param country_population_counts \emph{'Numeric Vector'} A vector of population counts selected from \code{\link[get_population_counts]{get_population_counts}}
#'
#' @return \emph{'Data.frame'} A data.frame of point estimates in long format.
#'
#' @export
fpem_calculate_results <-
  function(runlist,
           country_population_counts = NULL) {
    
    if(is.null(country_population_counts)) {
      country_population_counts <- population_counts
    }
    country_population_counts <- country_population_counts %>%
      dplyr::filter(division_numeric_code == runlist$core_data$units$division_numeric_code) %>%
      dplyr::filter(is_in_union == runlist$core_data$is_in_union)
    
    
    posterior_samples <- runlist$posterior_samples
    first_year <- runlist$core_data$year_sequence_list$result_seq_years %>% min()
    
    contraceptive_use_any <-
      get_contraceptive_use_any(posterior_samples = posterior_samples, first_year = first_year)
    contraceptive_use_modern <-
      get_contraceptive_use_modern(posterior_samples = posterior_samples, first_year = first_year)
    contraceptive_use_traditional <-
      get_contraceptive_use_traditional(posterior_samples = posterior_samples, first_year = first_year)
    non_use <-
      get_non_use(posterior_samples = posterior_samples, first_year = first_year)
    unmet_need_any <-
      get_unmet_need_any(posterior_samples = posterior_samples, first_year = first_year)
    unmet_need_modern <-
      get_unmet_need_modern(posterior_samples = posterior_samples, first_year = first_year)
    demand <-
      get_demand(posterior_samples = posterior_samples, first_year = first_year)
    demand_modern <-
      get_demand_modern(posterior_samples = posterior_samples, first_year = first_year)
    demand_satisfied <-
      get_demand_satisfied(posterior_samples = posterior_samples, first_year = first_year)
    demand_satisfied_modern <-
      get_demand_satisfied_modern(posterior_samples = posterior_samples, first_year = first_year)
    no_need <-
      get_no_need(posterior_samples = posterior_samples, first_year = first_year)
    
    contraceptive_use_any_population_counts <-
      get_estimated_counts(proportions = contraceptive_use_any,
                           annual_country_population_counts = country_population_counts)
    contraceptive_use_modern_population_counts <-
      get_estimated_counts(proportions = contraceptive_use_modern,
                           annual_country_population_counts = country_population_counts)
    traditional_cpr_population_counts <-
      get_estimated_counts(proportions = contraceptive_use_traditional,
                           annual_country_population_counts = country_population_counts)
    non_use_population_counts <-
      get_estimated_counts(proportions = non_use,
                           annual_country_population_counts = country_population_counts)
    unmet_need_population_counts <-
      get_estimated_counts(proportions = unmet_need_any,
                           annual_country_population_counts = country_population_counts)
    unmet_need_modern_population_counts <-
      get_estimated_counts(proportions = unmet_need_modern,
                           annual_country_population_counts = country_population_counts)
    demand_population_counts <-
      get_estimated_counts(proportions = demand,
                           annual_country_population_counts = country_population_counts)
    demand_modern_population_counts <-
      get_estimated_counts(proportions = demand_modern,
                           annual_country_population_counts = country_population_counts)
    demand_satisfied_population_counts <-
      get_estimated_counts(proportions = demand_satisfied,
                           annual_country_population_counts = country_population_counts)
    demand_satisfied_modern_population_counts <-
      get_estimated_counts(proportions = demand_satisfied_modern,
                           annual_country_population_counts = country_population_counts)
    no_need_population_counts <-
      get_estimated_counts(proportions = no_need,
                           annual_country_population_counts = country_population_counts)
    
    results <- list(
      contraceptive_use_any = contraceptive_use_any,
      contraceptive_use_modern = contraceptive_use_modern,
      contraceptive_use_traditional = contraceptive_use_traditional,
      non_use = non_use,
      unmet_need_any = unmet_need_any,
      unmet_need_modern = unmet_need_modern,
      demand = demand,
      demand_modern = demand_modern,
      demand_satisfied = demand_satisfied,
      demand_satisfied_modern = demand_satisfied_modern,
      no_need = no_need,
      contraceptive_use_any_population_counts = contraceptive_use_any_population_counts,
      contraceptive_use_modern_population_counts = contraceptive_use_modern_population_counts,
      traditional_cpr_population_counts = traditional_cpr_population_counts,
      non_use_population_counts = non_use_population_counts,
      unmet_need_population_counts = unmet_need_population_counts,
      unmet_need_modern_population_counts = unmet_need_modern_population_counts,
      demand_modern_population_counts = demand_modern_population_counts,
      demand_population_counts = demand_population_counts,
      demand_satisfied_population_counts = demand_satisfied_population_counts,
      demand_satisfied_modern_population_counts = demand_satisfied_modern_population_counts,
      no_need_population_counts = no_need_population_counts
    )
    return(results)
  }






