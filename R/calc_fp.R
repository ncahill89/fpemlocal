#' calc_fp
#'
#' @param posterior_samples 
#' @param population_data 
#' @param first_year 
#'
#' @return \emph{'Data.frame'} A data.frame of point estimates in long format.
#' @export
#'
#' @examples
calc_fp <-
  function(posterior_samples,
           population_data,
           first_year) {
    mcmc_proportion_dimensions <- dim(posterior_samples)
    years <- as.integer(first_year + 0:(mcmc_proportion_dimensions[3] - 1))
    results <-
      transform_proportions(posterior_samples = posterior_samples,
                            transformer = contraceptive_use_any) %>%
      tibble::add_column(year = years, .before = 1) %>%
      tidyr::gather(key = "percentile", value = "contraceptive_use_any") %>%
      tibble::add_column(
        transform_proportions_2(posterior_samples = posterior_samples,
                                transformer = contraceptive_use_modern)
      ) %>%
      tibble::add_column(
        transform_proportions_2(posterior_samples = posterior_samples,
                                transformer = contraceptive_use_traditional)
      ) %>%
      tibble::add_column(
        transform_proportions_2(posterior_samples = posterior_samples,
                                transformer = non_use)
      ) %>%
      tibble::add_column(
        transform_proportions_2(posterior_samples = posterior_samples,
                                transformer = unmet_need_any)
      ) %>%
      tibble::add_column(
        transform_proportions_2(posterior_samples = posterior_samples,
                                transformer = demand)
      ) %>%
      tibble::add_column(
        transform_proportions_2(posterior_samples = posterior_samples,
                                transformer = demand_modern)
      ) %>%
      tibble::add_column(
        transform_proportions_2(posterior_samples = posterior_samples,
                                transformer = demand_satisfied)
      ) %>%
      tibble::add_column(
        transform_proportions_2(posterior_samples = posterior_samples,
                                transformer = demand_satisfied_modern)
      ) %>%
      tibble::add_column(
        transform_proportions_2(posterior_samples = posterior_samples,
                                transformer = no_need)
      )
        
    results <- results %>%
      dplyr::mutate(contraceptive_use_any_population_count = get_estimated_counts(proportions =  . %>% select(year, contraceptive_use_any),
                                                                                  annual_country_population_counts = population_data)) %>%
      dplyr::mutate(contraceptive_use_modern_population_counts = get_estimated_counts(proportions =  . %>% select(year, contraceptive_use_modern),
                                                                                  annual_country_population_counts = population_data))


    # contraceptive_use_modern_population_counts <-
    #   get_estimated_counts(proportions = contraceptive_use_modern,
    #                        annual_country_population_counts = population_data)
    # traditional_cpr_population_counts <-
    #   get_estimated_counts(proportions = contraceptive_use_traditional,
    #                        annual_country_population_counts = population_data)
    # non_use_population_counts <-
    #   get_estimated_counts(proportions = non_use,
    #                        annual_country_population_counts = population_data)
    # unmet_need_population_counts <-
    #   get_estimated_counts(proportions = unmet_need_any,
    #                        annual_country_population_counts = population_data)
    # unmet_need_modern_population_counts <-
    #   get_estimated_counts(proportions = unmet_need_modern,
    #                        annual_country_population_counts = population_data)
    # demand_population_counts <-
    #   get_estimated_counts(proportions = demand,
    #                        annual_country_population_counts = population_data)
    # demand_modern_population_counts <-
    #   get_estimated_counts(proportions = demand_modern,
    #                        annual_country_population_counts = population_data)
    # demand_satisfied_population_counts <-
    #   get_estimated_counts(proportions = demand_satisfied,
    #                        annual_country_population_counts = population_data)
    # demand_satisfied_modern_population_counts <-
    #   get_estimated_counts(proportions = demand_satisfied_modern,
    #                        annual_country_population_counts = population_data)
    # no_need_population_counts <-
    #   get_estimated_counts(proportions = no_need,
    #                        annual_country_population_counts = population_data)
    return(results)
  }
