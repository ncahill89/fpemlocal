
fpem_get_subpopulation_labels <- function(contraceptive_use) {
  ifelse(
    contraceptive_use$age_group_bias == "+",
    "+",
    ifelse(
      contraceptive_use$age_group_bias == "-",
      "-",
      ifelse(
        contraceptive_use$age_group_bias == "?",
        "A",
        ifelse(
          contraceptive_use$has_traditional_method_bias == "Y",
          "F",
          ifelse(
            contraceptive_use$modern_method_bias == "-",
            "S-",
            ifelse(contraceptive_use$modern_method_bias == "+",
                   "S+",
                   "")
          )
        )
      )
    )
  )
}



#' Convert data frame to JSON string
#'
#' @param df `data.frame` Data frame to be converted
#' @return `character` JSON string
#' @export
to_json <- function(df) {
  df %>%
    setNames(snakecase::to_lower_camel_case(names(.))) %>%
    jsonlite::toJSON(null = "null",
                     na = "null",
                     pretty = TRUE)
}



#' Expand contraceptive use data to include derived values.
#'
#' @param contraceptive_use `data.frame` Contraceptive use data.
#' @param simplify_logicals `logical` Convert Y/N to TRUE/FALSE.
#' @return `data.frame` Expanded contraceptive use data..
expand_contraceptive_use <- function(contraceptive_use, simplify_logicals = TRUE) {
  contraceptive_use_all <- dplyr::coalesce(
    contraceptive_use$contraceptive_use_all,
    contraceptive_use$contraceptive_use_modern + contraceptive_use$contraceptive_use_traditional
  )
  
  ratio_modern_all <- contraceptive_use$contraceptive_use_modern / contraceptive_use_all
  
  unmet_need_modern <- dplyr::coalesce(
    as.numeric(contraceptive_use$unmet_need_modern),
    contraceptive_use$contraceptive_use_traditional + contraceptive_use$unmet_need_any
  )
  
  demand <- contraceptive_use_all + contraceptive_use$unmet_need_any
  demand_modern <- contraceptive_use$contraceptive_use_modern + unmet_need_modern
  
  demand_satisfied <- contraceptive_use_all / demand
  demand_satisfied_modern <- contraceptive_use$contraceptive_use_modern / demand_modern
  
  non_use <- 1 - contraceptive_use_all
  
  contraceptive_use$contraceptive_use_all <- contraceptive_use_all
  contraceptive_use$unmet_need_modern <- unmet_need_modern
  
  contraceptive_use %<>%
    tibble::add_column(ratio_modern_all, .after = "contraceptive_use_all") %>%
    tibble::add_column(
      demand,
      demand_modern,
      demand_satisfied,
      demand_satisfied_modern,
      non_use,
      .after = "unmet_need_any"
    )
  
  if (simplify_logicals) {
    contraceptive_use$is_in_union <- contraceptive_use$is_in_union == "Y"
    contraceptive_use$is_pertaining_to_methods_used_since_last_pregnancy <-
      contraceptive_use$is_pertaining_to_methods_used_since_last_pregnancy == "Y"
    contraceptive_use$has_geographical_region_bias <-
      contraceptive_use$has_geographical_region_bias == "Y"
    contraceptive_use$has_non_pregnant_and_other_positive_biases <-
      contraceptive_use$has_non_pregnant_and_other_positive_biases == "Y"
    contraceptive_use$has_traditional_method_bias <-
      contraceptive_use$has_traditional_method_bias == "Y"
    contraceptive_use$has_absence_of_probing_questions_bias <-
      contraceptive_use$has_absence_of_probing_questions_bias == "Y"
  }
  
  contraceptive_use
}

transform_posterior_samples <- function(posterior_samples, indicator, transformer, years) {
  dimensions <- dim(posterior_samples)
  
  iterations <- dimensions[1] * dimensions[2]
  period_years <- dimensions[3]
  
  posterior_samples %>%
    transform_yearly(transformer) %>%
    drop() %>%
    tibble::as_tibble() %>%
    setNames(years) %>%
    tidyr::gather(key = "year") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    tibble::add_column(indicator, .before = "value") %>%
    tibble::add_column(iteration = rep(1:iterations, period_years), .before = "year")
}

#' Get posterior samples for all indicators
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by indicator and year
#' @export
get_posterior_samples_for_all_indicators <- function(posterior_samples, years) {
  mcmc_proportion_dimensions <- dim(posterior_samples)
  
  if (length(mcmc_proportion_dimensions) != 4) {
    stop("MCMC proportions matrix must have four dimensions")
  }
  
  if (mcmc_proportion_dimensions[4] != 3) {
    stop("MCMC proportions must be modern, traditional and unmet")
  }
  
  dplyr::bind_rows(
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "contraceptive_use_modern",
      transformer = modern_cpr,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "contraceptive_use_traditional",
      transformer = traditional_cpr,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "unmet_need_any",
      transformer = unmet,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "contraceptive_use_all",
      transformer = total_cpr,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "demand",
      transformer = demand,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "demand_for_modern_methods",
      transformer = demand_modern,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "no_need",
      transformer = no_need,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "unmet_need_modern",
      transformer = unmet_modern,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "non_use",
      transformer = non_use,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "demand_satisfied",
      transformer = demand_satisfied,
      years = years
    ),
    transform_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = "demand_satisfied_modern",
      transformer = demand_satisfied_modern,
      years = years
    )
  )
}

transform_proportions <- function(posterior_samples, transformer) {
  posterior_samples %>%
    transform_yearly(transformer) %>%
    array_summarise() %>%
    drop() %>%
    tibble::as_tibble()
}

#' Get proportions
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @param transformer `function` Computes the desired result
#' @return `data.frame` Values by year and percentile
get_proportions <- function(posterior_samples, first_year, transformer) {
  mcmc_proportion_dimensions <- dim(posterior_samples)
  
  if (length(mcmc_proportion_dimensions) != 4) {
    stop("MCMC proportions matrix must have four dimensions")
  }
  
  if (mcmc_proportion_dimensions[4] != 3) {
    stop("MCMC proportions must be modern, traditional and unmet")
  }
  
  years <- as.integer(first_year + 0:(mcmc_proportion_dimensions[3] - 1))
  
  transform_proportions(
    posterior_samples = posterior_samples,
    transformer = transformer
  ) %>%
    tibble::add_column(year = years, .before = 1) %>%
    tidyr::gather(key = "percentile", value = "value", -year)
}





#' get_contraceptive_use_any
#'
#' `total_cpr = modern_cpr + traditional_cpr`
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
#' 
get_contraceptive_use_any <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = total_cpr
  )
}

#' Get modern CPR
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_contraceptive_use_modern <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = modern_cpr
  )
}

#' Get traditional CPR
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_contraceptive_use_traditional <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = traditional_cpr
  )
}

#' Get non-use
#'
#' `non_use = 1 - total_cpr`
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_non_use <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = non_use
  )
}

#' Get unmet need
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_unmet_need_any <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = unmet
  )
}

#' Get unmet need for modern methods
#'
#' `unmet_modern = traditional_cpr + unmet`
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_unmet_need_modern <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = unmet_modern
  )
}

#' Get total demand
#'
#' `demand = total_cpr + unmet`
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_demand <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = demand
  )
}

#' Get demand for modern methods
#'
#' `demand = modern_cpr + unmet_modern`
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_demand_modern <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = demand_modern
  )
}

#' Get demand satisfied
#'
#' `demand = total_cpr / demand`
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_demand_satisfied <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = demand_satisfied
  )
}

#' Get demand satisfied with a modern method
#'
#' `demand_satisfied_modern = modern_cpr / demand`
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_demand_satisfied_modern <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = demand_satisfied_modern
  )
}

#' Get no need
#'
#' `no_need = 1 - demand`
#'
#' @inheritParams fpem_calculate_results
#' @param first_year `integer` Earliest year represented in the data
#' @return `data.frame` Values by year and percentile
get_no_need <- function(posterior_samples, first_year) {
  get_proportions(
    posterior_samples = posterior_samples,
    first_year = first_year,
    transformer = no_need
  )
}


#' Get estimated population counts
#'
#' @param proportions `data.frame` Proportions, as returned from [get_proportions()]
#' @param annual_country_population_counts `data.frame`
#' Contains "mid_year" and "population_count" columns for group (e.g. married and 15-49) in one country.
#' There must be a single population count per country.
#' @return `data.frame` Values by year and percentile
get_estimated_counts <- function(proportions, annual_country_population_counts) {
  proportions %>%
    dplyr::inner_join(annual_country_population_counts, by = c("year" = "mid_year")) %>%
    dplyr::mutate(result_population_count = population_count * value) %>%
    dplyr::select(year, percentile, population_count = result_population_count)
}

select_posterior_samples <- function(posterior_samples, indicator, year) {
  posterior_samples[, , year, indicator]
}

select_relative_posterior_samples <- function(posterior_samples, indicator, year, relative_to_year = NULL) {
  samples <- posterior_samples[, , year, indicator]
  
  if (!is.null(relative_to_year)) {
    samples <- samples - posterior_samples[, , relative_to_year, indicator]
  }
  
  samples
}

#' Get posterior probability from proportion or population
#'
#' @inheritParams fpem_calculate_results
#' @param population_count `integer` Number of individuals in the sample population (1 to calculate from proportion)
#' @param indicator `integer` Indicator index (1 = modern, 2 = traditional, 3 = unmet)
#' @param year `integer` Year index
#' @param above `logical` If FALSE then it's below
#' @param cutoff `numeric` Cutoff proportion or population
#' @return `numeric` Posterior probability
get_posterior_probability_from_cutoff_target <- function(
  posterior_samples,
  population_count_year = 1,
  population_count_relative_to_year = 1,
  indicator,
  year,
  relative_to_year = NULL,
  above,
  cutoff) {
  samples <- select_posterior_samples(
    posterior_samples = posterior_samples,
    indicator = indicator,
    year = year
  )
  
  population_samples <- samples * population_count_year
  
  if (!is.null(relative_to_year)) {
    relative_to_samples <- select_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = indicator,
      year = relative_to_year
    )
    
    relative_to_population_samples <- relative_to_samples * population_count_relative_to_year
    
    population_samples <- population_samples - relative_to_population_samples
  }
  
  (1 - above) + ifelse(above, 1, -1) * mean(population_samples > cutoff)
}

#' Get proportion or population from posterior probability
#'
#' @inheritParams fpem_calculate_results
#' @param population_count `integer` Number of individuals in the sample population (1 to calculate proportion)
#' @param indicator `integer` Indicator index (1 = modern, 2 = traditional, 3 = unmet)
#' @param year `integer` Year index
#' @param greater_than `logical` If FALSE then it's less than
#' @param probability `numeric` Posterior probability
#' @return `numeric` Proportion or population
get_target_from_posterior_probability <- function(
  posterior_samples,
  population_count_year = 1,
  population_count_relative_to_year = 1,
  indicator,
  year,
  relative_to_year = NULL,
  greater_than,
  probability) {
  samples <- select_posterior_samples(
    posterior_samples = posterior_samples,
    indicator = indicator,
    year = year
  )
  
  population_samples <- samples * population_count_year
  
  if (!is.null(relative_to_year)) {
    relative_to_samples <- select_posterior_samples(
      posterior_samples = posterior_samples,
      indicator = indicator,
      year = relative_to_year
    )
    
    relative_to_population_samples <- relative_to_samples * population_count_relative_to_year
    
    population_samples <- population_samples - relative_to_population_samples
  }
  
  population_quantile <- quantile(population_samples, probs = 1 - probability, type = 8)
  
  (1 - greater_than) + ifelse(greater_than, 1, -1) * unname(population_quantile)
}

#' Get progress for indicator
#'
#' @inheritParams fpem_calculate_results
#' @param from_year_population_count `integer`
#' Number of individuals in the sample population for "from" year (1 to calculate proportion)
#' @param to_year_population_count `integer`
#' Number of individuals in the sample population for "to" year (1 to calculate proportion)
#' @param indicator `integer` Indicator index (1 = modern, 2 = traditional, 3 = unmet)
#' @param from_year `integer` First year
#' @param to_year `integer` Last year
#' @return `numeric` Median and 2.5%/97.5% quantiles of change
get_progress <- function(
  posterior_samples,
  from_year_population_count = 1,
  to_year_population_count = 1,
  indicator,
  from_year,
  to_year,
  absolute = FALSE) {
  from_year_posterior_samples <- select_posterior_samples(
    posterior_samples = posterior_samples,
    indicator = indicator,
    year = from_year
  )
  
  to_year_posterior_samples <- select_posterior_samples(
    posterior_samples = posterior_samples,
    indicator = indicator,
    year = to_year
  )
  
  from_year_values <- from_year_posterior_samples * from_year_population_count
  to_year_values <- to_year_posterior_samples * to_year_population_count
  
  change <- to_year_values - from_year_values
  
  if (absolute) {
    values <- change
  } else {
    values <- change / from_year_values
  }
  
  quantile(values, probs = c(0.025, 0.5, 0.975), type = 8)
}


#' Calculate married and unmarried women posterior samples
#'
#' @param in_union_posterior_samples `array` An array of n chains x n iterations x n years x n proportions
#' @param not_in_union_posterior_samples `array` An array of n chains x n iterations x n years x n proportions
#' @return `array` Posterior samples for all women
#' @export
calculate_all_women_posterior_samples <- function(
  in_union_posterior_samples,
  not_in_union_posterior_samples,
  in_union_population_counts,
  not_in_union_population_counts) {
  nyears <- dim(in_union_posterior_samples)[3]
  
  all_women_samples <- array(NA, dim(in_union_posterior_samples))
  
  for (t in 1:nyears) {
    all_women_samples[, , t, ] <- (
      (in_union_posterior_samples[, , t, ] *
         in_union_population_counts[t] +
         not_in_union_posterior_samples[, , t, ] *
         not_in_union_population_counts[t]) /
        (in_union_population_counts[t] + not_in_union_population_counts[t])
    )
  }
  
  all_women_samples
}
