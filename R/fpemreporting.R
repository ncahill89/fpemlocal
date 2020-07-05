
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



#returns values in wide format where columns are percentiles and rows are years
transform_proportions <- function(posterior_samples, transformer) {
  posterior_samples %>%
    transform_yearly(transformer) %>%
    array_summarise() %>%
    drop() %>%
    tibble::as_tibble()
}

# returns values in long format without percentile column
transform_proportion_2 <- function(posterior_samples, transformer) {
  transform_proportions(posterior_samples, transformer) %>%
    tidyr::gather(key = "percentile", value = paste(transformer)) %>%
    dplyr::select(-percentile)
}


#' #' Get proportions
#' #'
#' #' @inheritParams calc_fp
#' #' @param first_year `integer` Earliest year represented in the data
#' #' @param transformer `function` Computes the desired result
#' #' @return `data.frame` Values by year and percentile
#' get_proportions <- function(posterior_samples, first_year, transformer) {
#'   mcmc_proportion_dimensions <- dim(posterior_samples)
#' 
#'   if (length(mcmc_proportion_dimensions) != 4) {
#'     stop("MCMC proportions matrix must have four dimensions")
#'   }
#' 
#'   if (mcmc_proportion_dimensions[4] != 3) {
#'     stop("MCMC proportions must be modern, traditional and unmet")
#'   }
#' 
#'   years <- as.integer(first_year + 0:(mcmc_proportion_dimensions[3] - 1))
#' 
#'   transform_proportions(
#'     posterior_samples = posterior_samples,
#'     transformer = transformer
#'   ) %>%
#'     tibble::add_column(year = years, .before = 1) %>%
#'     tidyr::gather(key = "percentile", value = "value", -year)
#' }
#' 
#' 



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
    dplyr::pull(population_count = result_population_count)
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
#' @inheritParams calc_fp
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
#' @inheritParams calc_fp
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
#' @inheritParams calc_fp
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
