#' weight_generator
#'
#' @inheritParams weight_samples
#'
#' @return data.frame
#'
weight_generator <- function(division_level_data, population_data) {
  df <- dplyr::left_join(population_data, division_level_data) %>%
    dplyr::group_by(division_level, mid_year) %>%
    dplyr::mutate(poplevelsum = sum(population_count)) %>%
    dplyr::ungroup()
  df <- df %>%
    dplyr::mutate(weight = population_count / poplevelsum) %>%
    dplyr::select(weight, division_numeric_code, division_level)
  return(df)
}


#' aggregate_with_weights
#'
#' @inheritParams weight_samples
#' @param weights \emph{'Vector'} A vector of weights selected from \code{\link[weight_generator]{weight_generator}}
#' @export
#'
#' @return \emph{'Numeric array'} An array of samples of dimension chains x samples x years x proportions
#'
aggregate_with_weights <- function(posterior_samples, weights) {
  weighted_samples <- weights * aperm(posterior_samples, c(1, 3, 4, 2))
  weighted_samples <- aperm(weighted_samples, c(1, 4, 2, 3))
  weighted_samples <- apply(weighted_samples, 2:4, sum)
  weighted_samples <-
    array(weighted_samples, dim = c(1, dim(weighted_samples)))
  return(weighted_samples)
}


aggregate_set_per_level <- function(level, weight_data, posterior_samples) {
  weight_subset <- weight_data %>%
    dplyr::filter(division_level == level)
  divs <- weight_subset$division_numeric_code %>%
    unique()
  sample_subset <- posterior_samples[paste(divs), , , ]
  aggregate_samples <- aggregate_with_weights(posterior_samples = sample_subset, weights = weight_subset$weight)
  return(aggregate_samples)
}


#' weight_input_checker
#'
#' @inheritParams weight_samples
#'
#' @return informative error messages when inputs are incorrect
weight_input_checker <-
  function(division_level_data,
           population_data,
           posterior_samples) {
    if (is.null(dimnames(posterior_samples))) {
      stop("posterior_samples need dimnames attribute for divisions")
    }
    timeseq_pop <-
      unique(population_data$mid_year - min(population_data$mid_year) + 1)
    timeseq_samples <- 1:dim(posterior_samples)[3]
    div_pop <- division_level_data$division_numeric_code
    div_samples <- unlist(dimnames(posterior_samples)[1])
    if (!(all(timeseq_pop %in% timeseq_samples) &
          all(timeseq_samples %in% timeseq_pop))) {
      stop(
        "the years in the posterior_samples do not match the years in population_data, cannot obtain weights"
      )
    }
    if (!(all(div_pop %in% div_samples) &
          all(div_samples %in% div_pop))) {
      stop(
        "the division codes in your posterior_samples do not match the division codes supplied"
      )
    }
  }



#' weight_division_match
#'
#' @inheritParams weight_samples
#'
#' @return posterior_samples array with divisions in the order to match weights which will be calculated
weight_division_match <-
  function(division_level_data,
           population_data,
           posterior_samples) {
    order_index <-
      match(unique(division_level_data$division_numeric_code),
            as.numeric(dimnames(posterior_samples)[[1]])) #NC, 2019-11-06 needed unique codes
    posterior_samples <- posterior_samples[order_index, , , ]
  }



#' weight_samples
#'
#' @param division_level_data data frame with country codes and corresponding aggregate level
#' @param population_data population data, union and years of the samples
#' @param posterior_samples posteriors samples from \code{\link{posterior_samples_array_format}}
#'
#' @export
#'
#' @examples dimnames(posterior_samples) <- list(division_numeric_code, NULL, NULL, NULL) #provide corresponding division numeric codes in posterior_samples attributes
#' population_data <- population_counts %>%
#'   dplyr::filter(is_in_union == union) %>%
#'   dplyr::filter(mid_year <= last_year) %>%
#'   dplyr::filter(mid_year >= first_year)
#' division_level_data <- divisions %>%
#'   mutate(division_level = region_numeric_code)%>%
#'   select(division_numeric_code, division_level)
#' posterior_samples_list <- weight_samples(division_level_data, population_data, posterior_samples)
aggregate_fp <-
  function(
    posterior_samples,
    division_level_data,
    population_data
           ) {
    weight_input_checker(division_level_data, 
                         population_data, 
                         posterior_samples)
    posterior_samples <-
      weight_division_match(division_level_data, 
                            population_data, 
                            posterior_samples)
    weight_data <-
      weight_generator(division_level_data, population_data)
    levels <- division_level_data %>% dplyr::pull(division_level) %>% unique %>% as.list
    names(levels) <- division_level_data %>% dplyr::pull(division_level_name) %>% unique
    posterior_samples_list <- purrr::pmap(list(levels,
              weight_data %>% list,
              posterior_samples %>% list),
         aggregate_set_per_level
         )
    return(posterior_samples_list)
  }


division_level_data_maker <- function(
  level,
  division_numeric_code_vector
) {
  division_level_data <- divisions %>%
    dplyr::mutate(division_level = {{level}}) %>%
    dplyr::select(division_numeric_code, division_level) %>%
    dplyr::filter(division_numeric_code %in% {{division_numeric_code_vector}})
  return(division_level_data)
}



pluck_abind_fp_c <- function(fits, 
                             division_numeric_code_vector) {
  
  fits_v2 <- purrr::pmap(list(fits, #this is a list of country lists, thus supplying one country at a time
                              1,
                              "posterior_samples"),
                         purrr::chuck #throws an error if an element is null vs pull which returns null
  )
  posterior_samples <- purrr::invoke(abind::abind, fits_v2, along = 1)
  dimnames(posterior_samples) <- list(divs, NULL, NULL, NULL) # we may not need this is dimension before abind were named
  return(posterior_samples)
}

calc_fp_aggregate <- function(
  fits,
  division_numeric_code_vector,
  level,
  population_counts_custom = NULL,
  first_year,
  is_in_union) {
  # reduce the fits down to just the posterior samples and then bind them into a single array dim = [Country x Nsamples x Years x 3(from p,r,z)]
  posterior_samples <- pluck_abind_fp_c(fits = fits, 
                                              division_numeric_code_vector = divs)
  # division level data has the codes for the level and the country codes, the level column is variable and given a generic name to be accessed in deeper functions
  division_level_data <- divisions %>%
    dplyr::rename(division_level = {{level}}) %>%
    dplyr::select(division_numeric_code, division_level) %>%
    dplyr::filter(division_numeric_code %in% {{division_numeric_code_vector}})
  # filtering of population data, users may supply their own
  if (is.null(population_counts_custom)) {
    population_data = population_counts
  }
  last_year <- first_year + dim(posterior_samples)[3] - 1
  population_data <- population_data %>%
    dplyr::filter(division_numeric_code %in% {{division_numeric_code_vector}}) %>%
    dplyr::filter(is_in_union == {{is_in_union}}) %>%
    dplyr::filter(mid_year >= {{first_year}}) %>%
    dplyr::filter(mid_year <= last_year)
  # aggregate the samples using samples, population counts, and division data
  posterior_samples_aggregated <- aggregate_fp(posterior_samples = posterior_samples,
                                               division_level_data = division_level_data,
                                               population_data = population_data)
  # calculate based on the aggregated samples
  results_list <- purrr::pmap(list(posterior_samples = posterior_samples_aggregated,
                                   country_population_counts = list(population_data), #need to wrap any complex inputs in another list()
                                   first_year = first_year),
                              calc_fp)
  return(results_list)
}
