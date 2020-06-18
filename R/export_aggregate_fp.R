

#' aggregate_fp
#'
#' @param posterior_samples 
#' @param population_data 
#'
#' @return
#' @export
#'
#' @examples
aggregate_fp <- function(posterior_samples,
                         population_data) 
{
  aggregate_samples <- aggregate_fp_sub(posterior_samples = posterior_samples,
                                        weight_df = calculate_weight_df(population_data = population_data))
  return(aggregate_samples)
}

calculate_weight_df <- function(population_data){
  weight_df <- population_data %>%
    dplyr::group_by(mid_year) %>%
    dplyr::mutate(aggregate_annual_pop = sum(population_count)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weight = population_count / aggregate_annual_pop) %>%
    dplyr::select(weight, division_numeric_code)
  return(weight_df)
}

#' aggregate_fp_sub
#'
#' @inheritParams weight_samples
#' @param weights \emph{'Vector'} A vector of weights selected from \code{\link[weight_generator]{weight_generator}}
#' @export
#'
#' @return \emph{'Numeric array'} An array of samples of dimension chains x samples x years x proportions
#'
aggregate_fp_sub <- function(posterior_samples, weight_df) {
  # first order the division codes of the array to match weights
  divs <- weight_df %>% dplyr::pull(division_numeric_code) %>% unique
  posterior_samples <- posterior_samples[paste(divs), , , , drop = FALSE]
  # arrange array and multiple by weights
  weighted_samples <- weight_df$weight * aperm(posterior_samples, c(1, 3, 4, 2))
  # arrange array and sum the weighted samples to obtain aggregates
  weighted_samples <- aperm(weighted_samples, c(1, 4, 2, 3))
  weighted_samples <- apply(weighted_samples, 2:4, sum)
  weighted_samples <-
    array(weighted_samples, dim = c(1, dim(weighted_samples)))
  return(weighted_samples)
}



#' aggregate_fp_multilevel
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
#' 
aggregate_fp_multilevel <-
  function(posterior_samples,
           division_level_data,
           population_data) {
    weight_input_checker(division_level_data,
                         population_data,
                         posterior_samples)
    # create a named list of levels. subsequent maps will retain the names
    levels <-
      division_level_data %>% dplyr::pull(division_level) %>% unique %>% as.list
    names(levels) <-
      division_level_data %>% dplyr::pull(division_level_name) %>% unique
    # pull a vectors of division codes for each level, each element corresponds to a level
    divs_perlevel_list <-
      purrr::pmap(list(division_level_data = list(division_level_data),
                       level = levels), 
                  pull_divs)
    # subset posterior_sample arrays, each element corresponds to a level
    posterior_samples_perlevel_list <-
      purrr::pmap(list(list(posterior_samples),
                       divs_perlevel_list),
                  subset_samples)
    # subset population, each element corresponds to a level
    population_perlevel_list <- purrr:pmap(list(population_data,
                                                divs_perlevel_list),
                                           subset_population)
    # aggregate with the subset samples and pop counts, each element corresponds to a level
    aggregate_samples_perlevel_list <- purrr::pmap(list(posterior_samples = posterior_samples_perlevel_list,
                                                        population_data = population_perlevel_list),
                                                   aggregate_fp)
    return(aggregate_samples_perlevel_list)
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


#' pull_divs
#'
#' @param division_level_data 
#' @param level 
#'
#' @return
#'
#' @examples
pull_divs <- function(division_level_data, level) {
divs <- division_level_data %>% 
  dplyr::filter({{level}}) %>% 
  dplyr::pull(division_numeric_code)
return(divs)
}

#' subset_samples
#'
#' @param posterior_samples 
#' @param divs_perlevel 
#'
#' @return
#'
#' @examples
subset_samples <- function(posterior_samples, divs_perlevel) {
  return(posterior_samples[paste(divs_perlevel), , , , drop = FALSE])
}

#' subset_population
#'
#' @param population_data 
#' @param divs_perlevel 
#'
#' @return
#'
#' @examples
subset_population <- function(population_data, divs_perlevel) {
  population_perlevel <- population_data %>%
    dplyr::filter(division_numeric_code %in% divs_perlevel)
  returb(population_perlevel)
}






# division_level_data_maker <- function(
#   level,
#   division_numeric_code_vector
# ) {
#   division_level_data <- divisions %>%
#     dplyr::mutate(division_level = {{level}}) %>%
#     dplyr::select(division_numeric_code, division_level) %>%
#     dplyr::filter(division_numeric_code %in% {{division_numeric_code_vector}})
#   return(division_level_data)
# }
