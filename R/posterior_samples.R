
#' Posterior samples reformat
#'
#' Manipulate posterior samples into dimension chains x samples x years x proportions
#'
#' @param fit the object returned from jags
#' @param core_data consists of manipulated observations and run settings for \code{\link{fit_fp_c}}
#'
#' @return \emph{\sQuote{Numeric array}} An array of samples of dimension chains x samples x years x proportions
posterior_samples_array_format <- function(fit, core_data) {
  props <- c("mod", "trad", "unmet")
  nyears <-  max(core_data$year_sequence_list$model_seq)
  tempdim <- fit$BUGSoutput$sims.list$mod.ct
  total_iter <- dim(tempdim)[1]
  mod <- fit$BUGSoutput$sims.list$mod.ct %>% as.vector()
  trad <- fit$BUGSoutput$sims.list$trad.ct %>% as.vector()
  unmet <- fit$BUGSoutput$sims.list$unmet.ct %>% as.vector()
  posterior_samples <- array(
    data = c(mod, trad, unmet),
    dim = c(1, total_iter, nyears, length(props)),
    dimnames = list(
      chain = 1,
      iteration = 1:total_iter,
      unit_time = 1:nyears,
      props = props
    )
  )

  return(posterior_samples[1,1:total_iter,core_data$year_sequence_list$result_seq,1:length(props), drop = FALSE])
}




#' Get posterior samples for all women
#'
#' Combine posterior samples from in-union and not-in-union to obtain samples for all women
#'
#' @param in_union_posterior_samples \emph{`array`} An array of n chains x n iterations x n years x n proportions
#' @param not_in_union_posterior_samples \emph{`array`} An array of n chains x n iterations x n years x n proportions
#' @param core_data
#'
#' @return \emph{`array`} Posterior samples for all women
#' @export
posterior_samples_all_women <-
    function(in_union_posterior_samples,
           not_in_union_posterior_samples,
           core_data
  ) {

    division_numeric_code <- core_data$units$division_numeric_code
    first_year <- min(core_data$year_sequence_list$result_seq_years)
    last_year <- max(core_data$year_sequence_list$result_seq_years)
    nyears <- length(core_data$year_sequence_list$result_seq_years)
    in_union_population_counts = population_counts %>%
      dplyr::filter(division_numeric_code == !!division_numeric_code) %>%
      dplyr::filter(is_in_union == "Y") %>%
      dplyr::filter(age_range == "15-49") %>%
      dplyr::filter(mid_year >= first_year) %>%
      dplyr::filter(mid_year <= last_year) %>%
      dplyr::select(population_count) %>%
      unlist() %>%
      as.vector()
    not_in_union_population_counts = population_counts %>%
      dplyr::filter(division_numeric_code == !!division_numeric_code) %>%
      dplyr::filter(is_in_union == "N") %>%
      dplyr::filter(age_range == "15-49") %>%
      dplyr::filter(mid_year >= first_year) %>%
      dplyr::filter(mid_year <= last_year) %>%
      dplyr::select(population_count) %>%
      unlist() %>%
      as.vector()

    all_women_samples <- array(NA, dim(in_union_posterior_samples))

    for (t in 1:nyears) {
      all_women_samples[, , t,] <- (
        (
          in_union_posterior_samples[, , t,] *
            in_union_population_counts[t] +
            not_in_union_posterior_samples[, , t,] *
            not_in_union_population_counts[t]
        ) /
          (
            in_union_population_counts[t] + not_in_union_population_counts[t]
          )
      )
    }

    return(all_women_samples)
  }
