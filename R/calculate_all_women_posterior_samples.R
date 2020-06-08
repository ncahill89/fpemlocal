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
