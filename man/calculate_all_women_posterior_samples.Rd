% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_all_women_posterior_samples.R
\name{calculate_all_women_posterior_samples}
\alias{calculate_all_women_posterior_samples}
\title{Calculate married and unmarried women posterior samples}
\usage{
calculate_all_women_posterior_samples(
  in_union_posterior_samples,
  not_in_union_posterior_samples,
  in_union_population_counts,
  not_in_union_population_counts
)
}
\arguments{
\item{in_union_posterior_samples}{`array` An array of n chains x n iterations x n years x n proportions}

\item{not_in_union_posterior_samples}{`array` An array of n chains x n iterations x n years x n proportions}
}
\value{
`array` Posterior samples for all women
}
\description{
Calculate married and unmarried women posterior samples
}
