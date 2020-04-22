
#' Usual quantiles.
#'
#' @return Usual quantiles used to summarise credible intervals.
#' @export
standard_quantiles = function() c(
  0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)

#' Usual summary of a set of samples.
#'
#' @param x set of parameter samples to summarize.
standard_summary = function(x, q = standard_quantiles()) c(mean = mean(x),
                                                           quantile(x, q))
#' array_summarise
#'
#' Summarize indicator array for a single unit
#'
#' @param x an array of samples with margins:
#'        1) chain
#'        2) iteration
#'        4) (year) optionally
#'        5) indicator
#' @param h a function that takes a  vector of samples and
#'        returns one or more summaries of the vector.
#' @return a set of summaries with margins:
#'           1) (year) optionally
#'           2) summaries
array_summarise = function(x, h = standard_summary, ...) {
  nd = length(dim(x))
  y = apply(X = x, MARGIN = 3:nd, h, ...)
  y = rotate_dimensions(y)
  return(y)
}
