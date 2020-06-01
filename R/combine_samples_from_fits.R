combine_samples_from_fits <- function(fits, divs) {
  fits_v2 <- purrr::pmap(list(fits, #this is a list of country lists, thus supplying one country at a time
                              "fit",
                              "posterior_samples"),
                         purrr::chuck #throws an error if an element is null vs pull which returns null
  )
  posterior_samples <- purrr::invoke(abind::abind, fits_v2, along = 1)
  dimnames(posterior_samples) <- list(divs, NULL, NULL, NULL)
  return(posterior_samples)
}