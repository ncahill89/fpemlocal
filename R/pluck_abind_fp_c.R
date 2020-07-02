pluck_abind_fp_c <- function(fits) {
  
  fits_v2 <- purrr::pmap(list(fits,
                              1,
                              "posterior_samples"),
                         purrr::chuck #more robust than pluck, chuck will throw an error if an element is null vs pluck function which returns null
  )
  posterior_samples <- purrr::invoke(abind::abind, fits_v2, along = 1)
  return(posterior_samples)
}