#' Calculate fp indicators from samples, autosave
#'
#' A wrapper for \code{\link{calc_fp_c}} which reads in the model fit from the respective `runname`. The output is saved in the results directory with the same `runname`. The output can be automatically read in by proceeding wrapper functions with the `runname` argument specified.
#'
#' @param runname
#' @inheritParams calc_fp
#' 
#' @return \emph{NULL} 
#' 
#' @export
#'
calc_fp_c_autosave <-
  function(runname = NULL,
           population_data = NULL) {

    fit <- readRDS(file.path("output/runs", paste0(runname, ".rds")))
    results <- calc_fp_c(fit = fit,
                                         population_data = population_data)
    if (!dir.exists("output")) dir.create("output")
    if (!dir.exists("output/results")) dir.create("output/results")
    pathout <- file.path("output/results", paste0(runname, ".rds"))
    saveRDS(results, pathout)
    print(paste0("Your file was saved to ", pathout))
  }




#' Calculate fp indicators from samples
#'
#' Maps multiple sets of runs to \code{\link{calc_fp}}
#'
#' @param fit \emph{\sQuote{List}} The list returned from \code{\link{fit_fp_csub}}
#' @inheritParams calc_fp
#'
#' @return \emph{List} A list of long format data.frames
#' @export
#'
calc_fp_c <-
  function(fit,
           population_data = NULL) {
    purrr::pmap(list(fit, list(population_data)), calc_fp_csub)
  }



#' calc_fp_csub
#'
#' @param fit \emph{\sQuote{List}} The list returned from \code{\link{fit_fp_csub}}
#' @inheritParams calc_fp
#'
#' @return \emph{List} A list of long format data.frames
#' @export
#'
calc_fp_csub <- function(fit,
                         population_data = NULL) {
  posterior_samples <- fit %>% purrr::chuck("posterior_samples")
  population_data <- population_data_import(
    population_data = population_data,
    fit = fit
  )
  results <- calc_fp(posterior_samples = posterior_samples,
                     population_data = population_data,
                     first_year = fit %>% 
                       purrr::chuck("core_data","year_sequence_list", "result_seq_years") %>% 
                       min
                         )
  return(results)
}
