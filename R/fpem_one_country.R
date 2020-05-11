#' Run one country model and automatically save output
#'
#' @inherit do_1country_run
#' @export
fpem_one_country_autosave <- function(
  runname = NULL,
  ...
) {
  runlist <- fpem_one_country(
    ...
  )
  if (!dir.exists("output")) dir.create("output")
  if (!dir.exists("output/runs")) dir.create("output/runs")
  saveRDS(runlist, file.path("output/runs", paste0(runname, ".rds")))
}


#' Run one country model
#'
#' @inherit do_1country_run
#' @export
fpem_one_country <- function(
  is_in_union,
  ...
  
) {
  if(is_in_union == "ALL") {
    runy <- do_1country_run(
      is_in_union = "Y",
      ...
    )
    runn <- do_1country_run(
      is_in_union = "N",
      ...
    )
    samples_all <- posterior_samples_all_women(in_union_posterior_samples = runy$posterior_samples, 
                                               not_in_union_posterior_samples = runn$posterior_samples, 
                                               core_data = runy$core_data)
    core_data <- runy$core_data
    core_data$observations <- rbind(runy$core_data$observations,
                                    runn$core_data$observations)
    core_data$is_in_union <- is_in_union
    runall <- list(posterior_samples = samples_all,
                    core_data = core_data)
    runlist <- list(runy = runy,
                runn = runn,
                runall = runall)
    class(runlist) <- "all_union"
  } else {
    runlist <-   do_1country_run(
      is_in_union = is_in_union,
      ...
    )
    class(runlist) <- "single_union"
  }
  return(runlist)

}



