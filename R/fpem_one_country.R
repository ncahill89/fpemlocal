#' Run one country model and automatically save output
#'
#' @inherit do_1country_run
#' @export
fpem_one_country_autosave <- function(
  runname = NULL,
  is_in_union = "Y",
  surveydata_filepath = NULL,
  service_stats = FALSE,
  service_stats_filepath = NULL,
  division_numeric_code,
  first_year = NULL,
  last_year,
  subnational = FALSE
) {
  runlist <- fpem_one_country(
    is_in_union = is_in_union,
    surveydata_filepath = surveydata_filepath,
    service_stats = service_stats,
    service_stats_filepath = service_stats_filepath,
    division_numeric_code = division_numeric_code,
    first_year = first_year,
    last_year = last_year,
    subnational = subnational
  )
  if (!dir.exists("runs")) dir.create("runs")
  saveRDS(runlist, file.path("runs", paste0(runname, ".rds")))
}


#' Run one country model
#'
#' @inherit do_1country_run
#' @export
fpem_one_country <- function(
  is_in_union,
  surveydata_filepath = NULL,
  service_stats = FALSE,
  service_stats_filepath = NULL,
  division_numeric_code,
  first_year = NULL,
  last_year,
  subnational = FALSE,
  
) {
  if(is_in_union == "ALL") {
    runy <- do_1country_run(
      is_in_union = "Y",
      surveydata_filepath = surveydata_filepath,
      service_stats = service_stats,
      service_stats_filepath = service_stats_filepath,
      division_numeric_code = division_numeric_code,
      first_year = first_year,
      last_year = last_year,
      subnational = subnational
    )
    runn <- do_1country_run(
      is_in_union = "N",
      surveydata_filepath = surveydata_filepath,
      service_stats = service_stats,
      service_stats_filepath = service_stats_filepath,
      division_numeric_code = division_numeric_code,
      first_year = first_year,
      last_year = last_year,
      subnational = subnational
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
      surveydata_filepath = surveydata_filepath,
      service_stats = service_stats,
      service_stats_filepath = service_stats_filepath,
      division_numeric_code = division_numeric_code,
      first_year = first_year,
      last_year = last_year,
      subnational = subnational
    )
    class(runlist) <- "single_union"
  }
  return(runlist)

}



