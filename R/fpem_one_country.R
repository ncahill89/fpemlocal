#' Run one country model and automatically save output
#'
#' @inherit fpem
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
#' @inherit fpem_1country_1union
#' @export
fpem_one_country <- function(
  is_in_union,
  ...
  
) {
  if(is_in_union == "ALL") {
    runy <- fpem_1country_1union(
      is_in_union = "Y",
      ...
    )
    runn <- fpem_1country_1union(
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
  } else {
    runlist <-  fpem_1country_1union(
      is_in_union = is_in_union,
      ...
    )
    
  }
  return(runlist)

}




#' Run one country model
#'
#' Runs the family planning estimation model and returns samples and bias adjusted observations for respective run.
#'
#' @param is_in_union \emph{\sQuote{Character}} "Y" if women are in union.
#' @param surveydata_filepath \emph{\sQuote{Character}} If NULL package data is used.
#' @param service_stats \emph{\sQuote{Logical}} If FALSE service statistics are not used. Service stats are not public. Requires FPcounts/fpemservicestat package.
#' @param service_stats_filepath \emph{\sQuote{Character}}If NULL private package data is used. Requires FPcounts/fpemservicestat package.
#' @param division_numeric_code \emph{\sQuote{Numeric}} A number associated with the country. See the data from \code{\link{divisions}}.
#' @param first_year \emph{\sQuote{Numeric}} The first year of model estimates in output. The model will be fit to all data, including dates before this date if available.
#' @param last_year \emph{\sQuote{Numeric}} The last year of model estimates in output. The model will be fit to all data, including dates after this date if available.
#' @param subnational '\emph{\sQuote{Logical}} If FALSE runs the national model.
#'
#'
#' @return \emph{\sQuote{List}}
#' \enumerate{
#'   \item \strong{posterior_samples}  \emph{\sQuote{Numeric array}} An array of samples of dimension chains x samples x years x proportions.
#'   \item \strong{core_data}          \emph{\sQuote{Data.frame}} The processed data associated with the model run from \code{\link{core_data}}.
#' }
#'
#' @examples See the reposiotry url in references for detailed examples
#' @references \url{https://github.com/FPcounts/FPEM}
#' @export
fpem_1country_1union <- function(
  is_in_union = "Y",
  surveydata_filepath = NULL,
  service_stats = FALSE,
  service_stats_filepath = NULL,
  division_numeric_code,
  first_year = NULL,
  last_year,
  subnational = FALSE,
  diagnostic = FALSE,
  params_to_save = c("mod.ct", "unmet.ct", "trad.ct", "mu.in", "logitratio.yunmet.hat.i"),
  nchains = 3,
  niter = 2500,
  nburnin = 500,
  nthin = max(1, floor((niter - nburnin) / 1000))
) {
  # check inputs to this wrapper
  check_inputs(
    surveydata_filepath = surveydata_filepath,
    subnational = subnational,
    division_numeric_code = division_numeric_code
  )
  # core data consists of imported data which is filtered and settings for the run
  core_data <- core_data(
    is_in_union = is_in_union,
    surveydata_filepath = surveydata_filepath,
    division_numeric_code = division_numeric_code,
    first_year = first_year,
    last_year = last_year,
    subnational = subnational
  )
  # processing for jags model including index
  list_auxiliary <- list_auxiliary_data(core_data)
  list_global <- list_global_data(is_in_union, core_data)
  list_bias <- list_bias_data(core_data)
  list_service_stats <-
    list_service_stats(
      service_stats = service_stats,
      service_stats_filepath = service_stats_filepath,
      model_seq_years = core_data$year_sequence_list$model_seq_y,
      division_numeric_code,
      first_year
    )
  # write and run model
  write_jags_model(
    old_dm = TRUE,
    #is_in_union = is_in_union,
    include_ss_data = !is.null(list_service_stats),
    nulldata = nrow(core_data$observations) == 0,
    is_in_union = is_in_union
  )
  mod <- jags.parallel(
    data = c(list_auxiliary, list_global, list_bias, list_service_stats, Y = 1),
    parameters.to.save = params_to_save,
    model.file = "model.txt",
    n.chains = nchains,
    n.iter = niter,
    n.burnin = nburnin,
    n.thin = nthin
  )
  # bias adjusted data added to core data based on model estimates of bias
  if (nrow(core_data$observations) > 0) {
    core_data$observations <- bias_adj(
      core_data = core_data,
      list_auxiliary = list_auxiliary,
      list_global = list_global,
      mod = mod)
  }
  # reformat samples
  posterior_samples <- posterior_samples_array_format(mod, core_data)
  if (diagnostic) {
    return(list(
      posterior_samples = posterior_samples,
      jagsout = mod,
      core_data = core_data
    )
    )
  } else {
    return(list(
      posterior_samples = posterior_samples,
      core_data = core_data
    )
    )
  }
}


