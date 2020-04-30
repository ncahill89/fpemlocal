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
fpem_one_country <- function(
  is_in_union = "Y",
  surveydata_filepath = NULL,
  service_stats = FALSE,
  service_stats_filepath = NULL,
  division_numeric_code,
  first_year = NULL,
  last_year,
  subnational = FALSE
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
    runlist <- list(posterior_samples = samples_all,
                    core_data = core_data)
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
  }
    return(runlist)
}



