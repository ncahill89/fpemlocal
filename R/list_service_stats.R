
#' list_service_stats
#'
#'fit_fp_cservice statistics data
#'
#' @inheritParams fit_fp_c
#' @param service_stats_filepath File path to service stat csv
#' @param model_seq_years Years of model estimation from\code{\link{year_sequence_maker}}.
#'
list_service_stats <- function(
  service_stats = FALSE,
  service_stats_filepath,
  model_seq_years,
  division_numeric_code, # we need the unit ID added eventually to use that for filtering
  first_year_observed = NULL # to filter out the data prior to or in year of the most recent survey
)

{
  se_visits = 0.0254
  se_clients = 0.0331
  se_facilities = 0.0389
  se_users = 0.1199
  if (service_stats) {
      if (is.na(first_year_observed)) {
        stop("No available contraceptive use data for this run. Service statistics cannot be used.")
      }
      if (!is.null(service_stats_filepath)) {
        ss <- readr::read_csv(service_stats_filepath)
      }
      else {
        ss <- fpemservicestat::service_stats
      }
    format_check(service_stats_format, ss)
    ss <- ss %>% dplyr::arrange(year)
    ss <- ss %>%
      dplyr::filter(division_numeric_code == !! division_numeric_code) %>%
      dplyr::filter(year >= first_year_observed) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(ss_delta = c(NA,diff(emu))) %>%
      dplyr::mutate(ss_year_lag = dplyr::lag(year)) %>%
      dplyr::mutate(ss_se = ifelse(ss_type == "visits", se_visits,
                                    ifelse(ss_type == "clients", se_clients,
                                           ifelse(ss_type == "facilities", se_facilities,  se_users))))
    if (nrow(ss) == 0) {
      stop("No available service stats after filtering. Service stats were filtered based on division numeric code.")
    }

    k_index <- ss$ss_delta %>% is.na %>% `!` %>% which
    list_ss_data <- list(K = k_index %>% length,
                         get_t_k = match(x = floor(ss$year),model_seq_years), # k+1 years and k differences
                         ss_delta_k = ss$ss_delta[k_index],
                         ss_se_k = ss$se_emu[k_index]
                         )
  } else {
    list_ss_data <- NULL
  }
  return(list_ss_data)
}
