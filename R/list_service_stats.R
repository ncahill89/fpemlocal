
#' list_service_stats
#'
#' Filter service statistics data
#'
#' @inheritParams do_1country_run
#' @param service_stats_filepath File path to service stat csv
#' @param model_seq_years Years of model estimation from\code{\link{year_sequence_maker}}.
#'
list_service_stats <- function(
  service_stats = FALSE,
  service_stats_filepath,
  model_seq_years,
  division_numeric_code, # we need the unit ID added eventually to use that for filtering
  first_year = NULL # to filter out the data prior to or in year of the most recent survey
)
  # LA 2019/4/3: THIS DOES NOT WORK WHEN THERE ARE MISSING OBS, AND IT DOES NOT WORK WHEN YEARS ARE SKIPPED (THE YEAR MATCHING AT END WILL BE OFF FOR END YEAR OF GREATER THAN 1 YEAR PERIO)

{
  se_visits = 0.0254
  se_clients = 0.0331
  se_facilities = 0.0389
  se_users = 0.1199
  if (service_stats) {
      if (!is.null(service_stats_filepath)) {
        emu_diff <- readr::read_csv(service_stats_filepath)
      }
      else {
        emu_diff <- fpemservicestat::service_stats
      }
    format_check(service_stats_format, emu_diff)
    emu_diff <- emu_diff %>%
      dplyr::filter(division_numeric_code == !! division_numeric_code) %>%
      dplyr::mutate(emu_diff = c(0,diff(emu))) %>%
      dplyr::mutate(year_diff = c(0,diff(year))) %>%
      dplyr::mutate(year_delta_emu_j = (dplyr::lag(year)+year_diff/2)-0.5) %>%
      # note LA 2019/4/3: dplyr lag gives preceding year with NA for first one
      dplyr::mutate(delta_emu_j = emu_diff/year_diff) %>%
      dplyr::select(-c(emu_diff,year_diff)) %>%
      dplyr::mutate(se_emu = ifelse(ss_type == "visits", se_visits,
                                    ifelse(ss_type == "clients", se_clients,
                                           ifelse(ss_type == "facilities", se_facilities,  se_users))))
    if (nrow(emu_diff) == 0) {
      stop("service stats are not available for this country")
    }
    emu <- emu_diff$EMU
    year_emu <- emu_diff$year

    emu_diff_filtered <- emu_diff %>%
      dplyr::filter(year >= first_year) # EMUs after or in year_start are excluded, deltas that end before or in year start are excluded

    check_na <- any(is.na(emu_diff_filtered$delta_emu_j))
    if(check_na) # NA introduced with gap so for first entry (this function does not allow for missing values in the seqence)
    {
      delta_emu <- emu_diff_filtered$delta_emu_j[-1]
      year_delta_emu <- emu_diff_filtered$year_delta_emu_j[-1]
      se_emu <- emu_diff_filtered$se_emu[-1]
    }
    if (!check_na)
    {
      delta_emu <- emu_diff_filtered$delta_emu_j
      year_delta_emu <- emu_diff_filtered$year_delta_emu_j
      se_emu <- emu_diff_filtered$se_emu
    }

    list_ss_data <- list(n_ss = length(delta_emu)+1,
                         get_t_ss = c(match(x = floor(min(year_delta_emu)),model_seq_years),
                                      match(x = floor(year_delta_emu)+1, model_seq_years)),
                         delta_emu = delta_emu,
                         se_emu = se_emu,
                         emu = emu,
                         year_emu = year_emu)
  } else {
    list_ss_data <- NULL
  }
  return(list_ss_data)
}
