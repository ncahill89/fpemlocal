

#' plot country results
#' 
#' @inheritParams do_1country_run
#' @param country_results \emph{'Data.frame'} Results data from \code{\link{fpem_calculate_results}}
#' @param core_data data list from \code{\link{core_data}}
#' @param indicators name of indicators from results to be plotted
#'
#' @return list of plots
#' @export
fpem_plot_country_results <- function(
  country_results,
  core_data,
  indicators
) {
  observations <- core_data$observations
  first_year <- core_data$year_sequence_list$result_seq_years %>% min()
  last_year <- core_data$year_sequence_list$result_seq_years %>% max()
  y_label = "Proportion"
  breaks = seq(
    first_year,
    last_year,
    by = 5
  )
  # breaks = NULL
  # if (is.null(breaks)) {
  #   breaks <- ggplot2::waiver()
  # }
  pl <- list()
  for(indicator in indicators) {
    country_results_j <- country_results[[indicator]]
    pl[[indicator]] <- tidyr::spread(country_results_j, key = percentile, value = value) %>%
      ggplot2::ggplot(ggplot2::aes(x = year)) +
      ggplot2::ggtitle(indicator) +
      ggplot2::scale_x_continuous(name = "Year", breaks = breaks) +
      ggplot2::ylab(y_label) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
      ) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`), fill = "plum1") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `10%`, ymax = `90%`), fill = "plum") +
      ggplot2::geom_line(ggplot2::aes(y = `50%`), color = "black")
    if(!is.null(observations) &
       nrow(observations) > 0 &
       indicator %in% names(observations)
       ) {
      observations$subpopulation_labels <- fpem_get_subpopulation_labels(observations)
      pl[[indicator]] <- pl[[indicator]] + ggplot2::geom_point(
        data = observations,
        ggplot2::aes_string(
          x = "ref_date",
          y = indicator,
          color = "data_series_type",
          shape = "group_type_relative_to_baseline"
        ),
        size = 2) +
        ggplot2::geom_text(
          data = observations,
          ggplot2::aes_string(
            x = "ref_date",
            y = indicator,
            label = "subpopulation_labels"
          ),
          size = 3,
          hjust = -0.3,
          vjust = -0.3
        ) +
        ggplot2::labs(color = "Data series/type", shape = "Group")
    }
  }
  return(list(pl[[1]], pl[[2]], pl[[3]], pl[[4]]))
}



#' Reformat UNPD global data to long format
#'
#'
#' @param data emph{'Data.frame'} Estimates for modern/trad/unmet obtained from posterior of global model in wide format
#' @param code emph{'Numeric'} A country code to filter on since we only run one country at a time on our end
#' @export
#'
#' @return emph{'Data.frame} A long format data of specific country to be plotted
#'
#' @examples get_global_estimates_married() %>% filter(par =="unmet") %>% process_global(code)
process_global <- function(data, code) {
  data %>%
    dplyr::filter(division_numeric_code == !! code) %>%
    dplyr::select(division_numeric_code, Percentile,  "1975.5":"2020.5") %>%
    tidyr::gather(year, value, "1975.5":"2020.5") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    tidyr::spread(Percentile, value)
}
