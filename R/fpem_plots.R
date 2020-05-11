

#' plot country results
#' 
#' @param results \emph{'Data.frame'} Results data from \code{\link{fpem_calculate_results}}
#' @param core_data data list from \code{\link{core_data}}
#' @param indicators name of indicators from results to be plotted
#' @param compare_to_global logical, if TRUE plots estimates from global model with dotted lines
#' @return list of plots
#' @export
fpem_plots <- function(
  runlist,
  results,
  indicators,
  compare_to_global = FALSE
) {
  observations <- runlist$core_data$observations
  first_year <- runlist$core_data$year_sequence_list$result_seq_years %>% min()
  last_year <- runlist$core_data$year_sequence_list$result_seq_years %>% max()
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
    results_j <- results[[indicator]]
    pl[[indicator]] <- tidyr::spread(results_j, key = percentile, value = value) %>%
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
  
    # plot observations if they exist
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
  if (runlist$core_data$units$division_numeric_code %in% global_estimates$division_numeric_code &
      compare_to_global) {
    global_estimates <- global_estimates %>%
      dplyr::filter(division_numeric_code == runlist$core_data$units$division_numeric_code,
                    is_in_union == runlist$core_data$is_in_union)
    for(indicator in indicators[1:3]) { # hack since we only have the first three
      global_estimates_filt <- global_estimates %>%
        dplyr::filter(indicator == !!indicator)
      pl[[indicator]] <- pl[[indicator]] +
        ggplot2::geom_line(ggplot2::aes(x = year, y = `0.5`), data = global_estimates_filt, linetype = 'dashed') +
        ggplot2::geom_line(ggplot2::aes(x = year, y = `0.025`), data = global_estimates_filt, linetype = 'dashed') +
        ggplot2::geom_line(ggplot2::aes(x = year, y = `0.975`), data = global_estimates_filt, linetype = 'dashed')
    }
  }
  
  return(list(pl[[1]], pl[[2]], pl[[3]], pl[[4]]))
}



