#' plot country results
#'
#' @inherit plot_fp_csub
#' @export
plot_fp_c_autosave <- function(runname,
                               ...) {
  fit <- readRDS(file.path("output/runs", paste0(runname, ".rds")))
  results <- readRDS(file.path("output/results", paste0(runname, ".rds")))
  plotlist <- fpem_plot(
    fit = fit,
    results = results,
    ...
  )
  if (!dir.exists("output")) dir.create("output")
  if (!dir.exists("output/plots")) dir.create("output/plots")
  pathout <- file.path("output/plots", paste0(runname, ".pdf"))
  pdf(pathout, 18, 10)
  for (i in 1:length(plotlist)) {
    plots <- plotlist[[i]]
    gridExtra::grid.arrange(
      grobs = plots[1:length(indicators)],
      ncol = 2,
      top = paste(fit[[i]]$core_data$is_in_union, fit[[i]]$core_data$units$name_country)
    )
  }
  dev.off()
  print(paste0("Your file was saved to ", pathout))
}


#' plot country results
#'
#' @inherit plot_fp_csub
#' @export
plot_fp_c <- function(
  fit,
  results,
  indicators,
  compare_to_global = FALSE
) {
  purrr::pmap(list(fit, results, list(indicators), compare_to_global), plot_fp_csub)
}


#' plot country results
#'
#' @param results \emph{'Data.frame'} Results data from \code{\link{fpem_calculate_results}}
#' @param core_data data list from \code{\link{core_data}}
#' @param indicators name of indicators from results to be plotted
#' @param compare_to_global logical, if TRUE plots estimates from global model with dotted lines
#' @return list of plots
#' @export
plot_fp_csub <- function(
  fit,
  results,
  indicators,
  compare_to_global = FALSE
) {
  indicators <- indicators %>% unlist()
  observations <- fit$core_data$observations
  # This is a hack to fix downstream plotting errors caused my dplyr::filter, if resulting columns from filter have only NA's the column type becomes "unknown"
  # Changes vector value but not column type
  observations <- observations %>%
    dplyr::mutate_at(.vars = indicators, .funs = as.numeric)
  first_year <- fit$core_data$year_sequence_list$result_seq_years %>% min()
  last_year <- fit$core_data$year_sequence_list$result_seq_years %>% max()
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
  if (fit$core_data$units$division_numeric_code %in% global_estimates$division_numeric_code
      & compare_to_global
      & fit$core_data$is_in_union != "ALL") {
    global_estimates <- global_estimates %>%
      dplyr::filter(division_numeric_code == fit$core_data$units$division_numeric_code,
                    is_in_union == fit$core_data$is_in_union)

    for(indicator in indicators[1:3]) { # hack since we only have the first three
      global_estimates_filt <- global_estimates %>%
        dplyr::filter(indicator == !!indicator)

      # compare global and on country estimates for a particular indicator for one country one union
      res <- results[[indicator]] %>%
        dplyr::mutate(year = year + .5) %>%
        tidyr::spread(percentile, value)
      global_and_onecountry_estimates <- dplyr::left_join(res, global_estimates_filt)
      check_estimate(x = global_and_onecountry_estimates$`50%`,
                     y = global_and_onecountry_estimates$`0.5`,
                     division_numeric_code = fit$core_data$units$division_numeric_code,
                     is_in_union = fit$core_data$is_in_union,
                     indicator = indicator)
      # end checking

      pl[[indicator]] <- pl[[indicator]] +
        ggplot2::geom_line(ggplot2::aes(x = year, y = `0.5`), data = global_estimates_filt, linetype = 'dashed') +
        ggplot2::geom_line(ggplot2::aes(x = year, y = `0.025`), data = global_estimates_filt, linetype = 'dashed') +
        ggplot2::geom_line(ggplot2::aes(x = year, y = `0.975`), data = global_estimates_filt, linetype = 'dashed')
    }
  }

  return(pl)
}
