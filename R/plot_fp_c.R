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
  observations <- fit %>% purrr::chuck( "core_data", "observations")
  
  if(!is.null(observations) &
     nrow(observations) > 0) {
    observations$subpopulation_labels <- fpem_get_subpopulation_labels(observations)
    observations <- observations %>%
      dplyr::mutate(data_series_type = as.factor(data_series_type)) %>%
      dplyr::mutate(group_type_relative_to_baseline = as.factor(group_type_relative_to_baseline)) %>%
      dplyr::mutate(subpopulation_labels = as.factor(subpopulation_labels))
  }


  # This is a hack to fix downstream plotting errors caused my dplyr::filter, if resulting columns from filter have only NA's the column type becomes "unknown"
  # Changes vector value but not column type
  observations <- observations %>%
    dplyr::mutate_at(.vars = indicators, .funs = as.numeric)
  union <- fit %>% 
    purrr::chuck("core_data", "is_in_union")
  div <- fit %>%
    purrr::chuck("core_data", "units", "division_numeric_code")
  title <- paste0(fit$core_data$units$name_country, ", is_in_union = ",fit$core_data$is_in_union)

  first_year <- fit %>% 
    purrr::chuck("core_data","year_sequence_list", "result_seq_years") %>% 
    min
  last_year <- fit %>% 
    purrr::chuck("core_data","year_sequence_list", "result_seq_years") %>% 
    max
  breaks = seq(
    first_year,
    last_year,
    by = 5
  )
  # colorblind pallet
  cbp2 <- c("#000000",#black 1
            "#E69F00",#orange 2
            "#56B4E9",#lightblue 3
            "#009E73",#green 4
            "#F0E442",#yellow 5
            "#0072B2",#blue 6
            "#D55E00",#red 7
            "#CC79A7")#pink 8

  pl <- list()
  for(indicator in indicators) {
    estimates <- results %>% 
      purrr::pluck(indicator) %>%
      dplyr::mutate(model = "local") %>% 
      tidyr::spread(percentile, value)  %>%
      dplyr::select("year",  "model",   "2.5%", "10%",  "50%",   "90%" ,   "97.5%") %>%
      dplyr::mutate(year = year + .5) %>%
      dplyr::mutate(model = as.factor(model))
    
    if (compare_to_global) {
      glbl_estimates <- FPEMcountry::global_estimates %>%
        dplyr::filter(division_numeric_code == div,
                      is_in_union == union) %>% 
        dplyr::filter(indicator == !!indicator) %>%
        dplyr::select("year",  "model",   "2.5%", "10%",  "50%",   "90%" ,   "97.5%")
      estimates <- rbind(estimates, glbl_estimates) %>%
        dplyr::mutate(model = as.factor(model))
    }

    
    # start with local estimates
    pl[[indicator]] <- estimates %>%
      ggplot2::ggplot(ggplot2::aes(x = year)) +
      ggplot2::ggtitle(title) +
      ggplot2::scale_x_continuous(name = "Year", breaks = breaks) +
      ggplot2::ylab(indicator) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
      ) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`, fill = model), alpha = .2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `10%`, ymax = `90%`, fill = model), alpha = .2) +
      ggplot2::geom_line(ggplot2::aes(y = `50%`, color = model), alpha = .4) +
      ggplot2::scale_fill_manual(values = c(cbp2[3], cbp2[4])) +
      ggplot2::scale_color_manual(values = c(cbp2[3], cbp2[4]))
    
    
    
    #next plot global estimates if they exist for this country
    # if (division_numeric_code %in% global_estimates$division_numeric_code
    #     & compare_to_global
    #     & is_in_union != "ALL"
    #     & indicator != "contraceptive_use_any") {
    #   
    #     # To be revised 7/30/2020 change to global estimate format
    #     # check_estimate(x = global_and_onecountry_estimates$`50%`,
    #     #                y = global_and_onecountry_estimates$`0.5`,
    #     #                division_numeric_code = fit$core_data$units$division_numeric_code,
    #     #                is_in_union = fit$core_data$is_in_union,
    #     #                indicator = indicator)
    #     # end checking
    #   
    #     #plotting code starts here
    #     pl[[indicator]] <- pl[[indicator]] +
    #       ggnewscale::new_scale_color() +
    #       ggplot2::geom_ribbon(ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`), fill = cbp2[3]) +
    #       ggplot2::geom_ribbon(ggplot2::aes(ymin = `10%`, ymax = `90%`), fill = cbp2[3]) +
    #       ggplot2::geom_line(ggplot2::aes(y = `50%`), color = cbp2[3])
    #   } # end global estiamtes

    
    
    # plot observations if they exist
    if(!is.null(observations) &
       nrow(observations) > 0 &
       indicator %in% names(observations)
       ) {
      # low <- paste0("low_", indicator)
      # est <- paste0("est_", indicator)
      # up <- paste0("up_", indicator)
      
      # plotting code starts here
      pl[[indicator]] <- pl[[indicator]] +
        ggnewscale::new_scale_color() +
        ggplot2::geom_point(
          data = observations,
          ggplot2::aes_string(
            x = "ref_date",
            y = indicator,
            color = "data_series_type",
            shape = "group_type_relative_to_baseline"
          ),
          size = 2) #+
        # ggplot2::geom_errorbar(
        #   data = observations,
        #   ggplot2::aes_string(
        #     x = "ref_date",
        #     y = est,
        #     ymax = up,
        #     ymin = low
        #   ),
        #   color = "black",
        #   width = 0,
        #   alpha = 1
        # ) #+
        # ggplot2::scale_color_manual(values = c(cbp2[2], cbp2[1]))
        # subpopulation labels can go here, removing for now since we used up aes
        #   ggplot2::geom_text(
        #     data = observations,
        #     ggplot2::aes_string(
        #       x = "ref_date",
        #       y = indicator,
        #       label = "subpopulation_labels"
        #     ),
        #     size = 3,
        #     hjust = -0.3,
        #     vjust = -0.3
        #   ) #+
        # # ggplot2::labs(color = "Data series/type", shape = "Group") 
    } # end observation plotting
    
  } # end looping through indicators

  return(pl)
} # end function
