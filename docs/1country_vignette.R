## ---- echo=FALSE---------------------------------------------------------
options(warn=-1)

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width=10, fig.height=7) 
  library(fpemdata)
  library(fpemdata)
  library(fpemmodeling)
  library(ggplot2)
  library(grid)
  library(gridExtra)

## ------------------------------------------------------------------------
run_y <- fpemmodeling::do_1country_run(
  is_in_union = "Y",
  surveydata_filepath = NULL,
  service_stats = FALSE,
  division_numeric_code = 400,
  first_year = 1989,
  last_year = 2030
)
run_n <- fpemmodeling::do_1country_run(
  is_in_union = "N",
  surveydata_filepath = NULL,
  service_stats = FALSE,
  division_numeric_code = 400,
  first_year = 1989,
  last_year = 2030
)
core_data <- run_y$core_data
observations_y <- run_y$core_data$observations
observations_n <- run_n$core_data$observations
samples_all <- fpemmodeling::posterior_samples_all_women(in_union_posterior_samples = run_y$posterior_samples, 
                                                         not_in_union_posterior_samples = run_n$posterior_samples, 
                                                         core_data = core_data)

## ------------------------------------------------------------------------
 population_counts <- fpemdata::population_counts %>%
      dplyr::filter(division_numeric_code == core_data$units$division_numeric_code)
  results_y <- fpemreporting::fpem_calculate_results(
    posterior_samples = run_y$posterior_samples,
    country_population_counts = population_counts %>%
      dplyr::filter(is_in_union == "Y"),
    first_year = min(core_data$time_frame$limits())
  )
  results_n <- fpemreporting::fpem_calculate_results(
    posterior_samples = run_n$posterior_samples,
    country_population_counts = population_counts %>%
      dplyr::filter(is_in_union == "N"),
    first_year = min(core_data$time_frame$limits())
  )
  results_all <- fpemreporting::fpem_calculate_results(
    posterior_samples = samples_all,
    country_population_counts = population_counts,
    first_year = min(core_data$time_frame$limits())
  )

## ------------------------------------------------------------------------
indicators <- c(
    "unmet_need_any",
    "contraceptive_use_modern",
    "contraceptive_use_traditional",
    "contraceptive_use_any"
    )
plots <- fpemreporting::fpem_plot_country_results(
  country_results = results_y,
  observations = observations_y,
  first_year = core_data$time_frame$`.->.sequence` %>% min,
  last_year = core_data$time_frame$`.->.sequence` %>% max,
  is_in_union = "Y",
  indicators = indicators
  )
gridExtra::grid.arrange(grobs=plots[1:length(indicators)],
                 ncol=2,
                 top=textGrob("In-union women"))  

plots <- fpemreporting::fpem_plot_country_results(
  country_results = results_n,
  observations = observations_n,
  first_year = core_data$time_frame$`.->.sequence` %>% min,
  last_year = core_data$time_frame$`.->.sequence` %>% max,
  is_in_union = "N",
  indicators = indicators
  )
gridExtra::grid.arrange(grobs=plots[1:length(indicators)],
                 ncol=2,
                 top=textGrob("Not-in-union women"))  

plots <- fpemreporting::fpem_plot_country_results(
  country_results = results_all,
  observations = rbind(observations_n, observations_y),
  first_year = core_data$time_frame$`.->.sequence` %>% min,
  last_year = core_data$time_frame$`.->.sequence` %>% max,
  indicators = indicators
  )
gridExtra::grid.arrange(grobs=plots[1:length(indicators)],
                 ncol=2,
                 top=textGrob("All women"))       

