indicators_un <- c(
  "unmet",
  "modern",
  "traditional"
)
indicators <- c(
  "unmet_need_any",
  "contraceptive_use_modern",
  "contraceptive_use_traditional",
  "contraceptive_use_any"
)

plots <- fpem_plot_country_results(
  country_results = results_y,
  observations = run_y$core_data$observations,
  first_year = 1970,
  last_year = 2030,
  indicators = indicators
)

global <- global_estimates_married %>%
  dplyr::rename(division_numeric_code = Iso)
for(k in 1:length(indicators_un)) {
  df <- global %>% 
    dplyr::filter(par == indicators_un[k]) %>% 
    process_global(code = div) 
  plots[[k]] <-  plots[[k]] +
    ggplot2::geom_line(aes(x = year, y = `0.5`), data = df, linetype = 'dashed') +
    ggplot2::geom_line(aes(x = year, y = `0.025`), data = df, linetype = 'dashed') +
    ggplot2::geom_line(aes(x = year, y = `0.975`), data = df, linetype = 'dashed')
}
# pdf("bolivia.pdf", 18, 10)
gridExtra::grid.arrange(grobs=plots[1:length(indicators)],
                        ncol=2,
                        top="In-union women")
# dev.off()