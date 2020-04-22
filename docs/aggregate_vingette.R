## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width=10, fig.height=7) 
  library(fpemdata)
  library(fpemmodeling)
  library(fpemreporting)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(rjags)
  library(R2jags)

## ---- echo=FALSE---------------------------------------------------------
options(warn=-1)

## ------------------------------------------------------------------------
## Example codes for countries in Central Asia: Kazakhstan, Kyrgyzstan, Tajikistan, Turkmenistan, Uzbekistan respectively	
codes <- c(398,417,762,795,860)

## ------------------------------------------------------------------------
for(code in codes)
{
  post_samps <- fpemmodeling::do_1country_run(
    is_in_union = "Y",
    surveydata_filepath = NULL,
    division_numeric_code = code,
    first_year = 1989,
    last_year = 2030
  )$posterior_samples
  save(post_samps, file = paste0("post_samps_", code, ".rda"))
}

## ------------------------------------------------------------------------
## COMBINE RUNS
post_samps_combine <- fpemreporting::combine_runs(codes = codes)

## ------------------------------------------------------------------------
## Get population data
population_data <- fpemdata::population_counts %>%
 dplyr::filter(is_in_union == "Y") %>%
   dplyr::filter(mid_year <= 2030) %>%
   dplyr::filter(mid_year >= 1989) %>% 
   dplyr::filter(division_numeric_code %in% codes) %>% 
   dplyr::group_by(division_numeric_code,mid_year) %>%
   dplyr::top_n(1) %>% # years are doubled for some reason so this choses the correct ones
   dplyr::ungroup()

## Get divisions data
division_level_data <- fpemdata::divisions %>%
   dplyr::mutate(division_level = region_numeric_code)%>%
   dplyr::select(division_numeric_code, division_level) %>% 
   dplyr::filter(division_numeric_code %in% codes)

## Create the array with the weigthed posterior samples i.e., aggregate
# undebug(weight_samples)
# # debug(fpemreporting:::weight_division_match)
# debug(weight_generator)
posterior_samples_list <- fpemreporting::weight_samples(division_level_data, 
                                         population_data, 
                                         posterior_samples =  post_samps_combine)
## Pull out the aggregate samples, in this case we aggregated for a sinlge region
samps_central_asia <- posterior_samples_list$`935`


## ------------------------------------------------------------------------
## create results data 
results_central_asia <- fpemreporting::fpem_calculate_results(
  posterior_samples = samps_central_asia,
  first_year = 1989,
  country_population_counts = population_data)

## ----  echo=FALSE--------------------------------------------------------

indicators <- fpemreporting:::indicator_names()
plots <- fpemreporting::fpem_plot_country_results(
  country_results = results_central_asia,
  first_year = 1989,
  last_year = 2030,
  is_in_union = "Y",
  indicators = indicators
  )
gridExtra::grid.arrange(grobs=plots[1:length(indicators)],
                 ncol=2,
                 top=textGrob("In-union women"))  

## ------------------------------------------------------------------------
#remove sample files created for this vignettes
file.remove(paste0("post_samps_", codes, ".rda"))

