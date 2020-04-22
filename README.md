FPEM
================

## Table of Contents

1.  [Introduction](#intro)
2.  [Run models](#run)
3.  [Post-process](#post-process)
4.  [Plot model results](#plot)

## <a name="intro"></a>

## Introduction

This package is the one country implementation of FPEM. It uses higher
level parameters estimated in FPEMglobal. The package can be installed
by cloning and using `devtools::install()`. The source code for
vignettes can be found in
[/vignettes](https://github.com/FPRgroup/FPEMcountry/tree/master/vignettes).
Knitted versions of vignettes can be found in
[/docs](https://github.com/FPRgroup/FPEMcountry/tree/master/docs). The
directory [/design](https://github.com/FPRgroup/FPEM/tree/master/design)
contains technical details about the design of this project for package
maintainers and contributors. Below is a brief introduction.

## <a name="run"></a>

## Run models

`do_1country_run` is a wrapper function to run the family planning
estimation model for a country of interest for either married or
unmarried women. When a survey file is not provided (as in this example)
the function uses default data from `contraceptive_use`. The user may
also supply optional services statistics. See `??service_stats` for
required service statistic data format. See `??do_1country_run` for all
possible inputs to this wrapper function. If you wish to obtain results
for all women `posterior_samples_all_women` can be used after completing
a run for married women and a run for unmarried women. See
[FPEM/vignettes](https://github.com/FPRgroup/FPEMcountry/vignettes) for
more details

``` r
div <- 68 #64 bhutan #728 sudan
run_y <- do_1country_run(
  is_in_union = "Y",
  surveydata_filepath = NULL,
  service_stats = FALSE,
  division_numeric_code = div,
  first_year = 1975,
  last_year = 2030
)
```

## <a name="post-process"></a>

## Process the samples

`fpem_calculate_results` returns point-estimates for several indicators
in long-format.

``` r
population_counts <- population_counts %>%
  dplyr::filter(division_numeric_code == run_y$core_data$units$division_numeric_code) %>%
  dplyr::filter(is_in_union == "Y")
results_y <- fpemreporting::fpem_calculate_results(
  posterior_samples = run_y$posterior_samples,
  country_population_counts = population_counts,
  first_year = run_y$core_data$year_sequence_list$result_seq_years %>% min()
)
```

## <a name="plot"></a>

## Plot the results

`fpem_plot_country_results` plots the results of the model againts the
observations. Choose any indicators returned from
`fpem_calculate_results`

``` r
indicators <- c(
    "unmet_need_any",
    "contraceptive_use_modern",
    "contraceptive_use_traditional",
    "contraceptive_use_any"
    )

plots <- fpemreporting::fpem_plot_country_results(
  country_results = results_y,
  observations = run_y$core_data$observations,
  first_year = 1970,
  last_year = 2030,
  indicators = indicators
  )


gridExtra::grid.arrange(grobs=plots[1:length(indicators)],
                 ncol=2,
                 top="In-union women")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
