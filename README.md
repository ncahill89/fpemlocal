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
[/vignettes](https://github.com/FPcounts/FPEMcountry/tree/master/vignettes).
The directory
[/design](https://github.com/FPcounts/FPEM/tree/master/design) contains
technical details about the design of this project for package
maintainers and contributors. Below is a brief introduction.

## <a name="run"></a>

## Run models

`fpem_one_country` is a generic wrapper function to run the family
planning estimation model for a country of interest. The wrapper
functions ending in `_autosave` such as `fpem_one_country_autosave` save
the output and name the files using the runname argument. We will use
the division code for Bolivia here as the runname.

When a survey file is not provided (as in this example) the function
uses default data from `contraceptive_use`. The user may also supply
optional services statistics. See `??service_stats` for required service
statistic data format. See `??do_1country_run` for all possible inputs
to this wrapper function. If you wish to obtain results for all women
`posterior_samples_all_women` can be used after completing a run for
married women and a run for unmarried women. See
[FPEM/vignettes](https://github.com/FPcounts/FPEMcountry/vignettes) for
more details

``` r
div <- 68
fpem_one_country_autosave(
  runname = paste0(div),
  is_in_union = "ALL",
  service_stats = FALSE,
  division_numeric_code = div,
  first_year = 1970,
  last_year = 2030
)
```

## <a name="post-process"></a>

## Process the samples

`fpem_get_results` returns point-estimates for several indicators in
long-format.

``` r
fpem_get_results_autosave(
  runname = paste0(div)
)
```

## <a name="plot"></a>

## Plot the results

`fpem_get_plots` plots the results of the model againts the
observations. Choose any indicators returned from
`fpem_calculate_results`

``` r
indicators <- c(
    "unmet_need_any",
    "contraceptive_use_modern",
    "contraceptive_use_traditional",
    "contraceptive_use_any"
    )

fpem_get_plots_autosave(
  runname = paste0(div),
  indicators = indicators,
  compare_to_global = TRUE
  )
```

    ## png 
    ##   2
