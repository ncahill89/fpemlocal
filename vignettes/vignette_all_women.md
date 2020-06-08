Estimates for all women
================

## Introduction

There are two main versions of the family planning estimation model, one
for in-union women and another for not-in-union women. In addition to
this breakdown, there is an interest in obtaining family planning
indicators for all women. This vignette will step through the process of
obtaining point estimates and plots for all women. We can do this with
the same three functions.

1.  [Fit models and obtain samples for all women](#fit) `fit_fp_c`
2.  [Calculate point estimates for indicators](#results)
    `fpet_calculate_indicaotrs`
3.  [Plot the point estimates against the survey data](#plot)
    `plot_fp_c`

## <a name="fit"></a>

## 1\. Fit models and obtain samples for all women

`fit_fp_c` is a wrapper function to run the one-country implementation
of the family planning estimation model. The argument `is_in_union` is
used to specify the union. The function can be used to obtain samples
for all women denoted `"ALL"`. To obtain samples for all women, the two
individual union models are fit and the samples are combined. This
results in a list of three fits. We will demonstrate this below.

``` r
fitlist <- fit_fp_c(
  is_in_union = "ALL",
  division_numeric_code = 4,
  first_year = 1970,
  last_year = 2030
)
```

`fit_fp_c` returns a list of fits.

``` r
fitlist %>% names
```

    ## [1] "fity"   "fitn"   "fitall"

The fit contains posterior samples and another list called `core_data`.

``` r
fitlist$fity %>% names
```

    ## [1] "posterior_samples" "core_data"

Core data contains processed survey data and run specific data such as
the time frame, union, etc.

``` r
fitlist$fity$core_data %>% names
```

    ## [1] "is_in_union"        "units"              "start_year"         "observations"      
    ## [5] "year_sequence_list" "subnational"

## <a name="results"></a>

## 2\. Calculate point estimates for indicators

`calc_fp_c` is a wrapper function for calculating point estimates and
confidence intervals. By default the function uses package population
data (See `population_counts`) in order to calculate family planning
indicators. Custom population count data may be supplied (See
`??fpet_get_results`).

`calc_fp_c` utilizes `pmap` from the tidyverse package purr allowing it
to act on any number of fits. We will supply the entire list of fits
from `fit_fp_c`.

``` r
resultlist <- calc_fp_c(fitlist)
```

Like the previous function, `calc_fp_c` returns a list. Since we
supplied three fits the function returns three sets of calculated family
planning indicators.

``` r
resultlist %>% names
```

    ## [1] "fity"   "fitn"   "fitall"

A set of results here consist of the following family planning
indicators

``` r
resultlist$fitall %>% names
```

    ##  [1] "contraceptive_use_any"                      "contraceptive_use_modern"                  
    ##  [3] "contraceptive_use_traditional"              "non_use"                                   
    ##  [5] "unmet_need_any"                             "unmet_need_modern"                         
    ##  [7] "demand"                                     "demand_modern"                             
    ##  [9] "demand_satisfied"                           "demand_satisfied_modern"                   
    ## [11] "no_need"                                    "contraceptive_use_any_population_counts"   
    ## [13] "contraceptive_use_modern_population_counts" "traditional_cpr_population_counts"         
    ## [15] "non_use_population_counts"                  "unmet_need_population_counts"              
    ## [17] "unmet_need_modern_population_counts"        "demand_modern_population_counts"           
    ## [19] "demand_population_counts"                   "demand_satisfied_population_counts"        
    ## [21] "demand_satisfied_modern_population_counts"  "no_need_population_counts"

The point estimates for each indicator are long-format tibbles. Let’s
take a look at the tibble for the indicator `contraceptive_use_modern`

``` r
resultlist$fitall$contraceptive_use_modern
```

    ## # A tibble: 488 x 3
    ##     year percentile  value
    ##    <int> <chr>       <dbl>
    ##  1  1970 mean       0.0116
    ##  2  1971 mean       0.0123
    ##  3  1972 mean       0.0130
    ##  4  1973 mean       0.0138
    ##  5  1974 mean       0.0147
    ##  6  1975 mean       0.0156
    ##  7  1976 mean       0.0167
    ##  8  1977 mean       0.0179
    ##  9  1978 mean       0.0191
    ## 10  1979 mean       0.0205
    ## # ... with 478 more rows

## <a name="plot"></a>

## 3\. Plot the point estimates against the survey data

`plot_fp_c` plots the indicators we obtained from the model against the
indicators in the survey data. This function also handles lists. We will
supply the fit list and the results list to the function to obtain plots
for all three fits. We must also specify which indicators we wish to
plot. The function will return a plot per indicator. Indicators of
interest are supplied to the argument `indicators` as a vector.

``` r
plot_fp_c(
  fitlist,
  resultlist,
  indicators = c(
    "unmet_need_any",
    "contraceptive_use_modern",
    "contraceptive_use_traditional",
    "contraceptive_use_any"
    )
  )
```

    ## list()
