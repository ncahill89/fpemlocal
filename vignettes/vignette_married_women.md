Estimates for married women
================

## Installation

The package can be installed by cloning and using `devtools::install()`.
The source code for vignettes can be found in
[/vignettes](https://github.com/FPcounts/FPEMcountry/tree/master/vignettes).
Below is a brief introduction.

## Introduction

The FPEMcountry package is the one-country implementation of FPEM
(family planning estimation model) designed with tidyverse philosophy.
The model in this package uses global model results from the package
FPEMglobal to aid in the estimation of country level family planning
indicators. FPEMcountry comes equiped with survey data, country unit
data, and country population count data, to produce one-country runs.
Running FPEM is divided into three main functions.

1.  [Fit a one country model](#fit) `fpet_fit_model`
2.  [Calculate point estimates for indicators](#results)
    `fpet_calculate_indicaotrs`
3.  [Plot the point estimates against the survey data](#plot)
    `fpet_plot`

These three functions make running one country FPEM straightforward,
while retaining enough division to carry out a variety of developer and
client tasks. In this document we will cover the typical use of these
three functions.

To start a run we need to know the country code for the country of
interest. Our package contains country codes and other country units in
the dataset `divisions`.

``` r
divisions
```

    ## # A tibble: 232 x 13
    ##    division_numeri~ name_country
    ##               <dbl> <chr>       
    ##  1                4 Afghanistan 
    ##  2                8 Albania     
    ##  3               12 Algeria     
    ##  4               16 American Sa~
    ##  5               20 Andorra     
    ##  6               24 Angola      
    ##  7              660 Anguilla    
    ##  8               28 Antigua and~
    ##  9               32 Argentina   
    ## 10               51 Armenia     
    ## # ... with 222 more rows, and 11 more
    ## #   variables: name_region <chr>,
    ## #   name_sub_region <chr>,
    ## #   region_numeric_code <dbl>,
    ## #   sub_region_numeric_code <dbl>,
    ## #   is_developed_region <chr>,
    ## #   is_less_developed_region <chr>,
    ## #   is_least_developed_country <chr>,
    ## #   is_in_sub_saharan_africa <chr>,
    ## #   is_unmarried_sexual_activity <chr>,
    ## #   is_low_population <chr>,
    ## #   is_fp2020 <chr>

Our package data sets are tibbles. This is particularly useful for large
datasets because it only prints the first few rows. The country codes
used by our package, known as `division_numeric_code`, are found in this
data. In our example we will execute a one-country run for Afghanistan,
code `4`. Survey data is available in the dataset `contraceptive_use`.
See `??contraceptive_use` for a detailed description of this dataset.

## <a name="fit"></a>

## 1\. Fit a one country model

`fpet_fit_model` is a wrapper function to run the one-country
implementation of the family planning estimation model. There are two
versions of this model, one for in-union and another for not-in-union
women which can be specified with the argument `is_in_union`. These are
denoted `"Y"` and `"N"` respectively. The first\_year and last\_year
arguments determine the years of estimates exported from the run.
Regardless of these arguments, the function will use all years in which
data is available for estimation. When a survey file is not provided, as
in this example, the function uses default package contraceptive\_use.
The user may also supply optional services statistics.

``` r
fit <- fpet_fit_model(
  is_in_union = "Y",
  division_numeric_code = 4,
  first_year = 1970,
  last_year = 2030
)
```

## <a name="results"></a>

## 2\. Calculate point estimates for indicators

`fpet_calculate_indicators` is a wrapper function for calculating point
estimates and confidence intervals. By default the function uses package
population data (See `population_counts`) in order to calculate family
planning indicators. Custom population count data may be supplied (See
`??fpet_get_results`).

``` r
results <- fpet_calculate_indicators(fit)
```

A set of results here consist of the following family planning
indicators

``` r
results$fit %>% names
```

    ##  [1] "contraceptive_use_any"                     
    ##  [2] "contraceptive_use_modern"                  
    ##  [3] "contraceptive_use_traditional"             
    ##  [4] "non_use"                                   
    ##  [5] "unmet_need_any"                            
    ##  [6] "unmet_need_modern"                         
    ##  [7] "demand"                                    
    ##  [8] "demand_modern"                             
    ##  [9] "demand_satisfied"                          
    ## [10] "demand_satisfied_modern"                   
    ## [11] "no_need"                                   
    ## [12] "contraceptive_use_any_population_counts"   
    ## [13] "contraceptive_use_modern_population_counts"
    ## [14] "traditional_cpr_population_counts"         
    ## [15] "non_use_population_counts"                 
    ## [16] "unmet_need_population_counts"              
    ## [17] "unmet_need_modern_population_counts"       
    ## [18] "demand_modern_population_counts"           
    ## [19] "demand_population_counts"                  
    ## [20] "demand_satisfied_population_counts"        
    ## [21] "demand_satisfied_modern_population_counts" 
    ## [22] "no_need_population_counts"

The point estimates for each indicator are long-format tibbles. Let’s
take a look at the tibble for the indicator `contraceptive_use_modern`

``` r
results$fit$contraceptive_use_modern
```

    ## # A tibble: 488 x 3
    ##     year percentile  value
    ##    <int> <chr>       <dbl>
    ##  1  1970 mean       0.0115
    ##  2  1971 mean       0.0123
    ##  3  1972 mean       0.0131
    ##  4  1973 mean       0.0141
    ##  5  1974 mean       0.0152
    ##  6  1975 mean       0.0164
    ##  7  1976 mean       0.0178
    ##  8  1977 mean       0.0192
    ##  9  1978 mean       0.0208
    ## 10  1979 mean       0.0225
    ## # ... with 478 more rows

## <a name="plot"></a>

## 3\. Plot the point estimates against the survey data

`fpet_plot` plots the results of the model against the survey data. The
user supplies the objects exported from `fpet_fit_model` and
`fpet_calculate_indicators` as well as indicators of interest.
Indicators of interest are supplied to the argument `indicators`. The
argument `compare_to_global` adds point estimate and 95% credible
interval from the UNPD global model (See `global_estimates`). The global
model estimates are plotted using dotted lines. Since we are only using
the default data from UNPD the estimates from our model should align
with the UNPD estimates.

``` r
fpet_plot(
  fit,
  results,
  indicators = c(
    "unmet_need_any",
    "contraceptive_use_modern",
    "contraceptive_use_traditional",
    "contraceptive_use_any"
    ),
  compare_to_global = TRUE
  )
```

    ## Joining, by = "year"
    ## Joining, by = "year"
    ## Joining, by = "year"

    ## $fit
    ## $fit[[1]]

![](vignette_married_women_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## 
    ## $fit[[2]]

![](vignette_married_women_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

    ## 
    ## $fit[[3]]

![](vignette_married_women_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

    ## 
    ## $fit[[4]]

![](vignette_married_women_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->
