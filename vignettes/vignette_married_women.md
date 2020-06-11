Estimating family planning indicators for married women
================

1.  [Fit a one country model](#fit) `fit_fp_c`
2.  [Calculate point estimates for indicators](#results) `calc_fp_c`
3.  [Plot the point estimates against the survey data](#plot)
    `plot_fp_c`

## <a name="fit"></a>

## 1\. Fit a one country model

The primary input to `fit_fp_c` is country-level survey data of
contraceptive use aggregates. When a survey file is not provided, as in
this example, the function uses the default package dataset
`contraceptive_use`.

The country code for the country of interest is necessary to fit the
model. We will refer to these codes as division numeric codes. Division
numeric codes, with their corresponding country names, are found in the
package dataset called `divisions`. Enter `divisions` in the R console
to access this dataset.

``` r
divisions
```

    ## # A tibble: 232 x 13
    ##    division_numeri~ name_country name_region
    ##               <dbl> <chr>        <chr>      
    ##  1                4 Afghanistan  Asia       
    ##  2                8 Albania      Europe     
    ##  3               12 Algeria      Africa     
    ##  4               16 American Sa~ Oceania    
    ##  5               20 Andorra      Europe     
    ##  6               24 Angola       Africa     
    ##  7              660 Anguilla     Latin Amer~
    ##  8               28 Antigua and~ Latin Amer~
    ##  9               32 Argentina    Latin Amer~
    ## 10               51 Armenia      Asia       
    ## # ... with 222 more rows, and 10 more variables:
    ## #   name_sub_region <chr>,
    ## #   region_numeric_code <dbl>,
    ## #   sub_region_numeric_code <dbl>,
    ## #   is_developed_region <chr>,
    ## #   is_less_developed_region <chr>,
    ## #   is_least_developed_country <chr>,
    ## #   is_in_sub_saharan_africa <chr>,
    ## #   is_unmarried_sexual_activity <chr>,
    ## #   is_low_population <chr>, is_fp2020 <chr>

Our package data sets are tibbles. This is particularly useful for large
datasets because it only prints the first few rows. In our example we
will execute a one-country run for Afghanistan, code `4`.

`fit_fp_c` is a wrapper function to run the one-country implementation
of the family planning estimation model. There are two versions of this
model, one for in-union and another for not-in-union women which are
specified with the argument `is_in_union` denoted `"Y"` and `"N"`
respectively. Lastly, specify the years of estimates to be returned.
Note: These arguments will not filter the supplied survey data. All
years of available survey data will be used.

``` r
fit <- fit_fp_c(
  is_in_union = "Y",
  division_numeric_code = 4,
  first_year = 1970,
  last_year = 2030
)
```

## <a name="results"></a>

## 2\. Calculate point estimates for indicators

Calculate point estimates for family planning indicators with the
function `calc_fp_c`. Simply supply the fit object from `fit_fp_c`.
Alternatively, combine steps one and two with pipes.

``` r
results <- calc_fp_c(fit)
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

FPEMcountry also includes a function named `plot_fp_c` to plot the
calculated point estimates against the survey data. The arguments to
this function are, the fit object from step 1, the results from step 2,
and a vector of indicator names. The vector of indicator names
corresponds to the names which appear in the results from step 2.

``` r
plot_fp_c(
  fit,
  results,
  indicators = c(
    "unmet_need_any",
    "contraceptive_use_modern",
    "contraceptive_use_traditional",
    "contraceptive_use_any"
    )
  )
```

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
