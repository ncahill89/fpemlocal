Aggregate estimates for multiple fits
================

## Introduction

In this vignette we will fit FPET to multiple countries and aggregate
the samples to obtain results for aggregate levels.

## Table of Contents

1.  [Fit models](#fit)
2.  [Aggregate](#run)
3.  [Calculate results](#results)

## <a name="fit"></a>

## Fit models

Let’s task ourselves with obtaining results for each sub-region within
Africa.

``` r
divisions %>%
  dplyr::filter(name_region == "Africa") %>%
  dplyr::select(sub_region_numeric_code, name_sub_region) %>%
  unique()
#> # A tibble: 5 x 2
#>   sub_region_numeric_code name_sub_region
#>                     <dbl> <chr>          
#> 1                     912 Northern Africa
#> 2                     911 Middle Africa  
#> 3                     914 Western Africa 
#> 4                     913 Southern Africa
#> 5                     910 Eastern Africa
```

First, we need fits for all countries in Africa. Start by constructing a
list of country codes.

``` r
divs <- divisions %>%
  dplyr::filter(name_region == "Africa") %>%
  dplyr::filter(division_numeric_code %in% contraceptive_use$division_numeric_code) %>% # Filtering on the countries available in `contraceptive_use` because we are using this package data
  dplyr::pull(division_numeric_code) %>%
  as.list() 
```

Next, map the country codes to the function `fpet_fit_model`.

``` r
first_year <- 1970
last_year <- 2030
union <- "Y"
fits <- purrr::pmap(list(division_numeric_code = divs, 
                         is_in_union = union,
                         first_year = first_year, 
                         last_year = last_year
                         ), 
                    fpet_fit_model)
```

## <a name="aggregate"></a>

## Aggregate

Within out list of fits we have more complex lists. We only need the
samples from these fits. This function plucks the posterior samples and
combines each array of samples along index 1 making one large array. The
first index of this large array corresponds to the country of the
samples. Use `dimnames()` on the resulting list to see how to index a
country. This is the identical format of a posterior samples array from
FPEMglobal.

``` r
combined_samples <- combine_samples_from_fits(fits, divs)
```

To aggregate these samples we need to construct two tibbles. First,
create a tibble with the aggregation level and corresponding division
codes. The aggregation level column must be named division\_level and
contain division codes. In this example we will obtain aggregate results
for all sub-regions in Africa. Everything we need for our division level
column is in the column sub\_region\_numeric\_code of the data set
`division`.

``` r
## Get divisions data
division_level_data <- divisions %>%
   dplyr::mutate(division_level = sub_region_numeric_code)%>%
   dplyr::select(division_numeric_code, division_level) %>% 
   dplyr::filter(division_numeric_code %in% divs)
division_level_data
#> # A tibble: 54 x 2
#>    division_numeric_code division_level
#>                    <dbl>          <dbl>
#>  1                    12            912
#>  2                    24            911
#>  3                   204            914
#>  4                    72            913
#>  5                   854            914
#>  6                   108            910
#>  7                   132            914
#>  8                   120            911
#>  9                   140            911
#> 10                   148            911
#> # ... with 44 more rows
```

Lastly, we need the population counts for these countries within the
specified years.

``` r
## Get population data
population_data <- population_counts %>%
  dplyr::filter(division_numeric_code %in% divs) %>%
  dplyr::filter(is_in_union == union) %>%
  dplyr::filter(mid_year >= first_year) %>%
  dplyr::filter(mid_year <= last_year) 
population_data
#> # A tibble: 3,294 x 5
#>    is_in_union division_numeri~
#>    <chr>                  <dbl>
#>  1 Y                         12
#>  2 Y                         24
#>  3 Y                         72
#>  4 Y                        108
#>  5 Y                        120
#>  6 Y                        132
#>  7 Y                        140
#>  8 Y                        148
#>  9 Y                        174
#> 10 Y                        178
#> # ... with 3,284 more rows, and 3 more
#> #   variables: population_count <dbl>,
#> #   age_range <chr>, mid_year <dbl>
```

Now supply these two tibbles of data and the large array of posterior
samples to the function `fpem_aggregate`. This function returns a named
list of aggregated samples.

``` r
posterior_samples_list <- fpem_aggregate(division_level_data, 
                                         population_data, 
                                         posterior_samples =  combined_samples)
#> Joining, by = "division_numeric_code"
names(posterior_samples_list)
#> NULL
```

## <a name="results"></a>

## Calculate results

We can map the list of samples to `fpem_calculate_results` to obtain
results for each sub-region

``` r
results_list <- purrr::pmap(list(posterior_samples = posterior_samples_list,
                            country_population_counts = list(population_data), #need to wrap any complex inputs in another list()
                            first_year = first_year),
                       fpem_calculate_results)
results_list$`912`$contraceptive_use_modern
#> NULL
```
