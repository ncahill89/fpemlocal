Estimating subnational family planning indicators for married women with
custom user data
================

``` r
library(FPEMlocal)
```

## Introduction

1.  [Fit a one country model](#fit) `fit_fp_c`
2.  [Calculate point estimates for indicators](#results) `calc_fp_c`
3.  [Plot the point estimates against the survey data](#plot)
    `plot_fp_c`

## <a name="fit"></a>

## 1\. Fit a one country model

``` r
fit <- fit_fp_c(
  surveydata_filepath = "data-raw/manuscript_example_data/afghanistan_4_married_example.csv",
  division_numeric_code = 4,
  is_in_union = "Y",
  first_year = 1970,
  last_year = 2030,
  subnational = TRUE
)
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   is_in_union = col_character(),
    ##   age_range = col_character(),
    ##   data_series_type = col_character(),
    ##   group_type_relative_to_baseline = col_character(),
    ##   unmet_need_modern = col_logical(),
    ##   is_pertaining_to_methods_used_since_last_pregnancy = col_character(),
    ##   pertaining_to_methods_used_since_last_pregnancy_reason = col_logical(),
    ##   has_geographical_region_bias = col_character(),
    ##   geographical_region_bias_reason = col_character(),
    ##   has_non_pregnant_and_other_positive_biases = col_character(),
    ##   non_pregnant_and_other_positive_biases_reason = col_logical(),
    ##   age_group_bias = col_character(),
    ##   modern_method_bias = col_character(),
    ##   has_traditional_method_bias = col_character(),
    ##   traditional_method_bias_reason = col_logical(),
    ##   has_absence_of_probing_questions_bias = col_character(),
    ##   record_id = col_character()
    ## )
    ## [36mi[39m Use `spec()` for the full column specifications.

## <a name="results"></a>

## 2\. Calculate point estimates for indicators

``` r
population_data <- read.csv("data-raw/manuscript_example_data/afghanistan_4_married_popdata_example.csv")
results <- calc_fp_c(fit = fit,
                     population_data = population_data)
```

## <a name="plot"></a>

## 3\. Plot estimates and survey data

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

    ## $Y
    ## $Y$unmet_need_any

![](subnational_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    ## 
    ## $Y$contraceptive_use_modern

![](subnational_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

    ## 
    ## $Y$contraceptive_use_traditional

![](subnational_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

    ## 
    ## $Y$contraceptive_use_any

![](subnational_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->
