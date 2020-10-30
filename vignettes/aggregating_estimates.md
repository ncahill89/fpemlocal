Aggregating estimates from multiple fits with custom user data
================

## Introduction

In this vignette we will fit FPET to multiple countries and aggregate
the samples to obtain results for aggregate levels. We will fit models
for Botswana and Lesotho, country codes 72 and 426 respectively.

## Table of Contents

1.  [Fit models](#fit)
2.  [Read in population data](#pop)4
3.  [Calculate results](#results)

## <a name="fit"></a>

## Fit models

First, fit the models with the function `fit_fp_c`.

``` r
fit_botswana <- fit_fp_c(
  surveydata_filepath = "data-raw/manuscript_example_data/Botswana_72_married_example.csv",
  division_numeric_code = 72,
  is_in_union = "Y",
  first_year = 1970,
  last_year = 2030
)
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_character(),
    ##   division_numeric_code = col_double(),
    ##   start_date = col_double(),
    ##   end_date = col_double(),
    ##   contraceptive_use_modern = col_double(),
    ##   contraceptive_use_traditional = col_double(),
    ##   contraceptive_use_any = col_double(),
    ##   unmet_need_modern = col_logical(),
    ##   unmet_need_any = col_double(),
    ##   pertaining_to_methods_used_since_last_pregnancy_reason = col_logical(),
    ##   geographical_region_bias_reason = col_logical(),
    ##   non_pregnant_and_other_positive_biases_reason = col_logical(),
    ##   traditional_method_bias_reason = col_logical(),
    ##   se_modern = col_logical(),
    ##   se_traditional = col_logical(),
    ##   se_unmet_need = col_logical(),
    ##   se_log_r_modern_no_use = col_logical(),
    ##   se_log_r_traditional_no_use = col_logical(),
    ##   se_log_r_unmet_no_need = col_logical(),
    ##   source_id = col_double(),
    ##   se_log_r_unmet_no_need_imputed = col_double()
    ##   # ... with 2 more columns
    ## )
    ## [36mi[39m Use `spec()` for the full column specifications.

``` r
fit_lesotho <- fit_fp_c(
  surveydata_filepath = "data-raw/manuscript_example_data/Lesotho_426_married_example.csv",
  division_numeric_code = 426,
  is_in_union = "Y",
  first_year = 1970,
  last_year = 2030
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
    ##   geographical_region_bias_reason = col_logical(),
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

## <a name="pop"></a>

## Read in population data

Read in population data for the populations of interest. Create a single
dataset with the function `rbind`.

``` r
popdata_botswana <- read.csv("data-raw/manuscript_example_data/Botswana_72_married_popdata_example.csv")
popdata_lesotho <- read.csv("data-raw/manuscript_example_data/Lesotho_426_married_popdata_example.csv")
popdata <- rbind(popdata_botswana, popdata_lesotho)
```

## <a name="results"></a>

## Calculate results

Supply the fits in a list and the population data to the function
`calc_fp_aggregate`. The resulting object is a list of long format
tibbles with family planning estimates.

``` r
results <- calc_fp_aggregate(fits = list(fit_botswana, fit_lesotho),
                   population_data = popdata)
```
