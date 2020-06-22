Aggregating estimates from multiple fits with custom user data
================

## Introduction

In this vignette we will fit FPET to multiple countries and aggregate
the samples to obtain results for aggregate levels. We will fit models
for Botswana and Lesotho, country codes 72 and 426 respectively.

## Table of Contents

1.  [Fit models](#fit)
2.  [Aggregate samples](#run)
3.  [Calculate results](#results)

## <a name="fit"></a>

## Fit models

First, fit the models with the function `fit_fp_c`.

``` r
first_year <- 1970
last_year <- 2030
union <- "Y"
fit_botswana <- fit_fp_c(
  surveydata_filepath = "D:/git/FPEMcountry/data-raw/Botswana_72_married_example.csv",
  division_numeric_code = 72,
  is_in_union = union,
  first_year = first_year,
  last_year = last_year
)
#> Parsed with column specification:
#> cols(
#>   .default = col_character(),
#>   X1 = col_double(),
#>   division_numeric_code = col_double(),
#>   start_date = col_double(),
#>   end_date = col_double(),
#>   contraceptive_use_modern = col_double(),
#>   contraceptive_use_traditional = col_double(),
#>   contraceptive_use_any = col_double(),
#>   unmet_need_modern = col_logical(),
#>   unmet_need_any = col_double(),
#>   pertaining_to_methods_used_since_last_pregnancy_reason = col_logical(),
#>   geographical_region_bias_reason = col_logical(),
#>   non_pregnant_and_other_positive_biases_reason = col_logical(),
#>   traditional_method_bias_reason = col_logical(),
#>   se_modern = col_logical(),
#>   se_traditional = col_logical(),
#>   se_unmet_need = col_logical(),
#>   se_log_r_modern_no_use = col_logical(),
#>   se_log_r_traditional_no_use = col_logical(),
#>   se_log_r_unmet_no_need = col_logical(),
#>   source_id = col_double()
#> )
#> See spec(...) for full column specifications.
fit_lesotho <- fit_fp_c(
  surveydata_filepath = "D:/git/FPEMcountry/data-raw/Lesotho_426_married_example.csv",
  division_numeric_code = 426,
  is_in_union = union,
  first_year = first_year,
  last_year = last_year
)
#> Parsed with column specification:
#> cols(
#>   .default = col_double(),
#>   is_in_union = col_character(),
#>   age_range = col_character(),
#>   data_series_type = col_character(),
#>   group_type_relative_to_baseline = col_character(),
#>   unmet_need_modern = col_logical(),
#>   is_pertaining_to_methods_used_since_last_pregnancy = col_character(),
#>   pertaining_to_methods_used_since_last_pregnancy_reason = col_logical(),
#>   has_geographical_region_bias = col_character(),
#>   geographical_region_bias_reason = col_logical(),
#>   has_non_pregnant_and_other_positive_biases = col_character(),
#>   non_pregnant_and_other_positive_biases_reason = col_logical(),
#>   age_group_bias = col_character(),
#>   modern_method_bias = col_character(),
#>   has_traditional_method_bias = col_character(),
#>   traditional_method_bias_reason = col_logical(),
#>   has_absence_of_probing_questions_bias = col_character(),
#>   record_id = col_character()
#> )
#> See spec(...) for full column specifications.
fit_list <- list(fit_botswana, fit_lesotho)
```

## <a name="aggregate"></a>

## Aggregate samples

Within out list of fits we have more complex lists. We only need the
samples from these fits. This function plucks the posterior samples and
combines each array of samples along index 1 making one large array. The
first dimension is for country codes. Use `dimnames()` on the resulting
list to see how to index a country. This is the identical format of a
posterior samples array from FPEMglobal.

``` r
posterior_samples <- pluck_abind_fp_c(fit_list)
```

Next, read in your population data and make a single population data
set.

``` r
popdata_botswana <- read.csv("D:/git/FPEMcountry/data-raw/Botswana_72_married_popdata_example.csv")
popdata_lesotho <- read.csv("D:/git/FPEMcountry/data-raw/Lesotho_426_married_popdata_example.csv")
popdata <- rbind(popdata_botswana, popdata_lesotho)
```

Now supply the posterior samples and population count dataset to the
function `aggregate_fp`.

``` r
posterior_samples_aggregated <- aggregate_fp(posterior_samples =  posterior_samples,
                                             population_data = popdata)
```

## <a name="results"></a>

## Calculate results

Finally, supply the aggregated samples, the population data, and the
first\_year to `calc_fp` to calculate point estimates for family
planning indicators.

``` r
results <- calc_fp(posterior_samples = posterior_samples_aggregated,
                   population_data = popdata,
                   first_year = first_year)
```
