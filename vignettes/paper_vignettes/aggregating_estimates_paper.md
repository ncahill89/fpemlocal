
#### 1\. Fit one country models for populations of interest

``` r
library(fpemlocal)
fit_botswana <- fit_fp_c(
  surveydata_filepath = "data-raw/manuscript_example_data/Botswana_72_married_example.csv",
  division_numeric_code = 72,
  is_in_union = "Y",
  first_year = 1970,
  last_year = 2030
)
fit_lesotho <- fit_fp_c(
  surveydata_filepath = "data-raw/manuscript_example_data/Lesotho_426_married_example.csv",
  division_numeric_code = 426,
  is_in_union = "Y",
  first_year = 1970,
  last_year = 2030
)
```

#### 2\. Read in population data for the populations of interest. Create a single dataset with the function `rbind`.

``` r
popdata_botswana <- read.csv("data-raw/manuscript_example_data/Botswana_72_married_popdata_example.csv")
popdata_lesotho <- read.csv("data-raw/manuscript_example_data/Lesotho_426_married_popdata_example.csv")
popdata <- rbind(popdata_botswana, popdata_lesotho)
```

#### 3\. Supply the fits in a list and the population dataset to the function `calc_fp_aggregate`. The resulting object is a list of long format tibbles with family planning estimates.

``` r
results <- calc_fp_aggregate(fits = list(fit_botswana, fit_lesotho),
                   population_data = popdata)
results[["contraceptive_use_modern"]] %>% head()
```

    ## # A tibble: 6 x 3
    ##    year percentile  value
    ##   <int> <chr>       <dbl>
    ## 1  1970 mean       0.0817
    ## 2  1971 mean       0.0887
    ## 3  1972 mean       0.0963
    ## 4  1973 mean       0.104 
    ## 5  1974 mean       0.112 
    ## 6  1975 mean       0.121
