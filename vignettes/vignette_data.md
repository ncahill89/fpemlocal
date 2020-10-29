Package data
================

This vignette covers package data. Central to all vignettes are data
inputs in the form of, division codes, contraceptive use survey data,
and population count data. These three types of data are included as
package data.

1.  [division code data](#div) `divisions`
2.  [contraceptive use survey data](#cu) `contraceptive_use` and
    `contraceptive_use_track20`
3.  [population count data](#pc) `population_counts`

## <a name="div"></a>

## 1\. Division codes

Division data is used as the a link between low-level divisions
(country) and higher-level divisions (sub-regions, regions). All data
inputs include a column of country division codes known as division
numeric codes. After loading the package, enter `divisions` into the
console to access this data.

``` r
library(FPEMlocal)
divisions
```

    ## # A tibble: 232 x 13
    ##    division_numeri~ name_country name_region name_sub_region region_numeric_~
    ##               <dbl> <chr>        <chr>       <chr>                      <dbl>
    ##  1                4 Afghanistan  Asia        South-Central ~              935
    ##  2                8 Albania      Europe      Southern Europe              908
    ##  3               12 Algeria      Africa      Northern Africa              903
    ##  4               16 American Sa~ Oceania     Polynesia                    909
    ##  5               20 Andorra      Europe      Southern Europe              908
    ##  6               24 Angola       Africa      Middle Africa                903
    ##  7              660 Anguilla     Latin Amer~ Caribbean                    904
    ##  8               28 Antigua and~ Latin Amer~ Caribbean                    904
    ##  9               32 Argentina    Latin Amer~ South America                904
    ## 10               51 Armenia      Asia        Western Asia                 935
    ## # ... with 222 more rows, and 8 more variables:
    ## #   sub_region_numeric_code <dbl>, is_developed_region <chr>,
    ## #   is_less_developed_region <chr>, is_least_developed_country <chr>,
    ## #   is_in_sub_saharan_africa <chr>, is_unmarried_sexual_activity <chr>,
    ## #   is_low_population <chr>, is_fp2020 <chr>

``` r
??divisions
```

The country codes used by our package, known as `division_numeric_code`,
are found in this data. In our example we will execute a one-country run
for Afghanistan, code `4`.

## <a name="cu"></a>

## 2\. Contraceptive use survey data

There are two contraceptive use survey datasets within the package. The
dataset `contraceptive_use` is compiled by the Unit Nation Population
Division (UNPD) and the dataset `contraceptive_use_track20` is compiled
by Track20. The datasets are similar. There will be some differences for
FP2020 focus countries. Enter `??contraceptive_use` and
`??contraceptive_use_track20` for metadata.

``` r
contraceptive_use
```

    ## # A tibble: 1,896 x 34
    ##    division_numeri~ start_date end_date is_in_union age_range
    ##               <dbl>      <dbl>    <dbl> <chr>       <chr>    
    ##  1              170      1980     1980  Y           15-49    
    ##  2              795      2006     2006  Y           15-49    
    ##  3              218      1999.    2000. Y           15-49    
    ##  4              360      2005     2005  Y           15-49    
    ##  5              643      1996.    1996. Y           15-49    
    ##  6              784      1995     1995  Y           15-49    
    ##  7              524      1992.    1992. Y           15-49    
    ##  8              368      1974     1974  Y           15-49    
    ##  9              458      1974     1974  Y           15-49    
    ## 10              626      2003.    2004. Y           15-49    
    ## # ... with 1,886 more rows, and 29 more variables: data_series_type <chr>,
    ## #   group_type_relative_to_baseline <chr>, contraceptive_use_modern <dbl>,
    ## #   contraceptive_use_traditional <dbl>, contraceptive_use_any <dbl>,
    ## #   unmet_need_modern <lgl>, unmet_need_any <dbl>,
    ## #   is_pertaining_to_methods_used_since_last_pregnancy <chr>,
    ## #   pertaining_to_methods_used_since_last_pregnancy_reason <lgl>,
    ## #   has_geographical_region_bias <chr>,
    ## #   geographical_region_bias_reason <chr>,
    ## #   has_non_pregnant_and_other_positive_biases <chr>,
    ## #   non_pregnant_and_other_positive_biases_reason <chr>,
    ## #   age_group_bias <chr>, modern_method_bias <chr>,
    ## #   has_traditional_method_bias <chr>, traditional_method_bias_reason <chr>,
    ## #   has_absence_of_probing_questions_bias <chr>, se_modern <dbl>,
    ## #   se_traditional <dbl>, se_unmet_need <dbl>, se_log_r_modern_no_use <dbl>,
    ## #   se_log_r_traditional_no_use <dbl>, se_log_r_unmet_no_need <dbl>,
    ## #   source_id <dbl>, record_id <chr>, se_log_r_unmet_no_need_imputed <dbl>,
    ## #   se_log_r_traditional_no_use_imputed <dbl>,
    ## #   se_log_r_modern_no_use_imputed <dbl>

## <a name="cu"></a>

## 3\. Population count data

Access population count data by entering `population_counts` into the
console. This data is compiled by the UNPD. Enter `??populatoin_counts`
for the metadata.

``` r
population_counts
```

    ## # A tibble: 27,084 x 5
    ##    is_in_union division_numeric_code population_count age_range mid_year
    ##    <chr>                       <dbl>            <dbl> <chr>        <dbl>
    ##  1 Y                               4          2030527 15-49         1970
    ##  2 Y                               8           300141 15-49         1970
    ##  3 Y                              12          1995757 15-49         1970
    ##  4 Y                              16             3477 15-49         1970
    ##  5 Y                              24           980282 15-49         1970
    ##  6 Y                              28             5211 15-49         1970
    ##  7 Y                              31           755409 15-49         1970
    ##  8 Y                              32          3620060 15-49         1970
    ##  9 Y                              36          1788561 15-49         1970
    ## 10 Y                              40          1163242 15-49         1970
    ## # ... with 27,074 more rows
