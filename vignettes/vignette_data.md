Package data
================

This vignette covers package data. Central to all vignettes are data
inputs in the form, division codes, of contraceptive use survey data,
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
library(FPEMcountry)
divisions
```

    ## # A tibble: 232 x 13
    ##    division_numeri~ name_country name_region name_sub_region region_numeric_~ sub_region_nume~
    ##               <dbl> <chr>        <chr>       <chr>                      <dbl>            <dbl>
    ##  1                4 Afghanistan  Asia        South-Central ~              935              921
    ##  2                8 Albania      Europe      Southern Europe              908              925
    ##  3               12 Algeria      Africa      Northern Africa              903              912
    ##  4               16 American Sa~ Oceania     Polynesia                    909              957
    ##  5               20 Andorra      Europe      Southern Europe              908              925
    ##  6               24 Angola       Africa      Middle Africa                903              911
    ##  7              660 Anguilla     Latin Amer~ Caribbean                    904              915
    ##  8               28 Antigua and~ Latin Amer~ Caribbean                    904              915
    ##  9               32 Argentina    Latin Amer~ South America                904              931
    ## 10               51 Armenia      Asia        Western Asia                 935              922
    ## # ... with 222 more rows, and 7 more variables: is_developed_region <chr>,
    ## #   is_less_developed_region <chr>, is_least_developed_country <chr>, is_in_sub_saharan_africa <chr>,
    ## #   is_unmarried_sexual_activity <chr>, is_low_population <chr>, is_fp2020 <chr>

Our package data sets are tibbles. This is particularly useful for large
datasets because it only prints the first few rows. The country codes
used by our package, known as `division_numeric_code`, are found in this
data. In our example we will execute a one-country run for Afghanistan,
code `4`.

## <a name="cu"></a>

## 2\. Contraceptive use survey data

Two versions of survey data are available in the package. They are
identical in format but compiled by two different organizations.
`contraceptive_use` is compiled by the UNPD. `contraceptive_use_track20`
is compiled by Track20. Enter `??contraceptive_use` and
`??contraceptive_use_track20` for metadata.

``` r
contraceptive_use
```

    ## # A tibble: 1,896 x 31
    ##    division_numeri~ start_date end_date is_in_union age_range data_series_type group_type_rela~
    ##               <dbl>      <dbl>    <dbl> <chr>       <chr>     <chr>            <chr>           
    ##  1              218      1999.    2000. N           15-49     Other            UW              
    ##  2              498      1998.    1998. N           15-49     Other            UW              
    ##  3              600      1999.    1999. N           15-49     Other            UW              
    ##  4              694      2006.    2006. N           15-49     MICS             UW              
    ##  5              516      2007.    2007. N           15-49     DHS              UW              
    ##  6              222      2003.    2003. N           15-49     Other            UW              
    ##  7              214      2007.    2008. N           15-49     DHS              UW              
    ##  8              604      1981     1981  N           15-49     Other            UW              
    ##  9              268      2000.    2000. N           15-49     Other            UW              
    ## 10               52      1980     1981  N           15-49     Other            UW              
    ## # ... with 1,886 more rows, and 24 more variables: contraceptive_use_modern <dbl>,
    ## #   contraceptive_use_traditional <dbl>, contraceptive_use_any <dbl>, unmet_need_modern <lgl>,
    ## #   unmet_need_any <dbl>, is_pertaining_to_methods_used_since_last_pregnancy <chr>,
    ## #   pertaining_to_methods_used_since_last_pregnancy_reason <lgl>, has_geographical_region_bias <chr>,
    ## #   geographical_region_bias_reason <chr>, has_non_pregnant_and_other_positive_biases <chr>,
    ## #   non_pregnant_and_other_positive_biases_reason <chr>, age_group_bias <chr>, modern_method_bias <chr>,
    ## #   has_traditional_method_bias <chr>, traditional_method_bias_reason <chr>,
    ## #   has_absence_of_probing_questions_bias <chr>, se_modern <dbl>, se_traditional <dbl>,
    ## #   se_unmet_need <dbl>, se_log_r_modern_no_use <dbl>, se_log_r_traditional_no_use <dbl>,
    ## #   se_log_r_unmet_no_need <dbl>, source_id <dbl>, record_id <chr>

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
