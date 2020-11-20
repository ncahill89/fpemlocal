
#### 1\. Fit a one country model

``` r
library(fpemlocal)
fit <- fit_fp_c(
  division_numeric_code = 4,
  is_in_union = "Y",
  first_year = 1970,
  last_year = 2030
)
```

#### 2\. Calculate point estimates for indicators

``` r
results <- calc_fp_c(fit)
```

#### 3\. Plot estimates and survey data

``` r
plot_fp_c(
  fit,
  results,
  indicators = c(
    "contraceptive_use_modern"
    )
  )
```

    ## $Y
    ## $Y$contraceptive_use_modern

![](in_union_women_paper_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
