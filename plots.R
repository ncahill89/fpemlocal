---
  title: "FPEM"
output: github_document
---
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
options(warn=-1)
```

```{r, include=FALSE, eval=FALSE}
packagedocs::build_vignette()
```


## Table of Contents
1. [Introduction](#intro)
  2. [Install](#install)
    3. [Run models](#run)
      4. [Post-process](#post-process)
        5. [Plot model results](#plot)
          
          ## <a name="intro"></a>
          ## Introduction
          FPEM is composed of the following micro-packages [fpemdata](https://github.com/FPcounts/fpemdata), [fpemmodeling](https://github.com/FPcounts/fpemmodeling), and [fpemreporting](https://github.com/FPcounts/fpemreporting). Installing FPEM will install all aformentioned packages. Vignettes can be viewed after the [Install](#install) step with `browseVignettes("FPEM")`.The knitted version can also be viewed on github in [_gh-pages](https://github.com/FPcounts/FPEM/_gh-pages). The source code for vignettes can be found in [/vignettes](https://github.com/FPcounts/FPEM/tree/greg/vignettes). The directory [/design](https://github.com/FPcounts/FPEM/tree/greg/design) contains technical details about the design of this project for pacakge maintainers and contibutors. Below is a brief introduction to the FPEM packages. 
            
            ## <a name="install"></a>
            ## Install
            Setting `build_vignettes = TRUE` results in substantially longer installtion time but provide you with a dynamic view of vignettes via `browseVignettes("FPEM")`. All versions of the package can be found on the [release](https://github.com/FPcounts/FPEM/releases) page.
            ```{r, eval=FALSE}
            devtools::install_github(repo = "FPcounts/FPEM",
                                     ref = "develop",
                                     force = TRUE,
                                     build_vignettes = FALSE)
            ```
            
            ```{r, include=FALSE}
            knitr::opts_chunk$set(echo = TRUE)
            library(fpemdata)
            library(fpemmodeling)
            library(fpemreporting)
            library(R2jags)
            library(ggplot2)
            library(grid)
            library(gridExtra)
            ```
            ## <a name="run"></a>
            ## Run models
            `fpemmodeling::do_1country_run` is a wrapper function to run the family planning estimation model for a coutnry of interest for either married or unmarried women. When a survey file is not provided (as in this example) the function uses default data from `fpemdata::contraceptive_use`. The function takes in additional inputs such as services statistics if desired. See `??fpemmodeling::do_1country_run` for all possible inputs. If you wish to obtain results for all women `fpemmodeling::posterior_samples_all_women` can be used after completeing a run for married women and a run for unmarried women. See  [FPEM/vignettes](https://github.com/FPcounts/FPEM/vignettes) for more details
            ```{r}
            run_y <- fpemmodeling:::do_1country_run(
              is_in_union = "Y",
              division_numeric_code = 68,
              first_year = 1975,
              last_year = 2030
            )
            ```
            
            ## <a name="post-process"></a>
            ## Process the samples
            `fpemreporting::fpem_calculate_results` returns point-estimates for several indicators in long-format.
            ```{r}
            results <- fpemreporting::fpem_calculate_results(
              posterior_samples = run_y$posterior_samples,
              first_year = min(run_y$core_data$time_frame$.sequence),
              country_population_counts = fpemdata::get_population_counts())
            ```
            ## <a name="plot"></a>
            ## Plot the results
            `fpemreporting::fpem_plot_country_results` plots the results of the model againts the observations. Choose any indicators returned from `fpemreporting::fpem_calculate_results`
            ```{r}
            # not get indicator names from gregs branch
            
            fpemreporting:::indicator_names() %>% list2env(.GlobalEnv)
            
            plot_stuff <- function(
              country_results,
              contraceptive_use,
              indicator) {
              
              
              subpopulation_labels <- fpem_get_subpopulation_labels(contraceptive_use)
              
              tidyr::spread(country_results, key = percentile, value = value) %>%
                ggplot2::ggplot(ggplot2::aes(x = year)) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                  plot.title = ggplot2::element_text(hjust = 0.5),
                  axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
                ) +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`), fill = "plum1") +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = `10%`, ymax = `90%`), fill = "plum") +
                ggplot2::geom_line(ggplot2::aes(y = `50%`), color = "black") +
                ggplot2::geom_point(
                  data = contraceptive_use,
                  ggplot2::aes_string(
                    x = "start_date",
                    y = indicator
                  ),
                  size = 2
                )
            }
            
            
            
            pl <- list()
            for(j in 2:4) {
              results_j <- results[[indicator_results[j]]]
              pl[j] <- plot_stuff(
                results_j,
                contraceptive_use = run_y$core_data$observations,
                indicator = indicator[j]
              )
            }
            
            
            
            ```
            