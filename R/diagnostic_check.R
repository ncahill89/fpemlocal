diagnostic_check <- function(diagnostic, 
                             mod,
                             core_data
                             ) {
  if (diagnostic) { # Only check convergence if diagnostic is TRUE
    country_name <- core_data$units$name_country
    division_numeric_code <- core_data$units$division_numeric_code
    union <-core_data$is_in_union
    summ <- MCMCvis::MCMCsummary(mod) %>% 
      tibble::rownames_to_column(var = "pars") %>%
      dplyr::mutate(pars = pars %>% stringr::str_remove_all("[:digit:]") %>% stringr::str_remove("[,]") %>% stringr::str_remove("\\[]")) %>%
      # For now we are just inspecting a limited number of parameters. 
      dplyr::filter(pars %in% c("logit_mod.ct",
                                "logit_unmet.ct",
                                "logit_trad.ct",
                                "mod.ct",
                                "unmet.ct",
                                "trad.ct"))
    if (any(summ$Rhat > 1.1)) { # Only write report when Rhat > 1.1 for at least one parameter
      rhat_data <- summ %>% 
        dplyr::filter(Rhat > 1.1) %>% 
        dplyr::select(pars, Rhat)
      if (!dir.exists("output")) dir.create("output")
      cat(paste(division_numeric_code, union, "has max Rhat", max(summ$Rhat)), sep = "", append = TRUE, file = "output/automatic_convergence_check.txt", fill = TRUE)
      if (!dir.exists("output/diagnostic")) dir.create("output/diagnostic")
      write.table(rhat_data, paste0("output/diagnostic/", division_numeric_code, union, "_", country_name, "_rhats.txt"))
      
      bad_parnames <- summ %>% 
        dplyr::filter(Rhat > 1.1) %>% 
        dplyr::select(pars) %>% dplyr::pull()
      MCMCvis::MCMCtrace(mod,
                         params = bad_parnames,
                         ISB = FALSE,
                         pdf = TRUE,
                         post_zm = TRUE,
                         open_pdf = FALSE,
                         Rhat = TRUE,
                         filename = paste0("output/diagnostic/", division_numeric_code, union, "_", country_name, "_mcmctrace.pdf")
      )
    } # end conditional write report
  } # end diagnostics check
} # end function