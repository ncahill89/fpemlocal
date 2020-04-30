#' list_auxiliary_data
#' @param core_data 
#'
list_auxiliary_data <- function(core_data) {
  if(nrow(core_data$observations) == 0) {
    data = list(
      N = nrow(core_data$observations),
      C = nrow(core_data$units),
      nyears = max(core_data$year_sequence_list$model_seq), # replaced T by nyears
      prec = 1/0.0125^2,
      pmid.for.unmet = 0.4
    )
  } else {
    trad = core_data$observations$contraceptive_use_traditional
    modern = core_data$observations$contraceptive_use_modern
    unmet = core_data$observations$unmet_need_any
    geti.training.modonly.k <- seq(1,nrow(core_data$observations))[!is.na(core_data$observations$contraceptive_use_modern) & 
                                           is.na(core_data$observations$contraceptive_use_any) & 
                                           is.na(core_data$observations$contraceptive_use_traditional)]
    get_mod_i <- get_modtrad_i <-  which(!is.na(core_data$observations$contraceptive_use_modern) & !is.na(core_data$observations$contraceptive_use_traditional))
    get_ptot_i <- which(is.na(core_data$observations$contraceptive_use_modern))
    data = list(
      N = nrow(core_data$observations),
      C = nrow(core_data$units),
      nyears = max(core_data$year_sequence_list$model_seq), # replaced T by nyears
      get_mod_i = get_mod_i,
      n_mod = length(get_mod_i),
      get_unmet_i = which(!is.na(core_data$observations$unmet_need_any)),
      n_unmet = length(which(!is.na(core_data$observations$unmet_need_any))),
      get_ptot_i = get_ptot_i,
      n_ptot = length(get_ptot_i),
      get_c_i = as.numeric(factor(core_data$observations$division_numeric_code)),
      get_t_i = match(x = core_data$observations$ref_date,
                      core_data$year_sequence_list$model_seq_years),
      pmid.for.unmet = 0.4, 
      trad = trad,
      modern = modern,
      unmet = unmet,
      logit.ymodonly.i = logit(modern),
      geti.training.modonly.k = geti.training.modonly.k,
      n.training.modonly = length(geti.training.modonly.k),
      logit.ptot = ifelse(is.na(modern), qlogis(core_data$observations$contraceptive_use_any), NA),
      prec = 1/0.0125^2,
      se_log_r_unmet_no_need = core_data$observations$se_log_r_unmet_no_need,
      se_log_r_modern_no_use = core_data$observations$se_log_r_modern_no_use,
      se_log_r_traditional_no_use = core_data$observations$se_log_r_traditional_no_use,
      # or logratio one with biases (yet to be debugged)  
      ratios.trad.modern.in = cbind(log(trad/(1-modern-trad)), log(modern/(1-modern-trad))), # old order, trad goes first 
      logitratio.yunmet.i = log(unmet/(1-modern-trad - unmet))
      #we use log(p3/p4) = logit(p3/(p3+p4)) 
    )
  }
  return(data)
}
