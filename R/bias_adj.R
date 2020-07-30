get_sigma <- function(list_global, core_data) {
  sigmai <- list()
  for (i in 1:nrow(core_data$observations)) {
    sigma <- matrix(NA, 2, 2)
    sigma[1,1] <- core_data$observations$se_log_r_traditional_no_use_imputed[i]^2 + list_global$nonsample.se.trad.i[i]^2
    sigma[2,2] <- core_data$observations$se_log_r_modern_no_use_imputed[i]^2 + list_global$nonsample.se.modern.i[i]^2
    sigma[1,2] <- list_global$cor.trad.modern.i[i]*sqrt(sigma[1,1]*sigma[2,2])
    sigma[2,1] <- sigma[1,2]
    sigmai[[i]] <- sigma
  }
  return(sigmai)
}

recover_prop <- function(ratio){
  ratio/(1+ratio)
}



#' bias_adj
#'
#' @param core_data
#' @param list_auxiliary
#' @param list_global
#' @param mod
#'
bias_adj <- function(core_data, list_auxiliary, list_global, mod) {
  N = nrow(core_data$observations)
  S = mod$BUGSoutput$n.sims
  gett.i = list_auxiliary$get_t_i
  noneed.ct = 1 - mod$BUGSoutput$sims.list$mod.ct - mod$BUGSoutput$sims.list$trad.ct - mod$BUGSoutput$sims.list$unmet.ct
  mu.in = mod$BUGSoutput$sims.list$mu.in
  mu_unmet_s_i = mod$BUGSoutput$sims.list$logitratio.yunmet.hat.i
  qt = c(.025, .5, .975)
  logratio_mod_i = list_auxiliary$ratios.trad.modern.in[,2]
  logratio_trad_i = list_auxiliary$ratios.trad.modern.in[,1]
  logratio_unmet_i <- list_auxiliary$logitratio.yunmet.i
  var_unmet_i <- rep(NA, N)
  for(i in 1:N){
    if(is.na(core_data$observations$unmet_need_any[i])) next
    var_unmet_i[i] <- core_data$observations$se_log_r_unmet_no_need_imputed[i]^2 + list_global$nonsample.se.unmet.i[i]^2
  }
  true_mod_ct <- mod$BUGSoutput$sims.list$mod.ct
  true_trad_ct <- mod$BUGSoutput$sims.list$trad.ct
  true_unmet_ct = mod$BUGSoutput$sims.list$unmet.ct
  true_none_ct = 1 - mod$BUGSoutput$sims.list$mod.ct - mod$BUGSoutput$sims.list$trad.ct
  sigma_tradmod_i <- get_sigma(list_global, core_data)
  aux_modern <- list_auxiliary$modern
  aux_trad <- list_auxiliary$trad
  
  unmet_i <- dplyr::tibble(low_unmet_need_any = rep(NA,N), est_unmet_need_any =rep(NA,N), up_unmet_need_any = rep(NA,N)) %>% dplyr::mutate_if(is.logical, as.double)
  mod_i <- dplyr::tibble(low_contraceptive_use_modern = rep(NA,N), est_contraceptive_use_modern = rep(NA,N), up_contraceptive_use_modern = rep(NA,N)) %>% dplyr::mutate_if(is.logical, as.double)
  trad_i <- dplyr::tibble(low_contraceptive_use_traditional = rep(NA,N), est_contraceptive_use_traditional = rep(NA,N), up_contraceptive_use_traditional =  rep(NA,N)) %>% dplyr::mutate_if(is.logical, as.double)
  all_i <- dplyr::tibble(low_contraceptive_use_any = rep(NA,N), est_contraceptive_use_any = rep(NA,N), up_contraceptive_use_any =  rep(NA,N)) %>% dplyr::mutate_if(is.logical, as.double)
  for(i in 1:N) {
    if (!is.na(logratio_mod_i[i])){ # no unmet calculated either if modern is NA
      #muin becomes vector if only one observation and does not have columns to index
      if(N==1) {
        bias_lrtrad_s <- mu.in[,,1] - log(true_trad_ct[,,gett.i[i]]/true_none_ct[,,gett.i[i]])
        bias_lrmod_s <- mu.in[,,2] - log(true_mod_ct[,,gett.i[i]]/true_none_ct[,,gett.i[i]])
      } else {
        bias_lrtrad_s <- mu.in[,,1][,i] - log(true_trad_ct[,,gett.i[i]]/true_none_ct[,,gett.i[i]])
        bias_lrmod_s <- mu.in[,,2][,i] - log(true_mod_ct[,,gett.i[i]]/true_none_ct[,,gett.i[i]])
      }
      if(!is.na(logratio_unmet_i[i])){
        bias_lrunmet_s <- mu_unmet_s_i[,i] - log(true_unmet_ct[,,gett.i[i]]/(true_none_ct[,,gett.i[i]] - true_unmet_ct[,,gett.i[i]]))
      }
      unmet_s <- rep(NA,S)
      trad_s <- rep(NA,S)
      mod_s <- rep(NA,S)
      if(!is.na(logratio_unmet_i[i])){
        for(s in 1:S) { # having repeated code here (extra loop) is computationaly more efficient then if inside S loop
          logratios_s <- MASS::mvrnorm(1, c(logratio_trad_i[i], logratio_mod_i[i]) - c(bias_lrtrad_s[s], bias_lrmod_s[s]), sigma_tradmod_i[[i]])
          ratio_tot_s <- sum(exp(logratios_s))
          none <- 1 - recover_prop(ratio_tot_s)
          trad_s[s] <- exp(logratios_s[1])*none
          mod_s[s] <- exp(logratios_s[2])*none
          lrunmet <- rnorm(1, logratio_unmet_i[i] - bias_lrunmet_s[s], sqrt(var_unmet_i[i]))
          unmet_over_none <- 1/(1+exp(-lrunmet))
          unmet_s[s] <- unmet_over_none*(1-aux_modern[i] - aux_trad[i])
        }
        unmet_i[i,] <- tibble::as_tibble_row(quantile(unmet_s, qt) )
        mod_i[i,] <- tibble::as_tibble_row(quantile(mod_s, qt))
        trad_i[i,] <- tibble::as_tibble_row(quantile(trad_s, qt))
      } else {
        for(s in 1:S) {
          logratios_s <- MASS::mvrnorm(1, c(logratio_trad_i[i], logratio_mod_i[i]) - c(bias_lrtrad_s[s], bias_lrmod_s[s]), sigma_tradmod_i[[i]])
          ratio_tot_s <- sum(exp(logratios_s))
          none <- 1 - recover_prop(ratio_tot_s)
          trad_s[s] <- exp(logratios_s[1])*none
          mod_s[s] <- exp(logratios_s[2])*none
        }
        mod_i[i,] <- tibble::as_tibble_row(quantile(mod_s, qt))
        trad_i[i,] <- tibble::as_tibble_row(quantile(trad_s, qt))
      }
    }
  }
  bias_adj_obs <- core_data$observations %>%
    cbind(
      mod_i,
      trad_i,
      unmet_i,
      all_i
    )
  return(bias_adj_obs)
}