get_sigma <- function(list_global, core_data) {
  sigmaj <- list()
  for (j in 1:nrow(core_data$observations)) {
    sigma <- matrix(NA, 2, 2)
    sigma[1,1] <- core_data$observations$se_log_r_traditional_no_use[j]^2 + list_global$nonsample.se.trad.s[j]^2
    sigma[2,2] <- core_data$observations$se_log_r_modern_no_use[j]^2 + list_global$nonsample.se.modern.s[j]^2
    sigma[1,2] <- list_global$cor.trad.modern.s[j]*sqrt(sigma[1,1]*sigma[2,2])
    sigma[2,1] <- sigma[1,2]
    sigmaj[[j]] <- sigma
  }
  return(sigmaj)
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
  J = nrow(core_data$observations)
  S = mod$BUGSoutput$n.sims
  gett.j = list_auxiliary$get_t_i
  noneed.ct = 1 - mod$BUGSoutput$sims.list$mod.ct - mod$BUGSoutput$sims.list$trad.ct - mod$BUGSoutput$sims.list$unmet.ct
  mu.jn = mod$BUGSoutput$sims.list$mu.jn
  mu_unmet_s_j = mod$BUGSoutput$sims.list$logitratio.yunmet.hat.j
  qt = c(.025, .5, .975)
  logratio_mod_j = list_auxiliary$ratios.trad.modern.jn[,2]
  logratio_trad_j = list_auxiliary$ratios.trad.modern.jn[,1]
  logratio_unmet_j <- list_auxiliary$logitratio.yunmet.j
  var_unmet_j <- rep(NA, J)
  for(j in 1:J){
    if(is.na(core_data$observations$unmet_need_any[j])) next
    var_unmet_j[j] <- core_data$observations$se_log_r_unmet_no_need[j]^2 + list_global$nonsample.se.unmet.s[j]^2
  }
  true_mod_ct <- mod$BUGSoutput$sims.list$mod.ct
  true_trad_ct <- mod$BUGSoutput$sims.list$trad.ct
  true_unmet_ct = mod$BUGSoutput$sims.list$unmet.ct
  true_none_ct = 1 - mod$BUGSoutput$sims.list$mod.ct - mod$BUGSoutput$sims.list$trad.ct
  sigma_tradmod_j <- get_sigma(list_global, core_data)
  aux_modern <- list_auxiliary$modern
  aux_trad <- list_auxiliary$trad
  
  unmet_j <- dplyr::tibble(low_unmet_need_any = rep(NA,J), est_unmet_need_any =rep(NA,J), up_unmet_need_any = rep(NA,J))
  mod_j <- dplyr::tibble(low_contraceptive_use_modern = rep(NA,J), est_contraceptive_use_modern = rep(NA,J), up_contraceptive_use_modern = rep(NA,J))
  trad_j <- dplyr::tibble(low_contraceptive_use_traditional = rep(NA,J), est_contraceptive_use_traditional = rep(NA,J), up_contraceptive_use_traditional =  rep(NA,J))
  all_j <- dplyr::tibble(low_contraceptive_use_all = rep(NA,J), est_contraceptive_use_all = rep(NA,J), up_contraceptive_use_all =  rep(NA,J))
   for(j in 1:J) {
   if (!is.na(logratio_mod_j[j])){ # no unmet calculated either if modern is NA
    #mujn becomes vector if only one observation and does not have columns to index
    if(J==1) {
      bias_lrtrad_s <- mu.jn[,,1] - log(true_trad_ct[,,gett.j[j]]/true_none_ct[,,gett.j[j]])
      bias_lrmod_s <- mu.jn[,,2] - log(true_mod_ct[,,gett.j[j]]/true_none_ct[,,gett.j[j]])
    } else {
      bias_lrtrad_s <- mu.jn[,,1][,j] - log(true_trad_ct[,,gett.j[j]]/true_none_ct[,,gett.j[j]])
      bias_lrmod_s <- mu.jn[,,2][,j] - log(true_mod_ct[,,gett.j[j]]/true_none_ct[,,gett.j[j]])
    }
    if(!is.na(logratio_unmet_j[j])){
      bias_lrunmet_s <- mu_unmet_s_j[,j] - log(true_unmet_ct[,,gett.j[j]]/(true_none_ct[,,gett.j[j]] - true_unmet_ct[,,gett.j[j]]))
    }
    unmet_s <- rep(NA,S)
    trad_s <- rep(NA,S)
    mod_s <- rep(NA,S)
    if(!is.na(logratio_unmet_j[j])){
      for(s in 1:S) { # having repeated code here (extra loop) is computationaly more efficient then if inside S loop
        logratios_s <- MASS::mvrnorm(1, c(logratio_trad_j[j], logratio_mod_j[j]) - c(bias_lrtrad_s[s], bias_lrmod_s[s]), sigma_tradmod_j[[j]]) 
        ratio_tot_s <- sum(exp(logratios_s))
        none <- 1 - recover_prop(ratio_tot_s)
        trad_s[s] <- exp(logratios_s[1])*none
        mod_s[s] <- exp(logratios_s[2])*none
        lrunmet <- rnorm(1, logratio_unmet_j[j] - bias_lrunmet_s[s], sqrt(var_unmet_j[j]))
        unmet_over_none <- 1/(1+exp(-lrunmet))
        unmet_s[s] <- unmet_over_none*(1-aux_modern[j] - aux_trad[j])
      }
      unmet_j[j,] <- quantile(unmet_s, qt)  
      mod_j[j,] <- quantile(mod_s, qt)
      trad_j[j,] <- quantile(trad_s, qt)
    } else {
      for(s in 1:S) {
        logratios_s <- MASS::mvrnorm(1, c(logratio_trad_j[j], logratio_mod_j[j]) - c(bias_lrtrad_s[s], bias_lrmod_s[s]), sigma_tradmod_j[[j]]) 
        ratio_tot_s <- sum(exp(logratios_s))
        none <- 1 - recover_prop(ratio_tot_s)
        trad_s[s] <- exp(logratios_s[1])*none
        mod_s[s] <- exp(logratios_s[2])*none
      }
      mod_j[j,] <- quantile(mod_s, qt)
      trad_j[j,] <- quantile(trad_s, qt)
    }
  }
   }
  bias_adj_obs <- core_data$observations %>% 
    cbind(
      mod_j, 
      trad_j, 
      unmet_j, 
      all_j
    )
  return(bias_adj_obs)
}
