

#' list_global_data
#'
#' jags data obtained from the UNPD global model
#'
#' @inheritParams do_1country_run
#' @param core_data \emph{\sQuote{Data.frame}} The processed data associated with the model run from \code{\link{core_data}}.
#'
list_global_data = function(is_in_union, core_data) {
  is_in_union <- (is_in_union == "Y")
  div <- core_data$units$division_numeric_code
  start_year <- core_data$start_year
  first_year <- core_data$year_sequence_list$model_seq_years %>% min()
  subnational <- core_data$subnational
  if (is_in_union) {
    globaldata <- globalrun_output_m$mcmc.post
    globaldata_sd <- globalrun_output_m$mcmc.post.sd
    link <- index_m
    subreg <- link$index_area_df$subreg.c[link$index_area_df$division_numeric_code == div]
    reg <- link$index_area_df$reg.c[link$index_area_df$division_numeric_code == div]
    pmax_lower_bound <- .5
    is.dev.c <- ifelse(core_data$units$is_developed_region == "N", 0, 1)
    country.c <- which(link$index_area_df$division_numeric_code == div)
    tau.sourcemodonly <- (1/globaldata$sigma.sourcemodonly)^2
  } else {
    globaldata <- globalrun_output_u$mcmc.post
    globaldata_sd <- globalrun_output_u$mcmc.post.sd
    link <- index_u
    subreg <- link$index_area_df$subreg.c[link$index_area_df$division_numeric_code == div]
    reg <- link$index_area_df$reg.c[link$index_area_df$division_numeric_code == div]
    pmax_lower_bound <- .1
    is.dev.c <- ifelse(core_data$units$is_unmarried_sexual_activity == "Y", 0, 1) # dev = 1 for SA0 group
    country.c <- which(link$index_area_df$division_numeric_code == div)
    tau.sourcemodonly <- NA
  }

  if (!subnational | is.null(globaldata[[paste0('setlevel.c[',country.c, "]")]])) {
    if(is.null(globaldata[[paste0('setlevel.c[',country.c, "]")]])) {
      warning("unmarried parameters missing, higher level parameters used")
    }
    unmet.subreg <- globaldata[[paste0('unmet.subreg[',subreg, "]")]]
    sd_unmet.subreg <- globaldata_sd[[paste0('unmet.subreg[',subreg, "]")]]
    w.subreg <- globaldata[[paste0('w.subreg[',subreg, "]")]]
    sd_w.subreg <- globaldata_sd[[paste0('w.subreg[',subreg, "]")]]
    Rw.subreg <- globaldata[[paste0('Rw.subreg[',subreg, "]")]]
    sd_Rw.subreg <- globaldata_sd[[paste0('Rw.subreg[',subreg, "]")]]
    RT.subreg <- globaldata[[paste0('RT.subreg[',subreg, "]")]] - first_year + 1
    sd_RT.subreg <- globaldata_sd[[paste0('RT.subreg[',subreg, "]")]]
    mean_setlevel <- is.dev.c*globaldata$Shigher + (1-is.dev.c)*globaldata[[paste0('S.subreg[',subreg, "]")]]
    sd_mean_setlevel <- is.dev.c*globaldata_sd$Shigher + (1-is.dev.c)*globaldata_sd[[paste0('S.subreg[',subreg, "]")]]
    # for unmarried, dev <- 1 is SA0, so we have the hierarchy for SA1
    var_setlevel <- is.dev.c*globaldata$sigma.higherSc^2 + (1-is.dev.c)*globaldata$sigma.Sc^2
    lp.world <- globaldata$lp.world
    sd_lp.world <- globaldata_sd$lp.world
    lr.world <- globaldata$lr.world
    sd_lr.world <- globaldata_sd$lr.world
  } else {
    unmet.subreg <- globaldata[[paste0('unmet.intercept.c[',country.c, "]")]]
    sd_unmet.subreg <- globaldata_sd[[paste0('unmet.intercept.c[',country.c, "]")]]
    RT.subreg <- globaldata[[paste0('RT.c[',country.c, "]")]] - first_year + 1
    sd_RT.subreg <- globaldata[[paste0('RT.c[',country.c, "]")]]
    mean_setlevel <- globaldata[[paste0('setlevel.c[',country.c, "]")]]
    sd_mean_setlevel <- globaldata_sd[[paste0('setlevel.c[',country.c, "]")]]
    var_setlevel <- is.dev.c*globaldata$sigma.higherSc^2 + (1-is.dev.c)*globaldata$sigma.Sc^2


    # country parameters that need transformation:
    w.subreg <- get_logtr_omegas(globaldata[[paste0('omega.c[',country.c, "]")]], mino = 0.01, maxo = 0.5) # w.subreg is on logit omega scale
    sd_w.subreg <- 0 #get_logtr_omegas(globaldata_sd[[paste0('omega.c[',country.c, "]")]], mino = 0.01, maxo = 0.5) # w.subreg is on logit omega scale
    Rw.subreg <- get_logtr_omegas(globaldata[[paste0('Romega.c[',country.c, "]")]], mino = 0.01, maxo = 0.5) # Rw.subreg is on logit omega scale
    sd_Rw.subreg <- 0 #get_logtr_omegas(globaldata_sd[[paste0('Romega.c[',country.c, "]")]], mino = 0.01, maxo = 0.5)
    lp.world <- get_logtr_omegas(globaldata[[paste0('pmax.c[',country.c, "]")]], mino = pmax_lower_bound, maxo = 1)
    lr.world <- get_logtr_omegas(globaldata[[paste0('Rmax.c[',country.c, "]")]], mino = 0.5, maxo = 1)
    sd_lp.world <- 0
    sd_lr.world <- 0
  }
  tau_setlevel <- is.dev.c*1/globaldata$sigma.higherSc^2 + (1-is.dev.c)*1/globaldata$sigma.Sc^2


  if (!nrow(core_data$observations) == 0) {
    index_datatype <- core_data$observations$index_datatype
    index_datatype_unmet <- core_data$observations$index_datatype_unmet
    names <- c(
      "nonsample.se.modern.s",
      "nonsample.se.trad.s",
      "cor.trad.modern.s"
    )
    se <- extract_se(names, globaldata, index_datatype)
    nonsample.se.modern.i <- se$nonsample.se.modern.s
    nonsample.se.trad.i <- se$nonsample.se.trad.s
    cor.trad.modern.i <- se$cor.trad.modern.s

    names <- c("nonsample.se.unmet.s")
    se2 <- extract_se(names, globaldata, index_datatype_unmet)
    nonsample.se.unmet.i <- se2$nonsample.se.unmet.s
  } else {
    nonsample.se.modern.i <- 0
    nonsample.se.trad.i <- 0
    cor.trad.modern.i <- 0
    nonsample.se.unmet.i <- 0
  }

  return(list(
    # pars which change in model depending on sun-national
    tau.sourcemodonly = tau.sourcemodonly,
    pmax_lower_bound = pmax_lower_bound,
    nonsample.se.modern.i = nonsample.se.modern.i,
    nonsample.se.trad.i = nonsample.se.trad.i,
    cor.trad.modern.i = cor.trad.modern.i,
    nonsample.se.unmet.i = nonsample.se.unmet.i,
    var_setlevel = var_setlevel,
    # pars which are based on a hierarchical model in the global model
    lp.world = lp.world,
    sd_lp.world = sd_lp.world,

    lr.world = lr.world,
    sd_lr.world = sd_lr.world,

    unmet.subreg = unmet.subreg,
    sd_unmet.subreg = sd_unmet.subreg,

    w.subreg = w.subreg,
    sd_w.subreg = sd_w.subreg,

    Rw.subreg = Rw.subreg,
    sd_Rw.subreg = sd_Rw.subreg,

    RT.subreg = RT.subreg,
    sd_RT.subreg = sd_RT.subreg,

    mean_setlevel = mean_setlevel,
    sd_mean_setlevel = sd_mean_setlevel,

    ####### end pars based on hier hier
    ###### end pars which change

    rho.P = globaldata$rho.tot,
    rho.R = globaldata$rho.rat,
    rho.Z = globaldata$rho.unmet,
    sigma.P = globaldata$sigma.tot,
    sigma.R = globaldata$sigma.rat,
    sigma.Z = globaldata$sigma.ar.unmet,
    sigma.lpc = globaldata$sigma.lpc,
    sigma.lrc = globaldata$sigma.lrc,
    sigma.wc = globaldata$sigma.wc,

    sigma.Rwc = globaldata$sigma.Rwc,
    sigma.RTc = globaldata$sigma.RTc,
    sd_sigma.RTc = globaldata_sd$sigma.RTc,
    sigma.unmetc = globaldata$sigma.unmetc,
    sigma.sourcetot = globaldata$sigma.sourcetot,

    a.unmet = globaldata$a.unmet,
    b.unmet =  globaldata$b.unmet,
    c.unmet =  globaldata$c.unmet,

    v.folk = globaldata$v.folk,
    v.mics = globaldata$v.abs.probe.q,
    v.mpos = globaldata$v.mpos,
    v.mneg = globaldata$v.mneg,

    sigma.pos = globaldata$sigma.pos,
    mu.pos.m = c(globaldata$`mu.pos.m[1]`,
                 globaldata$`mu.pos.m[2]`),
    sigma.geo.m = c(globaldata$`sigma.geo.m[1]`,
                    globaldata$`sigma.geo.m[2]`),


    # hardcoded because not found in globalparameters
    t_star = start_year - first_year +1
    #t_star = global_run_paramestimates$t_star

  ))
}






extract_se <- function(names, data.global, numeric_source) {
  se_ls <- list()
  for(j in 1:length(names)){
    temp <- c()
    for(i in 1:length(numeric_source)) {
      temp[i] <- data.global[[which(names(data.global) == paste0(names[j],'[',numeric_source[i], "]") )]]
    }
    se_ls[[j]] <- temp
  }
  names(se_ls) <- names
  return(se_ls)
}


get_logtr_omegas <- function(omega, mino, maxo) {
  log((omega - mino)/(maxo - omega))
}

