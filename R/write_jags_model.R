write_jags_model <- function(old_dm = FALSE,
                             include_ss_data = FALSE,
                             nulldata,
                             is_in_union,
                             n_ptot) {
  
  # model taken from https://github.com/FPcounts/ContraceptiveUse_MWRA_Update
  # deleting all global-only stuff
  # remove jumping over years w/o observations
  # kept C because easier
  # but code is coded for C= 1 (ie hardcoded subreg index)
  
  cat("
model{
  Y  ~ dnorm(greghack, 1)
  greghack ~ dnorm(1,1)
  # AR part
  for (c in 1:C){
    # eps.P.ct[c, 1] ~ dnorm(0, tau.P.st) # for P
    # eps.R.ct[c,1] ~ dnorm(0, tau.R.st) # for R
    # eps.Z.ct[c,1] ~ dnorm(0, tau.Z.st) # for Z
    # for (t in 2:nyears){
    #   eps.P.ct[c, t] ~ dnorm(rho.P*eps.P.ct[c, t-1], tau.P)
    #   eps.R.ct[c, t] ~ dnorm(rho.R*eps.R.ct[c, t-1], tau.R)
    #   eps.Z.ct[c, t] ~ dnorm(rho.Z*eps.Z.ct[c, t-1], tau.Z)
    # }
    eps.P.ct[c, t_star] ~ dnorm(0, tau.P.st) # for P
    eps.R.ct[c, t_star] ~ dnorm(0, tau.R.st) # for R
    eps.Z.ct[c, t_star] ~ dnorm(0, tau.Z.st) # for Z
    for (t in (t_star+1):nyears){
      eps.P.ct[c, t] ~ dnorm(rho.P*eps.P.ct[c, t-1], tau.P)
      eps.R.ct[c, t] ~ dnorm(rho.R*eps.R.ct[c, t-1], tau.R)
      eps.Z.ct[c, t] ~ dnorm(rho.Z*eps.Z.ct[c, t-1], tau.Z)
    }
    for (t in 2:t_star){
      eps.P.ct[c, t-1] ~ dnorm(rho.P*eps.P.ct[c, t], tau.P)
      eps.R.ct[c, t-1] ~ dnorm(rho.R*eps.R.ct[c, t], tau.R)
      eps.Z.ct[c, t-1] ~ dnorm(rho.Z*eps.Z.ct[c, t], tau.Z)
    }
  }
  tau.P.st <- tau.P*(1-pow(rho.P,2))
  tau.P <- pow(sigma.P, -2)
  tau.R.st <- tau.R*(1-pow(rho.R,2))
  tau.R <- pow(sigma.R, -2)
  tau.Z.st <- tau.Z*(1-pow(rho.Z,2))
  tau.Z <- pow(sigma.Z, -2)
  # logistic curves and model for Z
  for (c in 1:C){
    for (t in 1:nyears){
      Rmu.ct[c, t] <- Romega.c[c]*(t - RT.c[c])
      Rstar.ct[c, t] <- Rmax.c[c]/(1+exp(-Rmu.ct[c, t]))
      R.ct[c, t] <- 1/(1+exp(-( logit(Rstar.ct[c,t]) + eps.R.ct[c,t])))
      logitZstar.ct[c,t] <- (unmet.intercept.c[c]
                             + a.unmet
                             + b.unmet * (P.ct[c,t] - pmid.for.unmet)
                             + c.unmet * pow(P.ct[c,t] - pmid.for.unmet,2))
      Z.ct[c,t] <- 1/(1+exp(-(logitZstar.ct[c,t] + eps.Z.ct[c,t])))
      #neg.explogitZ.ct[c,t] = exp(-logitZ.ct[c,t])
    }
    for(t in 1:(t_star-1)){
      ls.ct[c,(t_star-t)] <- s.ct[c, (t_star-t)+1] - eps.P.ct[c, t_star-t] #logit
      ils.ct[c,(t_star-t)] <- 1/(1+exp(-ls.ct[c,(t_star-t)])) #inv.logit
      #Step function; test for x >/= 0
      I[c,(t_star-t)] <- step(ils.ct[c,(t_star-t)] - pmax.c[c])
      ###Get P.ct directly in the backward direction
      #Only need this bit if I=0 i.e., ils.ct<pmax.c
      zeta.ct[c,(t_star-t)] <- (1-I[c,(t_star-t)])*(logit(min((1-0.00001),ils.ct[c,(t_star-t)]/pmax.c[c]))-omega.c[c])
      P.ct[c,(t_star-t)]<-(1-I[c,(t_star-t)])*(pmax.c[c]*(1/(1+exp(-zeta.ct[c,(t_star-t)])))) + I[c,(t_star-t)]*ils.ct[c,(t_star-t)]
      ###Get logit(P.ct)
      s.ct[c,(t_star-t)] <- logit(P.ct[c,(t_star-t)])
    } # end back extrapolation
    for(t in (t_star+1):nyears){
      #Step function; test for x >/= 0
      I[c,t] <- step(P.ct[c,t-1] - pmax.c[c])
      #Only need this bit if I=0 i.e., P.ct<pmax.c
      zeta.ct[c,t] <- (1-I[c,t])*(logit(min((1-0.000001),P.ct[c,t-1]/pmax.c[c])) + omega.c[c])
      s.ct[c,t] <- logit(I[c,t]*(P.ct[c,t-1]) + (1-I[c,t])*pmax.c[c]*(1/(1+exp(-zeta.ct[c,t])))) + eps.P.ct[c,t-1]
      P.ct[c,t] <- 1/(1 + exp(-s.ct[c,t]))
    }
    ### add pmax_lower_bound here
    pmax.c[c] <- pmax_lower_bound + (1-pmax_lower_bound)/(1+exp(-logitpmax.c[c]))
    logitpmax.c[c] ~ dnorm(lp.world, 1/(pow(sigma.lpc, 2) + pow(sd_lp.world, 2)))
# lower bound for rmax is 0.5 for married AND unmarried
    Rmax.c[c] <- 0.5 + (1-0.5)/(1+exp(-logitRmax.c[c]))
    logitRmax.c[c] ~ dnorm(lr.world, 1/(pow(sigma.lrc, 2) + pow(sd_lr.world, 2)))
    logitomega.c[c] ~ dnorm(w.subreg, 1/(pow(sigma.wc, 2) + pow(sd_w.subreg, 2)))
    omega.c[c] <- 0.01 + (0.5-0.01)/(1+exp(-logitomega.c[c]))
    Romega.c[c] <- 0.01 + (0.5-0.01)/(1+exp(-logitRomega.c[c]))
    logitRomega.c[c] ~ dnorm(Rw.subreg, 1/(pow(sigma.Rwc, 2) + pow(sd_Rw.subreg, 2)))
    s.ct[c,t_star] <- setlevel.c[c]
    setlevel.c[c] ~ dnorm(mean_setlevel, 1/(var_setlevel + pow(sd_mean_setlevel, 2)))
    P.ct[c,t_star] <- 1/(1+exp(-s.ct[c,t_star]))
    RT.c[c] ~ dnorm(RT.subreg, 1/(pow(sigma.RTc, 2) + pow(sd_RT.subreg, 2)))
    unmet.intercept.c[c] ~ dnorm(unmet.subreg, 1/(pow(sigma.unmetc, 2) + pow(sd_unmet.subreg, 2)))
  } # end country loop
  # tau.lrc <- pow(sigma.lrc,-2)
  # tau.lpc <- pow(sigma.lpc,-2)
  # tau.wc <- pow(sigma.wc, -2)
 # tau.Sc <- pow(sigma.Sc,-2)
 # tau.higherSc <- pow(sigma.Sc,-2)
  # tau.Rwc <- pow(sigma.Rwc, -2)
  # tau.RTc <- pow(sigma.RTc, -2)
  # tau.unmetc <- pow(sigma.unmetc,-2)
  #------
  # to export
  for (c in 1:C){
    for (t in 1:nyears){
      mod.ct[c,t] <- P.ct[c,t]*R.ct[c,t]
      trad.ct[c,t] <- P.ct[c,t]*(1-R.ct[c,t])
      unmet.ct[c,t] <- (1-P.ct[c,t])*Z.ct[c,t]
    }
  }
  ",sep="",append=FALSE, file = "model.txt", fill = TRUE)
  
  # DMS
  # update name of observed props to avoid confusion
  if (!old_dm & !nulldata){ # simple
    ## note: does not work yet for partial/all missing
    cat("
    for (i in 1:n_mod){
      modern.i[i] = mod.ct[get_c_i[get_mod_i[i]], get_t_i[get_mod_i[i]]]
      trad.i[i] = trad.ct[get_c_i[get_mod_i[i]], get_t_i[get_mod_i[i]]]
      modern[get_mod_i[i]] ~ dnorm(modern.i[i], prec)
      trad[get_mod_i[i]] ~ dnorm(trad.i[i], prec)
    }
   for (i in 1:n_unmet){
        unmet[get_unmet_i[i]] ~ dnorm(unmet.ct[get_c_i[get_unmet_i[i]], get_t_i[get_unmet_i[i]]], prec)
   }
   for (k in 1:n_ptot){
    ptot.k[k] = mod.ct[get_c_i[get_ptot_i[k]], get_t_i[get_ptot_i[k]]] + trad.ct[get_c_i[get_ptot_i[k]], get_t_i[get_ptot_i[k]]]
    logit.ptot[k] ~ dnorm(logit(ptot.k[k]), prec)
  }
   
  ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
    
  } else if (!nulldata) {
    # need to check if any updates in bounds min/max used here in Mark's most recent version
    # to decide if still to add periods?
    #  for (i in 1:n_mod){
    #  for (h in 1:getperiod.i[i]) {
    #    trad.ih[i, h] = p.ci[getc.i[i], getest.if[i, h]] * (1 - R.ci[getc.i[i], getis.if[i, h]])
    #      modern.ih[i, h] = p.ci[getc.i[i], getest.if[i, h]] * R.ci[getc.i[i], getis.if[i, h]]
    #      unmet.ih[i, h] = (1 - p.ci[getc.i[i], getest.if[i, h]])*(1 / (1 + neg.explogitZ.ci[getc.i[i], getis.if[i, h]]))
    #}
    #trad.i[i] = 1 / period.i[i] * inprod(trad.ih[i, 1:getperiod.i[i]], partialtime.xi[1:getperiod.i[i], i])
    #modern.i[i] = 1 / period.i[i] * inprod(modern.ih[i, 1:getperiod.i[i]], partialtime.xi[1:getperiod.i[i], i])
    #unmet.i[i] = 1 / period.i[i] * inprod(unmet.ih[i, 1:getperiod.i[i]], partialtime.xi[1:getperiod.i[i], i])
    
    cat("
####
# dms
  for (i in 1:n_mod){
# get_mod_i refers to indices with modern+trad use
   ratios.trad.modern.in[get_mod_i[i],1:2] ~ dmnorm(mu.in[get_mod_i[i], ],InvSigma[i,,]) #T.i[i,,])
   InvSigma[i,1:2,1:2] <- inverse(Sigma[i,1:2,1:2])
   Sigma[i,1,2] <- cor.trad.modern.i[get_mod_i[i]]*sqrt(Sigma[i,1,1]*Sigma[i,2,2])
   Sigma[i,2,1] <- cor.trad.modern.i[get_mod_i[i]]*sqrt(Sigma[i,1,1]*Sigma[i,2,2])
   Sigma[i,1,1] <- pow(se_log_r_traditional_no_use[get_mod_i[i]], 2) + pow(nonsample.se.trad.i[get_mod_i[i]],2)
   Sigma[i,2,2] <- pow(se_log_r_modern_no_use[get_mod_i[i]], 2) + pow(nonsample.se.modern.i[get_mod_i[i]],2)
  }
for (i in 1:n_unmet){
# get_unmet_i refers to indices with unmet
 logitratio.yunmet.i[get_unmet_i[i]] ~ dnorm(
   logitratio.yunmet.hat.i[get_unmet_i[i]], 1/(pow(nonsample.se.unmet.i[get_unmet_i[i]],2)+ pow(se_log_r_unmet_no_need[get_unmet_i[i]],2)) )
}

  ", sep = "", append = TRUE, file = "model.txt", fill = TRUE)
    
    
    if (n_ptot > 0) {
      cat(
        "
for (k in 1:n_ptot){
    logit.ptot[get_ptot_i[k]] ~ dnorm(logit.ptothat.i[get_ptot_i[k]], tau.sourcetot)
}

", sep = "", append = TRUE, file = "model.txt", fill = TRUE)
    }


cat(
  "
tau.sourcetot <- pow(sigma.sourcetot,-2)
# end dms for unmet and trad+modern
# this part refer to union of indices in unmet and modern

  for (i in 1:N){
 modern.i[i] <- mod.ct[get_c_i[i], get_t_i[i]]
 trad.i[i] <- trad.ct[get_c_i[i], get_t_i[i]]
 unmet.i[i] <- unmet.ct[get_c_i[i], get_t_i[i]]
 mu.in[i,1] <- log(max(0.0000001, q.ii[1,i])/none.adj.i[i])
 mu.in[i,2] <- log(max(0.0000001, q.ii[2,i])/none.adj.i[i])
 logitratio.yunmet.hat.i[i] <- logit(max(0.0000001,q.ii[3,i])/none.adj.i[i])
    ", sep = "", append = TRUE, file = "model.txt", fill = TRUE)


if (n_ptot > 0) {
  cat(
    "
 logit.ptothat.i[i] <- logit(max(0.0000001, 1-none.adj.i[i]))
 ", sep = "", append = TRUE, file = "model.txt", fill = TRUE)
}


cat(
  "
 
    sump.i[i] <- (trad.i[i]*Vtrad.i[i] + modern.i[i]* Vmodern.i[i]
                        + (1- trad.i[i] - modern.i[i]))
      # old order, 1 is trad!!!
    p.perturb.ii[1,i] <-  trad.i[i]*Vtrad.i[i]/sump.i[i]
      p.perturb.ii[2,i] <-  modern.i[i]* Vmodern.i[i]/sump.i[i]
      p.perturb.ii[3,i] <- unmet.i[i]/sump.i[i]
      p.perturb.ii[4,i] <- (1- trad.i[i] - modern.i[i] - unmet.i[i])/sump.i[i]
      ###Biases
      ##Inclusion of folk methods
      folkbias.i[i] <- step(folk.ind[i]-0.5)*v.folk* p.perturb.ii[3,i]
      ##Absence of probing
      micsbias.i[i] <- step(source.MICS.ind[i]-0.5)* v.mics * p.perturb.ii[1,i]
      ##Sterilization
      modposbias.i[i] <- step(mpos.ind[i]-0.5)*v.mpos* p.perturb.ii[4,i]
      modnegbias.i[i] <- step(mneg.ind[i]-0.5)*v.mneg * p.perturb.ii[2,i]
      ####Perturbed proportions adjusted for biases (1-4)
      q.ii[1,i] <- p.perturb.ii[1,i] - micsbias.i[i] + folkbias.i[i]
      q.ii[2,i] <- p.perturb.ii[2,i] + modposbias.i[i] - modnegbias.i[i]
      q.ii[3,i] <- p.perturb.ii[3,i] + micsbias.i[i] - folkbias.i[i]
      q.ii[4,i] <- p.perturb.ii[4,i] - modposbias.i[i] + modnegbias.i[i]
      none.adj.i[i] <- max(0.0000001, q.ii[3,i] + q.ii[4,i]) #la 2019/3/13
      Vtrad.i[i] <-  (
      V.geo.12i[1,geo.ind[i]]
      * V.age.12i[1,age.ind[i]]
      * V.hw.12i[1,hw.ind[i]]
      * V.emal.12i[1,emal.ind[i]]
      * V.sa.12i[1,sa.ind[i]]
      * V.posbias.12i[1,posbias.ind[i]]
      * V.posage.12i[1, posage.ind[i]]
      * V.negage.12i[1, negage.ind[i]]
      )
      Vmodern.i[i] <- (
      V.geo.12i[2,geo.ind[i]]  ##geographical region
      * V.age.12i[2,age.ind[i]] #Age group different from base (bias unknown)
      * V.hw.12i[2,hw.ind[i]] ##Husband and wives or both
      * V.emal.12i[2,emal.ind[i]] ##Ever married, all women
      * V.sa.12i[2,sa.ind[i]] ##All sexually active
      * V.posbias.12i[2,posbias.ind[i]] ## Non-pregnant/fertile/married SA women
      * V.posage.12i[2, posage.ind[i]] ##Age group with positive bias
      * V.negage.12i[2, negage.ind[i]] ##Age group with negative bias
      )
  }
# add dummy column in case ncol(V) = 1 => V becomes vector!
V.geo.12i[1,max(geo.ind)+1] <- 0
      V.age.12i[1,max(age.ind)+1] <- 0
      V.hw.12i[1,max(hw.ind)+1] <- 0
      V.emal.12i[1,max(emal.ind)+1] <- 0
      V.sa.12i[1,max(sa.ind)+1] <- 0
      V.posbias.12i[1,max(posbias.ind)+1] <- 0
      V.posage.12i[1,max(posage.ind)+1] <- 0
      V.negage.12i[1,max(negage.ind)+1] <- 0
      V.geo.12i[2,max(geo.ind)+1] <- 0
      V.age.12i[2,max(age.ind)+1] <- 0
      V.hw.12i[2,max(hw.ind)+1] <- 0
      V.emal.12i[2,max(emal.ind)+1] <- 0
      V.sa.12i[2,max(sa.ind)+1] <- 0
      V.posbias.12i[2,max(posbias.ind)+1] <- 0
      V.posage.12i[2,max(posage.ind)+1] <- 0
      V.negage.12i[2,max(negage.ind)+1] <- 0
# Multipliers V in [0,inf): geo, emal, hw, age other, sa (for trad only)
####All of these should get a log normal distribution....
        V.sa.12i[1,1] <- 1  ##Sexually active women (trad)
        V.geo.12i[1,1] <- 1 ##Geographical region (trad)
        V.geo.12i[2,1] <- 1 ##Geographical region (mod)
        V.age.12i[1,1] <- 1 ##Age different (trad)
        V.age.12i[2,1] <- 1 ##Age different (mod)
        V.hw.12i[1,1] <- 1  ##Husband/wives (trad)
        V.hw.12i[2,1] <- 1  ##Husband/wives (mod)
        V.emal.12i[1,1] <- 1 ##ever married/ all women (trad)
        V.emal.12i[2,1] <- 1 ##evr married/ all women (mod)
        
for (m in 1:2){
        for (i in 2:ncat.geo){
        V.geo.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
        }
        for (i in 2:ncat.age){
        V.age.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
        }
        tau.geo.m[m] <- pow(sigma.geo.m[m], -2)
        }
        for (i in 2:ncat.sa){
        V.sa.12i[1,i] ~ dlnorm(0, tau.geo.m[1])
        }
        V.sa.12i[2,1] <- 1
        V.posbias.12i[1,1] <- 1
        V.posbias.12i[2,1] <- 1
        V.posage.12i[1,1] <- 1
        V.posage.12i[2,1] <- 1
        V.negage.12i[1,1] <- 1
        V.negage.12i[2,1] <- 1
        # m = 2:
        for (i in 2:ncat.sa){
        W.sa.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
        V.sa.12i[2,i] <- 1+W.sa.12i[2,i]
        }
        for (i in 2:ncat.posbias){
        W.posbias.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
        V.posbias.12i[2,i] <- 1+W.posbias.12i[2,i]
        }
        for (i in 2:ncat.posage){
        W.posage.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
        V.posage.12i[2,i] <-1+W.posage.12i[2,i]
        }
        for (i in 2:ncat.negage){
        W.negage.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
        V.negage.12i[2,i] <- 1/(1+W.negage.12i[2,i])
        }
        tau.pos <- pow(sigma.pos, -2)
        # m=1
        # note: could simplify code and throw out these V's
        for (i in 2:ncat.posbias){
        V.posbias.12i[1,i] <- 1+exp(mu.pos.m[1])
        }
        for (i in 2:ncat.posage){
        V.posage.12i[1,i] <- 1+exp(mu.pos.m[1])
        }
        for (i in 2:ncat.negage){
        V.negage.12i[1,i] <- 1/(1+exp(mu.pos.m[1]))
        }
  ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
  }
if (is_in_union == "Y" & !nulldata) {
  cat("
        for(k in 1:n.training.modonly) {
            logit.ymodonly.i[geti.training.modonly.k[k]] ~ dnorm(logit(q.ii[2,geti.training.modonly.k[k]]), tau.sourcemodonly)
        }
        ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
}
if (is_in_union == "Y") {
  cat("
        for (m in 1:2){
        for (i in 2:ncat.emal){
        V.emal.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
        }
        for (i in 2:ncat.hw){
        V.hw.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
        }
        }
  
  
  ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
} else {
  cat("
  ## m = 1
  for (i in 2:ncat.emal){
    V.emal.12i[1,i] <- 1/(1+exp(mu.pos.m[1]))
  }
  
  ## m = 2
  for (i in 2:ncat.emal){
    W.emal.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
    V.emal.12i[2,i] <- 1/(1+W.emal.12i[2,i])
  }
  
  ## m = 1
  for (i in 2:ncat.hw){
    V.hw.12i[1,i] <- 1+exp(mu.pos.m[1])
  }
  
  ## m = 2
  for (i in 2:ncat.hw){
    W.hw.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
    V.hw.12i[2,i] <- 1+W.hw.12i[2,i]
  }
  ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
  
}


if (include_ss_data) {
  cat("
      for (k in 1:(n_ss-1)) {
        delta_emu[k] ~ dnorm(
        delta_modern[k], tau_emu[k])
        delta_modern[k] <- mod.ct[1,get_t_ss[k+1]] - mod.ct[1,get_t_ss[k]]
        tau_emu[k] <- pow(se_emu[k], -2)
}
",sep="",append=TRUE, file = "model.txt", fill = TRUE)
}
cat("} # end model",sep="",append=TRUE, file = "model.txt", fill = TRUE)
} # end write model function