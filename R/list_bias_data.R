#' list_bias_data
#'
#' jags data, numeric form of bias using \code{\link{core_data}}.
#'
#' @param core_data \emph{\sQuote{Data.frame}} The processed data associated with the model run from \code{\link{core_data}}.
#'
list_bias_data <- function(core_data){
  dat <- core_data$observations
  # get bias/mult info
  # is NOT yet set up to work for more than 1 country
  # (same multiplier would be assigned)
  # following old jags model implementation here
  # where 1 refers to no multiplier added
  data_biasmult <- dat %>%
    dplyr::mutate(geo.ind = ifelse(has_geographical_region_bias == "N",
                                   1,
                                   1+ 1)) %>%
    dplyr::mutate(hw.ind = ifelse(is.element(group_type_relative_to_baseline, c("BS", "HW", "PW")), 2, 1)) %>%
    dplyr::mutate(emal.ind = ifelse(is.element(group_type_relative_to_baseline, c("AL", "EM", "FM")), 2, 1)) %>%
    dplyr::mutate(sa.ind = ifelse(is.element(group_type_relative_to_baseline, c("SA")), 2, 1)) %>%
    dplyr::mutate(posbias.ind = ifelse(has_non_pregnant_and_other_positive_biases == "N", 1,
                                       1+ 1)) %>%
    dplyr::mutate(age.ind = ifelse(age_group_bias != "?", 1, 2)) %>% # again, 1 is baseline
    dplyr::mutate(posage.ind = ifelse(age_group_bias != "+", 1, 2)) %>% # again, 1 is baseline
    dplyr::mutate(negage.ind = ifelse(age_group_bias != "-", 1, 2)) %>% # again, 1 is baseline
    dplyr::mutate(folk.ind = ifelse(has_traditional_method_bias == "Y", 1, 0)) %>%
    dplyr::mutate(source.MICS.ind = ifelse(has_absence_of_probing_questions_bias == "Y", 1, 0)) %>%
    dplyr::mutate(mpos.ind = ifelse(modern_method_bias == "+", 1, 0)) %>%
    dplyr::mutate(mneg.ind = ifelse(modern_method_bias == "-", 1, 0)) %>%
    dplyr::select(geo.ind, hw.ind, emal.ind, sa.ind, posbias.ind, age.ind, posage.ind, negage.ind,
                  folk.ind, source.MICS.ind, mpos.ind, mneg.ind)

  maxcat <- apply(data_biasmult, 2, max) # relevant for multipliers

  jagsdata_add <- c(
    as.list(data_biasmult),
    list(
      ncat.geo = maxcat['geo.ind'],
      ncat.hw = maxcat['hw.ind'],
      ncat.emal = maxcat['emal.ind'],
      ncat.sa = maxcat['sa.ind'],
      ncat.posbias = maxcat['posbias.ind'],
      ncat.age = maxcat['age.ind'],
      ncat.posage = maxcat['posage.ind'],
      ncat.negage  = maxcat['negage.ind']
    )
  )
  return(jagsdata_add)
}
