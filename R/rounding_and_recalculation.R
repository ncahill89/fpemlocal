#' round_from_zero
#'
#' @param x 
#' @param UNPD_zero 
round_from_zero <- function(x) {
  min_round <- 0.001
  x[x < min_round] <- min_round
  return(x)
}


indicate_rounding <- function(x) {
  min_round <- 0.001
  ind <- as.numeric(x < min_round)
  return(ind)
}


# CP Trad to (CP Any - CP Modern) for obs with both modern and any but no trad
# If there are values for CP Any and CP Mod, not equal, but CP
ad_hoc_calculate_cp_trad <- function(obs) {
  obs <- obs %>%
    dplyr::mutate(contraceptive_use_traditional = ifelse(
      !is.na(contraceptive_use_any) & !is.na(contraceptive_use_modern) & is.na(contraceptive_use_traditional) & (contraceptive_use_modern != contraceptive_use_any), 
      contraceptive_use_any - contraceptive_use_modern, 
      contraceptive_use_traditional)
    )
    return(obs)
}

# re-calculated CP Any if CP Mod or CP Trad had been round up to make sure CP Any still equaled the sum of modern and traditional
ad_hoc_recalculate_cp_any <- function(obs) {
  obs <- obs %>%
    dplyr::mutate(recalc_cpany_indicator = (indicate_rounding_mod | indicate_rounding_trad) & !is.na(contraceptive_use_modern) & !is.na(contraceptive_use_traditional)) %>%
    dplyr::mutate(contraceptive_use_any = ifelse(recalc_cpany_indicator, 
                                                 contraceptive_use_modern + contraceptive_use_traditional, 
                                                 contraceptive_use_any
                                                 )
                  )
  return(obs)
}

# Blank out mod use if mod = any
ad_hoc_blankmodern_ifequals <- function(obs) {
  obs <- obs %>%
  dplyr::mutate(
    contraceptive_use_modern = ifelse(
      contraceptive_use_any == contraceptive_use_modern &
        is.na(contraceptive_use_traditional) &
        !is.na(contraceptive_use_any) &
        !is.na(contraceptive_use_modern),
      NA,
      contraceptive_use_modern
    )
  )
  return(obs)
}

