
get_max_1 <- function(wide_data, var) {
  temp <- wide_data[complete.cases(wide_data %>% dplyr::select(!! var)),] %>%
    dplyr::group_by(division_numeric_code) %>%
    dplyr::summarise(max = max(!!var))
  return(temp)
}




# return a list of two vectors of div codes, one vector of div codes which require method 1 imputation and another for method 2 imputation
method_divs <- function(wide_data, var) {
  #var <- rlang::ensym(var)
  wide_data$ones <- 1
  temp <- wide_data %>%
    dplyr::group_by(division_numeric_code) %>%
    dplyr::mutate(sum_na = sum(is.na(!! var)))  %>%
    dplyr::mutate(sum_obs = sum(ones))
  div_impute_1 <- temp$division_numeric_code[temp$sum_na == temp$sum_obs] %>%
    unique() %>%
    unlist()
  div_impute_2 <- temp$division_numeric_code[temp$sum_na != temp$sum_obs] %>%
    unique() %>%
    unlist()
  div_list <- list(div_impute_1 = div_impute_1, div_impute_2 = div_impute_2)
  return(div_list)
}



# return data frame with inputed values for var specified, implements method 1
impute_1 <- function(wide_data, var, method_divs) {
  #var <- rlang::ensym(var)
  med <- wide_data %>% get_max_1(var) %>%
    dplyr::select("max") %>%
    unlist() %>%
    as.numeric() %>%
    median()
  core_var <- wide_data %>%
    dplyr::select(!! var) %>% # a vector of the SE of interest
    dplyr::ungroup() %>%
    unlist()
  core_var[wide_data$division_numeric_code %in% method_divs$div_impute_1] <- med
  wide_data[paste(var)] <- core_var
  return(wide_data)
}


# return data frame with imputed values for var specified, implements method 2
impute_2 <- function(wide_data, var, method_divs) {
  #var <- rlang::ensym(var)
  char <- rlang::quo_text(var)
  #var <- rlang::enquo(var)
  div <- method_divs$div_impute_2
  var_vec <- wide_data %>% dplyr::select(!! var)
  imp_i <- wide_data %>%
    dplyr::filter(division_numeric_code %in% div) %>%
    dplyr::group_by(division_numeric_code) %>%
    dplyr::summarize(med = max(!! var, na.rm=TRUE))

  for(i in 1:dim(wide_data)[1]) {
    if (is.na(var_vec[i,]) &
        wide_data$division_numeric_code[i] %in% div) {
      imp <- imp_i %>%
        dplyr::filter(division_numeric_code == wide_data$division_numeric_code[i]) %>%
        dplyr::ungroup() %>%
        dplyr::select(med) %>%
        unlist()
      wide_data[char][i,] <- imp
    }
  }
  return(wide_data)
}



get_global_med_se <- function(wide_data, var) {
  med <- wide_data %>% get_max_1(var) %>%
    dplyr::select("max") %>%
    unlist() %>%
    as.numeric() %>%
    median()
  return(med)
}


impute_global_se <- function(wide_data) {
  vars <- list(
    rlang::quo(se_log_r_unmet_no_need),
    rlang::quo(se_log_r_traditional_no_use),
    rlang::quo(se_log_r_modern_no_use)
  )
  ints <- as.integer(NA)
  medians <- list(
    se_log_r_unmet_no_need = ints,
    se_log_r_traditional_no_use = ints,
    se_log_r_modern_no_use = ints
  )
  for(i in 1:length(vars)) {
    medians[[i]] <- get_global_med_se(wide_data, vars[[i]])
  }
  return(medians)
}

#' Impute se
#'
#' Imputes missing se based on Cahill et al 2017
#'
#' @param wide_data \emph{'Data.frame'} A data.frame from \code{\link[contraceptive_use]{contraceptive_use}}
#'
#' @return \emph{'Data.frame'} The input data with se imputed
#
impute_se <- function(wide_data) {
  vars <- list(
    rlang::quo(se_log_r_unmet_no_need),
    rlang::quo(se_log_r_traditional_no_use),
    rlang::quo(se_log_r_modern_no_use)
  )
  for(i in 1:length(vars)) {
    temp <- method_divs(wide_data, var = vars[[i]])
    wide_data <- impute_1(wide_data, var =  vars[[i]], method_divs = temp)
    wide_data <- impute_2(wide_data, var = vars[[i]], method_divs = temp)
  }
  wide_data$`~` <- NULL
  return(wide_data)
}


# The input data augmented with imputed max se
gen_max_se_data <- function(wide_data) {
  vars <- list(
    rlang::quo(se_log_r_unmet_no_need),
    rlang::quo(se_log_r_traditional_no_use),
    rlang::quo(se_log_r_modern_no_use)
  )
  ints <- rep(as.integer(NA),nrow(unique(wide_data["division_numeric_code"])))
  df <- data.frame(division_numeric_code = ints,
                   se_log_r_unmet_no_need = ints,
                   se_log_r_traditional_no_use = ints,
                   se_log_r_modern_no_use = ints
  )
  for(i in 1:length(vars)) {
    char <- rlang::quo_text(vars[[i]])
    temp <- wide_data %>% get_max_1(vars[[i]])
    df[,1] <- temp["division_numeric_code"] %>%
      unlist() %>%
      as.numeric()
    df[,i+1] <- temp["max"] %>%
      unlist() %>%
      as.numeric()
  }
  return(df)
}





#' Impute user survey data standard errors
#' 
#' 
#' If a user provides data where standard error (SE) columns have missing values, the 
#' corresponding pre-calculated SE from UNPD data are used to impute the missing values. 
#' The imputation of UNPD survey data is described at the bottom of this document in
#' the details section.
#' 
#' Imputation of (SE) variables is carried out annually with UNPD survey data \code{\link{fit_fp_csub}} 
#' as [@Cahill et al 2017 appendix page 16]. The summary of this procedure is described 
#' below. 
#' 
#' There are two scenarios. Each scenario has a corresponding procedure.
#' \describe{
#'     \item{1. (Completely missing) all entries missing for a coutnry's SE variable}
#'     \item{2. (Partially missing) some entries missing for a country's SE variable }
#' }
#' 
#' For scenario 1, the imputation is carried out by calculating the maximum of known 
#' sampling errors across all other countries and setting the unknown sampling errors 
#' equal to the median of these maximums.
#' 
#' For scenario 2 we impute the sampling errors by setting them equal to the maximum 
#' of the known sampling errors in that country.
#' 
#' 
#' @param user_data \emph{\sQuote{Data.frame}} Survey data such as \code{\link{contraceptive_use}}.
#' @inheritParams fit_fp_c
#'
#' @return \emph{\sQuote{Data.frame}} Imputed survey data
#' @export
#'
impute_user_se <- function(user_data, subnational, is_in_union) {
  user_data <- user_data %>% as.data.frame()
  div <- user_data["division_numeric_code"]
  div <- div[1,]
  if (subnational) {
    imputed_max_se <- imputed_data$medians %>%
      dplyr::filter(is_in_union == !!is_in_union)
    vars <- names(imputed_max_se)[1:3]
  } else {
    imputed_max_se <- imputed_data$imputed_max_se %>% #comes from gen_max_se
      dplyr::filter(division_numeric_code == div) %>%
      dplyr::filter(is_in_union == !!is_in_union)
    vars <- colnames(imputed_max_se)[2:4]
  }
  user_data %>%
    dplyr::mutate(se_log_r_modern_no_use_impute_ind := is.na(!!rlang::sym(vars[1]))) %>%
    dplyr::mutate(se_log_r_traditional_no_use_impute_ind := is.na(!!rlang::sym(vars[2]))) %>%
    dplyr::mutate(se_log_r_unmet_no_need_impute_ind := is.na(!!rlang::sym(vars[3])))
  if (all(!(vars %in% colnames(user_data)))) {
    warning("user_data does not have sampling error columns")
    for(i in 1:length(vars)) {
      user_data[vars[i]] <- rep(imputed_max_se[vars[i]], nrow(user_data))
    }
  } else {
    for(i in 1:length(vars)) {
      user_data[vars[i]][is.na(user_data[vars[i]])] <- unlist(imputed_max_se[vars[i]])
    }
  }
  return(user_data)
}


#' Impute indicator
#'
#' Adds indicator columns to indicate if a value has been imputed
#'
#' @param wide_data \emph{'Data.frame'} A data.frame from \code{\link[contrapcetive_use]{contrapcetive_use}}
#'
#' @return \emph{'Data.frame'} A data.frame with logical indicator TRUE indicating it is imputed FALSE being a core value
impute_indicator <- function(data) {
  var_names <- c(
    "se_log_r_modern_no_use",
    "se_log_r_traditional_no_use",
    "se_log_r_unmet_no_need"
  )
  data %>%
    dplyr::mutate(se_log_r_modern_no_use_impute_ind := is.na(!!rlang::sym(var_names[1]))) %>%
    dplyr::mutate(se_log_r_traditional_no_use_impute_ind := is.na(!!rlang::sym(var_names[2]))) %>%
    dplyr::mutate(se_log_r_unmet_no_need_impute_ind := is.na(!!rlang::sym(var_names[3])))
}


impute_package_data <- function(is_in_union) {
  wide_data_imputed <- contraceptive_use %>%
    dplyr::filter(is_in_union == !!is_in_union) %>%
    dplyr::filter(age_range == "15-49") %>%
    impute_indicator() %>%
    impute_se()
  imputed_max_se <-
    gen_max_se_data(wide_data = wide_data_imputed)
  medians <-
    impute_global_se(contraceptive_use %>%
                                  dplyr::filter(is_in_union == !!is_in_union))
  return(
    list(
      imputed_max_se = imputed_max_se,
      medians = medians,
      contraceptive_use_imputed = wide_data_imputed
    )
  )
}
