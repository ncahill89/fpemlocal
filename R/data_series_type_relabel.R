#' index_datatype_supplement
#'
#' @param data  \emph{\sQuote{Data.frame}} The processed data associated with the model run from \code{\link[core_data]{core_data}}
#'
#' @return data  \emph{\sQuote{Data.frame}} The processed data associated with the model run from \code{\link[core_data]{core_data}}
#'

data_Series_type_relabel <- function(data) {
  store <- data
  index <- index_m
  store$index_datatype <- 1
  store$index_datatype_unmet <- 1
  data$data_series_type_unmet <- data$data_series_type
  data$data_series_type[data$data_series_type == "National survey"] <- "NS"
  data$data_series_type_unmet[data$data_series_type %in% c("NS","MICS","PMA") ] <- "Other"

  for(i in 1:nrow(data)){
    store$index_datatype[i] <- index$index_datatype$map_cp$index_datatype[data$data_series_type[i] == index$index_datatype$map_cp$name_datatype] %>% as.character() %>% as.numeric()
    store$index_datatype_unmet[i] <- index$index_datatype$map_unmet$index_datatype[data$data_series_type_unmet[i] == index$index_datatype$map_unmet$name_datatype] %>% as.character() %>% as.numeric()
  }
  cp_low_ind <- store$contraceptive_use_any < 0.01
  union_ind <- store$is_in_union == "N"
  store$index_datatype[cp_low_ind & union_ind] <- 5
  return(store)
}
