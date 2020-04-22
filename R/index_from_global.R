
#' index_datatype
#'
#' Creates a list of data.frames for datatype codes used in the UNPD global run. These codes differ for contraceptive use and unmet data.
#'
#' @param meta \emph{'Data.frame'} A data.frame with meta data from a global UNPD run
#'
#' @return \emph{'List'}
#' \enumerate{
#'   \item \strong{map_cp}  \emph{'Data.frame'} A data.frame with source data corresponding to a global run for contracpetive data
#'   \item \strong{map_unmet}   \emph{'Data.frame'} A data.frame with source data corresponding to a global run for unmet data
#' }
#' @export
index_datatype <- function(meta) {
  num_source_map_cp <- matrix(NA, nrow = length(unique(meta$data.raw$data$source.j)), ncol = 2)
  num_source_map_cp[,1] <- unique(meta$data.raw$data$source.j)
  for(i in 1:length(unique(meta$data.raw$data$source.j))) {
    num_source_map_cp[i,2] <- unique(meta$winbugs.data$source.ind.j[unique(meta$data.raw$data$source.j)[i] == meta$data.raw$data$source.j])[1]
  }
  num_source_map_cp <- data.frame(name_datatype = num_source_map_cp[,1], index_datatype = num_source_map_cp[,2])

  num_source_map_unmet<- matrix(NA, nrow = length(unique(meta$data.raw$data$source.unmet.j)), ncol = 2)
  num_source_map_unmet[,1] <- unique(meta$data.raw$data$source.unmet.j)
  for(i in 1:length(unique(meta$data.raw$data$source.unmet.j))) {
    num_source_map_unmet[i,2] <- unique(meta$winbugs.data$source.ind.unmet.j[unique(meta$data.raw$data$source.unmet.j)[i] == meta$data.raw$data$source.unmet.j])[1]
  }
  num_source_map_unmet <- data.frame(name_datatype = num_source_map_unmet[,1], index_datatype = num_source_map_unmet[,2])
  return(list(map_cp = num_source_map_cp, map_unmet = num_source_map_unmet))
}




#' index_area_mwra
#'
#' Creates a data.frame similar to divisions data with codes used in the UNPD global run
#'
#' @param meta \emph{'Data.frame'} A data.frame with meta data from a global UNPD run
#'
#' @return \emph{'Data.frame'} A data.frame with division data corresponding to a global run
index_area_mwra <- function(meta) {
  index1 <- data.frame(
    division_numeric_code = meta$data.raw$country.info$iso.c,
    name = meta$data.raw$country.info$name.c,
    subreg.c = meta$data.raw$country.info$subreg.c,
    reg.c = meta$data.raw$country.info$reg.c
  )
  index2 <- data.frame(
    division_numeric_code =  meta$data.raw$country.info.no.data$iso.c,
    name =  meta$data.raw$country.info.no.data$name.c,
    subreg.c = meta$data.raw$country.info.no.data$subreg.c,
    reg.c = meta$data.raw$country.info.no.data$reg.c
  )
  index <- rbind(index1, index2)
  return(index)
}

#' index_area_uwra
#'
#' Creates a data.frame similar to divisions data with codes used in the UNPD global run
#'
#' @param meta \emph{'Data.frame'} A data.frame with meta data from a global UNPD run
#'
#' @return \emph{'Data.frame'} A data.frame with division data corresponding to a global run
index_area_uwra <- function(meta) {
  index1 <- data.frame(
    division_numeric_code = meta$data.raw$country.info$iso.c,
    name = meta$data.raw$country.info$name.c,
    subreg.c = meta$data.raw$country.info$reg.in.sex.ac.unm.SA1sub.c,
    reg.c = meta$data.raw$country.info$reg.in.sex.ac.unm.SA1sub.c
  )
  index2 <- data.frame(
    division_numeric_code =  meta$data.raw$country.info.no.data$iso.c,
    name =  meta$data.raw$country.info.no.data$name.c,
    subreg.c = meta$data.raw$country.info.no.data$reg.in.sex.ac.unm.SA1sub.c,
    reg.c = meta$data.raw$country.info.no.data$reg.in.sex.ac.unm.SA1sub.c
  )
  index <- rbind(index1, index2)
  return(index)
}

#' index_area
#'
#' Creates a data.frame similar to divisions data with codes used in the UNPD global run
#'
#' @param meta \emph{'Data.frame'} A data.frame with meta data from a global UNPD run
#' @param run_type \emph{'Character string'} "mwra" or "uwra"
#'
#' @return \emph{'Data.frame'} A data.frame with division data corresponding to a global run
#' @export
index_area <- function(meta, run_type) {
  if (run_type == "mwra"){
    index <- index_area_mwra(meta)
  } else {
    index <- index_area_uwra(meta)
  }
  return(index)
}
