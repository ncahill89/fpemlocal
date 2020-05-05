#' read_rda
#'
#' A function to read rda files
#'
#' @param path 
#'
#' @return The object retrieved from the file path
#'
#' @export
read_rda <- function(
  path
) {
  if (missing(path)) {
    stop("Calling function must provide file path.")
  }
  e <- new.env()
  load(path, env = e)
  loadedobjects <- ls(e, all=TRUE)
  stopifnot(length(loadedobjects)==1)
  e[[loadedobjects]]
}



