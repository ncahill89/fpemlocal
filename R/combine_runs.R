#' combine_runs
#'
#' @param codes
#'
#' @export
combine_runs <- function (codes)
{

  load(paste0("post_samps_", codes[1],".rda"))

  post_samps_combine <- array(NA, c(length(codes),
                                    dim(post_samps)[2:4]))
  post_samps_combine[1, , , ] <- post_samps
  for (c in 2:length(codes)) {
    load(paste0("post_samps_", codes[c], ".rda"))

    post_samps_combine[c, , , ] <- post_samps
    dimnames(post_samps_combine) <- list(codes, NULL, NULL, NULL)

  }
  return(post_samps_combine)
}
