#' plot country results
#'
#' @inherit fpem_plots
#' @export
fpem_get_plots_autosave <- function(
  runname,
  ...
) {
  runlist <- readRDS(file.path("runs", paste0(runname, ".rds")))
  results <- readRDS(file.path("results", paste0(runname, ".rds")))
  

  plotlist <- fpem_get_plots(runlist = runlist,
                             results = results,
                     indicators = indicators)
  if (!dir.exists("plots"))
    dir.create("plots")
  pdf(file.path("plots", paste0(runname, ".pdf")))
  for (i in 1:length(plotlist)) {
    plots <- plotlist[[i]]
    gridExtra::grid.arrange(grobs=plots[1:length(indicators)],
                            ncol=2,
                            top=runlist[[i]]$core_data$is_in_union)
  }
  dev.off()
}


#' plot country results
#'
#' @inherit fpem_plots
#' @export
fpem_get_plots <- function(
  ...
) {
  UseMethod("fpem_get_plots")
}


#' plot country results
#'
#' @inherit fpem_plots
#' @export
fpem_get_plots.single_union <- function(
  runlist,
  results,
  indicators
) {
  plotlist <- fpem_plots(
    runlist,
    results,
    indicators
  )
  return(plotlist)
}


#' plot country results
#'
#' @inherit fpem_plots
#' @export
fpem_get_plots.all_union <- function(
  runlist,
  results,
  indicators
) {
  plotlisty <- fpem_plots(
    runlist$runy,
    results$resultsy,
    indicators
  )
  plotlistn <- fpem_plots(
    runlist$runn,
    results$resultsn,
    indicators
  )
  plotlistall <- fpem_plots(
    runlist$runall,
    results$resultsall,
    indicators
  )
  return(list(
    plotlisty = plotlisty,
    plotlistn = plotlistn,
    plotlistall = plotlistall
  ))
}