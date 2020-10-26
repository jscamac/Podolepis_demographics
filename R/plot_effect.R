#' Plot the posterior distributions of modelled effects
#'
#' Plot the posterior distributions of modelled effects
#' intervals
#' @param model A 3-D array, matrix, list of matrices, or data frame of MCMC draws.
#' e.g. a `stanreg` or `brmsfit` object
#' @regex Character vector. A regular expression patterns identifying the
#' parameter names
#' @par_labels Optional named character vector of replacement parameter names
#' @prob_threshold The probability mass shaded
#' @param xlabel Character. X axis label
#' @param outfile Character. Output image file path. Containing directory
#'   will be created recursively if it does not already exist.
#' @param width Width of plot. If not defined will use size of current graphic
#'   device.
#' @param height Height of plot. If not defined will use size of current 
#'  graphic device.
#' @param units Character. Units corresponding to \code{height} and 
#'   \code{width}. Can be \code{"in"}, \code{"cm"}, or \code{"mm"}. Default is 
#'   inches (\code{"in"}).
#' @return An image is written to \code{outfile} if provided, and otherwise a
#'   \code{ggplot} object is returned.
#' @importFrom ggplot2 scale_y_discrete geom_vline xlab ggsave
#' @importFrom bayesplot mcmc_areas
#' @export

plot_effects <- function(model, pars, par_labels, 
                         prob_threshold = 0.95, xlabel = "Effect",
                         outfile, width, height, units) {
  
  p1 <- bayesplot::mcmc_areas(model, 
                              pars = pars, 
                              area_method = "equal height",
                              prob = 0.95) +
    ggplot2::geom_vline(xintercept = 0, col='red') +
    ggplot2::xlab(xlabel) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  
  if(!missing(par_labels)) {
    p1 <- p1 +
      ggplot2::scale_y_discrete(labels= par_labels, limits = rev(names(par_labels)))
  }
  
  # outfile supplied
  if(!missing(outfile)) {
    
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    ggplot2::ggsave(filename =  outfile, width = width, height = height, 
                    units = units, plot = p1)
  } else {
    p1 
  }
}
