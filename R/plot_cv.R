#' Plot CV Expected Log Pointwise Predictive Density (ELPD) estimates
#'
#' Plots CV Expected Log Pointwise Predictive Density (ELPD) estimates with 95%
#' confidence intervals for each model
#' @param df A `data.frame` containing elpd estimates (and CI limits) for
#' derived from `collate_loo_stats()`
#' @param model_labs A named vector of model names and the new supplied name. 
#' e.g. c("mod1" = expression(alpha), "mod2" = expression(beta))
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
#' @importFrom ggplot2 ggplot, aes, geom_point, geom_linerange, scale_x_discrete
#' theme_classic xlab ylab coord_flip ggsave
#' @export
plot_cv <- function(df, 
                    model_labs, 
                    outfile, 
                    width = NA, 
                    height = NA, 
                    units = c("in", "cm", "mm")) {
  
  p1 <- ggplot2::ggplot(data = df, ggplot2::aes(x= reorder(Model, Rank), y = Estimate)) +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lci, ymax=uci)) +
    ggplot2::scale_x_discrete(labels= model_labs) +
    ggplot2::theme_classic() +
    ggplot2::coord_flip() +
    ggplot2::ylab("ELPD") +
    ggplot2::xlab(NULL)
  
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