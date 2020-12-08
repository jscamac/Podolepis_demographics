#' Plot conditional survival curves
#'
#' Plot conditional survival curves with 95 CI intervals
#' @param model A `stanreg` object
#' @param new_data a data frame in which to look for variables with which to predict.
#' Each row can be a separate individual with different variable values. new_data must contain
#' a `key` column in order to appropriately label the legend keys.
#' If newdata is provided and any variables were transformed (e.g. rescaled) 
#' in the data used to fit the model, then these variables must also be transformed in new_data.
#' @param xlabel Character.
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
#' @importFrom ggplot2 ggplot, aes, geom_point, geom_segment, theme_classic
#' ggsave geom_vline ylab xlab element_text theme
#' @importFrom dplyr mutate filter arrange row_number
#' @importFrom magrittr %>%
#' @export
plot_survcurve <- function(model, 
                           new_data,
                           max_time,
                           legend_label = NULL, 
                           xlabel = "time",
                           outfile,
                           width,
                           height,
                           units) {
  
  preds <- rstanarm::posterior_survfit(model, newdata = new_data, prob = 0.95) %>%
    dplyr::mutate(key = as.factor(new_data$key[.$id]))
  
  p1 <- ggplot2::ggplot(data = preds, ggplot2::aes(x=time, group = key, col = key, fill=key)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lb, ymax=ci_ub), alpha = 0.5) +
    ggplot2::geom_line(aes(y=median)) +
    ggplot2::scale_fill_brewer(name = legend_label, palette = "Set2", aesthetics = c("colour", "fill")) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::ylab("Pr(Survival)") +
    ggplot2::xlab(xlabel) +
    ggplot2::theme_classic()
  
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