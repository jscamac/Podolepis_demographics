#' Plot random effect estimates from model
#'
#' Plot random effect estimates from model with 95% credible and 50% credible
#' intervals
#' @param model A `stanreg` object
#' @param type Character. The name of the random variable to plot.
#' Can be one of "origin_code", "commodity_code" or "year"
#' @param ylabel Character.
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

plot_ranefs <- function(model, 
                        type,
                        ylabel = NULL,
                        outfile,
                        width,
                        height,
                        units = c("in","cm","mm")) {
  
  if(!type %in% c("ind_id", "site_id")) {
    stop("Type can only be one of 'ind_id'
       'site_id")
  }
  
  dat <- tibble::as_tibble(brms::posterior_summary(model, probs = c(0.025,0.25,0.75, 0.975)), 
                           rownames ="predictor")
  
  if(type =="ind_id") {
    
    dat <- dat %>% 
      # Filter to commodity codes
      dplyr::filter(grepl("ind_id",predictor) == TRUE) %>%
      # Filter out Sigma estimates
      dplyr::filter(grepl("Sigma", predictor)  == FALSE) %>%
      dplyr::filter(grepl("sd_", predictor) == FALSE) %>%
      # Filter out marginal effect
      dplyr::filter(grepl("NEW", predictor) == FALSE) %>%
      dplyr::mutate(predictor = paste("Ind",gsub("\\D", "", predictor))) %>%
      dplyr::arrange(Estimate) %>%
      dplyr::mutate(rank = dplyr::row_number())
  }
  
  if(type =="site_id") {
    
    dat <- dat %>% 
      # Filter to commodity codes
      dplyr::filter(grepl("site_id",predictor) == TRUE) %>%
      # Filter out Sigma estimates
      dplyr::filter(grepl("Sigma", predictor)  == FALSE) %>%
      dplyr::filter(grepl("sd_", predictor) == FALSE) %>%
      # Filter out marginal effect
      dplyr::filter(grepl("NEW", predictor) == FALSE) %>%
      dplyr::mutate(predictor = paste("Site",gsub("\\D", "", predictor))) %>%
      dplyr::arrange(Estimate) %>%
      dplyr::mutate(rank = dplyr::row_number())
  }
  
  p1 <- ggplot2::ggplot(data = dat, aes(y=reorder(predictor,rank), x=Estimate)) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(ggplot2::aes(x=Q2.5,y=predictor, xend= Q97.5, yend=predictor)) +
    ggplot2::geom_segment(ggplot2::aes(x=Q25,y=predictor, xend=Q75, yend=predictor), size=1) +
    geom_vline(xintercept=0, col='red', lty=2) +
    ggplot2::theme_classic() +
    ggplot2::ylab(ylabel) +
    ggplot2::xlab("Effect")
  
  if(type =="ind_id") {
    p1 <- p1 + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                              axis.ticks.y = ggplot2::element_blank())
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

