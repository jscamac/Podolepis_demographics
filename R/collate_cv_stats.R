#' Collates Expected Log Pointwise Predictive Density (ELPD) 
#' from cross validation
#'
#' Collates elpd summary statistics across cross validated models
#'  and converts SE to 95% confidence intervals
#' @param x A list of loo fits obtained from fitting loo::loo or loo:kfold
#' @return A `data.frame`
#' @importFrom dplyr mutate bind_rows
#' @importFrom magrittr %>%
#' @importFrom tibble tibble as_tibble
#' @export
collate_cv_stats <- function(x) {
  lapply(x, function(x) {
    tibble::tibble(Model = attr(x, "model_name"), 
                   tibble::as_tibble(x$estimates)[
                     which(grepl("elpd",rownames(x$estimates))),])}) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(lci = Estimate - (1.96 * SE),
                  uci = Estimate + (1.96 * SE)) %>%
    dplyr::arrange(Estimate) %>%
    dplyr::mutate(Rank = dplyr::row_number())
}