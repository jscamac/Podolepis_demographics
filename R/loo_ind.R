#' Run leave one out at individual level
#'
#' Run leave one out at individual level
#' 
#' @param model a brmsfit or stanreg object
#' @return loo object
#' @author James Camac (\email{james.camac@gmail.com})
#' @importFrom rstanarm log_lik loo
#' @export

loo_ind <- function(model) {
  ind_ids <- model$data$ind_id
  ll <- rstanarm::log_lik(model)
  ll <- apply(ll, 1L, function(row) tapply(row, ind_ids, sum))
  rstanarm::loo(ll)
}