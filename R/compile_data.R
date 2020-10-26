#' Compile data for survival or growth analysis
#'
#' Prepares raw data for input into survival or growth analysis
#' 
#' @param file Path to csv file
#' @param type Character. Can be either survival (default) or growth
#' @return tibble
#' @author James Camac (\email{james.camac@gmail.com})
#' @importFrom readr read_csv cols_only
#' @importFrom dplyr mutate filter group_by ungroup arrange lead select
#' @importFrom magrittr '%>%'
#' @importFrom forcats as_factor fct_inorder
#' @export

compile_data <- function(file, type) {
  
  if(!type %in% c("survival","growth")) {
    stop("type can only be 'survival' or 'growth")
  }
  
  out <- readr::read_csv(file = file,
                         col_types = readr::cols_only(
                           site = "i",
                           elevation = "i",
                           aspect = "c",
                           treatment = 'c',
                           seedling = "i",
                           date = readr::col_date(format = "%d/%m/%y"),
                           survival = "i",
                           leaf_length = "d")) %>%
    dplyr::mutate(
      # Create Julian date column
      julian_start = julian(date, origin = min(date))/365.25,
      # Convert survival to death
      death = 1-survival,
      # Create unique site id
      site_id = as.integer(forcats::fct_inorder(forcats::as_factor(site))),
      # Create unique ind id
      ind_id = as.integer(forcats::fct_inorder(paste(site, seedling)))) %>%
    # Remove deaths in first census (these are attributed to transplant shock)
    dplyr::filter(!(death ==1 & date =="2017-03-13")) %>%
    dplyr::arrange(ind_id, date) %>%
    dplyr::group_by(ind_id) %>%
    dplyr::mutate(next_date = dplyr::lead(date, 1),
                  death_next = dplyr::lead(death, 1),
                  julian_next = dplyr::lead(julian_start, 1)) %>%
    
    # Remove records post-death
    dplyr::filter(!(death_next==1 & leaf_length ==0)) %>%
    dplyr::ungroup() %>%
    # Redefine individual id
    dplyr::mutate(ind_id = as.integer(
      forcats::fct_inorder(forcats::as_factor(ind_id))),
      treatment_id = ifelse(treatment=="control", 0, 1)) %>%
    # Add a census id column
    dplyr::mutate(census =  findInterval(date, 
                                         unique(.$date), 
                                         rightmost.closed=FALSE))
  
  if(type =="survival") {
    out <- out %>%
      dplyr::select(site_id, elevation, aspect, treatment,treatment_id, 
                    census, start_date = date, next_date, julian_start,
                    julian_next, ind_id, leaf_length, death_next) %>%
      dplyr::filter(!is.na(julian_next))
  }
  
  # Remove unneccesary columns to remove confusion
  # Preparing the data for mortality effectively prepares it for growth analysis
  if(type =="growth") {
    out <- out %>% 
      dplyr::arrange(site_id, ind_id, census) %>%
      dplyr::group_by(ind_id) %>%
      # First census (2017) will be treated as our baseline init size
      dplyr::mutate(init_leaf_length = dplyr::first(leaf_length),
                    # Rescale census by
                    census = census - 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(site_id, elevation, aspect, treatment,treatment_id,
                    census, start_date = date, julian_date = julian_start, 
                    ind_id, init_leaf_length, leaf_length) %>%
      # Remove first census (and any individuals that did not survive at least
      # to the first census)
      # Also need to remove the first census because we don't have climate data
      # prior to the first census
      dplyr::filter(census!=0)
  }
  return(out)
}